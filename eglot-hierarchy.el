;;; eglot-hierarchy.el --- Eglot hierarchical tree  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 by Sun Yi Ming

;; Author: Sun Yi Ming <dolmens@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The main idea was borrowed from lsp-treemacs,
;; but adapted to work with the builtin eglot and hierarchy neat API.
;;
;; eglot-hierarchy-type-hierarchy
;; eglot-hierarchy-call-hierarchy

;;; Code:

(require 'cl-lib)
(require 'eglot)
(require 'hierarchy)

(define-button-type 'eglot-hierarchy-file-button
  'follow-link t                        ; Click via mouse
  'face 'default)

(defconst eglot-hierarchy-buffer-name "*GLOT LSP Hierarchy*")

(defconst eglot-hierarchy-sub 0)
(defconst eglot-hierarchy-super 1)
(defconst eglot-hierarchy-both 2)

(defun eglot-hierarchy-type-hierarchy (direction)
  "Show the type hierarchy for the symbol at point.
With prefix 0 show sub-types.
With prefix 1 show super-types.
With prefix 2 show both which is default."
  (interactive "P")
  (eglot--server-capable-or-lose :typeHierarchyProvider)
  (let* ((eglot-current-server (eglot--current-server-or-lose))
	 (direction (or direction eglot-hierarchy-both))
	 (root (jsonrpc-request
		eglot-current-server
		:textDocument/typeHierarchy
		(thread-first
		  (eglot--TextDocumentPositionParams)
		  (plist-put :direction direction)
		  (plist-put :resolve 1))))
	 (hierarchy (hierarchy-new)))
    (when (null root)
      (eglot--error "No class under point"))

    (hierarchy-add-tree hierarchy root nil)

    ;; immediate nodes
    (hierarchy-add-trees
     hierarchy
     (eglot-hierarchy--type-children-nodes root direction)
     (lambda (_) root)
     (lambda (node)
       (condition-case err
	   (eglot--dbind (name direction uri range) node
	     (let ((resp (jsonrpc-request
			  eglot-current-server
			  :textDocument/typeHierarchy
			  `(:textDocument
			    (:uri ,uri)
			    :position ,(plist-get range :start)
			    :direction ,direction
			    :resolve 1))))
	       (eglot-hierarchy--type-children-nodes resp direction)))
	 (jsonrpc-error
	  (eglot--message "%s" (alist-get 'jsonrpc-error-message (cdr err)))
	  (eglot-clear-status eglot-current-server)
	  nil)))
     nil
     t)

    (pop-to-buffer
     (hierarchy-tree-display
      hierarchy
      (lambda (node _)
	(eglot--dbind
	    (name direction uri range) node
	  (insert-text-button
	   (concat name (cond ((eq direction eglot-hierarchy-sub)
			       (propertize " ↓" 'face 'shadow))
			      ((eq direction eglot-hierarchy-super)
			       (propertize " ↑" 'face 'shadow))))
	   :type 'eglot-hierarchy-file-button
	   'action
	   (lambda (btn)
	     ;; avoid this window not selected and returned
	     (let ((w (get-buffer-window (marker-buffer btn))))
	       (when w (select-window w)))
	     (eglot-hierarchy--open-file-in-mru (eglot--uri-to-path uri))
	     (goto-char (eglot--lsp-position-to-point
			 (plist-get range :start)))))))
      (get-buffer-create eglot-hierarchy-buffer-name)))

    ;; hierarchy.el doesn't have an :open property,
    ;; so we open the first level by a click.
    (goto-char (point-min))
    (widget-button-press (point))))

(defun eglot-hierarchy--type-children-nodes (node direction)
  (eglot--dbind (children parents) node
    (append
     (when (or (= direction eglot-hierarchy-super)
	       (= direction eglot-hierarchy-both))
       (mapcar (lambda (child)
		 (plist-put child :direction eglot-hierarchy-super))
	       (cl-sort (copy-sequence parents) #'string<
			:key (lambda (p) (plist-get p :name)))))
     (when (or (= direction eglot-hierarchy-sub)
	       (= direction eglot-hierarchy-both))
       (mapcar (lambda (child)
		 (plist-put child :direction eglot-hierarchy-sub))
	       (cl-sort (copy-sequence children) #'string<
			:key (lambda (p) (plist-get p :name))))))))

(defun eglot-hierarchy--open-file-in-mru (file)
  (select-window (get-mru-window (selected-frame) nil :not-selected))
  (find-file file))

(defun eglot-hierarchy-call-hierarchy (outgoing)
  "Show the incoming call hierarchy for the symbol at point.
With a prefix argument, show the outgoing call hierarchy."
  (interactive "P")
  (eglot--server-capable-or-lose :callHierarchyProvider)
  (let* ((eglot-current-server (eglot--current-server-or-lose))
	 (root (jsonrpc-request
		eglot-current-server
		:textDocument/prepareCallHierarchy
		(eglot--TextDocumentPositionParams)))
	 (hierarchy (hierarchy-new)))
    (when (= (length root) 0)
      (eglot--error "Not in calling hierarchy under point"))
    (hierarchy-add-trees
     hierarchy
     root
     nil
     (lambda (node)
       (condition-case err
	   (let ((resp (jsonrpc-request eglot-current-server
					(if outgoing
					    :callHierarchy/outgoingCalls
					  :callHierarchy/incomingCalls)
					`(:item ,node))))
	     (seq-map (lambda (item) (plist-get item :from)) resp))
	 (jsonrpc-error
	  (eglot--message "%s" (alist-get 'jsonrpc-error-message (cdr err)))
	  (eglot-clear-status eglot-current-server) nil)))
     nil
     t
     )

    (pop-to-buffer
     (hierarchy-tree-display
      hierarchy
      (lambda (node _)
	(eglot--dbind
	    (name direction uri range) node
	  (insert-text-button
	   name
	   :type 'eglot-hierarchy-file-button
	   'action
	   (lambda (btn)
	     ;; avoid this window not selected and returned
	     (let ((w (get-buffer-window (marker-buffer btn))))
	       (when w (select-window w)))
	     (eglot-hierarchy--open-file-in-mru (eglot--uri-to-path uri))
	     (goto-char (eglot--lsp-position-to-point
			 (plist-get range :start)))))))
      (get-buffer-create eglot-hierarchy-buffer-name)))

    ;; hierarchy.el doesn't have an :open property,
    ;; so we open the first level by a click.
    (goto-char (point-min))
    (widget-button-press (point))))
