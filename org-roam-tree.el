;;; org-roam-tree.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Brad Stewart
;;
;; Author: Brad Stewart <brad@bradstewart.ca>
;; Maintainer: Brad Stewart <brad@bradstewart.ca>
;; Created: décembre 31, 2025
;; Modified: décembre 31, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/brad/org-roam-tree
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(cl-defun org-roam-tree-backlinks-section (node &key (section-heading "Backlinks Tree:"))
  "A tree-style backlinks section for NODE, grouping by source file."
  (when-let ((tree (org-roam-tree-backlinks node)))
    (magit-insert-section (org-roam-tree-backlinks)
      ;; Top-level heading
      (magit-insert-heading section-heading)
      
      ;; Iterate over files
      (dolist (file-entry tree)
        (let ((file (car file-entry))
              (nodes (cdr file-entry)))
          ;; File-level section (collapsible)
          (magit-insert-section (org-roam-tree-file file)
            (magit-insert-heading (file-name-nondirectory file))
            
            ;; Optional: add indentation / vertical guide here
            
            ;; Iterate over nodes in this file
            (dolist (n nodes)
              (org-roam-node-insert-section
               :source-node n
               :point (org-roam-node-point n)
               :properties nil))) ;; or copy properties as needed
          ;; Optional newline between files
          (insert ?\n))))))


(defun org-roam-tree-backlinks (&optional node)
  "Return backlinks of NODE grouped by source file.

Return value:
  ((FILE . (NODE NODE ...)) ...)

NODE defaults to `org-roam-node-at-point` if nil."
  (require 'org-roam)
  (let* ((node (or node (org-roam-node-at-point)))
         (backlinks (org-roam-backlinks-get node :unique t))
         (table (make-hash-table :test 'equal)))
    (dolist (bl backlinks)
      (let* ((src (org-roam-backlink-source-node bl))
             (file (org-roam-node-file src)))
        (when src
          (puthash file
                   (cons src (gethash file table))
                   table))))
    (let (result)
      (maphash
       (lambda (file nodes)
         (push (cons file (nreverse nodes)) result))
       table)
      result)))


(provide 'org-roam-tree)
;;; org-roam-tree.el ends here
