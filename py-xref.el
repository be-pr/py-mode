;;; py-xref.el --- Find definitions in Python -*- lexical-binding: t -*-


;; Copyright (c) 2018, 2019, 2020 Bernhard Pröll

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'xref)
(require 'pcase)
(require 'py-repl)

(defvar py--def-rx)

(defun py-xref-backend () 'python)

(cl-defmethod xref-backend-identifier-at-point
    ((_backend (eql python)))
  (save-excursion
    (skip-syntax-forward "w_")
    (let ((bds (py-repl--primary-bounds t)))
      (when bds (apply #'buffer-substring-no-properties bds)))))

(cl-defmethod xref-backend-definitions ((_backend (eql python)) str)
  (let ((proc (get-buffer-process (py-repl-process-buffer))))
    (if (not proc)
        (py-xref--local-make str)
      (let ((result (py-repl-send proc t
                      "print(_get_location('" str "'))")))
        (pcase result
          (`(quote ,_)
            (or (py-xref--local-make str)
                (user-error result)))
          (`(,file . ,line)
            (if (equal file "<stdin>")
                (or (py-xref--local-make str)
                    (user-error "%s defined at <stdin>" str))
              (py-xref--make str file line 0)))
          ('None (user-error "Failed to locate %s" str))
          ;; If all fails, search current buffer.
          (_ (or (py-xref--local-make str)
                 (and result (user-error result)))))))))

(defun py-xref--find-inner-definition (str)
  (when (string-match "\\(?:self\\|cls\\)\\.\\([^.]+\\)" str)
    (setq str (match-string 1 str)))
  (cl-loop with level = (current-indentation)
           and nenv = (py-xref--nenv)
           and rx = (concat py--def-rx str "\\_>")
           and limit
           until (zerop level) do
           (funcall beginning-of-defun-function)
           ;; Find enclosing block statement.
           (while (= (current-indentation) level)
             (funcall beginning-of-defun-function))
           (setq limit (line-beginning-position))
           (funcall end-of-defun-function)
           if (cl-loop while (re-search-backward rx limit t 1)
                       unless (> (py-xref--nenv) nenv)
                       return t)
           return t
           else do
           (goto-char limit)
           (setq level (current-indentation))))

(defun py-xref--nenv ()
  (save-excursion
    (let ((inhibit-changing-match-data t)
          (n 0))
      (while (not (zerop (current-indentation)))
        (funcall beginning-of-defun-function)
        (if (looking-at py--def-rx) (cl-incf n)))
      n)))

(defun py-xref--find-top-level-definition (str)
  (save-excursion
    (goto-char (point-min))
    (cl-loop with rx = (concat py--def-rx str "\\_>")
             while (re-search-forward rx nil t 1)
             when (zerop (py-xref--nenv))
             return t)))

(defun py-xref--local-make (str)
  (save-excursion
    (when (or (py-xref--find-inner-definition str)
              (py-xref--find-top-level-definition str))
      (goto-char (match-beginning 0))
      (if buffer-file-name
          (py-xref--make str buffer-file-name (line-number-at-pos) 0)
        (py-xref--make str (current-buffer) (point-at-bol))))))

(defun py-xref--make (str loc &rest rest)
  (list (xref-make str (apply (if (bufferp loc)
                                  #'xref-make-buffer-location
                                #'xref-make-file-location)
                              loc rest))))

(defvar py-complete-completion-table)

(cl-defmethod xref-backend-identifier-completion-table
    ((_backend (eql python)))
  (or (featurep 'py-complete) (require 'py-complete))
  py-complete-completion-table)


(provide 'py-xref)
;;; py-xref ends here
