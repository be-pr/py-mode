;;; py-xref.el --- Find definitions in Python -*- lexical-binding: t -*-


;; Copyright (c) 2018, 2019 Bernhard Pröll

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

(require 'py-repl)
(require 'py-complete)
(require 'xref)
(require 'pcase)
(eval-when-compile (require 'subr-x)) ;string-empty-p, string-trim-right

(declare-function py--object-at-point "py-mode")

(defun py-xref-backend () 'python)

(cl-defmethod xref-backend-identifier-at-point
    ((_backend (eql python)))
  (py--object-at-point))

(cl-defmethod xref-backend-definitions ((_backend (eql python)) str)
  (let ((proc (get-buffer-process (py-repl-process-buffer))))
    (if (not proc)
        (py-xref--local-make str)
      (if (py-repl-send proc
            (format "print(_get_location(%S))" str))
          (when py-repl-output
            (let ((out (string-trim-right py-repl-output "\n")))
              (unless (string-empty-p out)
                (pcase (read out)
                  (`(quote ,_)
                    (or (py-xref--local-make str)
                        (error out)))
                  (`(,file . ,line)
                    (if (equal file "<stdin>")
                        (error "%s defined at %s" str file)
                      (py-xref--make str file line 0)))
                  ('None (error "Failed to locate %s" str))
                  ;; If all fails, search current buffer.
                  (_ (or (py-xref--local-make str)
                         (error out)))))))
        (py-xref--local-make str)))))

(defvar py-xref-rx-fmt
  "^[ \t]*\\(?:\\(?:async[ \t]+\\)?def\\|class\\)[ \t]+\\(%s\\)\\_>")

(defun py-xref--search-inner-definition (str)
  ;; Possibly strip "self" or "cls" from local definitions.
  (when (string-match "[^.]+\\.\\([^.]+\\)" str)
    (setq str (match-string 1 str)))
  (let (limit)
    (funcall beginning-of-defun-function)
    (while (not (bolp))
      (funcall beginning-of-defun-function))
    (setq limit (line-beginning-position))
    (funcall end-of-defun-function)
    (re-search-backward (format py-xref-rx-fmt str) limit t 1)))

(defun py-xref--local-make (str)
  (save-excursion
    (when (or (py-xref--search-inner-definition str)
              (progn (goto-char (point-max))
                     (re-search-backward
                      (format py-xref-rx-fmt str) nil t 1)))
      (let ((file (buffer-file-name)))
        (if file
            ;; For consistency with what we get out from _get_location, simply
            ;; jump to column 0.
            (py-xref--make str file (line-number-at-pos) 0)
          (py-xref--make str (current-buffer) (point-at-bol)))))))

(defun py-xref--make (str loc &rest rest)
  (list (xref-make
         str (apply (if (bufferp loc)
                        #'xref-make-buffer-location
                      #'xref-make-file-location)
                    loc rest))))

(defalias 'py-xref-table (py-complete--table-create t))

(cl-defmethod xref-backend-identifier-completion-table
    ((_backend (eql python)))
  #'py-xref-table)


(provide 'py-xref)
;;; py-xref ends here
