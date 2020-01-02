;; py-eldoc.el -*- lexical-binding: t -*-

;; Copyright (c) 2019 Bernhard Pröll

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'py-repl)
(eval-when-compile (require 'subr-x)) ;when-let*, string-blank-p

(declare-function py--object-at-point "py-mode")

(defun py-eldoc--openparen ()
  (if (eq major-mode 'py-repl-mode)
      (let ((pos (nth 1 (syntax-ppss))))
        (unless (and pos (<= pos (cdr comint-last-prompt)))
          pos))
    (nth 1 (syntax-ppss))))

(defun py-eldoc--function-name ()
  (when-let* ((openparen (py-eldoc--openparen)))
    (when (eq (char-after openparen) ?\()
      (save-excursion
        (goto-char openparen)
        (skip-chars-backward " \t")
        (py--object-at-point)))))

(defun py-eldoc--create ()
  (let (last-func last-sig)
    (lambda ()
      (when-let*
          ((func (py-eldoc--function-name))
           (buf (py-repl-process-buffer))
           (proc (get-buffer-process buf)))
        (if (equal func last-func)
            (eldoc-message last-sig)
          (py-repl-send proc
            (format "_get_signature(%S, globals())" func))
          (when-let* ((sig py-repl-output))
            (unless (string-blank-p sig)
              (setq last-func func)
              (when (string-match "\\(.*\\)\n" sig)
                (setq last-sig (match-string 1 sig))
                (eldoc-message last-sig)))))))))

(defalias 'py-eldoc-documentation-function (py-eldoc--create))


(provide 'py-eldoc)
;;; py-eldoc.el ends here
