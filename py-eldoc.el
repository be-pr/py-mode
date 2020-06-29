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

(defun py-eldoc--openparen ()
  (let* ((state (syntax-ppss))
         (start (nth 1 state)))
    (and start
         (not (nth 4 state))
         (let ((prompt (cdr comint-last-prompt)))
           (unless (and prompt (< start prompt))
             start)))))

(defun py-eldoc--function-name ()
  (let ((openparen (py-eldoc--openparen)))
    (when (and openparen (eq (char-after openparen) ?\())
      (save-excursion
        (goto-char openparen)
        (skip-chars-backward " \t")
        (let ((bds (py-repl--primary-bounds t)))
          (when bds (apply #'buffer-substring-no-properties bds)))))))

(defun py-eldoc--create ()
  (let (last-func last-sig)
    (lambda ()
      (let ((func (py-eldoc--function-name)))
        (cond
          ((not func) nil)
          ((equal func last-func) (eldoc-message last-sig))
          (t (let* ((buf (py-repl-process-buffer))
                    (proc (get-buffer-process buf)))
               (when proc
                 (let ((result
                        (py-repl-send proc nil
                          "_get_signature('''" func "''',globals())")))
                   (unless (string= result "")
                     (setq last-sig result)
                     (setq last-func func)
                     (eldoc-message last-sig)))))))))))

(defalias 'py-eldoc-documentation-function (py-eldoc--create))


(provide 'py-eldoc)
;;; py-eldoc.el ends here
