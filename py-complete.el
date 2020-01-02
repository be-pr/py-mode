;; py-complete.el -*- lexical-binding: t -*-

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

(require 'pcase)
(require 'py-repl)
(require 'py-eldoc)

(declare-function py--object-at-point "py-mode")

(defun py-complete--get-completions (proc name callfunc fullrefs)
  (py-repl-send proc
    (format "_lispify(_completer.get_completions(%S, %S, %s))"
            name callfunc fullrefs))
  (when py-repl-output
    (condition-case nil
        (read py-repl-output)
      (invalid-read-syntax nil))))

(defun py-complete--table-create (&optional fullrefs)
  (let (otick opoint table)
    (lambda (name pred flag)
      (pcase flag
        ('t (all-completions name table pred))
        ('nil
         (or (and (eq otick (buffer-modified-tick))
                  (eq opoint (point)))
             (input-pending-p)
             (let ((proc (get-buffer-process (py-repl-process-buffer))))
               (when proc
                 (let ((callfunc (or (py-eldoc--function-name) ""))
                       (id (or (py--object-at-point) "")))
                   (setq table (py-complete--get-completions
                                proc id callfunc
                                (if fullrefs "True" "False")))
                   (setq opoint (point))
                   (setq otick (buffer-modified-tick))))))
         (try-completion name table pred))
        ('metadata '(metadata (category . pymode)))))))

(defalias 'py-complete-table (py-complete--table-create))

(defun py-completion-function ()
  (save-excursion
    (skip-syntax-forward "w_")
    (let ((end (point)))
      (skip-syntax-backward "w_")
      (when (or (/= end (point)) (= (preceding-char) ?.))
        (list (point) end
              #'py-complete-table
              :exclusive 'no)))))


(provide 'py-complete)
;;; py-complete.el ends here
