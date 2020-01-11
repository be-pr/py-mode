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

(defun py-complete--get-completions (proc &optional name callfunc)
  (py-repl-send proc
    (concat "_lispify(_completer.get_completions('"
            name "', '" callfunc "'))"))
  (when py-repl-output
    (condition-case nil
        (read py-repl-output)
      (invalid-read-syntax nil))))

(defun py-complete--table-create (&optional func)
  (let (table oldname)
    (lambda (name pred flag)
      (pcase flag
        ('t (all-completions name table pred))
        ('nil (or (equal name oldname)
                  (input-pending-p)
                  (let* ((buf (py-repl-process-buffer))
                         (process (get-buffer-process buf)))
                    (when (process-live-p process)
                      (setq table (py-complete--get-completions
                                   process name func))
                      (setq oldname name))))
              (try-completion name table pred))
        ('metadata '(metadata (category . pymode)))))))

(defun py-completion-function ()
  (save-excursion
    (skip-syntax-forward "w_")
    (let ((end (point)))
      (skip-syntax-backward "w_")
      (while (= (preceding-char) ?.)
        (skip-chars-backward ".")
        (skip-syntax-backward "w_"))
      (when (/= end (point))
        (let ((func (py-eldoc--function-name)))
          (list (point) end
                (py-complete--table-create func)
                :exclusive 'no))))))


(provide 'py-complete)
;;; py-complete.el ends here
