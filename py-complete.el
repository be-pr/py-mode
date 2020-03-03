;; py-complete.el -*- lexical-binding: t -*-

;; Copyright (c) 2019, 2020 Bernhard Pröll

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

(defun py-complete-completion-table (str)
  (let* ((callfn (py-eldoc--function-name))
         (buf (py-repl-process-buffer))
         (process (get-buffer-process buf)))
    (when (process-live-p process)
      (py-repl-send process t
        "_lispify(_completer.get_completions('" str "','"
        callfn "'))"))))

(defun py-completion-function ()
  (save-excursion
    (skip-syntax-forward "w_")
    (let ((end (point)))
      (skip-syntax-backward "w_")
      (while (= (preceding-char) ?.)
        (skip-chars-backward ".")
        (skip-syntax-backward "w_"))
      (when (/= end (point))
        (let* ((buf (py-repl-process-buffer))
               (process (get-buffer-process buf))
               (name (buffer-substring-no-properties (point) end)))
          (when (process-live-p process)
            (list (point) end
                  (py-complete-completion-table name)
                  :exclusive 'no)))))))


(provide 'py-complete)
;;; py-complete.el ends here
