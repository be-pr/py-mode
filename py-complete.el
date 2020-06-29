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

(require 'py-repl)
(require 'py-eldoc)

(defun py-complete--table-create ()
  (let (table last-str)
    (lambda (str pred action)
      (cond
        ((eq action 'metadata) '(metadata (category . py-mode)))
        ((eq action 'lambda) nil)
        ((eq (car-safe action) 'boundaries)
         (let ((suffix (cdr action)))
           (cons 'boundaries
                 (cons (if (string-match "\\.[^.]*\\'" str)
                           (1+ (match-beginning 0))
                         0)
                       (if (string-match "\\." suffix)
                           (match-beginning 0)
                         (length suffix))))))
        (t (let (primary)
             (when (string-match "\\.[^.]*\\'" str)
               (setq primary (substring str 0 (match-beginning 0)))
               (setq str (substring str (1+ (match-beginning 0)))))
             (unless (equal last-str str)
               (setq last-str str)
               (let* ((buffer (py-repl-process-buffer))
                      (process (get-buffer-process buffer)))
                 (when process
                   (let ((callfn (py-eldoc--function-name)))
                     (setq table (py-repl-send process t
                                   "_lispify(_completer.get_completions('"
                                   primary "','" callfn "'))"))))))
             (if action
                 (all-completions str table pred)
               (let ((try (try-completion str table pred)))
                 (if (and primary (stringp try))
                     (concat primary "." try)
                   try)))))))))

(defvar py-complete-completion-table (py-complete--table-create))

(defvar py-mode-syntax-table)

(defvar py-complete-syntax-table
  (let ((table py-mode-syntax-table))
    (modify-syntax-entry ?. "_" table)
    table))

(defun py-completion-function ()
  (with-syntax-table py-complete-syntax-table
    (save-excursion
      (skip-syntax-forward "w_")
      (let ((end (point)))
        (skip-syntax-backward "w_")
        (when (> end (point))
          (list (point) end py-complete-completion-table))))))


(provide 'py-complete)
;;; py-complete.el ends here
