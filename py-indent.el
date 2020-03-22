;; py-indent.el -*- lexical-binding: t -*-

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

(require 'cl-lib)

;; The Python lexer considers everything following a semicolon to be a block
;; statement. The lines after the start of a block must be indented over by at
;; least one space character.
(defun py-indent--beginning-of-block-p ()
  (save-excursion
    (end-of-line)
    ;; Find the last line in a multiline condition.
    (let ((state (syntax-ppss)))
      (unless (zerop (car state))
        (goto-char (scan-lists (car (nth 9 state)) 1 0))
        (end-of-line)))
    ;; Skip trailing comment.
    (let ((state (syntax-ppss)))
      (when (nth 4 state)
        (goto-char (nth 8 state))))
    ;; Skip any whitespace character following the backslash.
    (skip-chars-backward " \t")
    ;; Jump over backslash continuations.
    (while (= (preceding-char) ?\\)
      ;; Skip comment lines.
      (forward-comment (point-max))
      (end-of-line)
      (skip-chars-backward " \t"))
    (and (= (preceding-char) ?:)
         ;; Not inside string or comment.
         (not (nth 8 (syntax-ppss))))))

(defun py-indent--dedent (arg)
  (unless (zerop (current-indentation))
    (delete-region
     (point) (+ (point)
                (min (current-indentation)
                     (* (abs arg) tab-width))))))

(defun py-indent--cmd-create (func)
  (lambda (arg)
    (interactive "p")
    (save-excursion
      (cond
        ((region-active-p)
         (save-excursion
           (let ((opoint (point-marker)))
             (move-marker opoint (region-end))
             (goto-char (region-beginning))
             (forward-line 0)
             (while (< (point) opoint)
               (funcall func arg)
               (forward-line 1))
             (set-marker opoint nil))))
        ((progn (forward-line 0) nil))
        ((py-indent--beginning-of-block-p)
         (let ((level (current-indentation)))
           (funcall func arg)
           (while (and (zerop (forward-line 1))
                       (> (current-indentation) level))
             (funcall func arg))))
        (t (funcall func arg)
           ;; Move parenthetical constructs starting on the current line.
           (let ((pos (point)))
             (end-of-line)
             (let* ((state (syntax-ppss))
                    (start (or (nth 1 state)
                               (and (nth 3 state) (nth 8 state)))))
               (when (and start (<= pos start))
                 (let ((end (point-marker)))
                   (set-marker end (if (nth 3 state)
                                       (scan-sexps start 1)
                                     (scan-lists start 1 0)))
                   (while (and (zerop (forward-line 1))
                               (< (point) end))
                     (funcall func arg))
                   (set-marker end nil))))))))
    (when (< (current-column) (current-indentation))
      (skip-chars-forward " \t"))))

(defalias 'py-indent-dedent
    (py-indent--cmd-create #'py-indent--dedent))
(defalias 'py-indent-indent
    (py-indent--cmd-create
     (lambda (arg) (indent-to (* arg tab-width)))))

;; Only the first physical line of a backslash continuation determines the
;; indentation level.
(defun py-indent--beginning-of-continuation ()
  (let (pos)
    (save-excursion
      (forward-comment (- (point)))
      (skip-chars-backward " \t")
      (while (= (preceding-char) ?\\)
        (forward-line 0)
        (setq pos (point))
        (forward-comment (- (point)))
        (skip-chars-backward " \t")))
    (when pos (goto-char pos))))

(define-inline py-indent--eolp ()
  ;; EOL or comment start syntax.
  (inline-quote
   (or (eolp) (eq (char-syntax (following-char)) ?<))))

(defun py-indent-function ()
  (let ((col (current-column))
        forward-sexp-function level)
    (save-excursion
      (forward-line 0)
      (skip-chars-forward " \t")
      (cond
        ;; Inside strings, fall back to the default value of
        ;; `indent-line-function'.
        ((nth 3 (syntax-ppss)) (setq level 'noindent))
        ;; Increase indentation level after backslash continuations.
        ((py-indent--beginning-of-continuation)
         (setq level (+ (current-indentation) tab-width)))
        ;; Increase indentation level after colons.
        ((save-excursion
           (forward-comment (- (point)))
           (when (= (preceding-char) ?:)
             (forward-line 0)
             (if (zerop (car (syntax-ppss)))
                 (py-indent--beginning-of-continuation)
               ;; Key-value pair definition in a dictionary.
               (let ((openparen (car (nth 9 (syntax-ppss)))))
                 (unless (eq (char-after openparen) ?\{)
                   (goto-char openparen))))
             (setq level (+ (current-indentation) tab-width))
             t)))
        ;; Multiline parenthetical grouping.
        ((not (zerop (car (syntax-ppss))))
         ;; Align a hanging closing paren on its own line with the first
         ;; non-whitespace character on the line that starts the multiline
         ;; construct (note that PEP-8 comes with two different suggestions).
         (let ((openparen (car (last (nth 9 (syntax-ppss)))))
               (closeparen-hanging-p
                (save-excursion
                  (and (eq (char-syntax (following-char)) ?\))
                       (skip-syntax-forward ")")
                       (skip-chars-forward " \t")
                       (py-indent--eolp)))))
           (forward-comment (- (point)))
           (cond
             ;; Inside dictionaries, align values with their keys.
             ((and (not closeparen-hanging-p)
                   (eq (char-after openparen) ?\{)
                   (not (= (preceding-char) ?,))
                   ;; Catch the `scan-error' on `forward-sexp' at openparen.
                   (condition-case nil
                       (progn
                         (while (not (memq (preceding-char) '(?, ?:)))
                           (forward-sexp -1)
                           (forward-comment (- (point))))
                         t)
                     (scan-error nil)))
              (skip-chars-forward " \t")
              (setq level (if (eolp)
                              (+ (current-indentation) tab-width)
                            (current-column))))
             ((progn (goto-char (1+ openparen)) nil))
             ((save-excursion
                (skip-chars-forward " \t")
                ;; I.e., we're at a hanging paren.
                (py-indent--eolp))
              (setq level
                    (+ (current-indentation)
                       (cond (closeparen-hanging-p 0)
                             ;; Add extra indentation to a block statement's
                             ;; multiline condition expression to visually
                             ;; distinguish it from the statement's body.
                             ((py-indent--beginning-of-block-p)
                              (* tab-width 2))
                             (t tab-width)))))
             ;; Line up with openparen.
             (t (let ((col (current-column)))
                  ;; Distinguish a block statement's condition expression from
                  ;; the body if necessary.
                  (if (and (py-indent--beginning-of-block-p)
                           (= col (+ (current-indentation)
                                     tab-width)))
                      (setq level (+ col tab-width))
                    (setq level col)))))))
        ;; Unless point is inside the indentation, keep an already indented
        ;; line in place.
        ((not (or (looking-at "[ \t]*$")
                  (<= col (current-indentation))))
         (setq level (current-indentation)))
        ;; Catch-all: line up with preceding line or parenthetical grouping.
        (t (let ((oparen (car (nth 9 (syntax-ppss)))))
             (forward-line -1)
             (let ((new-oparen (car (nth 9 (syntax-ppss)))))
               (unless (equal oparen new-oparen)
                 (goto-char new-oparen)))
             (setq level (current-indentation))))))
    (when (and (numberp level) (/= level (current-indentation)))
      ;; Use a marker to find the right position after indenting --
      ;; cf. `indent-relative'.
      (let ((opoint (point-marker)))
        (forward-line 0)
        (let ((beg (point)))
          (skip-chars-forward " \t")
          (delete-region beg (point)))
        (indent-to level 0)
        (when (> opoint (point))
          (goto-char opoint))
        (set-marker opoint nil)))
    (when (< (current-column) (current-indentation))
      (skip-chars-forward " \t"))
    level))


(provide 'py-indent)
;;; py-indent.el ends here
