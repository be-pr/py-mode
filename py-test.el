;; py-test.el -*- lexical-binding: t -*-

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

(require 'py-mode)
(require 'ert)

(defmacro py-test-with-temp-buffer (&rest body)
  "Run BODY in an `r-mode' buffer."
  (declare (indent 0) (debug body))
  `(with-temp-buffer
    (py-mode)
    ,@body))

(defmacro py-test-with-buffer (bufstr &rest body)
  "Run BODY with `buffer-string' set to BUFSTR.
Uppercase X in BUFSTR marks current point."
  (declare (indent 1) (debug t))
  `(let (case-fold-search endpos)
    (unless buffer-file-name
      (setf (buffer-string) ,bufstr)
      (goto-char (point-min))
      (when (re-search-forward "X" nil t 1)
        (replace-match "" t t))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "Y" nil t 1)
          (replace-match "" t t)
          (setq endpos (point))))
      (progn ,@body))))

(defun py-test-tab-level (bufstr)
  (py-test-with-buffer bufstr
    (indent-for-tab-command)
    (current-indentation)))

(defun py-test-nav-function (func bufstr &rest args)
  (declare (indent 1))
  (py-test-with-buffer bufstr
    (apply func args)
    (or (= (point) endpos)
        (progn (insert "<POINT>")
               (print (buffer-string))
               nil))))

(ert-deftest py-test-indentation ()
  (py-test-with-temp-buffer
    (should (zerop (py-test-tab-level "
if condition:
    pass

# Comment.
print()X")))
    ;; Backslash continuations.
    (should (= 4 (py-test-tab-level "
with expr1 as x, \\
expr2 as y, \\ X
expr3 as z:")))
    (should (= 4 (py-test-tab-level "
with expr1 as x, \\
expr2 as y:
X")))
    (should (= 4 (py-test-tab-level "
somevar = [
0,X
1,
]")))
    (should (zerop (py-test-tab-level "
somevar = [
0,
X]")))
    (should (= 11 (py-test-tab-level "
def func():
    acc = (spam,
Xbeans)")))
    ;; Keyword arguments.
    (should (= 9 (py-test-tab-level "
def func(arg=3,
X):
    pass")))
    (should (= 8 (py-test-tab-level "
def func(
    X):
    functionbody")))
    (should (= 8 (py-test-tab-level "
def func(
    Xlong_arg1,
    long_arg2) :
    functionbody")))
    (should (= 4 (py-test-tab-level "
def func(
    long_arg1,
    long_arg2):
    functionbodyX")))
    (should (= 8 (py-test-tab-level "
if (cond1 and
    Xcond2):
    return True")))
    ;; Dictionary elements.
    (should (= 8 (py-test-tab-level "d = {
    long_dictionary_key:
Xlong_dictionary_value,
")))
    (should (= 4 (py-test-tab-level "d = {
        long_dictionary_key:
            long_dictionary_value,
    X
}")))
    ;; Cf. http://google.github.io/styleguide/pyguide.html.
    (should (= 8 (py-test-tab-level "d = {
    long_dictionary_key:
   Xlong_dictionary_value,
}")))
    (should (= 9 (py-test-tab-level "d = {
    key: value1 +
value2, X
}")))
    (should (= 9 (py-test-tab-level "d = {
    key: value1 +
value2, X
}")))
    (should (= 11 (py-test-tab-level "d = {
    'key': 'value1'
'value2', X
}")))
    (should (= 4 (py-test-tab-level "d = {
    'key': 'value1'
'value2',
X
}")))
    (should (zerop (py-test-tab-level "d = {
    'key': 'value1'
'value2',
X }")))
    (should (= 11 (py-test-tab-level "d = {
    'key': 'value:1'
'value:2'X
}")))
    (should (= 8 (py-test-tab-level "d = {
    'key':
        'value:1'
X+ 'value:2'
}")))
    (should (= 9 (py-test-tab-level "d = {
    key: value
for key in ['x', 'y']
Xfor value in [1, 2]
}")))
    ;; Chained method calls.
    (should (= 6 (py-test-tab-level "
print(str(string)
.replace(',', '.')
.replace(' ', '')X
)")))
    ;; Keep already indented lines in place.
    (should (zerop (py-test-tab-level "
if condition:
    pass
return TrueX")))
    ;; Unless point is inside the indentation.
    (should (= 4 (py-test-tab-level "
if condition:
    pass
Xreturn True")))))

(ert-deftest py-test-beginning-of-defun ()
  (py-test-with-temp-buffer
    ;; Test `py-indent--beginning-of-continuation'.
    (should (py-test-nav-function 'beginning-of-defun "
Ywith expr1 as x, \\
expr2 as y, \\
Xexpr3 as z:"))
    (should (py-test-nav-function 'beginning-of-defun "
Ywith expr1 as x, \\
expr2 as y, \\ X
expr3 as z:"))
    (should (py-test-nav-function 'beginning-of-defun "
Ywith expr1 as x, \\
expr2 as y, \\
expr3 as z:
    pass
X"))
    (should (py-test-nav-function 'beginning-of-defun "
Ywith expr1 as x, \\
expr2 as y, \\
expr3 as z:
    Xpass"))
    ;; Test `py-indent--beginning-of-block-p'.
    (should (py-test-nav-function 'beginning-of-defun "
def func():
    pass

Ywith expr1 as x, \\
    expr2 as y, \\ \t
X    expr3 as z:
        pass"))
    (should (py-test-nav-function 'beginning-of-defun "
Yif (x == 0 and
    y == 1): X
    pass"))
    (should (py-test-nav-function 'beginning-of-defun "
Yif (x == 0 and
    # Comment.X
    y == 1):
    pass"))
    ;; Skip trailing comment with `py-indent--beginning-of-block-p'.
    (should (py-test-nav-function 'beginning-of-defun "
Yif x == 0 and y == 1: # Comment.X
    pass"))
    (should (py-test-nav-function 'beginning-of-defun "
if x == 0:
    pass
Yelse:
    X
"))
    (should (py-test-nav-function 'beginning-of-defun "
if x == 0:
    pass
Yelse:X"))
    (should (py-test-nav-function 'beginning-of-defun "
Yclass Test:

    def method1(self):
        pass
X"))
    (should (py-test-nav-function 'beginning-of-defun "
def func():
    pass

Yclass Test:X
    pass
"))
    (should (py-test-nav-function 'beginning-of-defun "

Ydef func():
X"))
    (should (py-test-nav-function 'beginning-of-defun "
def func():
    pass
X
Ydef func1():
    pass" -1))
    (should (py-test-nav-function 'beginning-of-defun "
class Test:
    def func():
        Yif condition:
            X
"))
    (should (py-test-nav-function 'beginning-of-defun "
Xdef func():
    pass
Ydef func1():
    pass
" -1))
    (should (py-test-nav-function 'beginning-of-defun "
X
def func():
    pass
Ydef func1():
    pass
" -2))
    (should (py-test-nav-function 'beginning-of-defun "
class Test:

    def func():X
        pass

    Ydef func1():
        pass
" -1))
    (should (py-test-nav-function 'beginning-of-defun "
Yclass Test:

    def func():
        pass
X
    def func1():
        pass
"))
    (should (py-test-nav-function 'beginning-of-defun "
Yclass Test:

    def func():
        pass
X"))))

(ert-deftest py-test-end-of-defun ()
  (py-test-with-temp-buffer
    (should (py-test-nav-function 'end-of-defun "
Xclass Test:
    def method1(self):
        pass
Y"))
    (should (py-test-nav-function 'end-of-defun "
def func():
    pass
Y
class Test:
    def func():
        pass
X" -1))
    (should (py-test-nav-function 'end-of-defun "
class Test:
    def func():
        pass
Y
    def func1():
        passX
" -1))))


(provide 'py-test)
;;; py-test.el ends here
