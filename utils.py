# Copyright (c) 2019 Bernhard Pröll

# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
# more details.

# You should have received a copy of the GNU General Public License along with
# this program. If not, see <http://www.gnu.org/licenses/>.

import sys
import re
import types
import linecache
import inspect
import keyword

def _get_signature(string, globals):
    try:
        obj = eval(string, globals)
    except Exception:
        pass
    else:
        if callable(obj):
            if hasattr(obj, '__name__'):
                string = obj.__name__
            try:
                return print(
                    string, str(inspect.signature(obj)), sep='')
            except ValueError:
                pass
            try:
                doc = inspect.getdoc(obj).splitlines()
                if doc:
                    print(string, ' -> ', doc[0], sep='')
            except ValueError:
                pass

class _Completer:
    def __init__(self):
        self.defaults = dir(__builtins__) + keyword.kwlist

    def get_completions(self, cstring, callfunc):
        primaries = cstring.split('.')
        identifier = primaries[-1]
        primary = '.'.join(primaries[0:-1])
        acc = []
        if primary:
            try:
                obj = eval(primary)
                if hasattr(obj, '__class__'):
                    # Remove duplicates.
                    acc = list(set(self._get_attrs(obj)))
                else:
                    acc = dir(obj)
                return [primary + "." + id for id in acc]
            except (NameError, SyntaxError):
                pass
        else:
            try:
                func = eval(callfunc)
                # Keyword-only parameters.
                acc = [kw + '=' for kw in func.__kwdefaults__]
            except (NameError, SyntaxError, AttributeError):
                pass
            return list(set(self.defaults + list(globals()) + acc))

    @staticmethod
    def _get_attrs(obj):
        acc = dir(obj)
        if hasattr(obj, '__bases__'):
            for base in obj.__bases__:
                acc.extend(_Completer._get_attrs(base))
        return acc

_completer = _Completer()

def _unfold(iterable):
    try:
        return [list(x) for x in iterable]
    except TypeError:
        return list(iterable)

def _lispify(expr):
    if isinstance(expr, list):
        print(str(expr)
              .replace('[', '(')
              .replace(']', ')')
              .replace(',', '')
              .replace("'", '"'),
              end='')
    else:
        print('()', end='')

# See inspect.getfile().
def _get_location(string):
    """Return an object's location for use in xref-find-definitions."""
    if string in keyword.kwlist:
        return string + ' is a builtin keyword'
    try:
        object = eval(string)
    except NameError:
        return '{!r} not found'.format(string)
    if isinstance(object, types.ModuleType):
        if hasattr(object, '__file__'):
            return '("{}" . {})'.format(object.__file__, 0)
        return 'Failed to locate {!r}'.format(object)
    # Check whether it's a class. Classes come with a __module__ attribute.
    if isinstance(object, type):
        if hasattr(object, '__module__'):
            module = sys.modules.get(object.__module__)
            if hasattr(module, '__file__'):
                file = module.__file__
                # Check cache for validity.
                linecache.checkcache(file)
                name = object.__name__
                pat = re.compile(r'^(\s*)class\s+' + name + r'[:( ]')
                i = 0
                for line in linecache.getlines(file):
                    i += 1
                    if pat.match(line):
                        return '("{}" . {})'.format(file, i)
        return 'Failed to locate {!r}'.format(object)
    if isinstance(object, (types.BuiltinFunctionType,
                           types.BuiltinMethodType)):
        return 'Failed to locate {!r}'.format(object)
    if isinstance(object, types.MethodType):
        object = object.__func__
    if isinstance(object, types.FunctionType):
        object = object.__code__
    if isinstance(object, types.TracebackType):
        object = object.tb_frame
    if isinstance(object, types.FrameType):
        object = object.f_code
    if (isinstance(object, types.CodeType) and
        hasattr(object, 'co_firstlineno')):
        return ('("{}" . {})'
                .format(object.co_filename,
                        object.co_firstlineno))
