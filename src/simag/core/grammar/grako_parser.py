#!/usr/bin/env python
# -*- coding: utf-8 -*-

# CAVEAT UTILITOR
#
# This file was automatically generated by Grako.
#
#    https://pypi.python.org/pypi/grako/
#
# Any changes you make to it will be overwritten the next time
# the file is generated.


from __future__ import print_function, division, absolute_import, unicode_literals

from grako.parsing import graken, Parser
from grako.util import re, RE_FLAGS  # noqa


__version__ = (2016, 2, 23, 20, 11, 53, 1)

__all__ = [
    'SIMAGParser',
    'SIMAGSemantics',
    'main'
]


class SIMAGParser(Parser):
    def __init__(self,
                 whitespace=None,
                 nameguard=None,
                 comments_re='\\*((?:.|\\n)*?)\\*|#(.*?)\\n',
                 eol_comments_re='#.*?$',
                 ignorecase=None,
                 left_recursion=True,
                 **kwargs):
        super(SIMAGParser, self).__init__(
            whitespace=whitespace,
            nameguard=nameguard,
            comments_re=comments_re,
            eol_comments_re=eol_comments_re,
            ignorecase=ignorecase,
            left_recursion=left_recursion,
            **kwargs
        )

    @graken()
    def _block_(self):

        def block0():
            self._stmt_()
        self._positive_closure(block0)

        self._check_eof()

    @graken()
    def _stmt_(self):
        with self._choice():
            with self._option():
                self._multi_assert_()
                self.ast['assertion'] = self.last_node
            with self._option():
                with self._group():
                    with self._choice():
                        with self._option():
                            self._comp_type_()
                        with self._option():
                            self._icond_type_()
                        self._error('no available options')
                self.ast['stmt'] = self.last_node
            with self._option():
                with self._group():
                    with self._choice():
                        with self._option():
                            self._icond_expr_()
                        with self._option():
                            self._comp_expr_()
                        self._error('no available options')
                self.ast['rule'] = self.last_node
            self._error('no available options')

        self.ast._define(
            ['assertion', 'stmt', 'rule'],
            []
        )

    @graken()
    def _icond_type_(self):
        self._token('(')

        def block0():
            with self._group():
                with self._choice():
                    with self._option():
                        self._var_decl_()
                        self.ast['vars'] = self.last_node
                    with self._option():
                        self._skol_decl_()
                        self.ast['skol'] = self.last_node
                    self._error('no available options')
        self._closure(block0)
        self._icond_expr_()
        self.ast['expr'] = self.last_node
        self._token(')')

        self.ast._define(
            ['vars', 'skol', 'expr'],
            []
        )

    @graken()
    def _icond_expr_(self):
        self._token('(')
        self._expr_node_()
        self.ast['lhs'] = self.last_node
        self._icond_op_()
        self.ast['op'] = self.last_node
        self._icond_node_()
        self.ast['rhs'] = self.last_node
        self._token(')')

        self.ast._define(
            ['lhs', 'op', 'rhs'],
            []
        )

    @graken()
    def _icond_node_(self):
        with self._group():
            with self._choice():
                with self._option():
                    self._terminal_node_()
                with self._option():
                    self._icond_expr_()
                self._error('no available options')

    @graken()
    def _comp_type_(self):
        self._token('(')

        def block0():
            self._skol_decl_()
            self.ast['skol'] = self.last_node
        self._closure(block0)
        with self._group():
            with self._choice():
                with self._option():
                    self._multi_assert_()
                    self.ast['assertion'] = self.last_node
                with self._option():
                    self._comp_expr_()
                    self.ast['expr'] = self.last_node
                self._error('no available options')
        self._token(')')

        self.ast._define(
            ['skol', 'assertion', 'expr'],
            []
        )

    @graken()
    def _comp_expr_(self):
        self._token('(')
        self._expr_node_()
        self.ast['lhs'] = self.last_node
        self._logic_op_()
        self.ast['op'] = self.last_node
        self._expr_node_()
        self.ast['rhs'] = self.last_node
        self._token(')')

        self.ast._define(
            ['lhs', 'op', 'rhs'],
            []
        )

    @graken()
    def _expr_node_(self):
        with self._group():
            with self._choice():
                with self._option():
                    self._terminal_node_()
                with self._option():
                    self._comp_expr_()
                self._error('no available options')

    @graken()
    def _var_decl_(self):
        self._token('(')
        self._token('let')
        with self._group():
            self._term_()
            with self._optional():
                self._token(':')
                self._op_arg_()
        self.ast.setlist('@', self.last_node)

        def block1():
            self._token(',')
            with self._group():
                self._term_()
                with self._optional():
                    self._token(':')
                    self._op_arg_()
            self.ast.setlist('@', self.last_node)
        self._closure(block1)
        self._token(')')

    @graken()
    def _skol_decl_(self):
        self._token('(')
        self._token('exists')
        with self._group():
            self._term_()
            with self._optional():
                self._token(':')
                self._op_arg_()
        self.ast.setlist('@', self.last_node)

        def block1():
            self._token(',')
            with self._group():
                self._term_()
                with self._optional():
                    self._token(':')
                    self._op_arg_()
            self.ast.setlist('@', self.last_node)
        self._closure(block1)
        self._token(')')

    @graken()
    def _terminal_node_(self):
        with self._group():
            with self._choice():
                with self._option():
                    self._class_decl_()
                    self.ast['klass'] = self.last_node
                with self._option():
                    self._func_decl_()
                    self.ast['func'] = self.last_node
                with self._option():
                    self._multi_assert_()
                    self.ast['assertion'] = self.last_node
                self._error('no available options')

        self.ast._define(
            ['klass', 'func', 'assertion'],
            []
        )

    @graken()
    def _multi_assert_(self):
        with self._choice():
            with self._option():
                self._token('(')
                with self._group():
                    with self._choice():
                        with self._option():
                            self._class_decl_()
                            self.ast.setlist('@', self.last_node)
                        with self._option():
                            self._func_decl_()
                            self.ast.setlist('@', self.last_node)
                        self._error('no available options')

                def block3():
                    self._and_op_()
                    with self._group():
                        with self._choice():
                            with self._option():
                                self._class_decl_()
                                self.ast.setlist('@', self.last_node)
                            with self._option():
                                self._func_decl_()
                                self.ast.setlist('@', self.last_node)
                            self._error('no available options')
                self._closure(block3)
                self._token(')')
            with self._option():
                self._token('(')
                self._token('decl')
                with self._group():
                    with self._choice():
                        with self._option():
                            self._class_decl_()
                            self.ast.setlist('@', self.last_node)
                        with self._option():
                            self._func_decl_()
                            self.ast.setlist('@', self.last_node)
                        self._error('no available options')

                def block10():
                    self._token(';')
                    with self._group():
                        with self._choice():
                            with self._option():
                                self._class_decl_()
                                self.ast.setlist('@', self.last_node)
                            with self._option():
                                self._func_decl_()
                                self.ast.setlist('@', self.last_node)
                            self._error('no available options')
                self._closure(block10)
                self._token(')')
            self._error('no available options')

    @graken()
    def _class_decl_(self):
        self._term_()
        self.ast['klass'] = self.last_node
        with self._optional():
            self._token('(')
            self._op_args_()
            self.ast['op_args'] = self.last_node
            self._token(')')
        self._args_()
        self.ast['args'] = self.last_node

        self.ast._define(
            ['klass', 'op_args', 'args'],
            []
        )

    @graken()
    def _func_decl_(self):
        with self._choice():
            with self._option():
                self._token('fn::')
                self._term_()
                self.ast['func'] = self.last_node
                with self._optional():
                    self._token('(')
                    self._op_args_()
                    self.ast['op_args'] = self.last_node
                    self._token(')')
                self._args_()
                self.ast['args'] = self.last_node
            with self._option():
                self._token('fn::')
                self._term_()
                self.ast['func'] = self.last_node
                self._token('(')
                self._op_args_()
                self.ast['op_args'] = self.last_node
                self._token(')')
            self._error('no available options')

        self.ast._define(
            ['func', 'op_args', 'args'],
            []
        )

    @graken()
    def _args_(self):
        self._token('[')
        self._arg_()
        self.ast.setlist('@', self.last_node)

        def block1():
            self._token(';')
            self._arg_()
            self.ast.setlist('@', self.last_node)
        self._closure(block1)
        self._token(']')

    @graken()
    def _arg_(self):
        self._term_()
        self.ast['term'] = self.last_node
        with self._optional():
            self._token(',')
            self._uval_()
            self.ast['uval'] = self.last_node

        self.ast._define(
            ['term', 'uval'],
            []
        )

    @graken()
    def _uval_(self):
        self._token('u')
        self._comp_op_()
        self.ast.setlist('@', self.last_node)
        self._number_()
        self.ast.setlist('@', self.last_node)

    @graken()
    def _op_args_(self):
        self._op_arg_()
        self.ast.setlist('@', self.last_node)

        def block1():
            self._token(',')
            self._op_arg_()
            self.ast.setlist('@', self.last_node)
        self._closure(block1)

    @graken()
    def _op_arg_(self):
        with self._group():
            with self._choice():
                with self._option():
                    self._string_()
                with self._option():
                    self._term_()
                self._error('no available options')
        self.ast['first_term'] = self.last_node
        with self._optional():
            self._comp_op_()
            self.ast['op'] = self.last_node
            with self._group():
                with self._choice():
                    with self._option():
                        self._string_()
                    with self._option():
                        self._term_()
                    self._error('no available options')
            self.ast['second_term'] = self.last_node

        self.ast._define(
            ['first_term', 'op', 'second_term'],
            []
        )

    @graken()
    def _icond_op_(self):
        self._token('|>')

    @graken()
    def _and_op_(self):
        self._token('&&')

    @graken()
    def _logic_op_(self):
        with self._choice():
            with self._option():
                self._token('<=>')
            with self._option():
                self._token('=>')
            with self._option():
                self._token('||')
            with self._option():
                self._token('&&')
            self._error('expecting one of: && <=> => ||')

    @graken()
    def _comp_op_(self):
        with self._group():
            with self._choice():
                with self._option():
                    self._token('=')
                with self._option():
                    self._token('<')
                with self._option():
                    self._token('>')
                self._error('expecting one of: < = >')

    @graken()
    def _term_(self):
        self._pattern(r'\$?[a-zA-Z0-9_]+')

    @graken()
    def _number_(self):
        self._pattern(r'-?[0-9\.]+')

    @graken()
    def _letters_(self):
        self._pattern(r'[a-zA-Z]+')

    @graken()
    def _string_(self):
        self._pattern(r'".*?"|\'.*?\'')


class SIMAGSemantics(object):
    def block(self, ast):
        return ast

    def stmt(self, ast):
        return ast

    def icond_type(self, ast):
        return ast

    def icond_expr(self, ast):
        return ast

    def icond_node(self, ast):
        return ast

    def comp_type(self, ast):
        return ast

    def comp_expr(self, ast):
        return ast

    def expr_node(self, ast):
        return ast

    def var_decl(self, ast):
        return ast

    def skol_decl(self, ast):
        return ast

    def terminal_node(self, ast):
        return ast

    def multi_assert(self, ast):
        return ast

    def class_decl(self, ast):
        return ast

    def func_decl(self, ast):
        return ast

    def args(self, ast):
        return ast

    def arg(self, ast):
        return ast

    def uval(self, ast):
        return ast

    def op_args(self, ast):
        return ast

    def op_arg(self, ast):
        return ast

    def icond_op(self, ast):
        return ast

    def and_op(self, ast):
        return ast

    def logic_op(self, ast):
        return ast

    def comp_op(self, ast):
        return ast

    def term(self, ast):
        return ast

    def number(self, ast):
        return ast

    def letters(self, ast):
        return ast

    def string(self, ast):
        return ast


def main(filename, startrule, trace=False, whitespace=None, nameguard=None):
    import json
    with open(filename) as f:
        text = f.read()
    parser = SIMAGParser(parseinfo=False)
    ast = parser.parse(
        text,
        startrule,
        filename=filename,
        trace=trace,
        whitespace=whitespace,
        nameguard=nameguard)
    print('AST:')
    print(ast)
    print()
    print('JSON:')
    print(json.dumps(ast, indent=2))
    print()

if __name__ == '__main__':
    import argparse
    import string
    import sys

    class ListRules(argparse.Action):
        def __call__(self, parser, namespace, values, option_string):
            print('Rules:')
            for r in SIMAGParser.rule_list():
                print(r)
            print()
            sys.exit(0)

    parser = argparse.ArgumentParser(description="Simple parser for SIMAG.")
    parser.add_argument('-l', '--list', action=ListRules, nargs=0,
                        help="list all rules and exit")
    parser.add_argument('-n', '--no-nameguard', action='store_true',
                        dest='no_nameguard',
                        help="disable the 'nameguard' feature")
    parser.add_argument('-t', '--trace', action='store_true',
                        help="output trace information")
    parser.add_argument('-w', '--whitespace', type=str, default=string.whitespace,
                        help="whitespace specification")
    parser.add_argument('file', metavar="FILE", help="the input file to parse")
    parser.add_argument('startrule', metavar="STARTRULE",
                        help="the start rule for parsing")
    args = parser.parse_args()

    main(
        args.file,
        args.startrule,
        trace=args.trace,
        whitespace=args.whitespace,
        nameguard=not args.no_nameguard
    )
