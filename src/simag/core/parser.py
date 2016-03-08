# -*- coding: utf-8 -*-

# ===================================================================#
#   Imports and globals
# ===================================================================#

import copy
import datetime
import re

from simag.core.grammar.grako_parser import SIMAGParser
from types import FunctionType
import itertools

__all__ = (
'logic_parser',
# Types:
'LogFunction', 
'LogPredicate',
'LogSentence',
)

# ===================================================================#
#   LOGIC SENTENCE PARSER
# ===================================================================#

parser = SIMAGParser()

class Semantics(object):
    __reserved_words = ['let', 'exists', 'fn', 'time']
    __reserved_fn_names = ['time_calc']
    
    def __init__(self):
        self.__all_reserved = \
            self.__reserved_fn_names + \
            self.__reserved_fn_names
    
    class ReservedWord(Exception):        
        def __init__(self, word):
            self.word = word
        
        def __str__(self):
            return "'{0}' is a reserved word".format(self.word)
    
    def arg(self, ast):
        if ast.term in self.__all_reserved:
            raise Semantics.ReservedWord(ast.term)
        return ast
    
    def var_decl(self, ast):
        error = [x for x in ast if x in self.__all_reserved]
        if len(error) > 0:
            raise Semantics.ReservedWord(error[0])
        return ast
    
    def skol_decl(self, ast):
        error = [x for x in ast if x in self.__all_reserved]
        if len(error) > 0:
            raise Semantics.ReservedWord(error[0])
        return ast
        
    def class_decl(self, ast):
        if ast.klass in self.__all_reserved:
            raise Semantics.ReservedWord(ast.klass)
        return ast
        
    def func_decl(self, ast):
        if ast.func in self.__reserved_words:
            raise Semantics.ReservedWord(ast.klass)
        return ast

semantic_actions = Semantics()

class ParserState(object):
    __instance = None
    def __new__(cls):
        if ParserState.__instance is None:
            ParserState.__instance = object.__new__(cls)
        ParserState.__instance._state = 'tell'
        return ParserState.__instance
    
    @property
    def state(self):
        return self._state
    
    @state.setter
    def state(self, val):
        self._state = val

parser_eval = ParserState()

class ParseResults(object):        
    def __init__(self):
        self.assert_memb = []
        self.assert_rel = []
        self.assert_rules = []
        self.assert_cogs = []
    
    @property
    def queries(self):
        if not hasattr(self, '_queries'):
            setattr(self, '_queries', [])
        return self._queries
    
def logic_parser(string, tell=True):
    """Takes a string and returns the corresponding structured representing
    object program for the logic function. It can parse several statements 
    at the same time, separated by newlines and/or curly braces. It includes 
    a scanner and parser for the synthatical analysis which translate to the
    `program` in form of an object.
    
    The parser is generated automatically through Grako:
    `https://pypi.python.org/pypi/grako/`
    """    
    if string is "" or string is None:
        raise SyntaxError("empty strings are not allowed")
    if tell is False: parser_eval.state = 'ask'
    else: parser_eval.state = 'tell'
    ast = parser.parse(string, rule_name='block', semantics=semantic_actions)
    results = ParseResults()
    for stmt in ast:
        if stmt.stmt is not None:
            sent = make_logic_sent(stmt.stmt)            
            results.assert_cogs.append(sent)
        elif stmt.rule is not None:
            sent = make_logic_sent(stmt)
            results.assert_rules.append(sent)
        elif stmt.assertion is not None:
            for assertion in stmt.assertion:
                if assertion.klass is not None:
                    if parser_eval.state == 'tell':
                        memb = make_fact(assertion, 'grounded_term')
                    else:
                        memb = make_fact(assertion, 'free_term')
                    results.assert_memb.append(memb)
                else:
                    func = make_function(assertion, 'relation')
                    results.assert_rel.append(func)
        elif stmt.query is not None:
            results.queries.append( Query(stmt.query) )
    return results

class LogSentence(object):
    """Object to store a first-order logic complex sentence.

    This sentence is the result of parsing a sentence and encode
    it in an usable form for the agent to classify and reason about
    objects and relations, cannot be instantiated directly.
    
    It's callable when instantiated, accepts as arguments:
    1) the working knowledge-base
    2) n strins which will subsitute the variables in the sentence
       or a list of string.
    """
    def __init__(self):
        self.depth = 0
        self.particles = []
        self._produced = []
        self.created = datetime.datetime.now()
    
    def __call__(self, ag, *args):
        # TODO: should be rewritten to use context manager        
        self.ag = ag
        if type(args[0]) is tuple or type(args[0] is list):
            args = args[0]
        self.assigned = {}
        if hasattr(self, 'var_types'):
            preds = [p for p in self.particles if p.cond == ':predicate:']
            self.assigned.update(
                { p.pred.date_var: p for p in preds if hasattr(p.pred, 'date_var') }
            )
        if hasattr(self, 'pre_assigned'):
            for key, val in self.pre_assigned.items():
                if self.var_types[key] is TimeFunc \
                and (issubclass(val.__class__, LogFunction) \
                or issubclass(val.__class__, LogFunction)):
                    self.assigned[key] = self.assign_time_val_cb(val)
                else: self.assigned[key] = val
        if not hasattr(self, 'var_order'):
            preds = self.get_preds(branch='r', unique=True)
            ag.thread_manager(preds)
            self.start.solve_proof(self)
            ag.thread_manager(preds, unlock=True)
        elif hasattr(self, 'var_order') \
        and len(self.var_order) == len(args):
            # Check the properties/classes an obj belongs to
            for n, const in enumerate(args):
                if const not in ag.individuals: return
                var_name = self.var_order[n]
                # Assign an entity to a variable by order.
                if hasattr(self, 'var_types') and var_name in self.var_types \
                and var_name not in self.assigned.keys():
                    type_ = self.var_types[var_name]
                    assert isinstance(const, type_), \
                    "{0} is not a {1} object".format(const, type_)
                self.assigned[var_name] = const
            # acquire lock
            preds = self.get_preds(branch='r', unique=True)
            ag.thread_manager(preds)
            self.start.solve_proof(self)
            ag.thread_manager(preds, unlock=True)
        if hasattr(self, 'result'): result = self.result
        else: result = None
        self._cln_res()
        return result
    
    def assign_time_val_cb(self, pred):
        def callback_pred():
            if hasattr(pred, 'substituted'):
                return pred.substituted.time
            # TODO: there may be two reasons why a None is being returned
            # here, either because the substitution of the precondition
            # failed or because unexpected behaviour due to a badly-formed
            # sentence. Ideally we want to test for the unexpected behaviour
            # at parse time, and raise an error there
            return None
        return callback_pred
        
    def get_ops(self, p, chk_op=['||', '=>', '<=>']):
        ops = []
        for p in self:
            if any(x in p.cond for x in chk_op):
                ops.append(p)
        for p in ops:
            x = p
            while x.cond != '|>' or x.parent == -1:
                if x.parent.cond == '|>' and x.parent.next[1] == x:
                    return False
                else:
                    x = x.parent
        return True
    
    def _cache_preds(self):
        conds = ('|>', '=>', '<=>')
        all_pred = []
        for p in self:
            if p.cond == ':predicate:':
                all_pred.append(p)
        left_preds, right_preds = [], []
        for p in all_pred:
            top, bottom = p.parent, p
            while top.cond not in conds and top.parent:
                top, bottom = top.parent, top
            if top.next[0] is bottom:
                if not isinstance(p.pred, TimeFunc):
                    left_preds.append(p)        
            elif top.next[1] is bottom:
                if not isinstance(p.pred, TimeFunc):
                    right_preds.append(p)
        self._preds = (tuple(left_preds), tuple(right_preds))
    
    def get_all_preds(self, unique=False):
        preds = itertools.chain.from_iterable(self._preds)
        if unique:
            preds = [
                p.pred.parent if issubclass(p.pred.__class__, LogPredicate)
                else p.pred.func for p in preds
            ]
            return set(preds)
        else: 
            return [p.pred for p in preds]
    
    def get_preds(self, branch='l', unique=False):
        if branch == 'l':
            if not unique:
                return [p.pred for p in self._preds[0]]
            else:
                return set([
                    p.pred.parent 
                    if issubclass(p.pred.__class__, LogPredicate)
                    else p.pred.func for p in self._preds[0]
                ])
        else:
            if not unique:
                return [p.pred for p in self._preds[1]]
            else:
                return set([
                    p.pred.parent 
                    if issubclass(p.pred.__class__, LogPredicate)
                    else p.pred.func for p in self._preds[1]
                ])
    
    def _cln_res(self):
        if hasattr(self, 'result'): del self.result
        del self.ag
        del self.produced_from
        for particle in self.particles: 
            if particle.cond == ':predicate:':
                try: del particle.pred.substituted
                except: pass
            del particle.results
    
    def __iter__(self):
        return iter(self.particles)
    
    def __repr__(self):
        def next_lvl_repr(obj):
            if hasattr(obj, 'next'):
                lhs = next_lvl_repr(obj.next[0])
                rhs = next_lvl_repr(obj.next[1])
                r = "".join(('{', lhs, ' ', obj.cond, ' ', rhs, '}'))
            else:
                r = ' ' + repr(obj) + ' '
            return r
        rep = next_lvl_repr(self.start)
        return rep
    
    @property
    def produced_from(self):
        return self._produced
    
    @produced_from.deleter
    def produced_from(self):
        self._produced = []
    
    @produced_from.setter
    def produced_from(self, val):
        self._produced = val
    
def make_logic_sent(ast):
    """Takes a parsed FOL sentence and creates an object with
    the embedded methods to resolve it.
    """
    
    class Particle(object):
        """Base class for logic particles which pertain 
        to a given sentence."""
        def __init__(self, cond, depth, parent, *args):
            self.depth = depth
            self.cond = cond
            self.parent = parent            
            if cond == ':predicate:':
                self.pred = args[0]
            else:
                self._results = []
                self.next = []
    
        def __str__(self):
            if self.cond != ':predicate:':
                s = "<operator {1} (depth: {0})>".format(
                    str(self.depth), self.cond)
            else:
                s = "<predicate {1} (depth: {0})>".format(
                    str(self.depth), self.cond)
            return s
        
        def __repr__(self):
            if self.cond != ':predicate:':
                s = "<operator '{}' (depth: {})>".format(
                    self.cond, self.depth)
            else:
                s = "<predicate '{}'>".format(self.pred)
            return s
        
        @property
        def results(self):
            return self._result
        
        @results.deleter
        def results(self):
            self._results = []
    
        def return_value(self, truth, *args):
            self._results.append(truth)
        
        def substitute(self, *args):
            raise AssertionError(
                "operators of the type `{0}` can't be on " \
                + "the left side of sentence: `{1}`".format(self.cond, self))
    
    class LogicIndCond(Particle):
        
        def solve_proof(self, proof):
            self.next[0].resolve(proof)
            if self._results[0] is True:
                if self.next[1].cond == '||':
                    self.next[1].substitute(proof, 'lhs')
                else:     
                    self.next[1].substitute(proof)
            elif self._results[0] is False:
                if self.next[1].cond == '||':
                    self.next[1].substitute(proof, 'rhs')
                else:
                    proof.result = False
            else:
                proof.result = None
        
        def resolve(self, proof, *args):
            raise AssertionError(
                "indicative conditional type " \
                + "arguments can only contain other indicative conditional " \
                + "or assertions in their right hand side")
        
        def substitute(self, proof, *args):
            if len(self._results) == 0:
                self.next[0].resolve(proof, *args)                
            if self._results[0] is True:
                if self.next[1].cond == '||':
                    self.next[1].substitute(proof, 'lhs', *args)
                else:
                    self.next[1].substitute(proof, *args)
                self.parent.return_value(True)
            elif self._results[0] is False:
                if self.next[1].cond == '||':
                    self.next[1].substitute(proof, 'rhs')
                    self.parent.return_value(True)
                else:
                    self.parent.return_value(False)
            else:
                self.parent.return_value(None)
            
    class LogicEquivalence(Particle):
        
        def solve_proof(self, proof):
            for p in self.next:
                p.resolve(proof)
            if any([True for x in self._results if x is None]):
                proof.result = None
            elif self._results[0] == self._results[1]:
                proof.result = True
            else:
                proof.result = False
        
        def resolve(self, proof, *args):
            for p in self.next:
                p.resolve(proof)
            if any([True for x in self._results if x is None]):
                self.parent.return_value(None)
            elif self._results[0] == self._results[1]:
                self.parent.return_value(True)
            else:
                self.parent.return_value(False)
    
    class LogicImplication(Particle):
        
        def solve_proof(self, proof):
            for p in self.next:
                p.resolve(proof)
            if (self._results[0] and self._results[1]) is None:
                proof.result = None
            else:
                if self._results[1] is False and self._results[0] is True:
                    proof.result = False
                else:
                    proof.result = True
        
        def resolve(self, proof, *args):
            for p in self.next:
                p.resolve(proof, *args)
            if (self._results[0] and self._results[1]) is None:
                self.parent.return_value(None)
            else:
                if self._results[1] is False and self._results[0] is True:
                    self.parent.return_value(False)
                else:
                    self.parent.return_value(True)
    
    class LogicConjunction(Particle):
        
        def solve_proof(self, proof):
            for p in self.next:
                p.resolve(proof)
            if all(self._results) and len(self._results) > 0:
                proof.result = True
            elif False in self._results:
                proof.result = False
        
        def resolve(self, proof, *args):
            for p in self.next:
                p.resolve(proof, *args)
            if all(self._results) and len(self._results) > 0:
                self.parent.return_value(True)
            elif False in self._results:
                self.parent.return_value(False)
            else:
                self.parent.return_value(None)
        
        def substitute(self, proof, *args):
            for p in self.next:
                p.substitute(proof, *args)
    
    class LogicDisjunction(Particle):
        
        def solve_proof(self, proof):
            for p in self.next:
                p.resolve(proof)
            if any(self._results) and len(self._results) > 0:
                proof.result = True
            elif all([True for x in self._results if x is False]):
                proof.result = False
        
        def resolve(self, proof, *args):
            for p in self.next:
                p.resolve(proof, *args)
            if any(self._results) and len(self._results) > 0:
                self.parent.return_value(True)
            elif all([True for x in self._results if x is False]):
                self.parent.return_value(False)
            else:
                self.parent.return_value(None)
        
        def substitute(self, proof, *args):
            if self.parent.cond != '|>':
                super().substitute()                    
            side = args[0]
            if side == 'lhs':
                self.next[0].substitute(proof)
            else:
                self.next[1].substitute(proof)
                
    class LogicAtom(Particle):
        
        def resolve(self, proof):
            ag = proof.ag
            result = None
            if issubclass(self.pred.__class__, LogFunction):
                # Check funct between a set/entity and other set/entity.
                args = self.pred.get_args()
                for x, arg in enumerate(args):
                    if arg in proof.assigned:
                        args[x] = proof.assigned[arg]
                test = self.pred.substitute(args)
                if args[0][0] == '$':
                    result = ag.individuals[args[0]].test_rel(
                        test, obj=True, copy_date=True)
                    if result:
                        proof.produced_from.append(result)
                        result = True
                else:
                    result = ag.classes[args[0]].test_rel(
                        test, obj=True, copy_date=True)
                    if result:
                        proof.produced_from.append(result)
                        result = True
            elif issubclass(self.pred.__class__, LogPredicate):
                # Check membership to a set of an entity.
                sbj = self.isvar(self.pred.term, proof)
                test = self.pred.substitute(sbj)
                if sbj[0] != '$':
                    try: 
                        result = ag.classes[sbj].test_ctg(
                            test, obj=True)
                    except KeyError:
                        result = None
                    if result:
                        proof.produced_from.append(result)
                        result = True
                else:
                    try:
                        result = ag.individuals[sbj].test_ctg(
                            test, obj=True)
                    except KeyError:                        
                        result = None
                    else:
                        if result:
                            proof.produced_from.append(result)
                            result = True
            else:
                # special function types
                if type(self.pred) == TimeFunc:
                    dates = {}
                    for arg, assigned_value in proof.assigned.items():
                        if arg in self.pred.args \
                        and type(assigned_value) is FunctionType:
                            dates[arg] = assigned_value()
                        elif arg in self.pred.args \
                        and not isinstance(assigned_value, str):
                            dates[arg] = assigned_value.get_date(proof, ag)
                        elif arg in self.pred.args:
                            dates[arg] = assigned_value
                    if None not in dates.values():
                        test = self.pred.substitute(dates)
                        if test: result = True
                        else: result = False
            self.parent.return_value(result)
        
        def substitute(self, proof):
            ag = proof.ag
            # add subtitute var(s) for constants
            # and pass to agent for declaration
            if issubclass(self.pred.__class__, LogFunction):
                args = self.pred.get_args()
                for x, arg in enumerate(args):
                    if arg in proof.assigned:
                        args[x] = proof.assigned[arg]
                pred = self.pred.substitute(args)
                pred = ag.up_rel(pred, return_val=True)
                ag.bmsWrapper.add(pred, proof)
            elif issubclass(self.pred.__class__, LogPredicate):
                sbj = self.isvar(self.pred.term, proof)
                pred = self.pred.substitute(sbj, val=None, ground=True)                
                pred = ag.up_memb(pred, return_val=True)
                ag.bmsWrapper.add(pred, proof)
            if hasattr(proof, 'result'):
                proof.result.append(pred)
            else:
                proof.result = [pred]
        
        def isvar(self, term, proof):
            try: term = proof.assigned[term]
            except KeyError: pass
            return term
        
        def get_date(self, proof, ag):
            date = None
            if issubclass(self.pred.__class__, LogFunction):
                args = self.pred.get_args()
                for x, arg in enumerate(args):
                    if arg in proof.assigned:
                        args[x] = proof.assigned[arg]
                test = self.pred.substitute(args)
                if '$' in args[0][0]:
                    date = ag.individuals[args[0]].get_date(test)
                else:
                    date = ag.classes[args[0]].get_date(test)
            elif issubclass(self.pred.__class__, LogPredicate):
                try:
                    sbj = proof.assigned[self.pred.term]
                    test = self.pred.substitute(sbj)
                except KeyError:
                    test = self.pred
                if '$' in test.term[0]:
                    date = ag.individuals[test.term].get_date(test)
                else:
                    date = ag.classes[test.term].get_date(test)
            return date
        
        def __repr__(self):
            return repr(self.pred)
    
    def traverse_ast(remain, parent, depth):
        def cmpd_stmt(preds, depth, parent):
            pred = preds.pop()
            if len(preds) > 1:
                form = _get_atom_type(pred, cmpd=True, sent=sent)
                particle = LogicAtom(':predicate:', depth, parent, form)          
                new_node = LogicConjunction('&&', depth, parent)
                parent.next.extend((new_node, particle))
                sent.particles.extend((new_node, particle))
                cmpd_stmt(preds, depth+1, new_node)                
            elif len(preds) == 1:
                form = _get_atom_type(pred, cmpd=True, sent=sent)
                particle1 = LogicAtom(':predicate:', depth, parent, form)
                form = _get_atom_type(preds.pop(0), cmpd=True, sent=sent)
                particle2 = LogicAtom(':predicate:', depth, parent, form)
                parent.next.extend((particle1, particle2))
                sent.particles.extend((particle1, particle2))
        
        form = _get_atom_type(remain, sent=sent)
        if form is not None:
            particle = LogicAtom(':predicate:', depth, parent, form)
        elif remain.assertion is not None:
            particle = LogicConjunction('&&', depth, parent)
            cmpd_stmt(remain.assertion, depth+1, particle)
        else:
            if remain.op == '|>':
                particle = LogicIndCond(remain.op, depth, parent)
            elif remain.op == '<=>':
                particle = LogicEquivalence(remain.op, depth, parent)
            elif remain.op == '=>':
                particle = LogicImplication(remain.op, depth, parent)
            elif remain.op == '&&':
                particle = LogicConjunction(remain.op, depth, parent)
            elif remain.op == '||':
                particle = LogicDisjunction(remain.op, depth, parent)
        sent.particles.append(particle)
        if parent: parent.next.append(particle)
        else: sent.start = particle
        # traverse down
        if remain.lhs is not None:
            traverse_ast(remain.lhs, particle, depth+1)
        if remain.lhs is not None:
            traverse_ast(remain.rhs, particle, depth+1)
        if sent.depth < depth: sent.depth = depth
        
    def disambiguate_vars(vars_):
        sent.var_order = []
        for v in vars_:
            if isinstance(v, str) and v not in sent.var_order:
                if v in sent.var_order:
                    m = "duplicate variable name declared: {0}".format(v)
                    raise ValueError(m)
                sent.var_order.append(v)
            else:
                var, type_, value = v[0], v[2].first_term, v[2].second_term
                if var in sent.var_order:
                    m = "duplicate variable name declared: {0}".format(var)
                    raise ValueError(m)
                if not hasattr(sent, 'var_types'):
                    sent.var_types = {}
                sent.var_types[var] = _get_type_class(type_)
                if value:
                    if not hasattr(sent, 'pre_assigned'):
                        sent.pre_assigned = {}
                    sent.pre_assigned[var] = value
    
    sent = LogSentence()
    if ast.vars is not None:
        disambiguate_vars(ast.vars)
    if ast.skol is not None:
        disambiguate_vars(ast.skol)
    depth, parent = 0, None
    if ast.expr:
        traverse_ast(ast.expr, parent, depth)
    elif ast.rule:
        traverse_ast(ast.rule, parent, depth)
    elif ast.query:
        traverse_ast(ast.query, parent, depth)
    sent._cache_preds()
    return sent

class Query(object):
    def __init__(self, sent):
        self.funcs = []
        self.preds = []
        self.process_query(sent)
        
    def process_query(self, sent):
        if sent.vars:
            self.var_order = sent.vars
        if sent.skol:
            self.skol_order = sent.skol
        if sent.query.assertion:
            for q in sent.query.assertion:
                pred = _get_atom_type(q, cmpd=True)
                if issubclass(pred.__class__, LogFunction):
                    self.funcs.append(pred)
                elif issubclass(pred.__class__, LogPredicate):
                    self.preds.append(pred)
        elif sent.query.func:
            pred = _get_atom_type(q, cmpd=True)
            self.funcs.append(pred)
        elif sent.query.klass:
            pred = _get_atom_type(q, cmpd=True)
            self.preds.append(pred)
    
def _get_atom_type(stmt, cmpd=False, sent=None):
    if stmt.func is not None:
        if cmpd is True:
            is_builtin = _check_reserved_words(stmt.func)
            if is_builtin:
                return make_function(stmt, is_builtin)
            else:
                return make_function(
                    stmt, 'relation', proof=sent)
        is_builtin = _check_reserved_words(stmt.func.func)
        if is_builtin:
            return make_function(stmt.func, is_builtin)
        else:
            return make_function(stmt.func, 'relation', proof=sent)
    elif stmt.klass is not None:
        if cmpd is True:
            return make_fact(stmt, 'free_term')
        return make_fact(stmt.klass, 'free_term')

# ===================================================================#
#   LOGIC CLASSES AND SUBCLASSES
# ===================================================================#


class MetaForAtoms(type):
    
    def __new__(cls, name, bases, attrs, **kwargs):
        attrs['_eval_time_truth'] = MetaForAtoms.__eval_time_truth        
        # Add methods from LogFunction to TimeFunc and store it at one location
        if name == 'TimeFunc':
            if not hasattr(MetaForAtoms, 'TimeFunc'):
                for m in globals()['LogFunction'].__dict__.values():
                    if type(m) == FunctionType \
                    and m.__name__ not in attrs.keys():                    
                        attrs[m.__name__] = m
                MetaForAtoms.TimeFunc = super().__new__(cls, name, bases, attrs)
            return MetaForAtoms.TimeFunc
        # Store FreeTerm and return from same memory address
        elif name == 'FreeTerm':
            if not hasattr(MetaForAtoms, 'FreeTerm'):
                MetaForAtoms.FreeTerm = super().__new__(cls, name, bases, attrs)
            return MetaForAtoms.FreeTerm
        # return the new class
        return super().__new__(cls, name, bases, attrs)
    
    def __eval_time_truth(self, other):
        now = datetime.datetime.now()
        isTrueOther, isTrueSelf = True, True
        if hasattr(self, 'dates'):
            if (len(self.dates) % 2 or len(self.dates) == 1) \
            and self.dates[-1] < now: 
                isTrueSelf = True
            else: 
                isTrueSelf = False
        if hasattr(other, 'dates'):
            if (len(other.dates) % 2 or len(other.dates) == 1) \
            and other.dates[-1] < now: 
                isTrueOther = True
            else:
                isTrueOther = False
        # Compare truthiness of both
        if (isTrueOther and isTrueSelf) is True \
        or (isTrueOther and isTrueSelf) is False:
            return True
        else:
            return False

class LogPredicate(metaclass=MetaForAtoms):
    """Base class to represent a ground predicate."""
    types = ['grounded_term', 'free_term']
    
    def __init__(self, pred):
        arg = pred.args[0]
        val, op = float(arg.uval[1]), arg.uval[0]
        if (val > 1 or val < 0):
            m = "illegal truth value: {0}, must be > 0, or < 1.".format(val)
            raise ValueError(m)
        dates = None
        if pred.op_args:
            for op_arg in pred.op_args:
                if op_arg.first_term == 'time':
                    date = _set_date(op_arg.second_term)
                    if type(date) is datetime.datetime:
                        if dates is None: dates = []
                        dates.append(date)
                    else: self.date_var = date
        return pred.klass, arg.term, val, op, dates

    def change_params(self, new=None, revert=False):
        if revert is not True:
            self.oldTerm, self.term = self.term, new
        else:
            self.term = self.oldTerm
            del self.oldTerm
    
    def term_is_ind(self):
        if self.term[0] == '$':
            return True
        else:
            return False
    
    def __repr__(self):
        return '{0}({1}: {2})'.format(
            self.__class__.__name__, self.parent, self.term)

class NotCompAssertError(Exception):
        "trying to compare different terms"
        
def make_fact(pred, f_type=None, **kwargs):
    """Parses a grounded predicate and returns a 'fact'."""
    
    class GroundedTerm(LogPredicate):
        
        def __init__(self, pred, from_free=False, empty=False, **kwargs):
            if from_free is True:
                assert isinstance(pred, FreeTerm), \
                       'The object is not of <FreeTerm> type'
                for name, value in pred.__dict__.items():
                    setattr(self, name, value)
                self.term = kwargs['sbj']
                if hasattr(self, 'date_var'): del self.date_var
                if hasattr(self, 'op'): del self.op
            elif empty: pass
            else:
                parent, term, val, op, dates = super().__init__(pred)
                self.parent = parent
                self.term = term                
                assert (op == '='), \
                       "It's a grounded predicate, must assign truth value."
                if (val > 1 or val < 0):
                    m = "Illegal value: {0}, must be > 0, or < 1." .format(val)
                    raise AssertionError(m)
                self.value = val
                if dates is not None:
                    self.dates = dates
        
        def __eq__(self, other):
            # Test if the statements are true at this moment in time
            time_truth = self._eval_time_truth(other)
            if time_truth is False: return False
            # test against other            
            if other.parent != self.parent \
            or other.term != self.term:
                raise NotCompAssertError
            if other.value == self.value:
                return True
            else: return False
        
        def substitute(self, sbj=None, val=None):
            empty = make_fact(self, f_type='grounded_term', **{'empty': True})
            empty.parent = self.parent
            if sbj: empty.term = sbj
            else: empty.term = self.term
            if val: empty.value = val
            else: empty.value = self.value
            if hasattr(self, 'dates'):
                empty.dates = copy.deepcopy(self.dates)
            return empty
        
        def update(self, other):
            if not issubclass(other.__class__, LogPredicate):
                raise TypeError(
                "object provided for updating must be of class GroundedTerm")
            assert (self.parent == other.parent)
            assert (self.term == other.term)
            self.value = other.value
        
        @property
        def time(self):
            now = datetime.datetime.now()
            if hasattr(self, 'dates'):
                if (len(self.dates) % 2 or len(self.dates) == 1) \
                and self.dates[-1] < now: 
                    return self.dates[-1]
                else: 
                    return False
            else: 
                return now
            
    class FreeTerm(LogPredicate):
        
        def __init__(self, pred):
            parent, term, val, op, dates = super().__init__(pred)
            self.parent = parent
            self.term = term
            self.value = val
            self.op = op
            if dates is not None:
                self.dates = dates
        
        def __eq__(self, other):
            if self.parent != other.parent \
            or self.term != other.term:
                raise NotCompAssertError
            # Test if the statements are ture at this moment in time
            time_truth = self._eval_time_truth(other)
            if time_truth is False: return False
            # test against other
            if not issubclass(other.__class__, LogPredicate):
                m = "{0} and {1} are not comparable.".format(other, LogPredicate)
                raise TypeError(m)
            if self.op == '=' and other.value == self.value:
                return True
            elif self.op == '>' and other.value > self.value:
                return True
            elif self.op == '<' and other.value < self.value:
                return True
            else: return False
        
        def substitute(self, sbj=None, val=None, ground=False):
            if ground is True:
                subs = make_fact(
                    self, 
                    f_type='grounded_term', 
                    **{'from_free': True, 'sbj':sbj})
            else:
                subs = copy.deepcopy(self)
                if sbj is not None: subs.term = sbj
                if val is not None: subs.value = val
            self.substituted = subs
            return subs
    
    assert (f_type in LogPredicate.types or f_type is None), \
            'Function {0} does not exist.'.format(f_type)
    if f_type == 'grounded_term': 
        if pred is None: return GroundedTerm
        return GroundedTerm(pred, **kwargs)
    elif f_type == 'free_term': 
        if pred is None: return FreeTerm
        return FreeTerm(pred)
    else:
        if pred is None: return LogPredicate 
        return LogPredicate(pred)


class LogFunction(metaclass=MetaForAtoms):
    """Base class to represent a logic function."""
    types = ['relation','time_calc']
    
    def __init__(self, fn, **extra):
        if 'proof' in extra and extra['proof'] is not None:
            proof = extra['proof']
        else: proof = None
        self._grounded = True
        self.func = fn.func
        dates, args_id, mk_args = None, list(), list()
        for a in fn.args:
            if a.uval is not None:
                val = float(a.uval[1])
                if (val > 1 or val < 0):
                    m = "illegal value: {0}, must be > 0, or < 1." .format(val)
                    raise ValueError(m)
                op = a.uval[0]           
                mk_args.append((a.term, val, op))
            else:
                mk_args.append(a.term)
            if proof \
            and ((hasattr(proof, 'var_order') and a.term in proof.var_order)
            or   (hasattr(proof, 'var_types') and a.term in proof.var_types)):        
                self._grounded = False
            args_id.append(a.term)
        if fn.op_args:
            for op_arg in fn.op_args:
                if op_arg.first_term == 'time':
                    date = _set_date(op_arg.second_term)
                    if isinstance(date, datetime.datetime):
                        if dates is None: dates = []
                        dates.append(date)
                    else: self.date_var = date
                elif proof is not None \
                and op_arg.first_term in proof.var_types \
                and op_arg.second_term == 'time':
                    if hasattr(proof, 'pre_assigned'):
                        proof.pre_assigned[op_arg.first_term] = self
                    else:
                        proof.pre_assigned = {op_arg.first_term: self}
        args_id = hash(tuple(args_id))
        return mk_args, args_id, dates
        
    @property
    def arity(self):
        return len(self.args)
    
    @property
    def time(self):
        if not self._grounded:
            raise AttributeError(
                "can't get truth times for non-grounded functions")
        now = datetime.datetime.now()
        if hasattr(self, 'dates'):
            if (len(self.dates) % 2 or len(self.dates) == 1) \
            and self.dates[-1] < now: 
                return self.dates[-1]
            else: 
                return False
        else:
            return now
    
    def arg_is_ind(self, arg):
        if arg[0] == '$':
            return True
        else:
            return False
    
    def substitute(self, args):
        subs = copy.deepcopy(self)
        subs.args_ID = hash(tuple(args))
        subs._grounded = True
        if isinstance(args, dict):
            for x, arg in enumerate(subs.args):
                if isinstance(arg, tuple) and arg in args:
                    subs.args[x] = list(arg)
                    subs.args[x][0] = args[arg]
                    subs.args[x] = tuple(subs.args[x])
                elif arg in args:
                    subs.args[x] = args[arg]
        elif isinstance(args, list):
            for x, arg in enumerate(subs.args):
                if isinstance(arg, tuple):
                    subs.args[x] = list(arg)
                    subs.args[x][0] = args[x]
                    subs.args[x] = tuple(subs.args[x])
                else:
                    subs.args[x] = args[x]
        self.substituted = subs
        return subs
    
    def __str__(self):
        return '{0}({1}: {2})'.format(
            self.__class__.__name__, self.func, self.args)
        
    def __repr__(self):
        return '{0}({1}: {2})'.format(
            self.__class__.__name__, self.func, self.args)

class NotCompFuncError(Exception):
    def __init__(self, args):
        self.err, self.arg1, self.arg2 = args

def make_function(sent, f_type=None, **kwargs):
    """Parses and makes a function of n-arity.
    
    Functions describe relations between objects (which can be instantiated
    or variables). This functions can have any number of arguments, the
    most common being the binary functions.
    
    This class is instantiated and provides a common interface for all the 
    function types, which are registered in this class. It acts as an 
    abstraction to hide the specific details from the clients.
    
    The types are subclasses and will implement the details and internal
    data structure for the function, but are not meant to be instantiated
    directly.
    """
    
    class RelationFunc(LogFunction):
        
        def __init__(self, fn, **kwargs):
            mk_args, args_id, dates = super().__init__(fn, **kwargs)
            self.args_ID = args_id
            self.args = mk_args
            # relation functions can only have a truth value for the 
            # first argument which represent the object of the relation,
            # the second argument is the subject, and the optional third
            # is the indirect object
            if dates is not None: self.dates = dates
        
        @property
        def value(self):
            return self.args[0][1]
        
        @value.setter
        def value(self, val):
            arg = list(self.args[0])
            arg[1] = val
            self.args[0] = tuple(arg)
        
        def update(self, other):
            self.value = other.value
            if hasattr(other, 'dates'):
                self.dates = other.dates
        
        def compare_args(self, other):
            for x, arg in enumerate(self.args):
                try:
                    if isinstance(arg, tuple):
                        oarg = other.args[x]
                        if not isinstance(oarg, tuple):
                            return False
                        if arg[2] == '=' and arg[1] != oarg[1]:  
                            return False                     
                        elif arg[2] == '>'and arg[1] > oarg[1]:
                            return False    
                        elif arg[2] == '<'and arg[1] < oarg[1]:  
                            return False
                    elif arg != other.args[x]:
                        return False
                except IndexError:
                    return False
            return True
        
        def __eq__(self, other):
            comparable = self.chk_args_eq(other)
            if comparable is not True:
                raise NotCompFuncError(comparable)
            # Check if both are equal
            for x, arg in enumerate(self.args):
                if isinstance(arg, tuple):
                    oarg = other.args[x]
                    if arg[2] == '=' and arg[1] != oarg[1]:  
                        result = False                      
                    elif arg[2] == '>'and arg[1] > oarg[1]:
                        result = False     
                    elif arg[2] == '<'and arg[1] < oarg[1]:  
                        result = False
                    else:
                        result = True
            # Test if the statements are true at this moment in time
            time_truth = self._eval_time_truth(other)
            if time_truth is True and result is True: return True
            else: return False
        
        def __ne__(self, other):
            comparable = self.chk_args_eq(other)
            if comparable is not True:
                raise NotCompFuncError(comparable)            
            # Check if both are not equal
            for x, arg in enumerate(self.args):
                if isinstance(arg, tuple):
                    oarg = other.arg[x]
                    if arg[2] == '=' and arg[1] != oarg[1]:
                        result = True                      
                    elif arg[2] == '>'and arg[1] < oarg[1]:
                        result = True     
                    elif arg[2] == '<'and arg[1] > oarg[1]: 
                        result = True
                    else:
                        result = True
            # Test if the statements are true at this moment in time
            time_truth = self._eval_time_truth(other)
            if time_truth is False and result is False: return True
    
        def chk_args_eq(self, other):            
            if other.arity != self.arity:
                return ('arity', other.arity, self.arity)
            if other.func != self.func:
                return ('function', other.func, self.func)
            for x, arg in enumerate(self.args):
                if hasattr(other, '_ignore_args') and x in other._ignore_args:
                    pass
                if isinstance(arg, tuple):
                    if other.args[x][0] != arg[0]:
                        return ('args', other.args[x][0], arg[0])
                elif other.args[x] != arg:
                    return ('args', other.args[x], arg)
            return True
        
        def change_params(self, new=None, revert=False):
            if revert is False:
                self.oldTerm = self.args.copy()
                for x, arg in enumerate(self.args):
                    if isinstance(arg, tuple):
                        self.args[x] = list(arg)
                        self.args[x][0] = new[x]
                        self.args[x] = tuple(self.args[x])
                    else:
                        self.args[x] = new[x]
            else:
                self.term = self.oldTerm
                del self.oldTerm 
    
        def replace_var(self, position, replacement, copy_=False, vrs=False):
            if copy_: subs = copy.deepcopy(self)
            else: subs = self
            if isinstance(subs.args[position], tuple):
                tmp = list(subs.args[position])
                tmp[0], subs.args[position] = replacement, tmp
            else:
                subs.args[position] = replacement
            if vrs: subs._ignore_args = vrs
            return subs
        
        def get_args(self):
            return [arg if not isinstance(arg, tuple) else arg[0] 
                    for arg in self.args]
    
    class TimeFunc(metaclass=MetaForAtoms):
        """A special case for time calculus, not considered a relation.        
        It's not a subclass of LogFunction.
        """
        
        def __init__(self, sent):
            if not sent.op_args[0].op:
                raise SyntaxError(
                    "provide an operator in the 'time_func' arguments" )
            self.args = [
                sent.op_args[0].first_term, sent.op_args[0].second_term ]
            self.operator = sent.op_args[0].op
        
        def __bool__(self):
            try:
                if self.operator == '<' and self.args[0] < self.args[1]:
                    return True
                elif self.operator == '>' and self.args[0] > self.args[1]:
                    return True
                elif self.operator == '=' and self.args[0] == self.args[1]:
                    return True
                else: 
                    return False
            except TypeError:
                raise TypeError(
                    "both 'time_calc' arguments must be datetime objects")
        
        def substitute(self, *args):
            subs = LogFunction.substitute(self, *args)
            for x, arg in enumerate(subs.args):
                if isinstance(arg, str): subs.args[x] = _set_date(arg)
            return subs
        
        def __str__(self):
            return "TimeFunc({} {} {})".format(
                self.args[0], self.operator, self.args[1])
        
        def __repr__(self):
            return "TimeFunc({} {} {})".format(
                self.args[0], self.operator, self.args[1])

    assert (f_type in LogFunction.types or f_type is None), \
            'Function {0} does not exist.'.format(f_type)
    if f_type == 'relation':
        if sent is None: return RelationFunc
        return RelationFunc(sent, **kwargs)
    if f_type == 'time_calc':
        if sent is None: return TimeFunc
        return TimeFunc(sent)
    else:
        if sent is None: return LogFunction
        return LogFunction(sent)

TimeFunc = make_function(None, 'time_calc')
GroundedTerm = make_fact(None, 'grounded_term')
FreeTerm = make_fact(None, 'free_term')

# ===================================================================#
#   HELPER FUNCTIONS
# ===================================================================#

def _check_reserved_words(word):
    if word == "time_calc": return word
    return False

re_str = re.compile(r"(\"|\')(?P<string>.*?)(\"|\')")
def _set_date(date):
    match = re.search(re_str, date)
    if not match:
        return date
    elif match.group('string') == "*now":
        return datetime.datetime.now()
    else:
        date = match.group('string').split('.')
    tb = ['year','month','day','hour','minute','second','microsecond']
    dobj = {}
    for i, p in enumerate(date):
        if 'tzinfo' not in p:
            dobj[ tb[i] ] = int(p)
    if 'year' not in dobj: raise ValueError('year not specified')
    if 'month' not in dobj: raise ValueError('month not specified')
    if 'day' not in dobj: raise ValueError('day not specified')
    if 'hour' not in dobj: dobj['hour'] = 0
    if 'minute' not in dobj: dobj['minute'] = 0
    if 'second' not in dobj: dobj['second'] = 0
    if 'microsecond' not in dobj: dobj['microsecond'] = 0
    if 'tzinfo' not in dobj: dobj['tzinfo'] = None
    return datetime.datetime(
        year=dobj['year'],
        month=dobj['month'],
        day=dobj['day'],
        hour=dobj['hour'],
        minute=dobj['minute'],
        second=dobj['second'],
        microsecond=dobj['microsecond'],
        tzinfo=dobj['tzinfo']
    )
    
def _get_type_class(type_): 
    if type_ == 'time':
        type_ = make_function(None, f_type='time_calc')
    else:
        raise TypeError(
            "function of the '{0}' type doesn't exist".format(type_))
    return type_
