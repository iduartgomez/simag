# -*- coding: utf-8 -*-

"""Main knowledge-base logic module, in this module exist the different 
classes that transform and store the data for the individual agents and 
serve as representations of the different objects and the relationships 
between them.

Main
----
:class: Representation. Main class, stores all the representations and
relationships for a given agent in a concrete time.

:class: Individual. Represents a singular entity, which is the unique
member of it's own set.

:class: Categories. The sets in which the agent can classify objects.
Also stores the types of relations an object can have.

Support classes (and methods) and functions
-------------------------------------------
:class: LogSentence. Stores a serie of logical atoms (be them predicates or
connectives), that form a well-formed logic formula. These are rulesets 
for reasoning, cataloging objects into sets/classes, and the relationships 
between these objects. 

LogSentences are owned by the funtion 'make_logic_sentence', and cannot be
called directly.

:class: Inference. Encapsulates the whole inference process, from making
a temporal substitution representation where the inference is operated to
solving the query (including query parsing, data fetching and unification).

@author: Ignacio Duart Gómez
"""

# On ASK, fix it so it can deal with queries that ask about relations
# of the same type with several objects.
#
# Add 'belief maintenance system' functionality.
# Refactor class membership to new data structure instead of raw tuples

# ===================================================================#
#   Imports and globals
# ===================================================================#

import uuid
import itertools
import re

import core.bms
from core.logic_parser import *

GL_PCONDS = [':icond:', ':implies:', ':equiv:']
SYMBS = dict([
               ('|>',':icond:'),
               ('<=>',':equiv:'), 
               (' =>',':implies:'),
               ('||',':or:'),
               ('&&',':and:')
             ])
rgx_ob = re.compile(r'\b(.*?)\]')

# ===================================================================#
#   REPRESENTATION OBJECTS CLASSES AND SUBCLASSES
# ===================================================================#

class Representation(object):
    """This class is a container for internal agent's representations. 
    An agent can have any number of such representations at any moment, 
    all of which are contained in this object.
    
    The class includes methods to encode and decode the representations 
    to/from data streams or idioms.
    
    Attributes:
        individuals -> Unique members (entities) of their own set/class.
        | Entities are denoted with a $ symbol followed by a name.
        classes -> Sets of objects that share a common property.
    """
    def __init__(self):
        self.individuals = {}
        self.classes = {}
        self.bmsWrapper = core.bms.BmsWrapper(self)

    def tell(self, sent):
        """Parses a sentence into an usable formula and stores it into
        the internal representation along with the corresponding classes.
        In case the sentence is a predicate, the objects get declared
        as members of their classes.
        
        Accepts first-order logic sentences sentences, both atomic 
        sentences ('Lucy is a professor') and complex sentences compossed 
        of different atoms and operators ('If someone is a professor,
        then it's a person'). Examples:
        
        >>> r.tell("professor[$Lucy,u=1]")
        will include the individual '$Lucy' in the professor category)
        >>> r.tell(":vars:x: (professor[x,u=1] |= person[x,u=1])")
        all the individuals which are professors will be added to the
        person category, and the formula will be stored in the professor
        class for future use.
        
        For more examples check the LogSentence class docs.
        """
        ori, comp, hier = parse_sent(sent)
        par_form = comp[ori]
        if not ':vars:' in par_form:
            if '[' in par_form and len(comp) == 1:
                # It's a predicate
                self.declare(par_form)
            elif any(symb in par_form for symb in GL_PCONDS):
                # It's a complex sentence with various predicates/funcs
                sent = make_logic_sent(ori, comp, hier)
                if sent.validity is True or sent.validity is None:
                    del sent.validity
                    self.save_rule(sent)
                else:
                    msg = "Illegal connectives used in the consequent " \
                        + " of an indicative conditional sentence."
                    raise AssertionError(msg)
            else:
                msg = "No indicative conditional, implication or " \
                "equality found."
                raise AssertionError(msg)
        else:
            # It's a complex sentence with variables
            sent = make_logic_sent(ori, comp, hier)
            self.add_cog(sent)
    
    def ask(self, sent, single=False):
        """Asks the KB if some fact is true and returns the result of
        that ask.
        """
        inf_proc = Inference(self, parse_sent(sent)[1])
        if single is True:
            for answ in inf_proc.results.values():
                for pred in answ.values():
                    if pred is False: return False
                    if pred is None: return None
            return True        
        return inf_proc.results

    def declare(self, sent, save=False):
        """Declares an object as a member of a class or the relationship
        between two objects. Declarations parse well-formed statements.
        
        Input: a string with one of the two following forms:
        1) "silly[$Lucy,u=0.2]" -> Declares the entity '$Lucy' as a member 
        of the 'silly' class. u = 0.2 is the fuzzy modifier, which can go 
        from 0 to 1.
        
        Declarations of membership can only happen to entities, objects
        which are the only member of their class. To denote an entity we use
        the $ symbol before the entity name.
        
        2) "<friend[$Lucy,u=0.2; $John]>" -> Declares a mapping of the 
        'friend' type between the entities '$Lucy' and '$John'. 
        $John: friend -> $Lucy, 0.2
        
        Declarations of mapping can happen between entities, classes, 
        or between an entity and a class (ie. <loves[cats,u=1, $Lucy]>).
        """
        
        def declare_memb():
            assert ('=' in e), "It's a predicate, must assign truth value."
            u = e.split(',u=')
            u[1] = float(u[1])
            pred = sets[0], (u[0], u[1])
            if (u[1] > 1 or u[1] < 0): 
                m = "Illegal value: {0}, must be > 0, or < 1.".format(u[1])
                raise AssertionError(m)
            self.up_memb(pred)
        
        sent = sent.replace(' ','')
        sets = rgx_ob.findall(sent)
        sets = sets[0].split('[')
        if ';' in sets[1]:
            sets[1] = sets[1].split(';')
        if '<' in sent[0]:
            # Is a function declaration -> implies a relation
            # between different objects or classes.           
            func = make_function(sent, 'relation')            
            self.up_rel(func)
        else:
            # Is a membership declaration -> the object(s) belong(s) 
            # to a set of objects.
            if isinstance(sets[1], list):
                for e in sets[1]:
                    declare_memb()
            else:
                e = sets[1]
                declare_memb()

    def up_memb(self, pred):
        # It's a membership declaration.
        #
        # Here the change should be recorded in the BMS
        # self.bmsWrapper.add(pred, True)
        categ, subject, val = pred[0], pred[1][0], pred[1][1]
        if subject not in self.individuals and '$' in subject:
            # An individual which is member of a class
            ind = Individual(subject)
            ind.add_ctg(categ, val)
            self.individuals[subject] = ind
        elif '$' in subject:
            # Add/replace an other class membership to an existing individual
            self.individuals[subject].add_ctg(categ, val)            
        elif subject in self.classes:            
            self.classes[subject].add_parent((categ,val))
        else:
            # Is a new subclass of an other class
            cls = Category(subject)
            cls.type_ = 'class'
            cls.add_parent((categ,val))
            self.classes[subject] = cls
        if categ not in self.classes:
            nc = Category(categ)
            nc.type_ = 'class'
            self.classes[categ] = nc

    def up_rel(self, func):
        # It's a function declaration.
        #
        # Here the change should be recorded in the BMS 
        # self.bmsWrapper.add(func, True)
        relation = func.func
        for subject in func.get_args():
            if '$' in subject:
                # It's a rel between an object and other obj/class.
                if subject not in self.individuals:
                    ind = Individual(subject)
                    ind.add_rel(func)
                    self.individuals[subject] = ind
                else:
                    ind = self.individuals[subject]
                    ind.add_rel(func)
                if relation not in self.classes:
                    rel = Relation(relation)
                    self.classes[relation] = rel
            else:
                # It's a rel between a class and other class/obj.
                if subject not in self.classes:
                    categ = Category(subject)
                    categ.add_rel(func)
                    self.classes[subject] = categ
                else:
                    self.classes[subject].add_rel(func)
                if relation not in self.classes:
                    rel = Relation(relation)
                    self.classes[relation] = rel

    def add_cog(self, sent):
        
        def chk_args(p):
            if sbj not in sent.var_order:
                if '$' in sbj and sbj in self.individuals:
                    self.individuals[sbj].add_cog(p, sent)
                elif '$' in sbj:
                    ind = Individual(sbj)
                    ind.add_cog(p, sent)
                    self.individuals[sbj] = ind
                elif sbj in self.classes:
                    self.classes[sbj].add_cog(sent)
                else:
                    if not issubclass(pclass, LogFunction): 
                        c = 'class'
                    else: c = 'relation'
                    nc = Category(sbj)
                    nc.type_ = c
                    nc.add_cog(sent)
                    self.classes[sbj] = nc
            else:
                if p in self.classes:
                    self.classes[p].add_cog(sent)
                else:
                    if not issubclass(pclass, LogFunction): 
                        c = 'class'
                    else: c = 'relation'
                    nc = Category(p)
                    nc.type_ = c
                    nc.add_cog(sent)
                    self.classes[p] = nc
        
        preds = []
        for p in sent:
            if p.cond == ':predicate:':
                preds.append(p.pred)
        for pred in preds:
            pclass = pred.__class__
            if issubclass(pclass, LogFunction):
                for arg in pred.args:
                    if isinstance(arg, tuple): sbj = arg[0]
                    else: sbj = arg
                    chk_args(pred.func)
            else:
                if ',u' in pred[1]:
                    sbj, p = pred[1].split(',u')[0], pred[0]
                else:
                    sbj, p = pred[1], pred[0]
                chk_args(p)

    def save_rule(self, proof):
        preds = proof.get_pred()
        preds.extend(proof.get_pred(branch='r'))
        n = []
        for p in preds:
            if issubclass(p.__class__, LogFunction): name = p.func
            else: name = p[0]
            n.append(name)
            if name in self.classes and \
            proof not in self.classes[name].cog:
                self.classes[name].add_cog(proof)
            else:
                c = 'class' if len(name) == 2 else 'relation'
                nc = Category(name)
                nc.type_ = c
                nc.add_cog(proof)
                self.classes[name] = nc
        # Run the new formula with individuals/classes that matches.
        obj_dic = self.inds_by_cat(set(n))
        cls_dic = self.cls_by_cat(set(n))
        obj_dic.update(cls_dic)
        subrpr = SubstRepr(self, obj_dic)
        for ind in subrpr.individuals.keys():
            proof(subrpr, ind)
            if hasattr(proof,'result'): del proof.result
        self.push(subrpr)

    def inds_by_cat(self, ctgs):
        ctg_dic = {}
        for ind in self.individuals.values():
            s = ind.check_cat(ctgs)
            t = set(ind.get_rel())
            t = t.intersection(ctgs)
            t = t.union(s)
            ctg_dic[ind.name] = t
        return ctg_dic
    
    def cls_by_cat(self, ctgs):
        ctg_dic = {}
        for cls in self.classes.values():
            s = cls.check_parents(ctgs)
            t = set(cls.get_rel())
            t = t.intersection(ctgs)
            t = t.union(s)
            ctg_dic[cls.name] = t
        return ctg_dic
    
    def push(self, subs):
        """Takes a SubstRepr object and pushes changes to self.
        It calls the BMS to record any changes and inconsistencies.
        """
        if hasattr(subs,'individuals'):
            self.individuals.update(subs.individuals)
        if hasattr(subs,'classes'):
            self.classes.update(subs.classes)

class Individual(object):
    """An individual is the unique member of it's own class.
    Represents an object which can pertain to multiple classes or sets.
    It's an abstraction owned by an agent, the internal representation 
    of the object, not the object itself.
    
    An Individual inherits the properties of the classes it belongs to,
    and has some implicit attributes which are unique to itself.
    
    Membership to a class is denoted (following fuzzy sets) by a
    real number between 0 and 1. If the number is one, the object will
    will always belong to the set, if it's zero, it will never belong to
    the set.
    
    For example, an object can belong to the set 'cold' with a degree of
    0.9 (in natural language then it would be 'very cold') or 0.1
    (then it would be 'a bit cold', the subjective adjectives are defined
    in the category itself).
    
    Attributes:
        id -> Unique identifier for the object.
        name -> Name of the unique object.
        categ -> Categories to which the object belongs.
        | Includes the degree of membership (ie. ('cold', 0.9)).
        attr -> Implicit attributes of the object, unique to itself.
        cog (opt) -> These are the cognitions/relations attributed to the
        | object by the agent owning this representation.
        relations (opt) -> Functions between objects and/or classes.
    """
    def __init__(self, name):
        self.id = str(uuid.uuid4())
        self.name = name
        self.categ = []
        self.attr = {}
        self.relations = {}
        self.cog = {}

    def set_attr(self, **kwargs):
        """Sets implicit attributes for the class, if an attribute exists
        it's replaced.
        
        Takes a dictionary as input.
        """
        for k, v in kwargs.items():
            self.attr[k] = v

    def infer(self):
        """Inferes attributes of the entity from it's classes."""
        pass
    
    def add_cog(self, p, sent):
        if p in self.cog and sent not in self.cog[p]:
            self.cog[p].append(sent)
        else:
            self.cog[p] = [sent]
        
    def add_ctg(self, ctg, val):
        ctg_rec = [x for (x,_) in self.categ]
        if ctg not in ctg_rec:
            self.categ.append((ctg, val))
        else:
            idx = ctg_rec.index(ctg)
            self.categ[idx] = (ctg, val)
    
    def check_cat(self, n):
        """Returns a list that is the intersection of the input iterable
        and the categories of the object.
        """
        s = [c[0] for c in self.categ if c[0] in n]
        return s

    def get_cat(self, ctg=None):
        """Returns a dictionary of the categories of the object and
        their truth values.
        
        If a single category is provided in the 'ctg' keyword argument,
        then the value for that category is returned. If it doesn't
        exist, None is returned.
        """
        cat = {k:v for k,v in self.categ}
        if ctg is None:
            return cat
        else:
            try: x = cat[ctg]
            except KeyError: return None
            else: return x
    
    def add_rel(self, func):
        try:
            rel = self.relations[func.func]
        except KeyError:
            self.relations[func.func] = [func]
        else:
            rel.append(func)
    
    def get_rel(self):
        """Returns a list of the relations the object is involved
        either as subject, object or indirect object.
        """
        rel = [k for k in self.relations]
        return rel
    
    def test_rel(self, func):
        """Checks if a relation exists; and returns true if it's 
        equal to the comparison, false if it's not, and None if it
        doesn't exist.
        """
        try:
            funcs = self.relations[func.func]
        except KeyError:
            return None      
        for f in funcs:
            if f.args_ID == func.args_ID:
                if func == f: return True
                else: return False
        return None

    def __str__(self):
        s = "<individual '" + self.name + "' w/ id: " + self.id + ">"
        return s

class Category(object):
    """A category is a set/class of entities that share some properties.    
    It can be a subset of others supersets, and viceversa.
    
    Membership is not binary, but fuzzy, being the extreme cases (0, 1)
    the classic binary membership. Likewise, membership to a class can be 
    temporal. For more info check 'Individual' class.
    
    All the attributes of a category are inherited by their members
    (to a degree).
    """
    def __init__(self, name, **kwargs):
        self.name = name
        self.cog = []
        if kwargs:
            for k, v in kwargs.items():
                if k == 'parents': setattr(self, 'parents', v)
                else: self[k] = v
    
    def infer(self):
        """Infers attributes of the class from it's members."""
        pass
    
    def add_cog(self, sent):
        if sent not in self.cog: self.cog.append(sent)
    
    def add_rel(self, func):
        if not hasattr(self, 'relations'):
            self.relations = dict()
            self.relations[func.func] = [func]
        else:
            try: rel = self.relations[func.func]
            except KeyError: self.relations[func.func] = [func]
            else: rel.append(func)
    
    def get_rel(self):
        """Returns a list of the relations the object is involved
        either as subject, object or indirect object.
        """
        if hasattr(self, 'relations'): rel = [k for k in self.relations]
        else: rel = []
        return rel
    
    def test_rel(self, func):
        """Checks if a relation exists; and returns true if it's 
        equal to the comparison, false if it's not, and None if it
        doesn't exist.
        """
        try:
            funcs = self.relations[func.func]
        except KeyError:
            return None      
        for f in funcs:
            if f.args_ID == func.args_ID:
                if func == f: return True
                else: return False
        return None
    
    def check_parents(self, n):
        """Returns a list that is the intersection of the input iterable
        and the parents of the object.
        """
        if not hasattr(self,'parents'): return list()
        return [c[0] for c in self.parents if c[0] in n]
    
    def get_parents(self, ctg=None):
        """Returns a dictionary of the parents of this class and
        their truth values.
        
        If a single category is provided in the 'ctg' keyword argument,
        then the value for that category is returned. If it doesn't
        exist, None is returned.
        """
        cat = {k:v for k,v in self.parents}
        if ctg is None: return cat
        else:
            try: x = cat[ctg]
            except KeyError: return None
            else: return x
    
    def add_parent(self, ctg):
        if not hasattr(self,'parents'): self.parents = [ctg]
        else: self.parents.append(ctg)
    
class Relation(Category):
    
    @property
    def add_rel(self, func):
        raise AttributeError("'Relation' object has no attribute 'add_rel'.")

class Group(Category):
    """A special instance of a category. It defines a 'group' of
    elements that pertain to a class.
    """

class Part(Category):
    """A special instance of a category. It defines an element
    which is a part of an other object.
    """

# ===================================================================#
#   LOGIC INFERENCE                                                  #
# ===================================================================#

class Inference(object):
    
    class InferNode(object):
        def __init__(self, nc, ants, cons, rule):
            self.rule = rule
            self.cons = cons
            self.ants = tuple(nc)
            self.subs = {v:set() for v in rule.var_order}
            for ant in ants:
                if not isinstance(ant, tuple):
                    args = ant.get_args()
                    for v in args:
                        if v in self.subs: self.subs[v].add(ant.func)
                else:
                    v = ant[1].split(',u')
                    if v[0] in self.subs:
                        self.subs[v[0]].add(ant[0])
    
    class NoSolutionError(Exception):
        """Cannot infer a solution error."""
    
    def __init__(self, kb, *args):
        self.kb = kb
        self.vrs = set()
        self.nodes = {}
        self.infer_facts(*args)

    def infer_facts(self, comp):
        """Inference function from first-order logic sentences.

        Gets a query from an ASK, encapsulates the query subtitutions, 
        processes it (including caching of partial results or tracking
        var substitution) and returns the answer to the query. If new 
        knowledge is produced then it's passed to an other procedure for
        addition to the KB.
        """
        
        def chk_result():
            isind = True if var[0] == '$' else False
            if issubclass(pclass, LogFunction):
                try:
                    if isind is True: 
                        res = self.subkb.individuals[var].test_rel(pred)
                    else:
                        res = self.subkb.classes[var].test_rel(pred)
                except KeyError: res = None
            else:
                try:
                    if isind is True:
                        ctgs = self.subkb.individuals[var].get_cat()
                    else:
                        ctgs = self.subkb.classes[var].get_parents()
                except KeyError: res = None
                else:
                    if pred[0] in ctgs:
                        val = ctgs[pred[0]]
                        qval = float(pred[1][2:])
                        if pred[1][1] == '=' and val == qval: res = True
                        elif pred[1][1] == '<' and val < qval: res = True
                        elif pred[1][1] == '>' and val > qval: res = True
                        else: res = False
                    else: res = None
            self.results[var][q] = res
        
        # Parse the query
        self.get_query(comp)
        # Get relevant rules to infer the query
        self.rules, self.done = set(), [None]
        while hasattr(self, 'ctgs'):
            try: self.get_rules()
            except Inference.NoSolutionError: pass
        # Get the caterogies for each individual/class
        self.obj_dic = self.kb.inds_by_cat(self.chk_cats)
        cls_dic = self.kb.cls_by_cat(self.chk_cats)
        self.obj_dic.update(cls_dic)
        # Create a new, filtered and temporal, work KB
        self.subkb = SubstRepr(self.kb, self.obj_dic)
        # Start inference process
        self.results = dict()
        for var, preds in self.query.items():
            if var in self.vrs:
                for pred in preds:
                    pclass = pred.__class__
                    if issubclass(pclass, LogFunction): q = pred.func
                    else: q = pred[0]
                    for var,v in self.obj_dic.items():
                        if q in v:
                            if var not in self.results:         
                                self.results[var] = {}
                            chk_result()
            else:
                self.results[var] = {}
                for pred in preds:
                    pclass = pred.__class__
                    self.rule_tracker()
                    if issubclass(pclass, LogFunction): 
                        self.actv_q, q = (var, pred.func), pred.func
                    else: 
                        self.actv_q, q = (var, pred[0]), pred[0]             
                    k, result, self.updated = True, None, list()                    
                    #print('query: {0}'.format(self.actv_q))
                    while result is not True  and k is True:
                        # Run the query, if there is no result and there is
                        # an update, then rerun it again, else stop
                        chk, done = list(), list()              
                        result = self.unify(q, chk, done)
                        k = True if True in self.updated else False
                        #run = 'result: {0}, updated: {1} // rerun: {2}'
                        #print(run.format(result, self.updated ,k ))
                        self.updated = list()
                    # Update the result from the subtitution repr
                    chk_result()

    def unify(self, p, chk, done):
        # for each node in the subtitution tree unifify variables
        # and try every possible substitution
        if p in self.nodes:
            for node in self.nodes[p]:     
                self.recursive_sub(node)
                if p not in done:
                    chk = list(node.ants) + chk
        if self.actv_q[0] in self.obj_dic and \
        self.actv_q[1] in self.obj_dic[self.actv_q[0]]:
            return True
        elif len(chk) > 0:
            done.append(p)
            p = chk.pop(0)
            self.unify(p, chk, done)

    def recursive_sub(self, node):
        def add_ctg():
            # added category/function to the object dictionary
            for r in node.rule.result:
                if issubclass(r.__class__, LogFunction):
                    args = r.get_args()
                    for sbs in args:
                        try:
                            self.obj_dic[sbs].add(r.func)
                        except KeyError:
                            self.obj_dic[sbs] = set([r.func])
                else:
                    cat, obj = r[0], r[1][0]
                    try :
                        self.obj_dic[obj].add(cat)
                    except KeyError:
                        self.obj_dic[obj] = set([cat])
            self.queue[node]['pos'].add(key)
        
        # check what are the possible var substitutions
        mapped = self.map_vars(node)
        # permute and find every argument combination
        mapped = list(itertools.product(*mapped))
        # run proof until a solution is found or there aren't more
        # combinations
        res = hasattr(node.rule, 'result')
        while res is False and (len(mapped) > 0):
            args = mapped.pop()
            key = hash(args)
            if key in self.queue[node]['neg'] and self.updated is True:
                node.rule(self.subkb, args)
                res = hasattr(node.rule, 'result')
                if res is True and node.rule.result is not False:
                    self.updated.append(True)
                    add_ctg()
                    del node.rule.result
                elif res is True:
                    self.queue[node]['neg'].add(key)
                    del node.rule.result
            elif (key not in self.queue[node]['pos']) \
            and (key not in self.queue[node]['neg']):
                node.rule(self.subkb, args)
                res = hasattr(node.rule, 'result')
                if res is True and node.rule.result is not False:
                    self.updated.append(True)
                    add_ctg()
                    del node.rule.result
                elif res is True:
                    self.queue[node]['neg'].add(key)
                    del node.rule.result

    def map_vars(self, node):
        # map values to variables for subtitution
        subs_num = len(node.subs)
        subactv = [set()] * subs_num
        for i, t in enumerate(node.subs.values()):
            y = len(t)               
            for obj, s in self.obj_dic.items():
                x = len(s)            
                if x >= y:
                    r = s.intersection(t)
                    if len(r) == y:
                        subactv[i].add(obj)
        return subactv

    def get_rules(self):
        if len(self.ctgs) > 0: c = self.ctgs.pop()
        else: c = None
        if c is not None:
            self.done.append(c)            
            try:
                chk_rules = set(self.kb.classes[c].cog)
                chk_rules = chk_rules.difference(self.rules)
            except:
                raise Inference.NoSolutionError(c)
            for sent in chk_rules:
                preds = sent.get_pred(conds=GL_PCONDS)
                nc = []
                for y in preds:
                    if type(y) == tuple: nc.append(y[0])
                    else: nc.append(y.func)
                self.mk_nodes(nc, preds, sent, 'right')
                nc2 = [e for e in nc if e not in self.done and e not in self.ctgs]
                self.ctgs.extend(nc2)
                if c in nc:
                    preds = sent.get_pred(branch='right', conds=GL_PCONDS)
                    nc = []
                    for y in preds:
                        if type(y) == tuple: nc.append(y[0])
                        else: nc.append(y.func)
                    self.mk_nodes(nc, preds, sent, 'left')
                    nc2 = [e for e in nc if e not in self.done \
                           and e not in self.ctgs]
                    self.ctgs.extend(nc2)
            self.rules = self.rules.union(chk_rules)
            self.get_rules()
        else:
            self.done.pop(0)
            self.chk_cats = set(self.done)
            del self.done
            del self.rules
            del self.ctgs

    def mk_nodes(self, nc, ants, rule, pos):
        # makes inference nodes for the evaluation
        preds = rule.get_pred(branch=pos, conds=GL_PCONDS)
        for cons in preds:
            if issubclass(cons.__class__, LogFunction):
                pred = cons.func
            else:
                pred = cons[0]
            node = self.InferNode(nc, ants, pred, rule)
            if node.cons in self.nodes:
                self.nodes[node.cons].append(node)
            else:
                self.nodes[node.cons] = [node]

    def rule_tracker(self):
        # create a dictionary for tracking what proofs have been run or not
        if hasattr(self, 'queue') is False:
            self.queue = dict()
            for query in self.nodes.values():
                for node in query:
                    self.queue[node] = {'neg': set(), 'pos': set()}
        else:
            for node in self.query:
                self.queue[node] = {'neg': set(), 'pos': set()}

    def get_query(self, comp):

        def break_pred():
            pr = rgx_ob.findall(p)[0].split('[')
            # It's a function
            if '<' in p[0]:                
                t = pr[1].split(';')
                for x, obj in enumerate(t):
                    t[x] = obj.split(',')[0]
                func = make_function(p, 'relation')
                return (t, func)                
            # It's a predicate
            if ';' in pr[1]:
                t = pr[1].split(';')
                if len(t) != 3:
                    t.append(None)
                pr[0], pr[1] = t[1], (pr[0], tuple(t[0].split(',')), t[2])
            else:
                t = pr[1].split(',')
                pr = t[0], (pr[0], t[1])
            return pr
        
        preds, del_ = list(), list()
        for i, pa in enumerate(comp):
            pa = pa.replace(' ','').strip()            
            if ':vars:' in pa:
                form = pa.split(':')
                for i, a in enumerate(form):
                    if a == 'vars':
                        vars_ = form[i+1].split(',')
                        for var in vars_: self.vrs.add(var)
                        del_.append(i)
            elif not any(s in pa for s in SYMBS.values()):
                preds.append(pa)
        for x in del_: comp.pop(x)
        for i, p in enumerate(preds):
            preds[i] = break_pred()
        terms, ctgs = {}, []
        for p in preds:
            names, pclass = p[0], p[1].__class__
            if issubclass(pclass, LogFunction):
                func = p[1]
                ctgs.append(func.func)
                for obj in names:
                    if obj not in terms.keys():
                        terms[obj] = [func]
                    else:
                        terms[obj].append(func)
            elif names not in terms.keys():
                terms[names] = [p[1]]
                ctgs.append(p[1][0])
            else:
                terms[names].append(tuple(p[1]))
                ctgs.append(p[1][0])
        self.query, self.ctgs = terms, ctgs

class SubstRepr(Representation):
    """During an inference the original KB is isolated and only
    the relevant classes and entities are copied into a temporal
    working KB.
    
    Once the inference is done, results are cleaned up, saved
    in the KB and the BMS routine is ran.
    """
    
    class FakeBms(object):
        
        def __init__(self):
            self.chgs_dict = dict()
        
        def register(self, form):
            self.chgs_dict[form] = (list(), list())
            self.chk_ls = self.chgs_dict[form][0]
            self.prod = self.chgs_dict[form][1]
        
        def prev_blf(self, arg):
            self.prod.append(arg)
        
        def check(self, arg):
            self.chk_ls.append(arg)

    def __init__(self, *args):
        self.individuals = {}
        self.classes = {}
        self.bmsWrapper = self.FakeBms()
        self.make(*args)
    
    def register(self, form):        
        self.bmsWrapped.register(form)
    
    def make(self, kb, obj_dic):
        for s in obj_dic:
            if '$' in s[0]:
                # It's an individual
                o_ind = kb.individuals[s]
                n_ind = Individual(s)
                for attr, val in o_ind.__dict__.items():
                    if attr != 'relations' or attr != 'categ':
                        n_ind.__dict__[attr] = val
                nrm_ctg = obj_dic[s]
                categ = []
                for c in o_ind.categ:
                    if c[0] in nrm_ctg:
                        categ.append(c)
                rels = {}
                for rel in o_ind.relations:
                    if rel in nrm_ctg:
                        rels[rel] = o_ind.relations[rel]
                n_ind.relations, n_ind.categ = rels, categ
                self.individuals[n_ind.name] = n_ind
            else:
                # It's a class
                o_cls = kb.classes[s]
                n_cls = Category(s)
                for attr, val in o_cls.__dict__.items():
                    if attr != 'relations' or attr != 'parents':
                        n_cls.__dict__[attr] = val
                nrm_ctg = obj_dic[s]
                categ = []
                if hasattr(o_cls, 'parents'):
                    for c in o_cls.parents:
                        if c[0] in nrm_ctg:
                            categ.append(c)
                    o_cls.parents = categ
                if hasattr(o_cls, 'relations'):
                    rels = {}
                    for rel in o_cls.relations:
                        if rel in nrm_ctg:
                            rels[rel] = o_cls.relations[rel]
                    n_cls.relations = rels
                n_cls.parents = categ
                self.classes[n_cls.name] = n_cls
        

if __name__ == '__main__':
    import os
    
    def load_sentences(test, path):
        logic_test = os.path.join(path, 'knowledge_base', test)
        ls, sup_ls = [], []
        with open(logic_test, 'r') as f:
            for line in f:
                if line.strip()[0] == '#': pass
                elif line.strip() == '{':
                    sup_ls, ls = ls, list()
                elif line.strip() == '}':
                    sup_ls.append(ls)
                    ls = sup_ls
                else: ls.append(line.strip())
        return ls    
    
    def test_ask(path, test, ask):        
        sents = load_sentences(test, path)        
        for s in sents[1]:
            r.tell(s)        
        results = []
        for q in ask:
            results.append(r.ask(q, single=False))
        print('\n==== RESULTS ====')
        for res in results:
            print(res)
    
    r = Representation()
    path = '~/dev/workspace/simag/tests'
    test = 'ask_func.txt'
    ask = ['<friend[$Lucy,u=0;$John]>']
    #test_ask(path, test, ask)
    fol = ['animal[cow,u=1]',
           'animal[chicken,u=1]']
    for s in fol:
        r.tell(s)
    res = r.ask(':vars:x: (animal[x,u=1])')
    print(res)
    
    