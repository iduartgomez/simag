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

Support classes and methods
---------------------------
:class: LogSentence. Stores a serie of logical atoms (be them predicates or
connectives), that form a well-formed logic formula. These are rulesets 
for reasoning, cataloging objects into sets/classes, and the relationships 
between these objects.

@author: Ignacio Duart Gómez
"""

# TO DO: Optimize chaining algorithm further: in functions it can check 
# wether the argument position fits or not before trying to solve it.
#
# Make declaration, save_rules and inference work with categories.
# Add 'belief maintenance system' functionality.

# ===================================================================#
#   Imports and constants
# ===================================================================#

import re
import os
import uuid
import copy

import core.bms
from builtins import ValueError

# Regex
rgx_par = re.compile(r'\{(.*?)\}')
rgx_ob = re.compile(r'\b(.*?)\]')
rgx_br = re.compile(r'\}(.*?)\{')

gr_conds = [':icond:', ':implies:', ':equiv:']

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
            else:
                # It's a complex sentence with various predicates/funcs
                sent = LogSentence(ori, comp, hier)
                if sent.validity is True:
                    del sent.validity
                    self.save_rule(sent)
                else:
                    msg = "Illegal connectives used in the consequent " \
                        + " of an indicative conditional sentence."
                    raise AssertionError(msg)
        else:
            # It's a complex sentence with variables
            sent = LogSentence(ori, comp, hier)
            self.add_cog(sent)
    
    def ask(self, sent, single=False):
        """Asks the KB if some fact is true and returns the result of
        that ask.
        """
        inf_proc = Inference(self, parse_sent(sent)[1])
        print(inf_proc.results)
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
        
        2) "<friend[$John, $Lucy,u=0.2]>" -> Declares a mapping of the 
        'friend' type between the entities '$Lucy' and '$John'. 
        $John: friend -> $Lucy, 0.2
        
        Declarations of mapping can happen between entities, classes, 
        or between an entity and a class (ie. <loves[$Lucy, cats]>).
        """
        
        def declare_memb():
            assert ('=' in e), "It's a predicate, must assign truth value."
            assert ('$' in e), 'The object is not an unique entity.'
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
            # Is an existing subclass subclass of an other class
            pass
        else:
            # Is a new subclass of an other class
            pass
        if categ not in self.classes:
            nc = Category(categ)
            nc['type'] = 'class'
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
                    c = 'class' if pclass is not LogFunction else 'relation'
                    nc = Category(sbj)
                    nc['type'] = c
                    nc.add_cog(sent)
                    self.classes[sbj] = nc
            else:
                if p in self.classes:
                    self.classes[p].add_cog(sent)
                else:
                    c = 'class' if pclass is not LogFunction else 'relation'
                    nc = Category(p)
                    nc['type'] = c
                    nc.add_cog(sent)
                    self.classes[p] = nc
        
        preds = []
        for p in sent:
            if p.cond == ':predicate:':
                preds.append(p.pred)
        for pred in preds:
            pclass = pred.__class__.__bases__[0]
            if pclass is LogFunction:
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
        for name in preds:
            if name[0] in self.classes and \
            proof not in self.classes[name[0]]['cog']:
                self.classes[name[0]].add_cog(proof)
            else:
                c = 'class' if len(name) == 2 else 'relation'
                nc = Category(name[0])
                nc['type'] = c
                nc.add_cog(proof)
                self.classes[name[0]] = nc
        n = [x[0] for x in preds]
        # Run the new formula with individuals/classes that matches.
        obj_dic = self.inds_by_cat(set(n))
        subrpr = SubstRepr(self, obj_dic)
        for ind in subrpr.individuals.keys():
            proof(subrpr, ind)
        self.push(subrpr)

    def inds_by_cat(self, ctgs):
        cat_dic = {}
        for ind in self.individuals.values():
            s = ind.check_cat(ctgs)
            t = set(ind.get_rel())
            t = t.intersection(ctgs)
            t = t.union(s)
            cat_dic[ind.name] = t
        return cat_dic
    
    def push(self, subs):
        """Takes a SubstRepr object and pushes changes to self.
        It calls the BMS to record any changes and inconsistencies.
        """
        self.individuals.update(subs.individuals)
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
        cog (opt) -> These are the cognitions attributed to the object by 
        | the agent owning this representation.
        relations (opt) -> Functions between objects and/or classes.
    """
    def __init__(self, name):
        self.id = str(uuid.uuid4())
        self.name = name
        self.attr = {}
        self.categ = []
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
    
    def check_cat(self, n):
        """Returns a list that is the intersection of the input iterable
        and the categories of the object.
        """
        s = [c[0] for c in self.categ if c[0] in n]
        return s
    
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

    def get_cat(self, ctg=None):
        """Returns a dictionary of the categories of the object and
        their 'u' values.
        
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
    
    def get_rel(self):
        """Returns a list of the relations the object is involved
        either as subject, object or indirect object.
        """
        rel = [k for k in self.relations]
        return rel         
    
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
            
    def add_rel(self, func):
        try:
            rel = self.relations[func.func]
        except KeyError:
            self.relations[func.func] = [func]
        else:
            rel.append(func)

    def __str__(self):
        s = "<individual '" + self.name + "' w/ id: " + self.id + ">"
        return s

class Category(dict):
    """A category is a set/class of entities that share some properties.    
    It can be a subset of others supersets, and viceversa. It inherits
    from the dict class.
    
    Membership is not binary, but fuzzy, being the extreme cases (0, 1)
    the classic binary membership. Likewise, membership to a class can be 
    temporal. For more info check 'Individual' class.
    
    All the attributes of a category are inherited by their members
    (to a degree).
    """
    def __init__(self, name):
        self['name'] = name
        self['cog'] = []
    
    def add_cog(self, sent):
        if sent not in self['cog']:
            self['cog'].append(sent)
    
    def infer(self):
        """Infers attributes of the class from it's members."""
        pass
    
    def add_rel(self, func):
        if not self.relations:
            self.relations = dict()
            self.relations[func.func] = [func]
        else:
            try:
                rel = self.relations[func.func]
            except KeyError:
                self.relations[func.func] = [func]
            else:
                rel.append(func)

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
#   LOGIC SENTENCE PARSER
# ===================================================================#

symbs = dict([
             ('|>',':icond:'),
             ('<=>',':equiv:'), 
             (' =>',':implies:'),
             ('||',':or:'),
             ('&&',':and:')
            ])
symb_ord = ['|>', '<=>', ' =>', '||', '&&']

def parse_sent(sent):
    """Parser for logic sentences."""

    def decomp_par(s, symb, f=0):
        initpar = []
        endpar = []
        idx = 0
        while idx < len(s):
            if s[idx] == symb[0]:
                initpar.append(idx)
            elif s[idx] == symb[1]:
                endpar.append(idx)
            idx += 1
        min_ = float('inf')
        for i in initpar:
            for e in endpar:
                diff = abs(e - i)
                if diff < min_ and i < e:
                    min_ = diff
                    par = (i, e)
        if len(initpar) == 0 and len(endpar) == 0:
            comp.append(s[:])
            s = s.replace(s[:], '{'+str(f)+'}')
            return
        elif (len(initpar) == 0 and len(endpar) != 0) or \
             (len(initpar) != 0 and len(endpar) == 0):
            raise AssertionError('Odd number of parentheses.')
        else:
            elem = s[par[0]+1:par[1]]
            comp.append(elem)
            s = s.replace(s[par[0]:par[1]+1], '{'+str(f)+'}')
            f += 1
            return decomp_par(s, symb, f)

    def decomp_symbs():
        symb = [x for x in symb_ord if x in form][0]
        memb = form.split(symb)
        if len(memb) > 2:
            while len(memb) > 2:
                last = memb.pop()                        
                memb[-1] =  memb[-1] + symb + last
        x, y = len(comp), len(comp)+1            
        comp[idx] = '{'+str(x)+'}'+symbs[symb]+'{'+str(y)+'}'
        comp.append(memb[0])
        comp.append(memb[1])
        return True
    
    def iter_childs():
        for n in range(0, ls):
            exp = comp[n]
            childs = rgx_par.findall(exp)
            childs = [int(x) for x in childs]
            if childs != []:
                hier[n] = {'childs': childs, 'parent': -1}
            else:
                hier[n] = {'childs': -1, 'parent': -1}
        for n in range(0, ls):
            childs = hier[n]['childs']
            if childs != -1:
                for c in childs:
                    hier[c]['parent'] = n
    
    comp = []
    hier = {}
    decomp_par(sent.rstrip('\n'), symb=('(', ')'))
    ori = len(comp) - 1
    for idx, form in enumerate(comp):            
        if any(symb in form for symb in symbs.keys()):
            decomp_symbs()
    ls = len(comp)
    iter_childs()
    return ori, comp, hier

class LogFunction(object):
    """Base class to represent a logic function."""
    
    def __init__(self, sent):
        self.args = self.mk_args(sent)
        self.arity = len(self.args)
    
    def mk_args(self, sent):
        func = rgx_ob.findall(sent)[0].split('[')
        self.func, vrs = func[0], func[1]
        args, hls = vrs.split(';'), list()
        for x, arg in enumerate(args):
            if ',u' in arg:
                narg = arg.split(',u')
                narg = narg[0], float(narg[1][1:]), narg[1][0]
                if narg[1] > 1 or narg[1] < 0:
                    raise ValueError(narg[1])
                hls.append(narg[0])
                args[x] = narg
            else:
                hls.append(arg)
        self.args_ID = hash(tuple(hls))
        return args
        
    def get_args(self):
        ls = []
        for arg in self.args:
            if isinstance(arg, tuple):
                ls.append(arg[0])
            else:
                ls.append(arg)
        return ls
    
    def substitute(self, args):
        subs = copy.deepcopy(self)
        subs.args_ID = hash(tuple(args))
        for x, arg in enumerate(subs.args):
            if isinstance(arg, tuple):
                subs.args[x] = list(arg)
                subs.args[x][0] = args[x]
                subs.args[x] = tuple(subs.args[x])
            else:
                subs.args[x] = args[x]
        return subs

def make_function(sent, f_type=None, *args):
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
    types = ['relation']
    
    class NotCompFuncError(Exception):
        """Logic functions are not comparable exception."""
    
        def __init__(self, args):
            self.err, self.arg1, self.arg2 = args  
    
    class RelationFunc(LogFunction):
    
        def __eq__(self, other):
            comparable = self.chk_args_eq(other)
            if comparable is not True:
                raise NotCompFuncError(comparable)
            for x, arg in enumerate(self.args):
                if isinstance(arg, tuple):
                    oarg = other.args[x]
                    if arg[2] == '=' and arg[1] != oarg[1]:  
                        return False                      
                    elif arg[2] == '>'and arg[1] > oarg[1]:
                        return False     
                    elif arg[2] == '<'and arg[1] < oarg[1]:  
                        return False
            return True
        
        def __ne__(self, other):
            comparable = self.chk_args_eq(other)
            if comparable is not True:
                raise NotCompFuncError(comparable)
            for x, arg in enumerate(self.args):
                if isinstance(arg, tuple):
                    oarg = other.arg[x]
                    if arg[2] == '=' and arg[1] != oarg[1]:
                        return True                      
                    elif arg[2] == '>'and arg[1] < oarg[1]:
                        return True     
                    elif arg[2] == '<'and arg[1] > oarg[1]: 
                        return True
            return False
    
        def chk_args_eq(self, other):
            if other.arity != self.arity:
                return ('arity', other.arity, self.arity)
            if other.func != self.func:
                return ('function', other.func, self.func)
            for x, arg in enumerate(self.args):
                if isinstance(arg, tuple):
                    if other.args[x][0] != arg[0]:
                        return ('args', other.args[x][0], arg[0])
                else:
                    if other.args[x] != arg:
                        return ('args', other.args[x], arg)
            return True
    
    assert (f_type in types or f_type is None), \
            'Function {0} does not exist.'.format(f_type)
    if f_type == 'relation':
        return RelationFunc(sent)
    else:
        return LogFunction(sent)

# ===================================================================#
#   LOGIC CLASSES AND SUBCLASSES
# ===================================================================#

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
    def __init__(self, ori, comp, hier):
        self.depth = 0
        self.var_order = []
        self.particles = []
        self.make_parts(ori, comp, hier)
        self.connect_parts()

    def __call__(self, ag, *args):
        if isinstance(args[0], list):
            args = args[0]
        # Clean up previous results.
        self.assigned = {}
        self.cln_res()
        if len(self.var_order) == len(args):
            # Check the properties/classes an obj belongs to
            for n, const in enumerate(args):
                if const not in ag.individuals:
                    return
                var_name = self.var_order[n]
                # Assign an entity to a variable by order.
                self.assigned[var_name] = const
            ag.bmsWrapper.register(self)
            self.start.solve(self, ag, key=[0])
            ag.bmsWrapper.register(self, stop=True)
        elif len(self.var_order) == 0:
            ag.bmsWrapper.register(self)
            self.start.solve(self, ag, key=[0])
            ag.bmsWrapper.register(self, stop=True)
        else:
            return

    def make_parts(self, ori, comp, hier, depth=0):
        form = comp[ori]
        childs = hier[ori]['childs']
        parent = hier[ori]['parent']
        self.new_test(form, depth, parent, ori, childs)
        depth += 1
        for child in childs:
            syb = hier[child]['childs']
            if syb != -1:
                self.make_parts(child, comp, hier, depth)
            else:
                form = comp[child]
                parent = hier[child]['parent']
                self.new_test(form, depth, parent, child, syb=[-1])

    def connect_parts(self):
        particles = []
        icond = False
        lvl = self.depth
        while lvl > -1:
            p = [part for part in self.particles if part.depth == lvl]
            for part in p:
                particles.append(part)
                if part.cond == ':icond:':
                    icond = part
            lvl -= 1
        self.particles = particles
        for p in self.particles:
            p.connect(self.particles)
        # Check for illegal connectives for implicative cond sentences
        self.validity = None
        if icond is not False:
            self.validity = self.get_ops(icond)
        for p in self.particles:
            del p.pID
            p.sent = self
            p.results = []
            if p.parent == -1:
                self.start = p

    def new_test(self, form, depth, parent, part_id, syb):        
        form = form.replace(' ','').strip()
        cond = rgx_br.findall(form)
        if depth > self.depth:
            self.depth = depth
        if len(cond) > 0:
            self.particles.append(Particle(cond[0], depth, part_id,
                                           parent, syb))
        elif any(x in form for x in [':vars:', ':exists:']):
            form = form.split(':')
            cond = ':stub:'
            for i, a in enumerate(form):
                if a == 'vars':
                    vars_ = form[i+1].split(',')
                    for var in vars_:
                        if var not in self.var_order:
                            self.var_order.append(var)
                    self.particles.append(Particle(cond, depth, part_id,
                                                   parent, syb))
        elif '[' in form:
            cond = ':predicate:'
            if '<' in form:
                form = make_function(form, 'relation')
            else:
                form = tuple(rgx_ob.findall(form)[0].split('['))
            self.particles.append(Particle(cond, depth, part_id,
                                           parent, syb, form))
        else:
            cond = ':stub:'
            self.particles.append(Particle(cond, depth, part_id,
                                           parent, syb, form))
    
    def cln_res(self):
        for p in self.particles:
            p.results = []

    def get_ops(self, p, chk_c=[':or:', ':implies:', ':equiv:']):
        ops = []
        for p in self:
            if any(x in p.cond for x in chk_c):
                ops.append(p)
        for p in ops:
            x = p
            while x.cond != ':icond:' or x.parent == -1:
                if x.parent.cond == ':icond:' and x.parent.next[1] == x:
                    return False
                else:
                    x = x.parent
        return True

    def get_pred(self, branch='left', conds=gr_conds):
        preds = []
        for p in self:
            if p.cond == ':predicate:':
                preds.append(p)
        res = []
        for p in preds:
            x = p
            while x.parent.cond not in conds:
                x = x.parent
            if branch == 'left' and x.parent.next[0] == x:
                res.append(p.pred)
            elif branch != 'left' and x.parent.next[1] == x:
                res.append(p.pred)
        return res

    def __iter__(self):
        return iter(self.particles)

class Particle(object):
    """A particle in a logic sentence, that can be either:
    * An operator of the following types: 
    indicative conditional, implies, equiv, and, or.
    * A predicate, declaring a variable/constant as a member of a set, 
    or a function between two variables.
    * A quantifier for a variable: universal or existential.
    """
    def __init__(self, cond, depth, id_, parent, syb, *args):
        self.pID = id_
        self.depth = depth
        self.cond = cond
        self.next = syb
        self.parent = parent
        self.results = []
        if cond == ':predicate:':
            self.pred = args[0]

    def solve(self, proof, ag, key, *args):
        """Keys for solving proofs:
        100: Substitute a child's predicates.
        101: Check the truthiness of a child atom.
        102: Incoming truthiness of an operation for recording.
        103: Return to parent atom.
        """
        #print self, '// Key:'+str(key), '// Args:', args
        if key[-1] == 103 and self.parent == -1:
            return
        if key[-1] == 102 and self.cond and self.cond != ':stub:':
            key.pop()
            self.results.append(args[0])
        if self.cond == ':stub:':
            if key[-1] == 102 or key[-1] == 103:
                self.parent.solve(proof, ag, key, *args)
            else:
                self.next[0].solve(proof, ag, key, *args)
        elif self.cond == ':icond:':
            self.icond(proof, ag, key, *args)
        elif self.cond == ':implies:':
            self.impl(proof, ag, key, *args)
        elif self.cond == ':equiv:':
            self.equiv(proof, ag, key, *args)
        elif self.cond == ':or:' or self.cond == ':and:':
            current = len(self.results)
            if key[-1] == 103 and len(self.next) >= 2:
                self.parent.solve(proof, ag, key)
            elif key[-1] == 103:
                key.pop()
            elif key[-1] == 101:
                if current < len(self.next):
                    key.append(101)
                    self.next[current].solve(proof, ag, key)
                else:
                    if self.cond == ':or:':
                        # Two branches finished, check if one is true.
                        self.disjunction(proof, ag, key)
                    elif self.cond == ':and:':
                        # Two branches finished, check if both are true.
                        self.conjunction(proof, ag, key)
            elif key[-1] == 100:
                if current < len(self.next) and \
                self.next[current].cond == ':predicate:':
                    key.append(103)
                    self.next[current].solve(proof, ag, key)
                elif current < len(self.next):
                    self.next[current].solve(proof, ag, key)
                else:
                    # All substitutions done
                    key.append(103)
                    self.parent.solve(proof, ag, key)
        elif self.pred:
            result = self.ispred(proof, ag, key)
            x = key.pop()
            if x != 100:
                key.append(102)
                self.parent.solve(proof, ag, key, result)
    
    def icond(self, proof, ag, key, *args):
        """Procedure for parsign indicative conditional assertions."""
        current = len(self.results)
        if key[-1] == 103:
            # Completed
            self.parent.solve(proof, ag, key)        
        elif current == 0:
            key.append(101)
            self.next[current].solve(proof, ag, key)
        elif current == 1 and self.results[0] is True:
            key.append(100)
            self.next[current].solve(proof, ag, key)
        elif current == 1 and self.results[0] is False:
            # The left branch was false, so do not continue.
            proof.result = False
            key.append(103)
            self.parent.solve(proof, ag, key, False)
        else:
            # Substitution failed.
            key.append(103)
            self.parent.solve(proof, ag, key, None)
    
    def equiv(self, proof, ag, key, *args):
        """Procedure for solving equivalences."""
        current = len(self.results)
        if key[-1] == 103:
            # Completed
            self.parent.solve(proof, ag, key)
        elif current == 0:
            key.append(101)
            self.next[current].solve(proof, ag, key)
        elif current == 1 and self.results[0] is True:
            # If it's not a predicate, follow standard FOL
            # rules for equiv
            if self.next[1].cond != ':predicate:':
                key.append(101)
                self.next[1].solve(proof, ag, key)
            else:
                key.append(103)
                self.next[1].solve(proof, ag, key)
        elif current > 1:
            # The second term of the implication was complex
            # check the result of it's substitution
            if (self.results[0] and self.results[1]) is None:
                result = None, None
            elif self.results[0] == self.results[1]:
                proof.result, result = True, True
            else:
                if hasattr(proof, 'result') is False:
                    proof.result = True
                result = True
            key.append(103)
            self.parent.solve(proof, ag, key, result)
        else:
            # Not known solution.
            key.append(103)
            self.parent.solve(proof, ag, key, None)
    
    def impl(self, proof, ag, key, *args):
        """Procedure for solving implications."""
        current = len(self.results)
        if key[-1] == 103:
            # Completed
            self.parent.solve(proof, ag, key)
        elif current == 0:
            key.append(101)
            self.next[current].solve(proof, ag, key)
        elif current == 1 and self.results[0] is True:
            # If it's not a predicate, follow standard FOL
            # rules for implication
            if self.next[1].cond != ':predicate:':
                key.append(101)
                self.next[current].solve(proof, ag, key)
            else:
                key.append(103)
                self.next[current].solve(proof, ag, key)
        elif current == 1 and self.results[0] is False:
            # The left branch was false, so do not continue.
            proof.result, result = False, False
            key.append(103)
            self.parent.solve(proof, ag, key, result)        
        elif current > 1:            
            # The second term of the implication was complex
            # check the result of it's substitution
            if (self.results[0] and self.results[1]) is None:
                result = None, None
            elif self.results[0] is True and self.results[1] is False:
                proof.result, result = False, False
            else:
                if hasattr(proof, 'result') is False:
                    proof.result = True
                result = True
            key.append(103)
            self.parent.solve(proof, ag, key, result)
        else:
            # Not known solution.
            key.append(103)
            self.parent.solve(proof, ag, key, None)
    
    def conjunction(self, proof, ag, key):
        left_branch, right_branch = self.results[0], self.results[1]
        if key[-1] == 101:
            key.append(102)
            # Two branches finished, check if both are true.
            if (left_branch and right_branch) is None:                
                self.parent.solve(proof, ag, key, None)            
            elif left_branch == right_branch:
                self.parent.solve(proof, ag, key, True)            
            else:
                self.parent.solve(proof, ag, key, False)
        elif key[-1] == 100:
            # Test if this conjunction fails
            if (left_branch and right_branch) is None:
                # unknown
                return None
            if left_branch == right_branch:
                # passes the test
                return True
            else:
                # fails the test
                return False

    def disjunction(self, proof, ag, key):
        left_branch, right_branch = self.results[0], self.results[1]
        if key[-1] == 101:
            # Two branches finished, check if both are true.
            key.append(102)
            if (left_branch and right_branch) is None:
                self.parent.solve(proof, ag, key, None)
            elif left_branch != right_branch:
                self.parent.solve(proof, ag, key, True)            
            else:
                self.parent.solve(proof, ag, key, False)
        elif key[-1] == 100:
            # Test if this disjunction fails
            if (left_branch and right_branch) is None:
                # unknown
                return None
            elif left_branch != right_branch:
                # passes the test
                return True
            else:
                # fails the test
                return False

    def ispred(self, proof, ag, key):
        
        def isvar(s):
            try:
                s = proof.assigned[s]
            except KeyError:
                pass
            return s
        
        pclass = self.pred.__class__.__bases__[0]
        if key[-1] == 101:            
            if pclass is LogFunction:                
                # Check funct between a set/entity and other set/entity.
                result = None
                args = self.pred.get_args()
                for x, arg in enumerate(args):
                    if arg in proof.assigned:
                        args[x] = proof.assigned[arg]
                test = self.pred.substitute(args)
                if '$' in args[0][0]:
                    result = ag.individuals[args[0]].test_rel(test)
                else:
                    result = ag.classes[args[0]].test_rel(test)
                if result is True:
                    ag.bmsWrapper.prev_blf(test)
            else:
                # Check membership to a set of an entity.
                sbj, u = self.pred[1].split(',u')
                sbj = isvar(sbj)
                categs = ag.individuals[sbj].get_cat()
                check_set = self.pred[0]
                uval = float(u[1:])
                # If is True, then the object belongs to the set.
                # Else, must be False, and the object must not belong.
                result = None
                if check_set in categs:
                    val = categs[check_set]
                    if u[0] == '=' and uval == val:
                        result = True
                    elif u[0] == '>' and uval > val:
                        result = True
                    elif u[0] == '<' and uval == val:
                        result = True
                    else:
                        result = False
                if result is True:
                    s = check_set+'['+sbj+',u'+u[0]+str(uval)+']'
                    ag.bmsWrapper.prev_blf(s)
            return result
        else:
            # marked for declaration
            # subtitute var(s) for constants
            # and pass to agent for updating
            if pclass is LogFunction:                
                args = self.pred.get_args()
                for x, arg in enumerate(args):
                    if arg in proof.assigned:
                        args[x] = proof.assigned[arg]
                pred = self.pred.substitute(args)
                ag.bmsWrapper.check(pred)
                ag.up_rel(pred)
            else:
                pred = list(self.pred)
                sbj, u = self.pred[1].split(',u')
                pred[1] = isvar(sbj)
                pred = (pred[0], [pred[1], u])
                ag.bmsWrapper.check(pred)
                pred[1][1] = float(u[1:])
                ag.up_memb(pred)
            if key[-1] == 103 and hasattr(proof, 'result'):                
                proof.result.append(pred)
            elif key[-1] == 103:
                proof.result = [pred]

    def __str__(self):
        if self.cond != ':predicate:':
            s = '<operator ' + ' (depth:' + str(self.depth) + ') "' \
            + str(self.cond) + '">'
        else:
            s = '<predicate ' + ' (depth:' + str(self.depth) + '): ' \
            + str(self.pred) + '>'
        return s
    
    def connect(self, part_list):
        for x, child in enumerate(self.next):
            for part in part_list:
                if part.pID == child:
                    self.next[x] = part
                    self.next[x].parent = self

# ===================================================================#
#   LOGIC INFERENCE
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
    
    def __init__(self, kb, *args):
        self.kb = kb
        self.vrs = {}
        self.nodes = {}
        self.queue = {}
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
            if var[0] == '$':
                try: ctgs = self.subkb.individuals[var].get_cat()
                except KeyError: return None
                else:
                    if pred[0] in ctgs:
                        val = ctgs[pred[0]]
                        qval = float(pred[1][2:])
                        if pred[1][1] == '=' and val == qval:
                            return True 
                        elif pred[1][1] == '<' and val < qval:
                            return True
                        elif pred[1][1] == '>' and val > qval:
                            return True
                        else:
                            return False
                    else:
                        return None
            else:
                # It's a class
                #
                #
                print("It's a class")
        
        # Parse the query
        self.get_query(comp)
        # Get relevant rules to infer the query
        self.rules = set()
        self.get_rules()
        # Get the caterogies for each individual
        self.obj_dic = self.kb.inds_by_cat(self.chk_cats)
        # Create a new, filtered and temporal, work KB
        self.subkb = SubstRepr(self.kb, self.obj_dic)
        # Start inference process
        self.results = dict()        
        for var, preds in self.query.items():
            if var in self.vrs:
                # It's a variable, find every object that fits the criteria
                # 
                #
                print("It's a variable")
            else:
                self.results[var] = {}
                for pred in preds:
                    self.actv_q, result = (var, pred[0]), None
                    k, self.updated = True, list()
                    while result is not True  and k is True:
                        chk, done = list(), list()                        
                        result = self.chain(pred[0], chk, done)
                        k = True if True in self.updated else False
                        self.updated = list()
                    self.results[var][pred[0]] = chk_result()
    
    def chain(self, p, chk, done):
        if p in self.nodes:
            for node in self.nodes[p]:       
                self.rcsv_test(node)
                if p not in done:
                    chk = list(node.ants) + chk
        if self.actv_q[0] in self.obj_dic and \
        self.actv_q[1] in self.obj_dic[self.actv_q[0]]:
            return True
        elif len(chk) > 0:
            done.append(p)
            p = chk.pop(0)
            self.chain(p, chk, done)

    def rcsv_test(self, node):

        def add_ctg():
            for r in node.rule.result:
                pclass = r.__class__.__bases__[0]
                if pclass is LogFunction:
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

        def try_new():
            args = []
            for v in node.rule.var_order:
                if v in mapped and len(mapped[v]) > 0:
                    x = mapped[v].pop()
                else:
                    if v in mapped:
                        del mapped[v]
                    break
                args.append(x)
            return args

        mapped = self.map_vars(node)
        i, j = len(node.subs), len(mapped)
        if mapped is not False:
            res = hasattr(node.rule, 'result')
            while res is False and i == j:
                j = len(mapped)
                args = try_new()
                key = hash(tuple(args))
                if key in self.queue[node]['neg'] and self.updated is True:
                    node.rule(self.subkb, args)
                    res = hasattr(node.rule, 'result')
                    if res is True and (node.rule.result is not False):
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
                    if res is True and (node.rule.result is not False):
                        self.updated.append(True)
                        add_ctg()
                        del node.rule.result
                    elif res is True:
                        self.queue[node]['neg'].add(key)
                        del node.rule.result

    def map_vars(self, node):
        subactv = {}
        for vr, t in node.subs.items():
            subactv[vr] = []
            y = len(t)               
            for obj, s in self.obj_dic.items():
                x = len(s)            
                if x >= y:
                    r = s.intersection(t)
                    if len(r) == y:
                        subactv[vr].append(obj)
        if len(node.subs) == len(subactv):
            return subactv
        else:
            return False

    def get_rules(self, done=[None]):
        if len(self.ctgs) > 0:
            c = self.ctgs.pop()
        else:
            c = None
        if c is not None:
            done.append(c)            
            try:
                chk_rules = set(self.kb.classes[c]['cog'])
                chk_rules = chk_rules.difference(self.rules)
            except:
                raise CannotInferSolutionError(self.query) 
            for sent in chk_rules:
                preds = sent.get_pred(conds=gr_conds)
                nc = []
                for y in preds:
                    if type(y) == tuple: nc.append(y[0])
                    else: nc.append(y.func)
                self.mk_nodes(nc, preds, sent, 'right')
                nc2 = [e for e in nc if e not in done and e not in self.ctgs]
                self.ctgs.extend(nc2)
                if c in nc:
                    preds = sent.get_pred(branch='right', conds=gr_conds)
                    nc = []
                    for y in preds:
                        if type(y) == tuple: nc.append(y[0])
                        else: nc.append(y.func)
                    self.mk_nodes(nc, preds, sent, 'left')
                    nc2 = [e for e in nc if e not in done \
                           and e not in self.ctgs]
                    self.ctgs.extend(nc2)
            self.rules = self.rules.union(chk_rules)
            self.get_rules(done)
        else:
            done.pop(0)
            self.chk_cats = set(done)
            del self.rules
            del self.ctgs

    def mk_nodes(self, nc, ants, rule, pos):
        preds = rule.get_pred(branch=pos, conds=gr_conds)
        for cons in preds:
            pclass = cons.__class__.__bases__[0]
            if pclass is LogFunction:
                pred = cons.func
            else:
                pred = cons[0]
            node = self.InferNode(nc, ants, pred, rule)
            self.queue[node] = {'neg': set(), 'pos': set()}
            if node.cons in self.nodes:
                self.nodes[node.cons].append(node)
            else:
                self.nodes[node.cons] = [node]

    def get_query(self, comp):

        def break_pred():
            pr = rgx_ob.findall(p)[0].split('[')
            if '<' in p:
                func = make_function(p, 'relation')
                return func
            if ';' in pr[1]:
                t = pr[1].split(';')
                if len(t) != 3:
                    t.append(None)
                pr[0], pr[1] = t[1], (pr[0], tuple(t[0].split(',')), t[2])
            else:
                t = pr[1].split(',')
                pr = t[0], (pr[0], t[1])
            return pr
        
        preds = []
        for pa in comp:
            pa = pa.replace(' ','').strip()
            if not any(s in pa for s in symbs.values()):
                preds.append(pa)
            if ':vars:' in pa:
                # It's a variable
                #
                #
                pass
        for i, p in enumerate(preds):
            preds[i] = break_pred()
        terms, ctgs = {}, []
        for p in preds:
            pclass = p.__class__.__bases__[0]
            if pclass is LogFunction:
                ctgs.append(p.func)
            elif p[0] not in terms.keys():
                terms[p[0]] = [p[1]]
                ctgs.append(p[1][0])
            else:
                terms[p[0]].append(tuple(p[1]))
        self.query, self.ctgs = terms, ctgs

class CannotInferSolutionError(Exception):
    """Cannot infer a solution error."""
    pass

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
        
        def register(self, form, stop=False):
            if stop is False:
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
    
    def make(self, kb, obj_dic):
        for s in obj_dic:
            if '$' in s[0]:
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
                #
                #
                pass

if __name__ == '__main__':
    
    def load_sentences(test, path):
        logic_test = os.path.join(path, 'knowledge_base', test)
        ls, sup_ls = [], []
        with open(logic_test, 'r') as f:
            for line in f:
                if line[0] == '#': pass
                elif line.strip() == '{':
                    sup_ls, ls = ls, list()
                elif line.strip() == '}':
                    sup_ls.append(ls)
                    ls = sup_ls
                else: ls.append(line.strip())
        return ls
    
    path = '/home/nacho/dev/workspace/simag/tests'
    test = 'eval_fol.txt'
    sents = load_sentences(test, path)
    r = Representation()
    for s in sents[0]:
        r.tell(s)
    result = r.ask('scum[$West,u=1] && good[$West,u=0]', single=True)
    print(result)

