# -*- coding: utf-8 -*-

"""Main knowledge-base logic module, in this module exists the different 
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
"""
# ===================================================================#
#   Imports and globals
# ===================================================================#
import re
import os
import uuid

import core.bms
from numpy import False_

gl_res = []

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
                   Entities are denoted with a $ symbol followed by a name.
        classes -> Sets of objects that share a common property.
    """
    def __init__(self):
        self.individuals = {}
        self.classes = {}
        self.bmsWrapper = core.bms.BmsWrapper(self)

    def tell(self, sentence):
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
        >>> r.tell(":forall:x: (professor[x,u=1] |= person[x,u=1])")
        all the individuals which are professors will be added to the
        person category, and the formula will be stored in the professor
        class for future use.
        
        For more examples check the LogSentence class docs.
        """
        comp = []
        hier = {}
        rgx_par = re.compile(r'\{(.*?)\}')
        
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
                raise AssertionError('Incorrect use of parentheses.')
            else:
                elem = s[par[0]+1:par[1]]
                comp.append(elem)
                s = s.replace(s[par[0]:par[1]+1], '{'+str(f)+'}')
                f += 1
                return decomp_par(s, symb, f)

        def decomp_all(tform, idx):
            symb = [x for x in symbs if x in tform]
            symb = symb[0]
            memb = tform.split(symb)
            if len(memb) > 2:
                while len(memb) > 2:
                    last = memb.pop()                        
                    memb[-1] =  memb[-1] + symb + last
            x, y = len(comp), len(comp)+1
            if symb == '|>':
                comp[idx] = '{'+str(x)+'}'+':icond:'+'{'+str(y)+'}'
            if symb == '<=>':
                comp[idx] = '{'+str(x)+'}'+':equiv:'+'{'+str(y)+'}'
            if symb == ' =>':
                comp[idx] = '{'+str(x)+'}'+':implies:'+'{'+str(y)+'}'
            if symb == '||':
                comp[idx] = '{'+str(x)+'}'+':or:'+'{'+str(y)+'}'
            if symb == '&&':
                comp[idx] = '{'+str(x)+'}'+':and:'+'{'+str(y)+'}'
            comp.append(memb[0])
            comp.append(memb[1])
            return True

        def iter_childs(ls):
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

        decomp_par(sentence.rstrip('\n'), symb=('(',')'))
        ori = len(comp)-1
        for i, form in enumerate(comp):
            symbs = ['|>', '<=>',' =>','||','&&']
            if any(symb in form for symb in symbs):
                decomp_all(form, idx=i)
        iter_childs(len(comp))
        par_form = comp[ori]
        if not any(x in par_form for x in [':forall:', ':exists:', ':icond']):
            if '[' in par_form and len(comp) == 1:
                # It's a predicate
                self.declare(par_form)
            else:
                # It's a complex sentence with various predicates/funcs
                sent = LogSentence(ori, comp, hier)
                self.add_bhv(sent)
                #raise AssertionError('Logic sentence synthax is incorrect.')
        else:
            # It's a complex sentence with variables
            sentence = LogSentence(ori, comp, hier)
            if sentence.validity is True:
                sentence.validity = None
                self.save_rule(sentence)
            elif sentence.validity is None:
                self.add_bhv(sentence)
            else:
                raise AssertionError('Illegal connectives used in the consequent' \
                                     + ' of an indicative conditional sentence')
    
    def ask(self, sent):
        """Parses a sentence, asks the KB and returns the result of that ask.
        """
        pass

    def declare(self, sent):
        """Declares an object as a member of a class or the relationship
        between two objects. Declarations parse well-formed statements.
        
        Input: a string with one of the two following forms:
        1) "silly[$Lucy,u=0.2]" -> Declares the entity '$Lucy' as a member 
        of the 'silly' class. u = 0.2 is the fuzzy modifier, which can go 
        from 0 to 1.
        
        Declarations of membership can only happen to entities, objects
        which are the only member of their class. To denote an entity we use
        the $ symbol before the entity name.
        
        2) "<friend[$John, $Lucy,u=0.2]>" -> Declares a mapping of the 'friend' type
        between the entities '$Lucy' and '$John'. $John: friend -> $Lucy, 0.2
        
        Declarations of mapping can happen between entities, classes, 
        or between an entity and a class (ie. <loves[$Lucy, cats]>).
        """
        sent = sent.strip()
        rgx_ob = re.compile(r'\b(.*?)\]')
        sets = rgx_ob.findall(sent)
        sets = sets[0].split('[')
        if ';' in sets[1]:
            sets[1] = sets[1].split(';')
        if '<' in sent:
            # Is a function declaration > implies an mapping
            # between an object (or a set) and other object (or set).
            assert (type(sets[1]) == list), \
                    'A function/map needs subject and object'
            u = sets[1][0].split(',u=')
            u[1] = float(u[1])
            x = tuple(u), sets[1][1]
            func = [sets[0], x, 'map']
            self.bmsWrapper.add(func, True)
            self.up_rel(func)
        else:
            # Is a membership declaration -> the object belongs 
            # to a set of objects.
            assert (type(sets[1]) != tuple), \
                    'Only one object can be declared as member of a set at once.'
            assert ('$' in sets[1]), 'The object is not an unique entity.'
            u = sets[1].split(',u=')
            u[1] = float(u[1])
            pred = sets[0], (u[0], u[1])
            self.bmsWrapper.add(pred, True)
            check = self.bmsWrapper.add(pred, True)
            self.up_memb(pred)
            if check is False:
                self.bmsWrapper.add(pred, True)

    def up_memb(self, pred):
        # It's a membership declaration.
        categ, subject, val = pred[0], pred[1][0], pred[1][1]
        if subject not in self.individuals:
            ind = Individual(subject)
            ind.categ.append((categ, val))
            self.individuals[subject] = ind
        else:
            ctg_rec = [x for (x,_) in self.individuals[subject].categ]
            if categ not in ctg_rec:
                self.individuals[subject].categ.append((categ, val))
            else:
                idx = ctg_rec.index(categ)
                self.individuals[subject].categ[idx] = (categ, val)
        if categ not in self.classes:
            new_class = Category(categ)
            new_class['type'] = 'class'
            self.classes[categ] = new_class

    def up_rel(self, func):
        # It's a function declaration between two objs/classes.
        relation = func[0]
        subject = func[1][1]
        obj = func[1][0][0]
        val = func[1][0][1]
        #It's a func between an object and other obj/class.
        if '$' in subject:
            if subject not in self.individuals:
                ind = Individual(subject)
                ind.relations[relation] = [(obj, val)]
                self.individuals[subject] = ind
            elif relation not in self.individuals[subject].relations:
                ind = self.individuals[subject]
                ind.relations[relation] = [(obj, val)]
            else:
                rel = self.individuals[subject].relations[relation]
                rel = [x for (x,_) in rel]
                ind = self.individuals[subject]
                if obj not in rel:
                    ind.relations[relation].append((obj, val))
                else:
                    idx = rel.index(obj)
                    ind.relations[relation][idx] = (obj, val)
            if relation not in self.classes:
                categ = Category(relation)
                categ['type'] = 'relation'
                self.classes[relation] = categ
        #It's a func between a class and other class/obj.
        else:
            if subject not in self.classes:
                categ = Category(subject)
                categ['bhv'] = {relation: [(obj, val)]}
                categ['type'] = 'relation'
                self.classes[subject] = categ
            elif relation not in self.classes[subject]['bhv']:
                
                self.classes[subject]['bhv'][relation] = [(obj, val)]
            else:
                x  = self.classes[subject].iter_rel(relation)
                if obj not in x:
                    self.classes[subject][relation]['bhv'].append((obj, val))
                else:
                    idx = x.index(obj)
                    self.classes[subject][relation]['bhv'][idx] = (obj, val)

    def add_bhv(self, sent):
        preds = []
        for p in sent.particles:
            if p.cond is 'predicate':
                preds.append(p.pred)
        for x in gl_res:
            if len(x) == 3:
                preds.append((x[0], x[1][1]))
            else:
                preds.append(x)
        for pred in preds:
            if len(pred) == 3:
                if ',u' in pred[1][0]:
                    sbj, p = pred[1][0].split(',u')[0], pred[0]
                else:
                    sbj, p = pred[1][0], pred[0]
            else:
                if ',u' in pred[1]:
                    sbj, p = pred[1].split(',u')[0], pred[0]
                else:
                    sbj, p = pred[1], pred[0]
            if sbj not in sent.var_order:
                if '$' in sbj and sbj in self.individuals:
                    self.individuals[sbj].add_bhv(p, sent)
                elif '$' in sbj:
                    ind = Individual(sbj)
                    ind.add_bhv(p, sent)
                    self.individuals[sbj] = ind
                elif sbj in self.classes:
                    self.classes[sbj].add_bhv(p, sent)
                else:
                    c = 'class' if len(sbj) == 2 else 'relation'
                    nc = Category(sbj)
                    nc['type'] = c
                    nc.add_bhv(p, sent)
                    self.classes[sbj] = nc
            else:
                if p in self.classes:
                    self.classes[p].add_bhv('SELF', sent)
                else:
                    c = 'class' if len(pred) == 2 else 'relation'
                    nc = Category(p)
                    nc['type'] = c
                    nc.add_bhv('SELF', sent)
                    self.classes[p] = nc

    def save_rule(self, proof):
        for part in proof.particles:
            if part.parent == -1:
                x = part
            part.results = []        
        x.get_pred(conds=['icond'])
        for name in gl_res:
            if name[0] in self.classes and \
            proof not in self.classes[name[0]]['rules']:
                self.classes[name[0]]['rules'].append(proof)
            else:
                c = 'class' if len(name) == 2 else 'relation'
                nc = Category(name[0])
                nc['type'] = c
                nc['rules'].append(proof)
                self.classes[name[0]] = nc
        n = set([x[0] for x in gl_res])
        del gl_res[:]
        # Run the new formula with every unique object that matches.
        for ind in self.individuals.values():
            common = list(ind.check_cat(n))
            proof(self, ind.name)
            tests = None
            for cat in common:
                tests = self.classes[cat]['rules']
            if tests:
                for test in tests:
                    test(self, ind.name)

    def test(self, *args):
        cats = []
        for ind in args:
            if ind in self.individuals:
                c = self.individuals[ind].get_cat()
                i = [k for k,_ in c.items()]
                j = [k for k,_ in self.individuals[ind].relations.items()]
                cats = cats + i + j
        tests = []
        for c in cats:
            tests = tests + self.classes[c]['rules']
        tests = set(tests)
        tests = list(tests)
        for t in tests:
            t(self, *args)
        # Tests are run twice, as the changes from the first run could
        # have introduced inconsistencies which need to be found.
        #
        # MUST BE OPTIMIZED, DETECT CHANGES AND RUN THE APPROPIATE
        # RULE TEST, not every single test again
        for t in tests:
            t(self, *args)


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
            Includes the degree of membership (ie. ('cold', 0.9)).
        attr -> Implicit attributes of the object, unique to itself.
        bhv (opt) -> These are the cognitions and/or behaviours attributed
                to the object by the agent owning this representation.
        relations (opt) -> Functions between two objects and/or classes.
    """
    def __init__(self, name):
        self.id = str(uuid.uuid4())
        self.name = name
        self.attr = {}
        self.categ = []
        self.relations = {}
        self.bhv = {}

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
        """Returns a set that is the interdiction of the input set
        and the union of the categories and relations of the object.
        """
        t = set([k for k,_ in self.relations.items()])
        s = set([c[0] for c in self.categ])
        s = s.union(t)
        s = s.intersection(n)
        return s
    
    def get_cat(self):
        """Returns a dictionary of the categories of the object and
        their 'u' values.
        """
        cat = {k:v for k,v in self.categ}
        return cat
    
    def get_rel(self, rel):
        """Returns a dictionary of the objects that hold an input type
        of relation and a list of their 'u' values.
        """
        if rel in self.relations:
            return {k:v for k,v in self.relations[rel]}
        else:
            return None
    
    def add_bhv(self, p, sent):
        if p not in self.bhv:
            self.bhv[p] = [sent]
        elif sent not in self.bhv[p]:
            self.bhv[p].append(sent)
    
    def __str__(self):
        s = "\n<individual '" + self.name + "' w/ id: " + self.id + ">"
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
        self['id'] = str(uuid.uuid4())
        self['name'] = name
        self['bhv'] = {}
        self['rules'] = []
        
    def iter_rel(self, rel):
        return [x for (x, _) in self[rel]]
    
    def infer(self):
        """Infers attributes of the class from it's members."""
        pass
    
    def add_bhv(self, p, sent):
        if p not in self['bhv']:
            self['bhv'][p] = [sent]
        elif sent not in self['bhv'][p]:
            self['bhv'][p].append(sent)

# ===================================================================#
#   LOGIC METHODS
# ===================================================================#


def infer_facts():
    """Inference method from first-order logic sentences.
    """
    
# ===================================================================#
#   SUPPORTING CLASSES AND SUBCLASSES
# ===================================================================#


class LogSentence(object):
    """Object to store a first-order logic complex sentence.

    A declaration formula is the result of parsing a sentence and encode
    it in an usable form for the agent to classify and reason about
    objects and relations.
    """
    def __init__(self, ori, comp, hier):
        self.depth = 0
        self.id = str(uuid.uuid4())
        self.var_order = []
        self.particles = []
        self.make_parts(ori, comp, hier)
        self.connect_parts()

    def __call__(self, ag, *args):
        # Clean up previous results.
        self.product = None
        self.assigned = {}
        if len(self.var_order) == len(args):            
            for part in self.particles:
                if part.results is not None:
                    part.results = []
            # Check the properties/classes an obj belongs to
            for n, const in enumerate(args):
                if const in ag.individuals:
                    memb = ag.individuals[const].get_cat()
                else:
                    return
                var_name = self.var_order[n]
                # Assign an entity to a variable by order.
                self.assigned[var_name] = [const, memb]
            ag.bmsWrapper.register(self)
            self.particles[-1].solve(self, ag, key=[0])
            ag.bmsWrapper.register(self, stop=True)
        elif len(self.var_order) == 0:
            self.particles[-1].solve(self, ag, key=[0])
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
                if part.cond is 'icond':
                    icond = part
            lvl -= 1        
        self.particles = particles
        for p in self.particles:
            p.connect(self.particles)            
        # Check for illegal connectives for implicative cond sentences
        self.validity = None
        if icond is not False:
            icond.get_ops()
            self.validity = icond.results[0]
        for p in self.particles:
            p.pID = None
            p.results = []

    def new_test(self, form, depth, parent, part_id, syb):

        def up_var():
            vars_ = form[i+1].split(';')
            for var in vars_:
                var_name = var.strip()
                if var_name not in self.var_order:
                    #self.vars[var_name] = [(depth, quant)]
                    self.var_order.append(var_name)
                #else:
                #    self.vars[var_name].append((depth, quant))
                #    self.var_order.append(var_name)
            self.particles.append(Particle(cond, depth, part_id, parent, syb))

        def break_pred(form):
            rgx_ob = re.compile(r'\b(.*?)\]')
            set_ = rgx_ob.findall(form)
            set_ = set_[0].split('[')
            if ';' in set_[1]:
                set_[1] = tuple(set_[1].split(';'))
            if '<' in form:
                set_.append('map')
            return set_

        if depth > self.depth:
            self.depth = depth
        if ':icond:' in form:
            cond = 'icond'
            self.particles.append(Particle(cond, depth, part_id, parent, syb))
        elif ':equiv:' in form:
            cond = 'equiv'
            self.particles.append(Particle(cond, depth, part_id, parent, syb))
        elif ':implies:' in form:
            cond = 'implies'
            self.particles.append(Particle(cond, depth, part_id, parent, syb))
        elif ':or:' in form:
            cond = 'or'
            self.particles.append(Particle(cond, depth, part_id, parent, syb))
        elif ':and:' in form:
            cond = 'and'
            self.particles.append(Particle(cond, depth, part_id, parent, syb))
        elif any(x in form for x in [':forall:', ':exists:']):
            # Only universal quantifiers are supported right now,
            # so the quantity is irrelevant (check declare method for more info).
            form = form.split(':')
            cond = 'check_var'
            for i, a in enumerate(form):
                if a == 'forall':
            #        quant = float('inf')
                    up_var()
            #    elif a == 'exists':
            #        quant = 1
            #        up_var()
        elif '[' in form:
            cond = 'predicate'
            form = tuple(break_pred(form))
            self.particles.append(Particle(cond, depth, part_id,
                                           parent, syb, form))


class Particle(object):
    """Is a node that represents a natural language particle, 
    that can be either:
    a) An operator of the following types: implies, equals,
    and, or, not.
    b) A predicate, declaring a variable as a member of a set,
    or a function between two variables.
    c) A quantifier for a variable: universal or existential.
    """
    def __init__(self, cond, depth, id_, parent, syb, *args):
        self.pID = id_
        self.depth = depth
        self.cond = cond
        self.next = syb
        self.parent = parent
        self.results = []
        if cond == 'predicate':
            self.pred = args[0]

    def connect(self, part_list):
        for x, child in enumerate(self.next):
            for part in part_list:
                if part.pID == child:
                    self.next[x] = part
                    self.next[x].parent = self

    def solve(self, proof, ag, key, *args):
        """Keys for resolving the proof:
        100: Substitute a child's predicates.
        101: Check the truthiness of a child particle.
        102: Incoming truthiness of an operation for storage.
        103: Substitute the right branch of a statement and store.
        104: Returns the truthiness of a substitution.
        """
        #print self, '// Key:'+str(key), '// Args:', args
        if key[-1] == 102:
            key.pop()
            self.results.append(args[0])
        if self.cond == 'check_var':
            self.next[0].solve(proof, ag, key)
        elif self.cond == 'icond':
            self.icond(proof, ag, key, *args)
        elif self.cond == 'or' or self.cond == 'and':
            if key[-1] == 104:
                key.pop()
                self.results.append(args[0])
            current = len(self.results)            
            if key[-1] == 101:
                if current < len(self.next):
                    key.append(101)
                    self.next[current].solve(proof, ag, key)
                else:
                    if self.cond == 'or':
                        # Two branches finished, check if one is true.                    
                        self.disjunction(proof, ag, key)
                    elif self.cond == 'and':
                        # Two branches finished, check if both are true. 
                        self.conjunction(proof, ag, key) 
            elif key[-1] == 100:
                if current < len(self.next) and \
                self.next[current].cond == 'predicate':
                    if self.cond == 'or':
                        key.append(101)
                        self.next[current].solve(proof, ag, key)
                    else:
                        key.append(105)
                        self.next[current].solve(proof, ag, key)
                elif current < len(self.next):
                    self.next[current].solve(proof, ag, key)
                else:
                    if self.cond == 'or':
                        result = self.disjunction(proof, ag, key)
                    elif self.cond == 'and':
                        result = self.conjunction(proof, ag, key)
                    key.append(104)
                    self.parent.solve(proof, ag, key, result)
            elif key[-1] == 103:
                self.next[1].solve(proof, ag, key)
            elif key[-1] == 105 and self.next[1].cond == 'predicate':
                key.pop()
                key.append(100)
                self.next[1].solve(proof, ag, key)
        elif self.pred:
            result = self.ispred(proof, ag, key)
            x = key.pop()
            if x != 100 and x != 103:
                key.append(102)
                self.parent.solve(proof, ag, key, result)
    
    def icond(self, proof, ag, key, *args):
        """Procedure for parsign indicative conditional assertions."""
        if key[-1] == 104:
            self.results.append(args[0])
            if self.next[1].cond is 'or' and self.results[1] is False:
                # This fails
                return
        else:
            current = len(self.results)
            # if the left branch is examined then solve, else don't.
            if current < len(self.next) and current == 0:
                key.append(101)
                self.next[current].solve(proof, ag, key)
            elif current == 1 and self.results[0] == True:
                key.append(100)
                self.next[current].solve(proof, ag, key)
            else:
                # The left branch was false, so do not continue.             
                #print '\nTested the left branch and failed.\n'
                return

    def conjunction(self, proof, ag, key):
        left_branch = self.results[0]
        right_branch = self.results[1]
        if key[-1] == 101:
        # Two branches finished, check if both are true.            
            if (left_branch and right_branch) is True:
                key.append(102)
                self.parent.solve(proof, ag, key, True)
        elif key[-1] == 100:
        # Test if this conjunction fails
            if (left_branch and right_branch) is True:
                # passes the proof
                return True
            else:
                # fails the proof
                return False

    def disjunction(self, proof, ag, key):
        left_branch = self.results[0]
        right_branch = self.results[1]
        if key[-1] == 101:
        # Two branches finished, check if both are true.
            if (left_branch or right_branch) is True:
                key.append(102)
                self.parent.solve(proof, ag, key, True)
        elif key[-1] == 100:
        # Test if this disjunction fails
            if left_branch != right_branch:
                # passes the proof
                return True
            else:
                # fails the proof
                return False

    def ispred(self, proof, ag, key):
        if key[-1] == 101:
            result = None
            if len(self.pred) == 3:
                # Check mapping of a set/entity to an other set/entity.                
                check_func = self.pred[0]
                if '$' in self.pred[1][1]:                    
                    var = self.pred[1][1]
                    obj, u = self.pred[1][0].split(',u')
                    uval = float(u[1:])
                    subject = proof.assigned[var][0]
                else:
                    var1, u = self.pred[1][0].split(',u')
                    uval = float(u[1:])
                    var = self.pred[1][1]
                    subject = proof.assigned[var][0]
                    obj = proof.assigned[var1][0]
                relation = ag.individuals[subject].get_rel(check_func)
                try:
                    val = relation[obj]
                except:
                    result = None
                else:
                    if u[0] == '=' and val == uval:
                        result = True
                    elif u[0] == '>' and val > uval:
                        result = True
                    elif u[0] == '<' and val < uval:
                        result = True
                    else:
                        result = False
                if result is True:
                    obj = obj + ',u' + u[0] + str(uval)
                    s = '<' + check_func + '['+subject+';' + obj + ']>'
                    ag.bmsWrapper.prev_blf(s)
            else:
                # Check membership to a set of an entity.
                var, u = self.pred[1].split(',u')
                categs = proof.assigned[var][1]
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
                    sbj = proof.assigned[var][0]
                    s = check_set + '[' + sbj + ',u' + u[0] + str(uval) + ']'
                    ag.bmsWrapper.prev_blf(s)
            return result
        elif key[-1] != 101:
            # marked for declaration
            # subtitute var(s) for object(s) name(s)
            # and pass to agent for updating
            pred = list(self.pred)
            x = len(proof.var_order)
            if type(pred[1]) is tuple and x != 0:
                if '$' in pred[1][1]:
                    obj, u = pred[1][0].split(',u')
                    var = pred[1][1]
                    var = proof.assigned[var][0]
                    pred[1] = (var, [obj, u])
                else:
                    var1 = pred[1][1]
                    var2, u = pred[1][0].split(',u')
                    var2 = proof.assigned[var2][0]
                    var1 = proof.assigned[var1][0]
                    pred[1] = ([var2, u], var1)
                ag.bmsWrapper.check(pred)
                pred[1][0][1] = float(u[1:])
                ag.up_rel(pred)
            elif x != 0:
                var, u = self.pred[1].split(',u')
                pred[1] = proof.assigned[var][0]
                pred = (pred[0], [pred[1], u])
                ag.bmsWrapper.check(pred)
                pred[1][1] = float(u[1:])
                ag.up_memb(pred)
            elif type(pred[1]) is tuple:
                raise 'FIX THIS'
            else:
                cst, u = self.pred[1].split(',u')
                pred = (pred[0], [cst, u])
                ag.bmsWrapper.check(pred)
                pred[1][1] = float(u[1:])
                ag.up_memb(pred)

    def get_pred(self, pos='left', k=0, conds=[None], *args):
        branch = 0 if pos == 'left' else 1
        if k == 1:
            self.results.append(args[0])
            k = 0
        if len(self.results) == 0:
            if self.cond == 'predicate':
                k = 1
                if self.pred not in gl_res:
                    gl_res.append(self.pred)
                self.parent.get_pred(pos, k, conds, True)
            else:
                self.next[branch].get_pred(pos, k, conds)
        elif len(self.results) == 1 and self.cond not in conds:
            if self.cond == 'predicate':
                k = 1
                if self.pred not in gl_res:
                    gl_res.append(self.pred)
                self.parent.get_pred(pos, k, conds, True)
            else:
                x = 1 if pos == 'left' else 0
                self.next[x].get_pred(pos, k, conds)
    
    def get_ops(self, k=0, *args):
        if k == 1:
            self.results.append(args[0])
            k = 0
        if self.cond is 'icond':
            if len(self.results) != 1:
                self.next[1].get_ops(k)
            else:
                return
        else:
            if self.cond is 'predicate':
                k = 1
                self.parent.get_ops(k, True)
            elif len(self.results) < 2 and self.cond is 'and':
                i = len(self.results)
                self.next[i].get_ops(k)
            elif self.cond is 'and':
                k = 1
                if any(p is False for p in self.results):
                    self.parent.get_ops(k, False)
                else:
                    self.parent.get_ops(k, True)
            else:
                k = 1
                self.parent.get_ops(k, False)

    def __str__(self):
        if self.cond != 'predicate':
            s = '<operator ' + ' (depth:' + str(self.depth) + ') "' \
            + str(self.cond) + '">'
        else:
            s = '<predicate ' + ' (depth:' + str(self.depth) + '): ' \
            + str(self.pred) + '>'
        return s


class Group(Category):
    """A special instance of a category. It defines a 'group' of
    elements that pertain to a class.
    """


class Part(Category):
    """A special instance of a category. It defines an element
    which is a part of an other object.
    """

if __name__ == '__main__':
    import datetime
    import pprint

    path = os.path.dirname(os.path.dirname(os.path.dirname(__file__)))
    logic_test = os.path.join(path, 'tests', 'logic_test_01.txt')
    ls = []
    with open(logic_test, 'r') as f:
        for line in f:
            if line[0] == '#':
                pass
            else:
                ls.append(line)
    r = Representation()
    d1 = datetime.datetime.now()
    for form in ls:
        r.tell(form)
    r.test('$Lucy','$John')
    #r.tell('<friend[$John,u=1;$Lucy]>')
    print '\n---------- RESULTS ----------'
    d2 = datetime.datetime.now()
    print (d2-d1)
    for ind in r.individuals.values():
        print ind
        print 'Relations:', ind.relations
        print 'Categories:', ind.categ
        #print 'Bhv:', ind.bhv
    print
    #pprint.pprint(r.bmsWrapper.container)
    #pprint.pprint(r.classes)
