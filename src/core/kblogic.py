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
#   Imports and constants
# ===================================================================#
import re
import os
import uuid

import core.bms

symbs = dict([
             ('|>',':icond:'),
             ('<=>',':equiv:'), 
             (' =>',':implies:'),
             ('||',':or:'),
             ('&&',':and:')
            ])

symb_ord = ['|>', '<=>', ' =>', '||', '&&']

#Regex
rgx_par = re.compile(r'\{(.*?)\}')
rgx_ob = re.compile(r'\b(.*?)\]')
rgx_br = re.compile(r'\}(.*?)\{')

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
        >>> r.tell(":forall:x: (professor[x,u=1] |= person[x,u=1])")
        all the individuals which are professors will be added to the
        person category, and the formula will be stored in the professor
        class for future use.
        
        For more examples check the LogSentence class docs.
        """
        ori, comp, hier = self.parse_sent(sent)
        par_form = comp[ori]
        if not any(x in par_form for x in [':forall:', ':icond:']):
            if '[' in par_form and len(comp) == 1:
                # It's a predicate
                self.declare(par_form)
            else:
                # It's a complex sentence with various predicates/funcs
                sent = LogSentence(ori, comp, hier)
                self.add_cog(sent)
        else:
            # It's a complex sentence with variables
            sent = LogSentence(ori, comp, hier)
            if sent.validity is True:
                sent.validity = None
                self.save_rule(sent)
            elif sent.validity is None:
                self.add_cog(sent)
            else:
                raise AssertionError('Illegal connectives used in the consequent' \
                                     + ' of an indicative conditional sentence')
    
    def ask(self, sent):
        """Parses a sentence, asks the KB and returns the result of that ask.
        """
        result = infer_facts(self, self.parse_sent, sent)
        return result

    def parse_sent(self, sent):

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
        ori = len(comp)-1
        for idx, form in enumerate(comp):            
            if any(symb in form for symb in symbs.keys()):
                decomp_symbs()
        ls = len(comp)
        iter_childs()
        return ori, comp, hier
        
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
        
        2) "<friend[$John, $Lucy,u=0.2]>" -> Declares a mapping of the 'friend' type
        between the entities '$Lucy' and '$John'. $John: friend -> $Lucy, 0.2
        
        Declarations of mapping can happen between entities, classes, 
        or between an entity and a class (ie. <loves[$Lucy, cats]>).
        """
        sent = sent.replace(' ','')        
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
            nc = Category(categ)
            nc['type'] = 'class'
            self.classes[categ] = nc

    def up_rel(self, func):
        # It's a function declaration between two objs/classes.
        relation = func[0]
        subject = func[1][1]
        obj = func[1][0][0]
        val = func[1][0][1]
        iobj = func[1][2] if len(func[1]) == 3 else None
        #It's a func between an object and other obj/class.
        if '$' in subject:
            if subject not in self.individuals:
                ind = Individual(subject)
                ind.relations[relation] = [(obj, val, iobj)]
                self.individuals[subject] = ind
            elif relation not in self.individuals[subject].relations:
                ind = self.individuals[subject]
                ind.relations[relation] = [(obj, val, iobj)]
            else:
                rel = self.individuals[subject].relations[relation]
                rel = [x for (x,_,_) in rel]
                ind = self.individuals[subject]
                if obj not in rel:
                    ind.relations[relation].append((obj, val, iobj))
                else:
                    idx = rel.index(obj)
                    ind.relations[relation][idx] = (obj, val, iobj)
            if relation not in self.classes:
                categ = Category(relation)
                categ['type'] = 'relation'
                self.classes[relation] = categ
        #It's a func between a class and other class/obj.
        else:
            if subject not in self.classes:
                categ = Category(subject)
                categ[relation] = [(obj, val, iobj)]
                categ['type'] = 'class'
                self.classes[subject] = categ
            elif relation not in self.classes[subject]:
                self.classes[subject][relation] = [(obj, val, iobj)]
            else:
                x  = self.classes[subject].iter_rel(relation)
                if obj not in x:
                    self.classes[subject][relation].append((obj, val, iobj))
                else:
                    idx = x.index(obj)
                    self.classes[subject][relation][idx] = (obj, val, iobj)

    def add_cog(self, sent):
        preds = []
        for p in sent:
            if p.cond == ':predicate:':
                preds.append(p.pred)
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
                    self.individuals[sbj].add_cog(p, sent)
                elif '$' in sbj:
                    ind = Individual(sbj)
                    ind.add_cog(p, sent)
                    self.individuals[sbj] = ind
                elif sbj in self.classes:
                    self.classes[sbj].add_cog(sent)
                else:
                    c = 'class' if len(sbj) == 2 else 'relation'
                    nc = Category(sbj)
                    nc['type'] = c
                    nc.add_cog(sent)
                    self.classes[sbj] = nc
            else:
                if p in self.classes:
                    self.classes[p].add_cog(sent)
                else:
                    c = 'class' if len(pred) == 2 else 'relation'
                    nc = Category(p)
                    nc['type'] = c
                    nc.add_cog(sent)
                    self.classes[p] = nc

    def save_rule(self, proof):
        for part in proof:
            part.results = []
        x = proof.start
        setattr(proof, 'gl_res', list())
        x.get_pred(conds=[':icond:'])
        for name in proof.gl_res:
            if name[0] in self.classes and \
            proof not in self.classes[name[0]]['cog']:
                self.classes[name[0]].add_cog(proof)
            else:
                c = 'class' if len(name) == 2 else 'relation'
                nc = Category(name[0])
                nc['type'] = c
                nc.add_cog(proof)
                self.classes[name[0]] = nc
        n = set([x[0] for x in proof.gl_res])
        del proof.gl_res
        # Run the new formula with every unique object that matches.
        for ind in self.individuals.values():
            common = list(ind.check_cat(n))
            proof(self, ind.name)
            tests = None
            for cat in common:
                tests = self.classes[cat]['cog']
            if tests:
                for test in tests:
                    test(self, ind.name)

    def inds_by_cat(self, cats):
        cat_dic = {}
        for ind in self.individuals.values():
            s = ind.check_cat(cats)
            for c in s:
                if c in cat_dic:
                    cat_dic[ind.name].add([c])
                else:
                    cat_dic[ind.name] = set([c])
        return cat_dic
    
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
            tests = tests + self.classes[c]['cog']
        tests, tests = set(tests), list(tests)
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
        | Includes the degree of membership (ie. ('cold', 0.9)).
        attr -> Implicit attributes of the object, unique to itself.
        cog (opt) -> These are the cognitions attributed to the object by 
        | the agent owning this representation.
        relations (opt) -> Functions between two objects and/or classes.
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
        """Returns a set that is the intersection of the input set
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
            return {k:(v1,v2) for k,v1,v2 in self.relations[rel]}
        else:
            return None
    
    def add_cog(self, p, sent):
        if p in self.cog:
            self.cog[p].append(sent)
        else:
            self.cog[p] = [sent]
    
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
        self['name'] = name
        self['cog'] = []
        
    def iter_rel(self, rel):
        return [x for (x, _, _) in self[rel]]
    
    def infer(self):
        """Infers attributes of the class from it's members."""
        pass
    
    def add_cog(self, sent):
        self['cog'].append(sent)


class Group(Category):
    """A special instance of a category. It defines a 'group' of
    elements that pertain to a class.
    """


class Part(Category):
    """A special instance of a category. It defines an element
    which is a part of an other object.
    """


# ===================================================================#
#   LOGIC CLASSES AND SUBCLASSES
# ===================================================================#


class LogSentence(object):
    """Object to store a first-order logic complex sentence.

    This sentence is the result of parsing a sentence and encode
    it in an usable form for the agent to classify and reason about
    objects and relations.
    """
    def __init__(self, ori, comp, hier):
        self.depth = 0
        self.id = hash(self)
        self.var_order = []
        self.particles = []
        self.make_parts(ori, comp, hier)
        self.connect_parts()

    def __call__(self, ag, *args):
        if isinstance(args[0], list):
            args = args[0]
        # Clean up previous results.
        self.product = None
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
            self.start.solve(self, ag, key=[0])
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
            print self.validity
        for p in self.particles:
            del p.pID
            p.sent = self
            p.results = []
            if p.parent == -1:
                self.start = p

    def new_test(self, form, depth, parent, part_id, syb):

        def up_var():
            vars_ = form[i+1].split(',')
            for var in vars_:
                if var not in self.var_order:
                    self.var_order.append(var)
            self.particles.append(Particle(cond, depth, part_id, parent, syb))

        def break_pred(form):
            p = rgx_ob.findall(form)[0].split('[')
            if ';' in p[1]:
                p[1] = tuple(p[1].split(';'))
            if '<' in form:
                p.append('map')
            return p
        
        form = form.replace(' ','').strip()
        cond = rgx_br.findall(form)
        if depth > self.depth:
            self.depth = depth
        if len(cond) > 0:
            self.particles.append(Particle(cond[0], depth, part_id, parent, syb))
        elif any(x in form for x in [':forall:', ':exists:']):
            # Only universal quantifiers are supported right now,
            # so the quantity is irrelevant (check declare method for more info).
            form = form.split(':')
            cond = ':check_var:'
            for i, a in enumerate(form):
                if a == 'forall':
                    up_var()
        elif '[' in form:
            cond = ':predicate:'
            form = tuple(break_pred(form))
            self.particles.append(Particle(cond, depth, part_id,
                                           parent, syb, form))
        else:
            cond = ':stub:'
            self.particles.append(Particle(cond, depth, part_id,
                                           parent, syb, form))
    
    def cln_res(self):
        for p in self.particles:
            p.results = []

    def get_ops(self, p):
        print 'VALIDITY'
        ops = []
        for p in self:
            if any(x in p.cond for x in [':or:', ':implies:', ':equiv:']):
                ops.append(p)
        for p in ops:
            x = p
            while x.cond != ':icond:' or x.parent == -1:
                print x.parent.cond
                if x.parent.cond == ':icond:':
                    return False
                else:
                    x = x.parent

    def __iter__(self):
        return iter(self.particles)

class Particle(object):
    """A particle in a logic sentence, that can be either:
    * An operator of the following types: 
    indicative conditional, implies, equals, and, or.
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

    def connect(self, part_list):
        for x, child in enumerate(self.next):
            for part in part_list:
                if part.pID == child:
                    self.next[x] = part
                    self.next[x].parent = self

    def solve(self, proof, ag, key, *args):
        """Keys for solving proofs:
        100: Substitute a child's predicates.
        101: Check the truthiness of a child atom.
        102: Incoming truthiness of an operation for recording.
        103: Return to parent atom.
        """
        #print self, '// Key:'+str(key), '// Args:', args
        if key[-1] == 102:
            key.pop()
            self.results.append(args[0])
        if self.cond == ':check_var:' or self.cond == ':stub:':
            self.next[0].solve(proof, ag, key, *args)
        elif self.cond == ':icond:':
            self.icond(proof, ag, key, *args)
        elif self.cond == ':implies:':
            self.impl(proof, ag, key, *args)
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
                    if self.cond == ':or:':
                        pass
                    else:
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
    
    def impl(self, proof, ag, key, *args):
        """Procedure for solving implications."""
        current = len(self.results)
        # if the left branch is examined then solve, else don't.
        if current == 0:
            key.append(101)
            self.next[current].solve(proof, ag, key)
        elif current == 1 and self.results[0] == True:
            print self.results
            #self.next[current].solve(proof, ag, key)
        else:
            # The left branch was false, so do not continue.
            return False
    
    def icond(self, proof, ag, key, *args):
        """Procedure for parsign indicative conditional assertions."""
        if key[-1] == 103:
            # Completed
            return
        else:
            current = len(self.results)
            # if the left branch is examined then solve, else don't.
            if current == 0:
                key.append(101)
                self.next[current].solve(proof, ag, key)
            elif current == 1 and self.results[0] == True:
                key.append(100)
                self.next[current].solve(proof, ag, key)
            else:
                # The left branch was false, so do not continue.
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
        
        def isvar(s):
            try:
                s = proof.assigned[s]
            except:
                pass
            return s
        
        if key[-1] == 101:
            if len(self.pred) == 3:
                # Check mapping of a set/entity to an other set/entity.                
                check_func, sbj = self.pred[0], self.pred[1][1]
                obj, u = self.pred[1][0].split(',u')
                sbj, obj, uval = isvar(sbj), isvar(obj), float(u[1:])
                if len(self.pred[1]) == 3:
                    iobj = isvar(self.pred[1][2])
                else: iobj = None
                relation = ag.individuals[sbj].get_rel(check_func)
                try:
                    val, ciobj = relation[obj][0], relation[obj][1]
                except:
                    result = None
                else:
                    if ciobj == iobj:
                        if u[0] == '=' and val == uval:
                            result = True
                        elif u[0] == '>' and val > uval:
                            result = True
                        elif u[0] == '<' and val < uval:
                            result = True
                        else: 
                            result = False
                    else:
                        result = False
                if result is True:
                    obj = obj + ',u' + u[0] + str(uval)
                    s = '<' + check_func + '['+sbj+';' + obj + ']>'
                    ag.bmsWrapper.prev_blf(s)
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
            pred = list(self.pred)
            if type(pred[1]) is tuple:
                obj, u = pred[1][0].split(',u')
                obj, sbj = isvar(obj), isvar(pred[1][1])
                iobj = isvar(pred[1][2]) if len(pred[1]) == 3 else None
                pred[1] = ([obj, u], sbj, iobj)                                
                ag.bmsWrapper.check(pred)
                pred[1][0][1] = float(u[1:])
                ag.up_rel(pred)
            else:
                sbj, u = self.pred[1].split(',u')
                pred[1] = isvar(sbj)
                pred = (pred[0], [pred[1], u])
                ag.bmsWrapper.check(pred)
                pred[1][1] = float(u[1:])
                ag.up_memb(pred)

    def get_pred(self, pos='left', k=0, conds=[None], *args):
        branch = 0 if pos == 'left' else 1
        if k == 1:
            self.results.append(args[0])
            k = 0
        if len(self.results) == 0:
            if self.cond == ':predicate:':
                k = 1
                if self.pred not in self.sent.gl_res:
                    self.sent.gl_res.append(self.pred)
                self.parent.get_pred(pos, k, conds, True)
            else:
                if self.cond == ':check_var:':
                    self.next[0].get_pred(pos, k, conds)
                else:
                    self.next[branch].get_pred(pos, k, conds)
        elif len(self.results) == 1 and self.cond not in conds:
            if self.cond == ':predicate:':
                k = 1
                if self.pred not in self.sent.gl_res:
                    self.sent.gl_res.append(self.pred)
                self.parent.get_pred(pos, k, conds, True)
            else:
                x = 1 if pos == 'left' else 0
                self.next[x].get_pred(pos, k, conds)

    def __str__(self):
        if self.cond != ':predicate:':
            s = '<operator ' + ' (depth:' + str(self.depth) + ') "' \
            + str(self.cond) + '">'
        else:
            s = '<predicate ' + ' (depth:' + str(self.depth) + '): ' \
            + str(self.pred) + '>'
        return s

# ===================================================================#
#   LOGIC INFERENCE
# ===================================================================#

gr_conds = [':icond:', ':implies:', ':equiv:']
    
def infer_facts(kb, parser, sent):
    """Inference function from first-order logic sentences.
    
    Gets a query from an ASK, encapsulates the query subtitutions, 
    processes it (including caching of partial results or tracking
    var substitution) and returns the answer to the query. If new 
    knowledge is produced then it's passed to an other procedure for
    addition to the KB.
    """
    ori, comp, hier = parser(sent)
    query = Inference(kb, comp)
    query.rules = set()
    query.get_rules(query.ctgs)
    subs_dic = kb.inds_by_cat(query.chk_cats)
    query.chain(subs_dic)


class Inference(object):

    def __init__(self, kb, *args):
        self.kb = kb
        self.vrs = {}
        self.nodes = {}
        self.get_query(*args)

    def chain(self, subs_dic):
        self.subkb = subs_dic
        self.queue = []
        for var, pred in self.query.items():
            if var in self.vrs:
                # It's a variable, find every object that fits the criteria
                pass
            else:
                for p in pred:
                    if p[0] in self.subkb and var in self.subkb[p[0]]:
                        print 'SOLUTION FOUND'
                        return
                    else:
                        self.rcsv_sub(p[0])

    def rcsv_sub(self, ctg):
        if ctg not in self.nodes:
            pass
        else:        
            for node in self.nodes[ctg]:
                mapped = self.map_vars(node)
                if mapped is not False:
                    args = []
                    for v in node.rule.var_order:
                        args.append(mapped[v])
                    print 'MAPPED', args
                    node.rule(self.kb, args)
                else:                    
                    print 'FAILED', ctg
                    for ant in node.ants:
                        self.rcsv_sub(ant)

    def map_vars(self, node):
        subactv = {}
        for obj, s in self.subkb.items():
            x = len(s)
            for vr, t in node.subs.items():
                y = len(t)
                if x >= y:
                    r = s.intersection(t)
                    if len(r) == y:
                        subactv[vr] = obj
        if len(node.subs) == len(subactv):
            return subactv
        else:
            return False

    def get_query(self, comp):

        def break_pred():
            pr = rgx_ob.findall(p)[0].split('[')
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
            if ':forall:' or ':exists:' in pa:
                pass
        for i, p in enumerate(preds):
            preds[i] = break_pred()
        terms, ctgs = {}, []
        for p in preds:
            if p[0] not in terms.keys():
                terms[p[0]] = [p[1]]
                ctgs.append(p[1][0])
            else:
                terms[p[0]].append(tuple(p[1]))
        self.query, self.ctgs = terms, ctgs

    def get_rules(self, ctg, done=[None]):
        if len(ctg) > 0:
            c = ctg.pop()
        else:
            c = None
        if c is not None:
            done.append(c)
            try:
                chk_rules = set(self.kb.classes[c]['cog'])
                chk_rules = chk_rules.difference(self.rules)
            except:
                print 'SOLUTION CANNOT BE FOUND'
                return
            for sent in chk_rules:
                sent.cln_res()
                setattr(sent, 'gl_res', list())
                sent.start.get_pred(conds=gr_conds)
                nc = [y[0] for y in sent.gl_res]
                if c in sent.gl_res:
                    del sent.gl_res[:]
                    sent.start.get_pred(pos='right', conds=gr_conds)
                    nc = [y[0] for y in sent.gl_res]
                    self.mk_nodes(nc, sent.gl_res, sent, 'left')
                else:
                    nc = [y[0] for y in sent.gl_res]
                    self.mk_nodes(nc, sent.gl_res, sent, 'right')
                nc = [e for e in nc if e not in done and e not in ctg]
                ctg.extend(nc)
            self.rules = self.rules.union(chk_rules)
            self.get_rules(ctg, done)
        else:
            done.pop(0)
            self.chk_cats = set(done)
            del self.rules
            del self.ctgs
    
    def mk_nodes(self, nc, ants, rule, pos):
        ants = list(ants)
        del rule.gl_res[:]
        rule.cln_res()
        rule.start.get_pred(pos=pos, conds=gr_conds)
        for cons in rule.gl_res:
            node = InfNode(nc, ants, cons[0], rule)
            if node.cons in self.nodes:
                self.nodes[node.cons].append(node)
            else:
                self.nodes[node.cons] = [node]


class InfNode(object):
    
    def __init__(self, *args):
        self.mk_cons(*args)
        
    def mk_cons(self, nc, ants, cons, rule):
        self.rule = rule
        self.cons = cons
        self.ants = nc
        self.subs = {v:set() for v in rule.var_order}
        for ant in ants:
            if isinstance(ant[1], tuple):
                v = ant[1][1]     
                if v in self.subs:
                    self.subs[v].add(ant[0])
            else:
                v = ant[1].split(',u')
                if v[0] in self.subs:
                    self.subs[v[0]].add(ant[0])

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
    r.ask('criminal[$West,u=1]')
    #r.ask('criminal[$West,u=1] && <sells[x,u=1;$West;$Nono]> && missile[x,u=1]')
    print '\n---------- RESULTS ----------'
    d2 = datetime.datetime.now()
    print (d2-d1)
    for ind in r.individuals.values():
        print ind
        print 'Relations:', ind.relations
        print 'Categories:', ind.categ
        #print 'cog:', ind.cog
    print
    #pprint.pprint(r.classes)
