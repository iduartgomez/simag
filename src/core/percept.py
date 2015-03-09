# -*- coding: utf-8 -*-
"""Main perception module, in this module exists the different classes
that store data for the individual agents and serve as representations
of the different objects and the relationships between them.

:class: Representation. Main class, stores all the representations and
relationships for a given agent in a concrete time.

:class: Proof stores a serie of logical atoms (be them predicates or
connectives), that form a well-formed logic formula. These are rulesets 
for cataloging objects into sets/classes, and the relationships between 
these objects.

"""
# ===================================================================#
#   Imports and globals
# ===================================================================#
import re
import os

# ===================================================================#
#   REPRESENTATION OBJECTS CLASSES AND SUBCLASSES
# ===================================================================#


class Representation(object):
    """This class is a container for internal agent's representations
    of the 'simulated reality'. An agent can have any number of such
    representations at a moment in time, all of which are contained
    in this object.
    
    The class includes methods to encode and decode the representations 
    to/from data streams or idioms.
    
    Attributes:
        singles -> Unique members (entities) of their own set/class.
                   Entities are denoted with a $ symbol followed by a name.
        classes -> Sets of objects that share a common property.
        relations -> A function/map between two objects.
        formulae -> Stores the different logical formulae.

    """
    def __init__(self):
        self.singles = {}
        self.classes = {}
        self.relations = {}
        self.formulae = {}

    def encode(self, formula):
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

        def decomp_all(tform, symb, idx):
            if symb in tform:
                memb = tform.split(symb)
                if len(memb) > 2:
                    while len(memb) > 2:
                        last = memb.pop()                        
                        memb[-1] =  memb[-1] + symb + last
                x, y = len(comp), len(comp)+1
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

        decomp_par(formula.rstrip('\n'), symb=('(',')'))
        ori = len(comp)-1
        for i, form in enumerate(comp):
            symbs = ['<=>',' =>','||','&&']
            for symb in symbs:
                if decomp_all(form, symb, idx=i):
                    break
        iter_childs(len(comp))
        par_form = comp[ori]
        if not any(x in par_form for x in [':forall:', ':exists:']):
            # It's a declaration/definition
            if '[' in par_form and len(comp) == 1:
                self.declare(par_form)
            else:
                raise AssertionError('Formula synthax is wrong.')
        else:
            # It's a formula, not a declaration/definition
            proof = Proof(ori, comp, hier)
            self.save_proof(proof)

    def declare(self, form):
        """Declares an object as a member of a class or the relationship
        between two objects. Declarations parse well-formed predicates.
        
        Input: a string with one of the two following forms:
        1) professor[$Lucy] -> Declares the entity '$Lucy' as a member of the
        'professor' class.
        
        Declarations of membership can only happen to entities, objects
        which are the only member of their class. To denote an entity we use
        the $ symbol before the entity name.
        
        2) <friend[$Lucy,$John]> -> Declares a mapping of the 'friend' type
        between the entities '$Lucy' and '$John'.
        
        Declarations of mapping can happen between entities, classes, 
        or between an entity and a class (ie. <loves[$Lucy, cats]>).
        """
        rgx_ob = re.compile(r'\b(.*?)\]')
        set_ = rgx_ob.findall(form)
        set_ = set_[0].split('[')
        if ',' in set_[1]:
            set_[1] = tuple(set_[1].split(','))
        if '<' in form:
            # Is a function declaration > implies an mapping
            # between an object (or a set) and other object (or set).
            assert (type(set_[1]) == tuple), \
                    'A mapping needs a subject and object'
            self.up_attr(set_, key=1)
        else:
            # Is a membership declaration -> the object belongs 
            # to a set of objects.
            assert (type(set_[1]) != tuple), \
                    'Only one object can be declared as member of a set at once.'
            assert ('$' in set_[1]), 'The object is not an entity.'
            self.up_attr(set_)

    def save_proof(self, proof):
        names = []
        for part in proof.particles:
            if part.cond == 'predicate':
                names.append(part.pred[0])
        for name in names:
            if name not in self.formulae:
                self.formulae[name] = [proof]
            else:
                self.formulae[name].append(proof)

        # Run the new proof with every 'single' object that matches.
        for obj in self.singles.iterkeys():
            self.prove(obj)

    def prove(self, *args):
        keys = []
        for arg in args:
            if arg in self.singles:
                k = self.singles[arg]
                for x in k:
                    keys.append(x)
        forms = []
        for key in keys:
            if key in self.formulae:
                for proof in self.formulae[key]:
                    if proof not in forms:
                        forms.append(proof)
        for x, proof in enumerate(forms):
            proof(self, *args)

    def up_attr(self, var, key=0):
        if len(var) > 2 and var[2] is False:
                var[0] = 'NOT__' + var[0]
        if key == 0:
            subject = var[1]
            property_ = var[0]
            if subject not in self.singles:
                self.singles[subject] = [property_]
            elif property_ not in self.singles[subject]:                
                self.singles[subject].append(property_)            
            if property_ not in self.classes:
                self.classes[property_] = [subject]
            elif subject not in self.classes[property_]:
                self.classes[property_].append(subject)
        elif key == 1:
            relation = var[0]
            subject = var[1][0]
            obj = var[1][1]
            if subject not in self.relations:
                self.relations[subject] = {relation: [obj]}
            elif obj not in self.relations[subject][relation]:
                self.relations[subject][relation].append(obj)


# ===================================================================#
#   SUPPORTING CLASSES AND SUBCLASSES
# ===================================================================#


class Proof(object):
    """Object to store a logic formula."""
    def __init__(self, ori, comp, hier):
        self.depth = 0
        #self.vars = {}
        self.var_order = []
        self.particles = []
        self.make_parts(ori, comp, hier)
        self.connect_parts()
        self.depth = None
    
    def __call__(self, ag, *args):
        #print('\n----- NEW TEST -----')
        if len(self.var_order) == len(args):
            self.assigned = {}
            self.clean_results()
            for n, const in enumerate(args):
                memb = self.check_existence(const, ag)
                if memb is None:
                    return
                var_name = self.var_order[n]
                # Assign an entity to a variable by order.
                self.assigned[var_name] = [const, set(memb)]
            self.particles[-1].resolve(self, ag, key=[0])
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
        lvl = self.depth
        while lvl > -1:
            p = [part for part in self.particles if part.depth == lvl]
            for part in p:
                particles.append(part)
            lvl -= 1
        self.particles = particles
        for p in self.particles:
            p.connect(self.particles)

    def new_test(self, form, depth, parent, part_id, syb):

        def up_var():
            vars_ = form[i+1].split(',')
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
            if '~' in form:
                neg = False
            else:
                neg = True
            rgx_ob = re.compile(r'\b(.*?)\]')
            set_ = rgx_ob.findall(form)
            set_ = set_[0].split('[')            
            if len(set_[1]) > 1:
                set_[1] = tuple(set_[1].split(','))
            set_.append(neg)
            if '<' in form:
                set_.append('map')
            return set_

        if depth > self.depth:
            self.depth = depth
        if ':equiv:' in form:
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
            # Only universal quantifier is supported right now,
            # so the quantity is irrelevant.

            form = form.split(':')
            cond = 'check_var'
            for i, a in enumerate(form):
                if a == 'forall':
                    #quant = float('inf')
                    up_var()
            #    elif a == 'exists':
            #        quant = 1
            #        up_var()
        elif '[' in form:
            cond = 'predicate'
            form = tuple(break_pred(form))
            self.particles.append(Particle(cond, depth, part_id, parent, syb, form))

    def clean_results(self):
        """Clean up previous results."""
        for part in self.particles:
            if part.results is not None:
                part.results = []
    
    def check_existence(self, name, ag):
        """Check if an entity existence is known by the agent."""
        if name in ag.singles:
            return ag.singles[name]
        else:
            return None


class Particle:
    """Is a node that represents a logic atom, that can be either:
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

    def __str__(self):
        if self.cond != 'predicate':
            s = '<operator ' + str(self.pID) + ' (depth:' \
            + str(self.depth) + ') "' + str(self.cond) + '">'
        else:
            s = '<predicate ' + str(self.pID) + ' (depth:' \
            + str(self.depth) + '): ' + str(self.pred) + '>'
        return s

    def connect(self, part_list):
        for x, child in enumerate(self.next):
            for part in part_list:
                if part.pID == child:
                    self.next[x] = part
                    self.next[x].parent = self

    def resolve(self, proof, ag, key, *args):
        """Keys for resolving the proof:
        100: Get a child's predicate.
        101: Check the truthiness of a child particle.
        102: Incoming truthiness of an operation for storage.
        103: Incoming predicate for substitution.
        104: A test is passed and returns true.
        """
        #print self, '// Key:'+str(key), '// Args:', args
        if key[-1] == 102:
            key.pop()
            self.results.append(args[0])
        if key[-1] == 103:
            self.results.append(args[0])
        if self.cond == 'check_var':
            self.next[0].resolve(proof, ag, key)
        elif self.cond == 'implies':
            if key[-1] == 104:
                self.results.append(args[0])
                self.implies(proof, ag, key)
            else:
                current = len(self.results)
                # if the left branch is examined then solve, else don't.
                if current < len(self.next):
                    key.append(100) if current == 1 else key.append(101)
                    self.next[current].resolve(proof, ag, key)
                else:
                    self.implies(proof, ag, key)
        elif self.cond == 'equiv':
            pass
            # equivalence
            #
        elif self.cond == 'and':
            if key[-1] == 104:
                key.pop()
                self.results.append(args[0])
            current = len(self.results)
            if key[-1] == 101:
                if current < len(self.next):
                    key.append(101)
                    self.next[current].resolve(proof, ag, key)
                else:
                    self.conjunction(proof, ag, key)                    
            elif key[-1] == 100:
                if current < len(self.next) and \
                self.next[current].cond == 'predicate':
                    key.append(101)
                    self.next[current].resolve(proof, ag, key)
                elif current < len(self.next):
                    self.next[current].resolve(proof, ag, key)
                else:
                    result = self.conjunction(proof, ag, key)
                    key.append(104)
                    self.parent.resolve(proof, ag, key, result)
        elif self.cond == 'or':
            if key[-1] == 104:
                key.pop()
                self.results.append(args[0])
            current = len(self.results)            
            if key[-1] == 101:
                if current < len(self.next):
                    key.append(101)
                    self.next[current].resolve(proof, ag, key)
                else:
                    # Two branches finished, check if one is true.
                    self.disjunction(proof, ag, key)
            elif key[-1] == 100 or key[-1] == 104:
                if current < len(self.next) and \
                self.next[current].cond == 'predicate':
                    key.append(101)
                    self.next[current].resolve(proof, ag, key)
                elif current < len(self.next):
                    self.next[current].resolve(proof, ag, key)
                else:
                    result = self.disjunction(proof, ag, key)
                    key.append(104)
                    self.parent.resolve(proof, ag, key, result)
        elif self.pred:
            result = self.ispred(proof, ag, key)
            key.append(102) if key.pop() == 101 else key.append(103)
            self.parent.resolve(proof, ag, key, result)

    def implies(self, proof, ag, key):
        if key[-1] == 104:
            print '\nTESTED THE RIGHT BRANCH'
            print self.results
        else:
            # two branches finished, check if left is true
            left_branch = self.results[0]
            right_branch = self.results[1]
            if left_branch is True and key[-1] == 103:
                # marked for resolution
                # subtitute var(s) for object(s) name(s)
                # and pass to agent for updating proper classes
                if type(right_branch[1]) is tuple:
                    var1, var2 = right_branch[1][0], right_branch[1][1]
                    var1 = proof.assigned[var1][0]
                    var2 = proof.assigned[var2][0]
                    right_branch[1] = (var1, var2)
                    ag.up_attr(right_branch, key=1)
                else:
                    var = right_branch[1]
                    right_branch[1] = proof.assigned[var][0]
                    ag.up_attr(right_branch)

    def conjunction(self, proof, ag, key):
        left_branch = self.results[0]
        right_branch = self.results[1]
        if key[-1] == 101:
        # Two branches finished, check if both are true.            
            if (left_branch and right_branch) is True:
                key.append(102)
                self.parent.resolve(proof, ag, key, True)
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
                self.parent.resolve(proof, ag, key, True)
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
            if len(self.pred) == 4:
                # Check mapping of a set/entity to an other set/entity.                
                subject = proof.assigned[self.pred[1][0]][0]
                obj = proof.assigned[self.pred[1][1]][0]
                check_func = self.pred[0]
                must_be = self.pred[2]
                try:
                    mapped = ag.relations[subject][check_func]
                except:
                    result = False
                else:
                    if must_be is True:
                        result = True if obj in mapped else False
                    else:
                        result = False if obj in mapped else True
            else:
                # Check membership to a set of an entity.
                var = self.pred[1]
                belongs_to_sets = proof.assigned[var][1]
                check_set = self.pred[0]
                must_be = self.pred[2]
                # If must be True, then the object must belong to the set.
                # Else, must be False, and the object must not belong to the set.
                if must_be is True:
                    result = True if check_set in belongs_to_sets else False
                else:
                    result = False if check_set in belongs_to_sets else True
            return result
        if key[-1] == 100:
            # Return predicate for substitution
            result = [x for x in self.pred]
            return result


if __name__ == '__main__':
    import datetime

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
        r.encode(form)
    r.prove('$Lucy','$John')
    print '\n---------- RESULTS ----------'
    d2 = datetime.datetime.now()
    print (d2-d1)
    print r.singles
    print r.classes
    print r.relations
