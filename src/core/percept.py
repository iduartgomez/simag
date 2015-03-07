# -*- coding: utf-8 -*-
import re
import os

class Representation(object):
    """This class is a container for internal agent's representations
    of the 'simulated reality'. An agent can have any number of such
    representations at a moment in time, all of which are contained
    in this object.

    The representations are modal and fuzzy logical sentences in
    a local notation. The class includes methods to encode and decode
    the representations to/from machine language.
    """
    def __init__(self):
        self.singles = {}
        self.classes = {}
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
                raise SyntaxError('Incorrect use of parentheses.')
            else:
                elem = s[par[0]+1:par[1]]
                comp.append(elem)
                s = s.replace(s[par[0]:par[1]+1], '{'+str(f)+'}')
                f += 1
                return decomp_par(s, symb, f)
            
        def decomp_all(tform, symb, idx):
            if symb in tform:
                memb = tform.split(symb)
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
            elif '[' in par_form and len(comp) > 1:
                raise SyntaxError('Statement not well constructed.')
            else:
                # Is a conditional function declaration:
                # the action taken depends on the operators.
                pass
        else:
            # It's a formula, not a declaration/definition
            proof = Proof(ori, comp, hier)
            self.save_proof(proof)

    def declare(self, form):
        rgx_ob = re.compile(r'\b(.*?)\]')
        set_ = rgx_ob.findall(form)
        if '<' in form:
            # Is a function declaration > implies an action
            # between an object and the env or other objects.
            pass
        else:
            # Is a membership declaration -> the object belongs 
            # to a set of objects.
            set_ = set_[0].split('[')
            if ',' in set_[1]:
                set_[1] = tuple(set_[1].split(','))
            if isinstance(set_[1], tuple):
                raise IndexError('Only one object can be declared as \
                                  member of a set at once.')
            else:
                if '$' in set_[1]:
                    self.up_classes(set_)

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
        # Run the new proof with every 'single' object that matches
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
            print 'NEW PROOF .....'
            proof(self, *args)

    def up_classes(self, var, *args):
        if var[1] not in self.singles:
            self.singles[var[1]] = [var[0]]
        else:
            if var[0] not in self.singles[var[1]]:                
                self.singles[var[1]].append(var[0])
        if var[0] not in self.classes:
            self.classes[var[0]] = [var[1]]
        else:
            if var[1] not in self.classes[var[0]]:
                self.classes[var[0]].append(var[1])


class Proof(object):
    """Object to store logic proofs.
    """
    def __init__(self, ori, comp, hier):
        self.depth = 0
        #self.vars = {}
        self.var_order = []
        self.particles = []
        self.make_parts(ori, comp, hier)
        self.connect_parts()
        
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
            if '<' in form:
                set_[0] = '<' + set_[0] + '>'
            if len(set_[1]) > 1:
                set_[1] = tuple(set_[1].split(','))
            set_.append(neg)
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
            # so the quantity is irrelevant
            
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

    def __call__(self, ag, *args):
        if len(self.var_order) == len(args):
            self.assigned = {}
            self.clear_results()
            for n, const in enumerate(args):
                memb = self.check_membership(const, ag)
                if memb is None:
                    return
                var_name = self.var_order[n]
                self.assigned[var_name] = [const, set(memb)]
            self.particles[-1].resolve(self, ag, key=[0])
        else:
            return
    
    def clear_results(self):
        for part in self.particles:
            if part.results is not None:
                part.results = []
    
    def check_membership(self, name, ag):
        if name in ag.singles:
            return ag.singles[name]
        else:
            return None

class Particle:
    def __init__(self, cond, depth, id_, parent, syb, *args):
        self.pID = id_
        self.depth = depth
        self.cond = cond
        self.next = syb
        self.parent = parent
        self.results = []
        if cond == 'predicate':
            self.pred = args[0]
        if syb[0] != -1:
            self.results = []

    def __str__(self):
        if self.cond != 'predicate':
            s = '<operator ' + str(self.pID) + ' (depth:' \
            + str(self.depth) + ') "' + str(self.cond) + '">'
        else:
            s = '<predicate ' + str(self.pID) + ' (depth:' \
            + str(self.depth) + '): ' + str(self.pred) + '>'
        return s

    def connect(self, part_list):
        for part in part_list:
            if self.parent == part.pID:
                self.parent = part
                break
        for x, child in enumerate(self.next):
            for part in part_list:
                if part.pID == child:
                    self.next[x] = part

    def resolve(self, proof, ag, key, *args):
        """Keys for resolving the substitution:
        100: Get a child's predicate.
        101: Check the truthiness of an operation.
        102: Incoming truthiness of an operation for storage.
        103: Incoming predicate for resolution.
        """
        print self, '// Key:'+str(key), '// Args:', args
        if key[-1] == 102:
            key.pop()
            self.results.append(args[0])
        if key[-1] == 103:
            self.results.append(args[0])
        if self.cond == 'check_var':
            self.next[0].resolve(proof, ag, key)
        elif self.cond == 'implies':
            current = len(self.results)
            if current < len(self.next):
                # if the left branch is examined then solve, else don't.
                key.append(100) if current == 1 else key.append(101)
                self.next[current].resolve(proof, ag, key)
            else:
                # two branches finished, check if left is true
                left_branch = self.results[0]
                right_branch = self.results[1]
                if left_branch is True and key[-1] == 103:
                    # marked for resolution                    
                    # subtitute var for object's name
                    var = right_branch[1]
                    right_branch[1] = proof.assigned[var][0]
                    # pass to agent for updatign proper classes                    
                    ag.up_classes(right_branch)
        elif self.cond == 'and':
            current = len(self.results)
            if current < len(self.next):
                key.append(101)
                self.next[current].resolve(proof, ag, key)            
            else:
                left_branch = self.results[0]
                right_branch = self.results[1]
                if (left_branch and right_branch) is True:
                    key.append(102)
                    self.parent.resolve(proof, ag, key, True)
        elif self.cond == 'or':
            current = len(self.results)
            if current < len(self.next):
                key.append(101)
                self.next[current].resolve(proof, ag, key) 
            else:
                left_branch = self.results[0]
                right_branch = self.results[1]
                if (left_branch or right_branch) is True:
                    key.append(102)
                    self.parent.resolve(proof, ag, key, True)
        elif self.pred:
            result = self.ispred(proof, key)
            key.append(102) if key.pop() == 101 else key.append(103)
            self.parent.resolve(proof, ag, key, result)

    def ispred(self, proof, key, *args):
        if key[-1] == 101:
            belongs_to_sets = proof.assigned[self.pred[1]][1]
            checking_set = self.pred[0]
            must_be = self.pred[2]
            # if must be True, then the object must belogn to the set.
            # else, must be False, and the object mustn't belong to the set.
            if must_be is True:
                result = True if checking_set in belongs_to_sets else False
            else:
                result = False if checking_set in belongs_to_sets else True
            return result
        if key[-1] == 100:
            result = [x for x in self.pred]
            return result


if __name__ == '__main__':
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
    for form in ls:
        r.encode(form)
    
    print '---------------'
    print r.singles
    print r.classes
