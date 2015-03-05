# -*- coding: utf-8 -*-
import re


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
                raise ValueError('Incorrect use of parentheses.')
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
                raise TypeError('Statement not well constructed.')
            else:
                # Is a conditional function declaration:
                # the action taken depends on the operators.
                pass
        else:
            # It's a formula, not a declaration/definition
            proof = Proof()
            make_form(proof, ori, comp, hier)
            print 'COMP:', proof.instructions
            proof(self, '$John')
            print '---------------'
    
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
                raise ValueError('Only one object can be declared as \
                                  member of a set at once.')
            else:
                if '$' in set_[1]:
                    self.up_classes(set_, key=0)
    
    def up_classes(self, var, key):
        if key == 0:
            if var[1] not in self.singles:
                self.singles[var[1]] = [var[0]]
            else:
                self.singles[var[1]].append(var[0])
            if var[0] not in self.classes:
                self.classes[var[0]] = [var[1]]
            else:
                self.classes[var[0]].append(var[1])
            
    def propositions(self):
        """Propositions are analysed to extract the classes of
        the different elements."""
        return


class Proof(object):
    """Object to store logic proofs."""

    def __init__(self):
        self.vars = {}
        self.var_order = []
        self.instructions = []
        self.depth = 0

    def __call__(self, ag, *args):
        self.ag = ag
        if len(self.vars) == len(args):
            self.assign = {}
            for n, const in enumerate(args):
                memb = self.check_membership(const)
                if memb is None:
                    return
                var_name = self.var_order[n]
                self.assign[var_name] = [const, memb]
                hier = [x[0] for x in self.instructions]
                self.check_conditions(hier, 0)
        else:
            return

    def check_membership(self, name):
        if name in self.ag.singles:
            return self.ag.singles[name]
        else:
            return None
    
    def check_conditions(self, hier, depth):
        for i, lvl in enumerate(hier):
            if lvl == depth:
                atoms = self.instructions[i]     
        if 'implies' in atoms[1]:
            atoms = []
            for i, lvl in enumerate(hier):
                if lvl == depth + 1:
                    atoms.append(self.instructions[i])
            if atoms[0][0]  == self.depth:
                rgx_par = re.compile(r'\[(.*?)\]')
                rgx_ob = re.compile(r'\b(.*?)\]')
                for n, atom in enumerate(atoms):                    
                    vars_ = rgx_par.findall(atom[2])
                    if len(vars_) == 1:
                        var = vars_[0]
                        var = [x[1] if x[0] < depth else None for x in self.vars[var]] 
                        var = var[0]                                         
                        set_ = rgx_ob.findall(atom[2])
                        set_ = set_[0].split('[')
                        if n == 0 and set_[0] in self.assign[set_[1]][1]:
                            cond = True
                        elif n == 0:
                            cond = False
                        if n != 0 and cond is True:
                            set_[1] = self.assign[set_[1]][0]
                            self.ag.up_classes(set_, key=0)
        if depth < self.depth:
            depth += 1
            self.check_conditions(hier, depth)
    
    # optimizable en Representation.encode.decomp_all()
    def new_test(self, form, depth):
        
        def up_var(quant):
            vars_ = form[i+1].split(',')
            for var in vars_:
                var_name = var.strip()
                if var_name not in self.vars:
                    self.vars[var_name] = [(depth, quant)]
                    self.instructions.append((depth, condition, var_name))
                    self.var_order.append(var_name)
                else:
                    self.vars[var_name].append(depth, quant)
                    self.instructions.append((depth, condition, var_name))
                    self.var_order.append(var_name)
        
        self.depth = depth
        if ':equiv:' in form:
            condition = 'equiv'
            self.instructions.append((depth, condition))
        if ':implies:' in form:
            condition = 'implies'
            self.instructions.append((depth, condition))
        if ':or:' in form:
            condition = 'or'
            self.instructions.append((depth, condition))
        if ':and:' in form:
            condition = 'and'
            self.instructions.append((depth, condition))
        if any(x in form for x in [':forall:', ':exists:']):            
            form = form.split(':')
            condition = 'check_var'
            for i, a in enumerate(form):
                if a == 'forall':
                    quant = float('inf')
                    up_var(quant)
                elif a == 'exists':
                    quant = 1
                    up_var(quant)
        elif '[' in form:
            condition = 'predicate'
            self.instructions.append((depth, condition, form.strip()))


def make_form(proof, ori, comp, hier, depth=0):
    form = comp[ori]
    childs = hier[ori]['childs']
    proof.new_test(form, depth)
    depth += 1
    for child in childs:
        if hier[child]['childs'] != -1:
            make_form(proof, child, comp, hier, depth)
        else:
            form = comp[child]
            proof.new_test(form, depth)
