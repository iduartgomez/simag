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

    def encode(self, formula):        
        comp = []
        hier = {}
        rgx_par = re.compile(r'\{(.*?)\}')
        rgx_ob = re.compile(r'\b(.*?)\]')

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
        
        def up_classes(var, key):
            if key == 0:
                if var[1] not in self.singles:
                    self.singles[var[1].strip('$')] = [var[0]]
                else:
                    self.singles[var[0]].append(var[1])
                if var[0] not in self.classes:
                    self.classes[var[0]] = [var[1]]
                else:
                    self.classes[var[0]].append(var[1])

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
                set_ = rgx_ob.findall(form)
                if '<' in par_form:
                    # Is a function declaration > implies an action
                    # between an object and the env or other objects.
                    pass
                else:
                    # Is a set declaration -> the object belongs 
                    # to a set of objects.
                    set_ = set_[0].split('[')
                    if ',' in set_[1]:
                        set_[1] = tuple(set_[1].split(','))
                    if isinstance(set_[1], tuple):
                        raise ValueError("Only one object can be declared as \
                                          member of a set at once.")
                    else:
                        if '$' in set_[1]:
                            up_classes(set_, key=0)
            elif '[' in par_form and len(comp) > 1:
                raise TypeError('Statement not well constructed.')
            else:
                # Is a conditional function declaration:
                # the action taken depends on the operators.
                print comp
        else:
            # It's a formula, not a declaration/definition
            formula = Formula(self, ori, comp, hier)

    def propositions(self):
        """Propositions are analysed to extract the classes of
        the different elements."""
        return


class Formula(object):
    """Object to store logic formulas."""
    class Variable():
        def __init__(self):
            self.values = []
        
        def __call__(self):
            pass
        
        def __str__(self):
            return str(self.values)
        
        def append(self, hier, val):
            self.values.insert(hier, val)
    
    class Proof():
        def __init__(self):
            self.atoms = {}
        
        def __call__(self):
            pass
        
        def new_test(self, form, depth):
            if ':implies:' in form:
                self.condition = ':implies:
        
        def childs(self):
            """If it has childs, makes and stores them."""
            pass
        
            
    def __init__(self, parent, ori, *args):
        self.vars = {}
        self.parent = parent
        self.proof = self.Proof()
        self.make_form(ori, *args)
    
    def __call__(self):
        pass    
    
    def make_form(self, ori, comp, hier, depth=0):
        form = comp[ori]
        print 'PROCESSING!', form
        self.proof.new_test(form, depth)
        childs = hier[ori]['childs']
        depth += 1
        for child in childs:            
            if hier[child]['childs'] != -1:                
                self.make_form(child, comp, hier, depth)
            else:
                form = comp[child]                
                print 'PROCESSING LAST ATOM:', form
                self.proof.new_test(form, depth)
        
        """
        par_form = comp[ori].split(':')
        for i, a in enumerate(par_form):
            if a == 'forall':
                vars_ = par_form[i+1].split(',')
                for var in vars_:
                    var_name = var.strip()
                    var = self.Variable()
                    var.append(0, float('inf'))
                    self.vars[var_name] = var
            elif a == 'exists':
                vars_ = par_form[i+1].split(',')
                for var in vars_:
                    var_name = var.strip()
                    var = self.Variable()
                    var.append(0, 1)
                    self.vars[var_name] = var"""
    
    def up_classes(self, var, key):
        if key == 0:
            if var[1] not in self.parent.singles:
                self.parent.singles[var[1].strip('$')] = [var[0]]
            else:
                self.parent.singles[var[0]].append(var[1])
            if var[0] not in self.classes:
                self.parent.classes[var[0]] = [var[1]]
            else:
                self.parent.classes[var[0]].append(var[1])

# :forall:x: (student[x] => person[x])

"""
formulas: { 'student': obj formula() }

formula():
    vars == {'x': -inf}
    __call__() == self.person()
    proposition(x) == x = 'person',
                     update_dict(person,vars['x'])

0. process(('student','$Bill'))
1. form = formulas['student']
2. form('$Bill')
3. form.vars[x] = $Bill
4. update_dict(person, '$Bill')

"""