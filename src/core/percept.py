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
            
        def get_membs(i):
            par = hier[i]['parent']
            brothers = hier[par]['childs']
            for x, b in enumerate(brothers):
                if b == i:
                    brothers.pop(x)                
            return brothers, par
        
        def up_dict(var, key):
            if key == 0:
                if not var[1] in self.singles:
                    self.singles[var[1].strip('$')] = [var[0]]
                else:
                    self.singles[var[0]].append(var[1])
                if not var[0] in self.classes:
                    self.classes[var[0]] = [var[1]]
                else:
                    self.classes[var[0]].append(var[1])

        decomp_par(formula.rstrip('\n'), symb=('(',')'))
        for i, form in enumerate(comp):
            symbs = ['<=>',' =>','||','&&']
            for symb in symbs:
                if decomp_all(form, symb, idx=i):
                    break
        idx = len(comp)
        iter_childs(idx)
        for idx, form in enumerate(comp):
            if '[' in form:
                set_ = rgx_ob.findall(form)
                if '<' in form:
                    # This is a function class > implies an action between
                    # an object and the enc or an obj and other objects.
                    pass
                else:
                    # This is a set class > an object belongs 
                    # to a set of objects.
                    set_ = set_[0].split('[')
                    if ',' in set_[1]:
                        set_[1] = tuple(set_[1].split(','))
                    if isinstance(set_[1], tuple):
                        pass
                    else:
                        if '$' in set_[1]:
                            up_dict(set_, key=0)                            
            elif ':implies:' in form:
                par = hier[idx]['parent']
                par_form = comp[par]
                while not any(x in par_form for x in [':forall:', ':exists:']):                    
                    par = hier[idx]['parent']
                    par_form = comp[par]
                par_form = par_form.split(':')
                for i, a in enumerate(par_form):
                    if a == 'forall':
                        var = par_form[i+1].split(',')
                        var = var[0] if len(var) == 1 else None
                childs = hier[par]['childs']                
                print comp, childs
                
        #print comp

    def propositions(self):
        """Propositions are analysed to extract the classes of
        the different elements."""
        return
