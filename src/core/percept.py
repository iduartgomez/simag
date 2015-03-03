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
        self.others = {}    
    
    def encode(self, sentence):

        def decomp_par(s, f=0, loop=0):
            initpar = []
            endpar = []
            idx = 0
            while idx < len(s):
                if s[idx] == '(':
                    initpar.append(idx)
                elif s[idx] == ')':
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
                return decomp_par(s, f)
        
        def iter_childs(ls):
            for n in range(0,ls):
                exp = comp[n]
                childs = rgx.findall(exp)
                childs = [int(x) for x in childs]
                if childs != ():
                    hier[n] = {'childs': childs, 'parent': -1}
                else:
                    hier[n] = {'childs': -1, 'parent': -1}
            for n in range(0,ls):
                childs = hier[n]['childs']
                if childs != -1:
                    for c in childs:
                        hier[c]['parent'] = n

        def up_dict(others, prop):
            try:
                old = self.others[prop]
            except:
                for x, each in enumerate(others):
                    others[x] = (each, 1)
                self.others[prop] = others
            else:
                for oitem in old:
                    for x, nitem in enumerate(others):
                        others[x] =  (nitem, 1)
                        if oitem[0] == nitem:
                            others[x] =  (nitem, oitem[1] + 1)
                self.others[prop] = others

        comp = []
        hier = {}
        # logsymb = ['::=', '=>', '<=>', ':nand:', ':xor:', ':forall::',
        #            ':exists::', ':true:', ':false:',':provable:', ':therefor:']
        decomp_par(sentence.replace(' ', ''))
        rgx = re.compile('(?<={)[^}]*(?=})')
        idx = len(comp)
        iter_childs(idx)
        for i, prop in enumerate(comp):
            if '&' in prop:
                prop = (':and:', prop.split('&'))
                comp[i] = prop
            elif '||' in prop:
                prop = (':or:', prop.split('||'))
                comp[i] = prop
            elif ':exists::' in prop:
                prop = prop.replace(':exists::','')
                par = hier[i]['parent']
                brothers = hier[par]['childs']
                for x, b in enumerate(brothers):
                    if b == i:
                        brothers.pop(x)
                p_prop = comp[par]
                if p_prop.find('=') != -1:
                    p_prop = p_prop.partition('=')
                if '{'+str(i)+'}' in p_prop[0]:
                    memb = int(p_prop[2].replace('{','').replace('}',''))
                    others = comp[memb].split('&')
                    up_dict(others, prop)
                else:
                    memb = int(p_prop[1].replace('{','').replace('}',''))
                    others = comp[memb].split('&')
                    up_dict(others, prop)
            elif ':therefor:' in prop:
                #elem = prop.split(':therefor:')
                pass

    def propositions(self):
        """Propositions are analysed to extract the classes of 
        the different elements."""
        return

sentence = "((Nacho) = (Human & ~Machine & ~Ugly)) :therefor: ((:exists::Human) = (~Machine & ~Ugly))"
r = Representation()
r.others['Human'] = [('~Machine', 2)]
r.encode(sentence)
