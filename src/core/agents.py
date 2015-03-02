"""Main agent module."""

import datetime
import time
import uuid
import re

from core.actions import percept_routines, action_routines

prop_list = ['name', 'born', 'ag_state', 'pos']


class BasicAgent(object):
    """Implements the basic Agent object."""
    def __init__(self, config=None, load=None, **kwargs):
        self.props = {'oID': str(uuid.uuid4()),
                      'type': 'b_ag',
                      'born': datetime.datetime.utcnow(),
                      'ag_state': 'idle',
                      'percept_modes': ['std'],
                      'eval_modes': ['std']}
        self.representations = Representation()
        self.assets = BalanceSheet()
        self.liabilities = BalanceSheet()
        if load is not None:
            self.props = load['properties']
            self.assets.add(**load['assets'])
            self.liabilities.add(**load['liabilities'])
            self.props['position'] = load['position']
            if 'oID' not in load.keys():
                self.props['oID'] = str(uuid.uuid4())
        for prop, val in kwargs.items():
            self.props[prop] = val
        self.register()

    def register(self):
        if 'position' not in self.props.keys():
            raise KeyError("No position provided for the agent.")
        position = self.props['position']
        oID = self.props['oID']
        type_ = self.props['type']
        return position, oID, self, type_

    def state(self):
        return self.props['ag_state']

    def update(self, prop=None, val=None, **kwargs):
        self.props[prop] = val
        for prop, val in kwargs.items():
            self.props[prop] = val

    def perceive(self, eval_funcs, optimal=None):
        """Evaluates the environment and creates an internal representation
        based on this 'perception'.
        """
        set_percept_modes = self.props['percept_modes']
        percept = percept_routines(self, set_percept_modes, eval_funcs,
                                   optimal)
        world_rep = Representation(percept)
        reg = float(time.time())
        self.representationss[reg] = world_rep

    def action(self, act):
        set_eval_mode = self.props['eval_mode']
        action_routines(self, set_eval_mode, act)


class Institution(object):
    """Institutions are social constructs of different types.
    They are defined by the type of organisation, members and
    resources controlled by them.

    Agents can belong to none or n institutions.
    """
    pass


class Representation(object):
    """This class is a container for internal representations of agents
    of the 'simulated reality'. An agent can have any number of such
    representations, all of which are contained in this object.

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
                return s
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
        
        comp = []
        hier = {}
        # logsymb = ['::=', '=>', '<=>', ':nand:', ':xor:', ':forall::',
        #            ':exists::', ':true:', ':false:',':provable:', ':therefor:']
        s = decomp_par(sentence.replace(' ', ''))
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
                    for x, each in enumerate(others):
                        others[x] = (each, 1)
                    self.others[prop] = others
                else:
                    print 'No'
            elif ':therefor:' in prop:
                elem = prop.split(':therefor:')
            #self.extset
        print self.others

    def propositions(self):
        """Propositions are analysed to extract the classes of 
        the different elements."""
        return


class BalanceSheet(dict):
    """This object represents the assets and liabilities of
    agents and institutions.
    """
    def add(self, item=None, quant=None, value=None, **kwargs):
        if item is not None and quant is not None:
            self[item] = (quant, value)
        self.update(kwargs)

    def remove(self, item):
        del self[item]

    def valuation(self, item, value):
        old_values = self[item]
        new_values = self[item] = (old_values[0], value)
        self[item] = new_values

sentence = "((Nacho) = (Human & ~Machine & ~Ugly)) :therefor: ((:exists::Human) = (~Machine & ~Ugly))"
r = Representation()
r.encode(sentence)
