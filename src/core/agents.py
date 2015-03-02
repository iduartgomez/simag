"""Main agent module."""

import datetime
import time
import uuid

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
    def encode(self, sentence):
        comp = []
        logsymb = [':=', '=>', '<=>', '~', ':AND:', ':NAND:', ':OR:',
                   ':XOR:', ':FORALL:', ':EXIST:', ':TRUE:', ':FALSE:',
                   ':PROVABLE:', ':THEREFOR:']

        def decomp_par(s):
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
                    if diff < min_:
                        min_ = diff
                        par = (i, e)
            if len(initpar) == 0 and len(endpar) == 0:
                return s
            elif (len(initpar) == 0 and len(endpar) != 0) or \
                 (len(initpar) != 0 and len(endpar) == 0):
                print(s)
                raise ValueError('Incorrect use of parentheses.')
            else:
                comp.append(s[par[0]+1:par[1]])
                print(s[par[0]+1:par[1]])
                #return decomp_par(s)
        s = decomp_par(sentence.replace(' ', ''))
        return comp

    def propositions(self):
        """Sentences are analysed for similarities to extract invariant
        qualities and generalized to predicates and formulas."""
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

sentence = "(Nacho = Human :AND: (~Machine)) :THEREFOR: (:EXIST:Machines = ~Human)"
s = Representation().encode(sentence)
