"""Main agent module."""

import datetime
import time
import uuid

from core.actions import eval_routines, action_routines


prop_list = ['name','born', 'ag_state', 'pos']

class Representation(object):
    """This class is a container for internal representations of agents
    of the 'simulated reality'. An agent can have any number of such 
    representations.    
    """    
    def __init__(self, owner, target):
        self.owner = owner # To which agent belongs the representation
        self.target = target # The target of the representation

class BasicAgent(object):
    """Implements the basic Agent object."""
    def __init__(self, config=None, **kwargs):        
        self.props = {
                      'oID': str(uuid.uuid4()),
                      'type': 'b_ag', 
                      'born': datetime.datetime.utcnow(), 
                      'ag_state': 'idle',
                      'percept_mode': 'std',
                      'eval_mode': 'std'
                      }
        self.percepts = {}
        self.assets = BalanceSheet()
        self.liabilities = BalanceSheet()
        for prop, val in kwargs.items():
            self.props[prop] = val
        self.register()
    
    def register(self):
        if not 'pos' in self.props.keys(): 
            raise KeyError("No position provided for the agent.")
        position = self.props['pos']
        oID = self.props['oID']
        type_ = self.props['type']
        return position, oID, self, type_
    
    def state(self):
        return self.props['ag_state']
    
    def update(self, prop=None, val=None, **kwargs):
        self.props[prop] = val
        for prop, val in kwargs.items():
            self.props[prop] = val
        
    def evaluate(self):
        """Evaluates the environment and creates an internal representation
        based on this 'perception'.        
        """
        set_percept_mode = self.props['percept_mode']
        percept = eval_routines(self, set_percept_mode)
        reg = float(time.time())
        self.percepts[reg] = percept
        
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

class BalanceSheet(dict):
    """This object represents the assets and liabilities of
    agents and institutions.
    """            
    def add(self, item=None, quant=None, value=None, **kwargs):
        if item != None and quant != None:
            self[item] = (quant, value)
        self.update(kwargs)
    
    def remove(self, item):
        del self[item]
    
    def valuation(self, item, value):
        old_values = self[item]
        new_values = self[item] = (old_values[0], value)
        self[item]  = new_values
    