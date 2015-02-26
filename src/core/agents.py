"""Main agent module."""

import datetime
import uuid

from core.actions import *


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

    def __init__(self, **kwargs):        
        self.props = {
                      'born': datetime.datetime.utcnow(), 'ag_state': 'idle',
                      'type': 'b_ag', 'oID': str(uuid.uuid4())
                      }
        self.assets = BalanceSheet()
        self.liabilities = BalanceSheet()
        for prop, val in kwargs.items():
            self.props[prop] = val
        self.register()
    
    def register(self):
        if not 'pos' in self.props.keys(): 
            raise KeyError('No position (tupple of 3 floats) provided for the agent.')
        position = self.props['pos']
        _type = self.props['type']
        oID = self.props['oID']
        return position, oID, self
    
    def state(self):
        return self.props['ag_state']
    
    def update_prop(self, prop, val):
        self.props[prop] = val
        
    def evaluate(self):
        """Evaluates the environment and creates an internal representation
        based on this 'perception'.        
        """        
        # Registers the representations and keep them
        
    def action(self, act=None):        
        check_action_routines(act, self.props['ag_state'],
                              self.props['env_state'])
    
    
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
        
    def add(self, item=None, quant=None, **kwargs):
        if item != None and quant != None:
            self[item] = quant
        self.update(kwargs)
    
    def remove(self, item):
        del self[item]
    
    