# -*- coding: utf-8 -*-
"""Belief maintenance system for the agent system.

This module adds methods and classes for:
1) Recording how a belief came to existence to the agent.
2) Detecting inconsistences between new and old beliefs.
3) Fixing those inconsitences.
"""
# ===================================================================#
#   Imports and globals
# ===================================================================#
import datetime

# ===================================================================#
#   Recording subsystem
# ===================================================================#


class BeliefRecord(object):
    """Representation of how a belief become to existence.
    
    On initialisation it receives which belief came to existence,
    and the reference of the formula which produced the belief.
    """
    def __init__(self, form, container):
        self.form = form
        self.bms = container
        self.beliefs = []
        self.prod = []

    def add(self, s):
        """Rebuilds the belief in a logic predicate form and adds it."""
        if len(s) == 2:
            pred = ''.join([s[0][0],'[',s[1],',u=',str(s[0][1]),']'])            
        else:
            pred = ''.join(['<',s[0],'[',s[1][0],';',s[1][1][0],\
                            ',u=',str(s[1][1][1]),']>'])
        self.beliefs.append(pred)
        self.save()

    def prev_blf(self, belief):
        """The belief may be a product of past beliefs, the possibility
        is explored and stores the immeate previous belief that 
        produced this belief.
        """
        if belief in self.bms.container:
            self.prod.append(belief)

    def save(self):
        if len(self.prod) != 0:
            for b in self.beliefs:
                self.bms.container[b] = {'form': self.form, 
                                         'prev': tuple(self.prod),
                                         'date': datetime.datetime.now()}
        else:
            for b in self.beliefs:
                self.bms.container[b] = {'form': self.form, 
                                         'prev': None,
                                         'date': datetime.datetime.now()}
        #pprint.pprint(self.bms.container)


class BmsContainer(object):
    """Acts as a wrapper for the Belief Maintenance System for a given
    agent. 
    
    Converts from/to data in the database and serves to keep the
    believes alive in memory.
    """
    def __init__(self):
        self.container = {}
    
    def start_reg(self, form):
        self.working_bel = BeliefRecord(form, self)

    def add(self, *args):
        if len(args) < 2:
            self.working_bel.add(*args)
        elif args[1] is True:
            self.container[args[0]] = {'form': 'SELF',
                                       'prev': None,
                                       'date': datetime.datetime.utcnow()}
    
    def prev_blf(self, *args):
        self.working_bel.prev_blf(*args)
    
    def k_chain(self):
        """Reconstructs a chain which represent the beliefs that 
        produced an input belief for inconsistence fixing.
        """
        pass

# ===================================================================#
#   Maintenance subsystem
# ===================================================================#


def check_consistency():
    """Check if a new belief is consistent with all past beliefs."""
    pass
    
