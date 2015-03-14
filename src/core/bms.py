# -*- coding: utf-8 -*-

"""Belief Maintenance System for the agent system.

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


class BmsWrapper(object):
    """Acts as a wrapper for the Belief Maintenance System for a given
    agent. 
    
    Converts from/to data in the database and serves to keep the
    believes alive in memory.
    """
    class WrappDecl(object):
        def __init__(self, parent):
            self.beliefs = []
            self.parent = parent

        def remake(self):
            sets = self.beliefs[0]
            if len(sets) == 3:
                s = ['<', sets[0], '[', sets[1][0], ';', sets[1][1][0],
                     ',u=', str(sets[1][1][0]), ']>']
            else:
                s = [sets[0][0], '[', sets[1], ',u=', str(sets[0][1]), ']']
            s = ''.join(s)
            self.parent.container[s] = {'form': 'SELF',
                                        'prev': None,
                                        'date': datetime.datetime.utcnow()}

    def __init__(self, ag):
        self.container = {}
        self.ag = ag

    def start_reg(self, form):
        self.wrk_bel = BeliefRecord(form, self)

    def add(self, *args):            
        if len(args) < 2:
            self.wrk_bel.add(*args)
        elif args[1] is True:
            self.wrk_bel = self.WrappDecl(self)
            self.wrk_bel.beliefs.append(args[0])
            self.check()

    def prev_blf(self, *args):
        self.wrk_bel.prev_blf(*args)
    
    def k_chain(self):
        """Reconstructs a chain which represent the beliefs that 
        produced an input belief for inconsistence fixing.
        """
        pass
    
    def check(self):
        """Initialises the sequence to detect inconsistencies between new
        beliefs and old beliefs.
        """
        for pred in self.wrk_bel.beliefs:
            if len(pred) == 2 and pred[1] in self.ag.individuals:                
                cat, val, sbj = pred[0][0], pred[0][1], pred[1]
                categs = self.ag.individuals[sbj].get_cat()
                if cat in categs and val != categs[cat]:
                    print 'INCONSISTENCY', pred, categs
            elif len(pred) == 3 and pred[0] in self.ag.classes:
                rel, sbj, obj = pred[0], pred[1][0], pred[1][1][0]
                val = pred[1][1][1]
                if '$' in sbj and rel in self.ag.individuals[sbj].relations:
                    rel = self.ag.individuals[sbj].get_rel(rel)
                    print 'CHECKING FOR INCON', obj, rel[obj], val
                    if obj in rel and val != rel[obj]:
                        print 'INCONSISTENCY', pred
        self.wrk_bel.remake()


class BeliefRecord(object):
    """Representation of how a belief become to existence.
    
    On initialisation it receives which belief came to existence,
    and the reference of the formula which produced the belief.
    """
    def __init__(self, form, wrapper):
        self.form = form
        self.bms = wrapper
        self.beliefs = []
        self.prod = []
    
    def add(self, s):
        self.beliefs.append(s)
    
    def remake(self):
        """Remakes the beliefs in a logic predicate form and stores them."""
        for i, s in enumerate(self.beliefs):
            if len(s) == 2:
                pred = ''.join([s[0][0],'[',s[1],',u=',str(s[0][1]),']'])            
            else:
                pred = ''.join(['<',s[0],'[',s[1][0],';',s[1][1][0],\
                                ',u=',str(s[1][1][1]),']>'])
            self.beliefs[i] = pred
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

# ===================================================================#
#   Maintenance subsystem
# ===================================================================#


def check_consistency():
    """Check if a new belief is consistent with all past beliefs."""
    pass
    
