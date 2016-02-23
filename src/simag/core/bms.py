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
from datetime import datetime

from simag.core.parser import LogFunction, LogPredicate


# ===================================================================#
#   Recording subsystem
# ===================================================================#
class BmsWrapper(object):
    """Acts as a wrapper for the Belief Maintenance System for a given
    agent. 
    
    Converts from/to data in the database and serves to keep the
    believes alive in memory.
    """

    def __init__(self, ag):
        self.ag = ag

    def add(self, pred, proof=None):
        """Initialises the sequence to detect inconsistencies between new
        beliefs and old beliefs.
        """
        def add_or_create():
            if postpone is True:
                BeliefRecord(self, pred, proof)
            else:
                prev_rec.add_entry(pred, proof)
        
        postpone = False
        if not hasattr(pred, 'term'): ls = pred.get_args()
        else: ls = [pred.term]
        for e in ls:
            try:
                if e[0] == '$': obj = self.ag.individuals[e]
                else: obj = self.ag.classes[e]
            except KeyError: postpone = True
            else:
                if issubclass(pred.__class__, LogFunction):
                    prev_rec = obj.get_rel(pred)
                elif issubclass(pred.__class__, LogPredicate):
                    prev_rec = obj.get_ctg(pred, obj=True)
                if not prev_rec or not hasattr(prev_rec, 'belief_record'):
                    postpone = True
                else:
                    prev_rec = prev_rec.belief_record
            add_or_create()
    
    def _check_prev_recs(self, belief):
        """After a change look for all the changes that were produced
        due to this belief previous value and call a rollback if it applies.
        """
        produced = None
        if produced:
            self.__rollback()
    
    def __rollback(self):
        """Rollback previous beliefs if they no longer apply after a change.
        """
        pass

class BeliefRecord(object):
    """Representation of how a belief became to existence.
    
    On initialisation it receives which belief came to existence,
    and the reference of the formula which produced the belief.
    """
    from collections import namedtuple
    BmsRecord = namedtuple('BmsRecord', ['date', 'form', 'change'])
    
    def __init__(self, bmswrapper, pred, form=None):
        self.bms = bmswrapper
        self.entries = []
        pred.belief_record = self
        entry = self.new_entry(form, pred.value)
        self.entries.append(entry)
    
    def add_entry(self, pred, form=None):
        if pred.value == self.entries[-1].change: return
        entry = self.new_entry(form, pred.value)
        self.entries.append(entry)
        # TODO: retrieve all the changes that were produced by consequence
        # of this element and re-run the test to see if it applied yet
        # else roll-back as it no longer applies
        self.bms._check_prev_recs(self)
    
    @classmethod
    def new_entry(cls, form, val):
        return cls.BmsRecord(
            date = datetime.now(),
            form = form,
            change = val
        )

