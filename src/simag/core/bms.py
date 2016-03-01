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

from simag.core.parser import LogPredicate, LogSentence


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
    
    def add(self, pred, form=None):
        """Initialises the sequence to detect inconsistencies between new
        beliefs and old beliefs.
        """
        if hasattr(pred, 'belief_record'):
            record = pred.belief_record
            is_new = False
        else:
            record = BeliefRecord(self, pred)
            is_new = True
        record.add_entry(self.ag, pred, form, is_new=is_new)
    
    @staticmethod
    def _rollback(ag, record):
        """After a change look for all the changes that were produced
        due to this belief previous value and test if they still apply.
        If the facts no longer hold true rollback them.
        """
        if len(record.entries) < 2: return
        last = record.entries[-2]
        if len(last.produced) == 0: return
        for entry in last.produced:
            if entry is entry.record[-1]:
                # ask the agent again if the value to be rolled back still holds true
                answ = ag.ask(entry.pred)
                if not answ:
                    # the result is no longer right, so changes produced by this
                    # value must be rolled back
                    if len(entry.record) > 1:
                        rb = entry.record[-2]
                        val = rb.value
                    else:
                        # this wasn't a grounded fact before, val should be None
                        rb = entry.record[-1]
                        val = None
                    rb.pred.value = val
                    entry.record.add_entry(ag, rb.pred, rollback=False)
                if len(entry.produced) > 0:
                    BmsWrapper._rollback(ag, entry.record)
        
class BeliefRecord(object):
    """Representation of how a belief became to existence.
    
    On initialisation it receives which belief came to existence,
    and the reference of the formula which produced the belief.
    """
    from collections import namedtuple
    BmsRecord = namedtuple('BmsRecord', ['date', 'value', 'produced', 'pred', 'record'])
    
    def __init__(self, wrapper, pred):
        self.entries = []
        pred.belief_record = self
    
    def add_entry(self, ag, pred, form=None, is_new=False, rollback=True):
        if is_new or self.entries[-1].value != pred.value:
            entry = self.new_entry(pred, form)
            self.entries.append(entry)
            if rollback:
                BmsWrapper._rollback(ag, self)
        
    def __getitem__(self, key):
        return self.entries[key]
    
    def __len__(self):
        return len(self.entries)
    
    def new_entry(self, pred, form):
        entry = BeliefRecord.BmsRecord(
                date = datetime.now(),
                value = pred.value,
                pred = pred,
                produced = list(),
                record = self
        )
        if not form: return entry
        if not issubclass(form.__class__, LogSentence):
            raise TypeError("expected a subclass of LogSentence")
        for predecessor in form.produced_from:
            rec = predecessor.belief_record.entries[-1]
            rec.produced.append(entry)
        return entry
    