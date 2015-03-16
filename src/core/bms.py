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
            self.parent = parent

        def remake(self, sets):
            # SUBTITUTE symbol u= can be u< or u>
            if len(sets) == 3:
                s = ['<', sets[0], '[', sets[1][0], ';', sets[1][1][0],
                     ',u=', str(sets[1][1][1]), ']>']
            else:
                s = [sets[0][0], '[', sets[1], ',u=', str(sets[0][1]), ']']
            s = ''.join(s)
            self.parent.container[s] = {'form': 'SELF',
                                        'prev': None,
                                        'date': datetime.datetime.utcnow()}

    def __init__(self, ag):
        self.container = {}
        self.ag = ag

    def register(self, form, stop=False):
        if stop is True:
            self.wrk_bel.save()
        else:
            self.wrk_bel = BeliefRecord(form, self)

    def add(self, *args):
        if args[1] is True:
            self.wrk_bel = self.WrappDecl(self)
            self.check(args[0])

    def prev_blf(self, *args):
        self.wrk_bel.prev_blf(*args)
    
    def k_chain(self, pred, pval):
        """Reconstructs a chain which represent the beliefs that produced
        an input belief.
        """
        # SUBTITUTE symbol u= can be u < or u >
        rel, sbj, obj = pred[0], pred[1][0], pred[1][1][0]
        opred = '<'+rel+'['+sbj+';'+obj+',u='+str(pval)+']>'
        if self.container[opred]['form'] is 'SELF':
            # the fact has changed from the initial predicate
            pass
        else:
            pass
    
    def check(self, pred):
        """Initialises the sequence to detect inconsistencies between new
        beliefs and old beliefs.
        """
        # MUST CHECK u symbol, can be =, >, <
        if len(pred) == 2 and pred[1] in self.ag.individuals:
            cat, val, sbj = pred[0][0], pred[0][1], pred[1]
            categs = self.ag.individuals[sbj].get_cat()
            if cat in categs and val != categs[cat]:
                print 'INCONSISTENCY', pred, categs
            else:
                self.wrk_bel.remake(pred)
        elif len(pred) == 3 and pred[0] in self.ag.classes:
            rel0, sbj, obj = pred[0], pred[1][0], pred[1][1][0]
            val = pred[1][1][1]
            if '$' in sbj and rel0 in self.ag.individuals[sbj].relations:
                rel = self.ag.individuals[sbj].get_rel(rel0)
                if obj in rel and val != rel[obj]:
                    print 'INCONSISTENCY', '{'+obj+': '+str(val)+'}', rel
                    self.wrk_bel.remake(pred)
                    chk_const(self, pred, rel[obj])
        elif len(pred) == 3:
            self.wrk_bel.remake(pred)


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
    
    def remake(self, s):
        """Remakes the beliefs in a logic predicate form and stores them."""
        # SUBTITUTE symbol u= can be u< or u>
        if len(s) == 2:
            pred = ''.join([s[0][0],'[',s[1],',u=',str(s[0][1]),']'])            
        else:
            pred = ''.join(['<',s[0],'[',s[1][0],';',s[1][1][0],\
                            ',u=',str(s[1][1][1]),']>'])
        self.beliefs.append(pred)

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


def chk_const(wrapper, *args):
    """Check what predicates are the cause of the inconsistency."""
    p_known = wrapper.k_chain(*args)
    
