# -*- coding: utf-8 -*-
"""Belief maintenance system for the agent system.

This module adds methods and classes for:
1) Recording how a belief was created by an agent.
2) Detecting inconsistences between beliefs.
3) Fixing those inconsitences.

"""
# ===================================================================#
#   Imports and globals
# ===================================================================#
import pprint

container = {}

# ===================================================================#
#   Recording subsystem
# ===================================================================#


class BeliefRecord(object):
    """Representation of how a belief become to existence.
    
    On initialisation it receives which belief came to existence,
    and the reference of the formula which produced the belief.
    """
    def __init__(self, form):
        self.form = form
        self.beliefs = []
        self.product = []
    
    def add(self, s):
        """Rebuilds the belief in a logic predicate form."""
        if len(s) == 2:
            pred = ''.join([s[0][0],'[',s[1],',u=',str(s[0][1]),']'])            
        else:
            pred = ''.join(['<',s[0],'[',s[1][0],';',s[1][1][0],\
                            ',u=',str(s[1][1][1]),']>'])
        self.beliefs.append(pred)
        self.k_chain()
    
    def prev_blf(self, belief):
        if belief in container:
            self.product.append(belief)
    
    def k_chain(self):
        """The belief may be a product of past beliefs, the possibility
        is explored and produces a chain which represent the product of
        beliefs that produced this belief for recording.
        
        Example:
        0) belief(X)    
        # In this case is not a product, it's a declaration of a fact.
        1) P(1) -> belief(Y)    
        # The formula P(1) produces the belief b
        2) P(2) = P(1) x belief(X) -> belief(Z)
        # P(2) is the product of P(1) and belief X, which produces belief Z
        # so P(1) > P(2) > belief(Z) is recorded.
        """
        self.form
        self.save()

    def save(self):
        for b in self.beliefs:
            container[b] = self.form.id
        print self.product
        #pprint.pprint(container)
        
# ===================================================================#
#   Maintenance subsystem
# ===================================================================#


def check_consistency():
    """Check if a new belief is consistent with all past beliefs."""
    pass
    
