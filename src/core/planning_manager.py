# -*- coding: utf-8 -*-

"""Planning Manager module.

This module manages the different planning implementations, the context 
for planning and the selection of the different algorithms based on context.
"""

# ===================================================================#
#   Imports and globals
# ===================================================================#

from types import MethodType, FunctionType


# ===================================================================#
#   CONTEXT MANAGER
# ===================================================================#

class Context(object):
    """Wrapps the context data and acts as an interface for the
    different problem sets. Decission is delegated then to an
    the strategy manager based on the problem set.
    """    
    pass

def context_manager():
    """Extracts the data from the current agent knowledge necessary
    for planning actions.
    
    Input -> agent object
    Output -> context object
    """
    pass

# ===================================================================#
#   PLANNING ALGORITHMS IMPLEMENTATIONS
# ===================================================================#
# 
# Planning algorithms are loaded based on the current context and
# problem case.

class ProblemMeta(type):
    __problems = list()
    def __call__(cls, *args, **kwargs):
        for pcls in cls.__problems:
            if isinstance(pcls, cls): return pcls
        ProblemMeta.check_data_structure()
        new_cls = super().__call__(*args, **kwargs)
        cls.__problems.append(new_cls)
        return new_cls

    def check_data_structure():
        """When a subclass of ProblemCase is created, it's checked
        if the data input interface is compatible with the context.
        """
        pass

class ProblemSolver(metaclass=ProblemMeta):
    """An interface to define the different problem types, 
    it's solution algorithms, and transformation of the data 
    from the context to solve the problem.
    
    When subclassed, it defines the 'problem context' upon
    which the algorithms are selected for problem solving and
    loaded as needed.
    
    When a subclass, representing a problem case, is instantiated, 
    can be loaded with the different implementation algorithms to
    solve that particular set of problems.  
    """
    def __init__(self, *args):
        for f in args:
            func = MethodType(f, self)
            setattr(self, f.__name__, func)
        
    def __call__(self, *args, **kwargs):
        if hasattr(self, 'default'): self.default(*args, **kwargs)
        else: raise AttributeError('Need to set default algorithm, ' \
                'use the set_default method.')

    def add_algo(self, func):
        f = MethodType(func, self)
        setattr(self, func.__name__, f)
        
    def set_algorithm(self, func):
        if func is None: del self.default
        elif issubclass(func.__class__, self.__class__):
            self.default = func
        elif type(func) is FunctionType:
            if hasattr(self, func.__name__): 
                self.default = getattr(self, func.__name__)
            else: 
                self.add_algo(func)
                self.default = getattr(self, func.__name__)

#=========================================#

class ExampleProblem1(ProblemSolver):
    def solve(self, test_case): self.test = test_case 

class ExampleProblem2(ProblemSolver): pass

def sample_algo_01(self, test): 
    print("sample algo #1 running... {0}".format(test))
    
def sample_algo_02(self, test):
    print("sample algo #2 running... {0}".format(test))
    
class SolveProblem1WithAlgo1(ExampleProblem1): 
    def __call__(self, test): 
        print('attempting solution with algo 1 to problem 1: {0}'.format(test))
    
class SolveProblem1WithAlgo2(ExampleProblem1):
    def algo(self, test): 
        print('attempting solution with algo 2 to problem 1: {0}'.format(test))
    
test = 'THIS IS A SAMPLE OF A TEST'
problem1 = ExampleProblem1(sample_algo_01)
problem1.set_algorithm(sample_algo_01)
problem1(test)
problem1.set_algorithm(SolveProblem1WithAlgo1())
problem1(test)
problem1.set_algorithm(sample_algo_02)
problem1(test)
