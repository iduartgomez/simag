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
    """Manages the creation of 'ProblemDomain' subclasses.
    
    When a subclass of ProblemDomain is created, it's checked if the data 
    input interface is compatible with the context.
    
    It also checks if the output is compatible with the existing available 
    actions/choices to the agent, and the instructions are in a compatible 
    data structure.
    """
    __problems = list()
    def __call__(cls, *args, **kwargs):
        new_cls = super().__call__(*args, **kwargs)
        subs, subcls = False, False
        for i, pcls in enumerate(cls.__problems):
            if isinstance(pcls, cls):
                subs = True
                break
            if issubclass(cls, pcls.__class__):
                subs, subcls = True, True
                break
        if subcls is False:
            ProblemMeta.__check_input_data(cls)
            ProblemMeta.__check_output_data(cls)            
            if subs is True: cls.__problems[i] = new_cls
            else: cls.__problems.append(new_cls)
        return new_cls

    def __check_input_data(cls): pass
    
    def __check_output_data(cls): pass

class ProblemDomain(metaclass=ProblemMeta):
    """An interface to define the problems domain, its solution algorithms, 
    and transformation of the data from the context to solve the problem.
    
    When subclassed, it defines the 'problem context' upon which 
    the algorithms are selected for problem resolution and loaded as needed.
    
    When a subclass, representing a problem domain, is instantiated, can be 
    loaded with the different implementation algorithms to solve that 
    particular set of problems.
    """
    def __init__(self, relations, knowledge, actions):
        cls = self.__class__
        setattr(cls, 'relations', relations)
        setattr(cls, 'knowledge', knowledge)
        setattr(cls, 'actions', actions)
    
    def __call__(self, agent, *args, **kwargs):
        if hasattr(self, 'default'):
            self.agent = agent
            self.inspect_domain()
            self.default(*args, **kwargs)
        else: 
            raise AttributeError('Need to set default algorithm, ' \
            'use the set_default method.')

    def __add_algo(self, *algos):
        for algo in algos:
            f = MethodType(algo, self)
            setattr(self, algo.__name__, f)
        
    def set_algo(self, func=None):
        if func is None: del self.default
        elif type(func) is FunctionType:
            if hasattr(self, func.__name__): 
                self.default = getattr(self, func.__name__)
            else: 
                self.__add_algo(func)
                self.default = getattr(self, func.__name__)
        else:
            self.default = func
            
    def inspect_domain(self):
        """Inspects the problem domain definition and continues 
        if there isn't any incompatibility problem found."""
        # check if the required agent actions exists
        for action in self.__class__.actions:
            if action not in self.agent.actions:
                raise AttributeError("The agent {0} doesn't have " \
                "the required action.".format(agent))
        # check if the a priori knowledge and relations exist
        for relation in self.__class__.relations:
            if self.agent.has_relation(relation) is False:                 
                raise AttributeError("The agent {0} doesn't have " \
                "the required relation.".format(agent))
        for cog in self.__class__.knowledge:
            if self.agent.has_knowledge(cog) is False:
                raise AttributeError("The agent {0} doesn't have " \
                "the required knowledge.".format(agent))
    
    def require_relations(self, relations):
        cls = self.__class__
        for rel in relations: 
            if rel not in cls.relations: 
                cls.relations.append(rel)
    
    def require_knowledge(self, knowledge):
        cls = self.__class__
        for cog in knowledge: 
            if cog not in cls.knowledge: 
                cls.relations.append(cog)

#=============================================================#

class FakeAgent(object):
    
    def __init__(self):
        self.actions = {'fake_action1':True, 'fake_action2':True}
        
    def has_relation(self, *args): return True
    
    def has_knowledge(self, *args): return True

class ExampleProblem1(ProblemDomain):
    
    def problem(self):
        print('THIS WILL BE THE PROBLEM')

def sample_algo_1(self, test): 
    print("sample algo #1 running: {0}".format(test))
    print("I'm trying to solve:", self.problem, '\n')
    
class SolveProblem1WithAlgo2(object):
    def __init__(self): pass
    def __call__(self, test): 
        print('attempting solution with algo #2 to problem 1: {0}\n'.format(test))

agent = FakeAgent()
relations = ['relations ag should have']
knowledge = ['knowledge ag should have']
actions = ['fake_action1', 'fake_action2']
test = 'THIS IS A SAMPLE OF A TEST'
problem1 = ExampleProblem1(relations, knowledge, actions)
problem1.set_algo(sample_algo_1)
problem1(agent, test)
problem1.set_algo(SolveProblem1WithAlgo2())
problem1(agent, test)
