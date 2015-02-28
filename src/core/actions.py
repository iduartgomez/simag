"""This module implements:
1) The virtual actions committed by agents towards the environment 
(which includes other agents). 
2) Elements for the previous decission making about what actions to commit.
"""
pmodes = []

def action_routines(agent, eval_mode, act):
    if act == None:
        # Check the current state and comit action, if possible
        # according to the current state.
        pass
    else:
        # Check if the passed action is in the repertory
        # and not incompatible with the current state.
        pass

def percept_routines(agent, percept_modes, eval_funcs, optimal=None):
    for mode in percept_modes:
        if mode in pmodes and optimal == None:
            func = eval_funcs.collection[mode]
            return func()
        elif optimal in pmodes and optimal in percept_modes:
            return 'Placeholder percept: func => optimal mode'
        else:
            if optimal not in percept_modes:
                percept_routines(agent, percept_modes, eval_funcs)
            else:
                raise ValueError('Perception method not found.')
        
def deliberation():
    """Represents the practical deliberation of the agents.
    Input => The perceived current state of the environment and the agent.
             (Percepts and states.)
    Output => What is the intended state the agent wants to achieve.
    """
    pass

def means_end():
    """Represents the means-end deliberation of the agent.
    Input => Intended state the agent wants to achieve.
    Output => Actions to commit to achieve the state.
    """
    pass

class PerceptionFuncs(object):
    """Registers the different evaluation functions.
    
    Evaluation functions get an agent and a perception mode as input.
    And output a 'perception' data structure which is valid for that agent.
    """
    def __init__(self):
        self.collection = {}
    
    def add(self, func):
        self.collection[func.__name__] = func
        pmodes.append(func.__name__)
        
        