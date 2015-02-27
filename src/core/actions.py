"""This module implements:
1) The virtual actions committed by agents towards the environment 
(which includes other agents). 
2) Elements for the previous decission making about what actions to commit.
"""

def action_routines(agent, eval_mode, act):
    if act == None:
        # Check the current state and comit action, if possible
        # according to the current state.
        pass
    else:
        # Check if the passed action is in the repertory
        # and not incompatible with the current state.
        pass

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