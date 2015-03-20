# -*- coding: utf-8 -*-

"""Main agent module, implements the different agent objects and all the
supporting methods and classes.

Actions
-------
1) The virtual actions committed by agents towards the environment
(which includes other agents).
2) Elements for the previous decission making about what actions to commit.
"""
# ===================================================================#
#   Imports and globals
# ===================================================================#
import datetime
import uuid

from core.kblogic import Representation

prop_list = ['name', 'born', 'ag_state', 'pos']
pmodes = []
actmodes = []

# ===================================================================#
#   AGENT OBJECTS CLASSES AND SUBCLASSES
# ===================================================================#


class BasicAgent(object):
    """Implements the basic Agent object."""
    def __init__(self, config=None, load=None, **kwargs):
        self.props = {'oID': str(uuid.uuid4()),
                      'type': 'b_ag',
                      'born': datetime.datetime.utcnow(),
                      'ag_state': 'idle',
                      'percept_modes': ['std'],
                      'eval_modes': ['std']}
        self.percepts = Representation()
        self.assets = BalanceSheet()
        self.liabilities = BalanceSheet()
        if load is not None:
            self.props = load['properties']
            self.assets.add(**load['assets'])
            self.liabilities.add(**load['liabilities'])
            self.props['position'] = load['position']
            if 'oID' not in load.keys():
                self.props['oID'] = str(uuid.uuid4())
        for prop, val in kwargs.items():
            self.props[prop] = val
        self.register()

    def register(self):
        if 'position' not in self.props.keys():
            raise KeyError("No position provided for the agent.")
        position = self.props['position']
        oID = self.props['oID']
        type_ = self.props['type']
        return position, oID, self, type_

    def state(self):
        return self.props['ag_state']

    def update(self, prop=None, val=None, **kwargs):
        self.props[prop] = val
        for prop, val in kwargs.items():
            self.props[prop] = val

    def perceive(self, eval_funcs, optimal=None):
        """Evaluates the environment and creates an internal representation
        based on this 'perception'. Which is then registered for that moment.
        """
        percept = self.perception_routine(eval_funcs, optimal)
        print percept

    def action_routine(self, action_funcs):
        """Evaluates current set of beliefs and intentions and produces
        a plan to achieve the desired state if possible.
        """
        eval_mode = self.props['eval_mode']
        # Current set of beliefs about the env
        beliefs = self.representations
        # Current set of intentions the agent has
        not_comp_acts = []
        incomp = False
        # Pick an action which is compatible with the current set of
        # beliefs and intentions.
        while incomp is True:
            act = self.check_intetions(not_comp_acts)
            incomp = self.is_incompatible(beliefs, act)
            if incomp is True:
                not_comp_acts.append(act)
        pertinent = True
        while pertinent is True:
            # Comit action
            self.commit(act)
            # Re-evaluate the situation to see if the actions still
            # are pertinent to the current state of the simulation.
            pertinent = self.eval_action(eval_mode, act)
            # wait X time before re-evaluation (async)

    def perception_routine(self, eval_funcs, optimal=None):
        percept_modes = self.props['percept_modes']
        if optimal is None:
            # mode = check_optimal_perc_mode(self, percept_modes)
            func = eval_funcs.collection['perc_std']
            return func()
        if optimal in pmodes and optimal in percept_modes:
            # func = eval_funcs.collection[optimal]
            return 'Placeholder percept: func => optimal mode'
        else:
            if optimal not in percept_modes:
                self.percept_routine(percept_modes, eval_funcs)
            else:
                raise ValueError('Perception method not found.')

    def commit(self, act):
        """Commits an action if pertinent."""
        pass


class Institution(object):
    """Institutions are social constructs of different types.
    They are defined by the type of organisation, members and
    resources controlled by them.

    Agents can belong to none or n institutions.
    """
    pass

# ===================================================================#
#   SUPPORTING CLASSES AND SUBCLASSES
# ===================================================================#


class BalanceSheet(dict):
    """This object represents the assets and liabilities of
    agents and institutions.
    """
    def add(self, item=None, quant=None, value=None, **kwargs):
        if item is not None and quant is not None:
            self[item] = (quant, value)
        self.update(kwargs)

    def remove(self, item):
        del self[item]

    def valuation(self, item, value):
        old_values = self[item]
        new_values = self[item] = (old_values[0], value)
        self[item] = new_values


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


class ActionFuncs(object):
    """Registers the different action functions.

    Action functions get an agent and an evaluation mode as input.
    And output a set of instructions which is valid for that agent.
    """
    def __init__(self):
        self.collection = {}

    def add(self, func):
        self.collection[func.__name__] = func
        actmodes.append(func.__name__)

# ===================================================================#
#   Action deliberation and commitment functions
# ===================================================================#


def deliberation():
    """The practical deliberation function of the agents. It includes
    the level of commitment to an end (blind, single-minded, open-minded).

    Input: The perceived current state of the environment and the agent.
    Output: What is the intended (end) state the agent wants to achieve.
    """
    pass


def means():
    """The means-end deliberation function of the agent.

    Given an end what means to use to achieve such state and the level
    of commitment the agent will take when using those means, indepent
    of the level of commitment to the end itself.

    Input: Intended state the agent wants to achieve.
    Output: Actions to commit to achieve the state.
    """
    pass
