# -*- coding: utf-8 -*-

"""Set up the classes and methods for the basic environment
where the actors reside and where the 'simulated world' exists.

In it's basic form is an application where the actor objects
and material type objects are registered for interaction.
"""
import os
import pickle

import numpy as np

ag_types = ['b_ag']


class Molecule(object):
    """This is the basic building block for the simulated world."""
    pass


class Materials(object):
    """This represent different types of materials in the simulated.
    It's an homogenous compound of different molecules.
    """
    pass


class Block(object):
    """This represents material objects in the simulated world.
    And it's built of n materials and/or blocks of the same or different types.

    It's the basic building block of the enviroment material infrastructure.
    """
    pass


class Env(object):
    """Initialises the environment which represents the 'simulated world'.

    The different blocks that belong to this world are registered and
    tracked by this Env object. New blocks are registered and consumed
    blocks are unregistered.

    Agents are also tracked in this enviroment.
    """
    def __init__(self):
        self.world = np.array([[0., 0., 0.]])
        self.agents = {}
        self.blocks = {}
        self.total_obj = 0

    def run(self):
        self.__init__()

    def load_data(self, path):
        self.world = np.load(os.path.join(path, 'world.npy'))
        data = pickle.load(os.path.join(path, 'cache01.dat'))
        self.total_obj = data[0]
        self.agents = data[1]
        self.blocks = data[2]

    def save_data(self, path):
        np.save(os.path.join(path, 'world.npy'), self.world)
        data = [self.total_obj, self.agents, self.blocks]
        pickle.dump(data, os.path.join(path, 'cache01.dat'))

    def register(self, item):
        self.total_obj += 1
        pos, oID, obj, type_ = item.register()
        if type_ in ag_types:
            self.agents[oID] = (self.total_obj, obj)
        self.world = np.append(self.world, [pos], axis=0)
