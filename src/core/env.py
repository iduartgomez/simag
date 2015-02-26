"""Set up the classes and methods for the basic environment 
where the actors reside and where the 'simulated world' exists.

In it's basic form is an application where the actor objects
and material type objects are registered for interaction.
"""

import numpy as np


class Molecule(object):
    """This is the basic building block for the simulated world."""    
    pass

class Materials():
    """This represent different types of materials in the simulated.
    It's an homogenous compound of different molecules.    
    """    
    pass

class Block():
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
        self.world = np.array([[0.,0.,0.]])
        self.idx = []
        self.ag_container =  {}
        
    def run(self):
        self.__init__()
    
    def load_data(self, file):
        self.world = np.load(file)
    
    def save_data(self, outfile):
        np.save(outfile, self.world)
        
    def register(self, item):
        pos, oID, obj = item.register()
        self.ag_container[oID] = obj
        self.idx.append((oID, len(self.idx) + 1))
        self.world = np.append(self.world,[pos],axis=0)
        
        