import unittest
import os

from simag.core.planner_manager import *

#====================#
#    UNIT TESTING    #
#====================#

@unittest.skip
class test_solve_problem(unittest.TestCase):
    
    def test_move_box_to_table(self):
        actions = ['move_box','drop_box','pick_box', 'paint']
        file = os.path.join(os.path.dirname(__file__),
            'planner_manager','move_box_to_table.json')
        ag = FakeAg(actions)
        MoveBoxToTable = makeProblemDomain(file)
        init_problem = MoveBoxToTable()
        init_problem.set_algo(SolveProblemWithAlgo3)
        init_problem(ag, vrs={'B1':'blue_box','T1':'table'}, test='TEST')

#==============================#
#    HELP FUNCTIONS & CLASSES  #
#==============================#

class FakeAg(object):
    """Fake agent class to help unit testing this."""
    
    def __init__(self, actions):
        self.actions = actions
        
    def has_relation(self, *args):
        for rel in args:
            if rel not in self.relations: return False
        return True
    
    def has_knowledge(self, *args):
        for cog in args:
            if rel not in self.knowledge: return False
        return True
    
    def ask(self, sent, single=False):
        """if sent in self.knowledge: return True
        elif sent in self.known_false: return False
        else: return None"""
        return True
    
    def action(self, act): 
        if act in self.action: return True


if __name__ == "__main__":
    unittest.main()

