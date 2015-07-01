import unittest
import os

from core.planner_manager import *

#====================#
#    UNIT TESTING    #
#====================#

class test_solve_problem(unittest.TestCase):
    
    def test_move_box_to_table(self):        
        class MoveBoxTable(ProblemDomain):
            actions = ['move_box','drop_box']
            goal = [':vars:x,y: (<on[x=1,y]>)']
            init = [':vars:x,y: (box[x,u=1] && red[x,u=1] && table[y,u=1])']
        
        ag = FakeAg(MoveBoxTable.actions)
        problem = MoveBoxTable()
        
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
        if sent in self.knowledge: return True
        elif sent in self.known_false: return False
        else: return None
    
    def action(self, act): 
        if act in self.action: return True


if __name__ == "__main__":
    unittest.main()

