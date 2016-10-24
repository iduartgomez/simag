import unittest
import os

from simag.core.planner_manager import *

#====================#
#    UNIT TESTING    #
#====================#

class SolveProblem(unittest.TestCase):

    def test_loading_problem_and_solver(self):
        actions = ['move_box','drop_box','pick_box', 'paint']
        file = os.path.join(os.path.dirname(__file__),
            'planner_manager','move_box_to_table.json')
        ag = FakeAg(actions)
        MoveBoxToTable = make_problem_domain(file)
        init_problem = MoveBoxToTable()
        params = {'B1':'blue_box','T1':'table'}

        with self.assertRaisesRegex(AssertionError, 'Algo1'):
            init_problem.set_algo(
                SolveProblemWithAlgo1,
                subplans=[SolveProblemWithAlgo2],
                SolveProblemWithAlgo2=[SolveProblemWithAlgo3])
            init_problem(ag, vrs=params, solution='Algo1')

        with self.assertRaisesRegex(AssertionError, 'Algo2'):
            init_problem.set_algo(
                SolveProblemWithAlgo2,
                subplans=[SolveProblemWithAlgo3])
            init_problem(ag, vrs=params, solution='Algo2')

        with self.assertRaisesRegex(AssertionError, 'Algo3'):
            init_problem.set_algo(SolveProblemWithAlgo3)
            init_problem(ag, vrs=params, solution='Algo3')
                
    @unittest.skip
    def test_move_box_to_table(self):
        pass

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
            if cog not in self.knowledge: return False
        return True

    def ask(self, sent, single=False):
        """if sent in self.knowledge: return True
        elif sent in self.known_false: return False
        else: return None"""
        return True

    def action(self, act):
        if act in self.action: return True

class SolveProblemWithAlgo1(SolutionTemplate):
    def solve(self, **kw):
        m = "attempting solution with algo {0} to problem {1}:" \
            .format(self, self.master_problem, kw['solution'])
        self.call_plan(
            SolveProblemWithAlgo2, **kw)

class SolveProblemWithAlgo2(SolutionTemplate):
    def solve(self, **kw):
        m = "attempting solution with algo {0} to problem {1}" \
            .format(self, self.master_problem)
        self.call_plan(SolveProblemWithAlgo3, **kw)

class SolveProblemWithAlgo3(SolutionTemplate):
    def solve(self, **kw):
        m = "attempting solution with algo {0} to problem {1}\n" \
        "{2}\n".format(self, self.master_problem, kw['solution'])
        raise AssertionError(kw['solution'])

if __name__ == "__main__":
    unittest.main()
