# -*- coding: utf-8 -*-

import os
from pathlib import PurePath

from simag.core._helpers import *
from simag.core.agents import *
from simag.core.env import Env

path = PurePath(os.path.abspath(__file__))
path = str(path.parents[3])

env = Env()
eval_funcs = PerceptionFuncs()
act_funcs = ActionFuncs()
cfg_test = os.path.join(path, 'tests', 'planner_manager', 'ag_test_01.xml')

def perc_std():
    return 'Placeholder percept: func => std mode'


if __name__ == '__main__':
    agents, configs = import_configs(cfg_test)
    ag = BasicAgent(load=agents['Nacho'])
    env.register(ag)
    eval_funcs.add(perc_std)
    ag.perceive(eval_funcs)

