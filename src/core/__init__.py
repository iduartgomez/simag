# -*- coding: utf-8 -*-

import os

from core.env import Env
from core.agents import *
from core.tools import *

path = os.path.dirname(os.path.dirname(os.path.dirname(__file__)))

env = Env()
eval_funcs = PerceptionFuncs()
act_funcs = ActionFuncs()
cfg_test = os.path.join(path, 'tests', 'ag_test_01.xml')

def perc_std():
    return 'Placeholder percept: func => std mode'


if __name__ == '__main__':
    d1 = datetime.datetime.now()

    agents, configs = import_configs(cfg_test)
    ag = BasicAgent(load=agents['Nacho'])
    env.register(ag)
    eval_funcs.add(perc_std)
    ag.perceive(eval_funcs)
    
    d2 = datetime.datetime.now()
