# -*- coding: utf-8 -*-

import os

from core.env import Env
from core.agents import *
from core.tools import *

path = os.path.dirname(os.path.dirname(os.path.dirname(__file__)))

env = Env()
eval_funcs = PerceptionFuncs()
act_funcs = ActionFuncs()
cfg_test = os.path.join(path, 'data', 'ag_test_01.xml')
logic_test = os.path.join(path, 'data', 'logic_test_01.txt')


def perc_std():
    return 'Placeholder percept: func => std mode'


if __name__ == '__main__':
    ls = []
    with open(logic_test, 'r') as f:
        for line in f:
            if line[0] == '#':
                pass
            else:
                ls.append(line)                
    r = Representation()

    d1 = datetime.datetime.now()

    """
    agents, configs = import_configs(cfg_test)
    ag = BasicAgent(load=agents['Nacho'])
    env.register(ag)
    eval_funcs.add(perc_std)
    ag.perceive(eval_funcs)
    print(ag.percepts)
    """
    for form in ls:
        r.encode(form)

    d2 = datetime.datetime.now()
    print(d2-d1)
    print r.singles
    print r.classes
    