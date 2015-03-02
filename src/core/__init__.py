from core.env import Env
from core.agents import BasicAgent, datetime
from core.actions import *
from core.tools import *

env = Env()
eval_funcs = PerceptionFuncs()
cfg_file = '/home/nacho/workspace/simag/data/sample.xml'


def perc_std():
    return 'Placeholder percept: func => std mode'

if __name__ == '__main__':
    d1 = datetime.datetime.now()

    agents, configs = import_configs(cfg_file)
    ag = BasicAgent(load=agents['Nacho'])
    env.register(ag)
    eval_funcs.add(perc_std)
    ag.perceive(eval_funcs)
    print(ag.percepts.items())

    d2 = datetime.datetime.now()
    print(d2-d1)
