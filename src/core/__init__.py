import random

from core.env import Env
from core.agents import *
from core.tools import *

env = Env()
cfg_file = '/home/nacho/workspace/simag/data/sample.xml'

if __name__ == '__main__':
    d1 = datetime.datetime.now()
    
    agents, configs = import_configs(cfg_file)
    for x in range(0,1000):
        props = configs['cfg_01'][0]        
        props['name'] = 'ag_' + str(x)
        props['pos'] = [random.random(), random.random(), random.random()]
        ag = BasicAgent(**props)
        env.register(ag)
    agents = []
    #for k, v in env.agents.items():
    #    agents.append(k)
    #ag = env.agents[agents[0]][1]
    #ag.update(**{'ag_state': 'tired'})
    
    d2 = datetime.datetime.now()
    print(d2-d1)
    