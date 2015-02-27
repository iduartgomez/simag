import random

from core.agents import *
from core.env import Env


env = Env()

if __name__ == '__main__':
    d1 = datetime.datetime.now()
    for x in range(0,1000):
        
        ag = BasicAgent(**{'name':'ag_'+str(x), 'pos':[
                                                       random.random(), 
                                                       random.random(), 
                                                       random.random()
                                                       ]})
        env.register(ag)
    agents = []
    for k, v in env.agents.items():
        agents.append(k)
    ag = env.agents[agents[0]][1]
    ag.assets.add('peperoni',500, 5)
    ag.assets.add(**{'cheese':(100, 20)})    
    print ag.assets
    
    d2 = datetime.datetime.now()
    print(d2-d1)
    