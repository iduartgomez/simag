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
    print(env.idx[0])
    print(env.world[1])
    
    d2 = datetime.datetime.now()
    print(d2-d1)