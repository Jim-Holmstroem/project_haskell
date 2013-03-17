#!/usr/bin/python

import json
from collections import Iterable
from functools import partial

def render_tensor(raw):
    def printer(raw, tabs=0):
        print('    '*tabs+str(raw))
    raw=raw.replace(',]',']')
    t=json.loads(raw)#the tensor actually is valid json 
    def render(t, tabs=0):
        if(isinstance(t, Iterable)):
            printer('[', tabs)
            list(map(
                partial(render, tabs=tabs+1),
                t
            ))
            printer(']', tabs)
        else:
            printer(t, tabs)  
    
    render(t)

if __name__ == '__main__':
    import sys
    render_tensor(raw_input())
