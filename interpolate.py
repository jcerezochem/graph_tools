#!/usr/bin/env python

import sys
import numpy as np
from scipy import interpolate

filein = sys.argv[1]
fileou = sys.argv[2]

try:
    data=np.loadtxt(filein)
except:
    raise BaseException('File not found')

# Interpolation
#  New grid
N=3000
x0=1.5
xf=6.5
#  Interpolate
f = interpolate.interp1d(data[:,0],data[:,1],fill_value=0.0,bounds_error=False)
x = np.arange(x0,xf,(xf-x0)/float(N))
y = f(x)

with open(fileou,'w') as f:
    for xi,yi in zip(x,y):
        print >>f, xi, yi



