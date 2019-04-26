#!/usr/bin/env python

import sys
import numpy as np
from scipy import interpolate

# Default output
fileout = 'sum.dat'

filesin=[]
get_out=False
do_help=False
for arg in sys.argv[1:]:
    if arg == '-o':
        get_out=True
    elif get_out:
        fileout = arg
        get_out = False
    elif arg == '-h':
        do_help=True
    else:
        filesin.append(arg)

if do_help:
    print """

   Program to sum spectra with arbitrary x-axis ranges
----------------------------------------------------------

Usage:
%s <inputplots> -o sum.dat
"""%(sys.argv[0])

    sys.exit()


xs=[]
ys=[]
xmax=-100.
xmin=9999999.
dx=99999.
for filein in filesin:
    print 'Input plot: '+filein
    try:
        data=np.loadtxt(filein)
    except:
        raise BaseException('File not found: '+filein)
    
    xs.append(data[:,0])
    ys.append(data[:,1])

    xmax=max(xmax,xs[-1].max())
    xmin=min(xmin,xs[-1].min())
    d=(xs[-1][-1]-xs[-1][0])/len(xs[-1])
    dx = min(d,dx)

print 'Range:', xmin,xmax, ' with dx=', dx

# Get new common grid (x)
xnew = np.arange(xmin,xmax,dx)
ynew = np.zeros(len(xnew))

# Now sum all data over such grid after interpolation
for x,y in zip(xs,ys):
    # Interpolate
    f = interpolate.interp1d(x,y,fill_value=0.0,bounds_error=False)
    ynew += f(xnew)
    

# Print sum
with open(fileout,'w') as f:
    for xi,yi in zip(xnew,ynew):
        print >>f, xi, yi

print 'Sum written to '+fileout


