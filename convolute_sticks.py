#!/usr/bin/env python3
"""
DESCRIPTION
-----------
Perform a Gaussian convolution on the input spectrum. The Gaussian
is normalized. 

Instructions:
-Get info from a transition: right-mouse-click on a stick
-Set the next/previous indexed transiton: +/- keys
-Place a label: left-mouse-click on a stick
-Move a label: hold the label with the left-mouse-button
-Remove one label: on the label, right-mouse-click
-Clean info about transitons: left-mouse-click on the title
-Clean all labels: right-mouse-click on the title
-Export to xmgrace: central-mouse-click on the title

HISTORY
-------
v0              : First stable version: fcc_analyzer

NOTES AND BUGS
---------------
* Needs python3 
   
"""
import numpy as np
from scipy import interpolate

# Class PointBrowser will take care of 
        
class PointBrowser:
    """
    DESCRIPTION
    ------------
    Functions to highlight a transition and identify it with the init->final modes
    
    NOTES
    -----
    Based on matplotlib example: data_browser.py
    http://matplotlib.org/examples/event_handling/data_browser.html
    
    *Comments on the original class:
    Click on a point to select and highlight it -- the data that
    generated the point will be shown in the lower axes.  Use the 'n'
    and 'p' keys to browse through the next and previous points
    
    *Updates in this addaptation
    This class handles vlines (not plot) to represent the stick spectrum
    and we use +/- instead of n/p
    """
    def __init__(self):
        self.lastind = 0

        #tranform: set text coordinates to viewport(grace analog) instead of world, which is the default
        self.text = ax.text(0.8, 0.95, '',
                            transform=ax.transAxes, va='top')
        
        self.selected  = ax.vlines([xs[0]], [zero[0]], [ys[0]], linewidths=3,
                                  color='yellow', visible=False)

    def onpress(self, event):
        if self.lastind is None: return
        if event.key not in ('+', '-'): return
        if event.key=='+': inc = 1
        else:  inc = -1

        self.lastind += inc
        self.lastind = np.clip(self.lastind, 0, len(ener)-1)
        self.update()

    def onpick(self, event):
        if event.mouseevent.button != 3: return True
        if event.artist!=line: return True
        
        N = len(event.ind)
        if not N: return True
        
        # the click locations
        x = event.mouseevent.xdata
        y = event.mouseevent.ydata
        
        
        distances = np.hypot(x-xs[event.ind], y-ys[event.ind])
        indmin = distances.argmin()
        dataind = event.ind[indmin]
        
        self.lastind = dataind
        self.update()

    def update(self):
        if self.lastind is None: return

        dataind = self.lastind

        self.selected.set_visible(False)
        
        self.selected  = ax.vlines([xs[dataind]], [zero[dataind]], [ys[dataind]], linewidths=3,
                                  color='yellow', visible=True, alpha=0.7)

        self.text.set_text('Transition=%s\n%s'%(z[dataind],description[dataind]))
        
        
        fig.canvas.draw()
        
    def reset(self,event):
        # Clean the view when clicking on the title
        if event.artist!=title: return True
        self.text.set_text('')
        self.selected.set_visible(False)
        fig.canvas.draw()
        
class LabelSet:
    """
    DESCRIPTION
    ------------
    Functions to create and manage labels over the sticks
    
    NOTES
    -----
    Based on matplotlib example: looking_glass.py
    http://matplotlib.org/examples/event_handling/looking_glass.html
    """
    def __init__(self):
        self.pressevent = None
        self.x0 = xs[0]
        self.y0 = ys[0]
        self.lab = ax.annotate('',xy=(0.,0.))
        return
    
    def onpick(self, event):
        if event.mouseevent.button != 1: return True
        if event.artist!=line: return True
        
        N = len(event.ind)
        if not N: return True
        
        # the click locations
        x = event.mouseevent.xdata
        y = event.mouseevent.ydata
        
        
        distances = np.hypot(x-xs[event.ind], y-ys[event.ind])
        indmin = distances.argmin()
        dataind = event.ind[indmin]
        
        self.lastind = dataind
        #self.update()
        self.put_label()
   
    def put_label(self):
        if self.lastind is None: return

        dataind = self.lastind

        #Now set data and label positions
        xd = xs[dataind]
        yd = ys[dataind]
        xl = xs[dataind] - 0.00
        #Since the intensity may change several orders of magnitude, 
        # we better don't shift it
        yl = ys[dataind] + 0.0
        
        #Define label as final modes description
        label    = str(dataind+1)
        agrlabel = label

        #In labelref we get the annotation class corresponding to the 
        #current label. labelref is Class(annotation)
        labelref = ax.annotate(label, xy=(xd, yd), xytext=(xl, yl),picker=1,
                            arrowprops=dict(arrowstyle="-",
                                            color='grey'))

        #Check whether the label was already assigned or not
        set_lab = True
        for labref in labs:
            lab = labs[labref]
            if lab == agrlabel:
                print("This label was already defined")
                set_lab = False
        if set_lab:
            # The dictionary labs relates each labelref(annotation) to 
            # the agrlabel. Note that the mpl label can be retrieved
            # from labelref.get_name()
            labs[labelref] = agrlabel
            fig.canvas.draw()
        else:
            labelref.remove()
    
    def getlabel(self,event):
        picked_lab = False
        for labref in labs:
            if event.artist==labref: picked_lab = True
        if not picked_lab: return True
    
        self.lab = event.artist
        self.x0, self.y0 = self.lab.get_position()
    
    def onpress(self,event):
        if event.button != 1: return
        if event.inaxes!=ax:
         return
        if not self.lab.contains(event)[0]:
            return
        self.pressevent = event
    
    def onrelease(self,event):
        if event.button != 1: return
        if event.inaxes!=ax:
         return
        self.pressevent = None

    def onmove(self,event):
        if event.button != 1: return
        if self.pressevent is  None or event.inaxes!=self.pressevent.inaxes: return
    
        dx = event.xdata - self.pressevent.xdata
        dy = event.ydata - self.pressevent.ydata
        self.lab.set_position((self.x0+dx,self.y0+dy))
        fig.canvas.draw() 
        
    def delete(self,event):
        if event.button != 3: return
        if not self.lab.contains(event)[0]:
            return
        #Remove the label
        self.lab.remove()
        #And substract the corresponding entry from the dict
        labs.pop(self.lab)
        #We deactivate the lab. Otherwise, if we reclikc on the same point, it raises an error
        self.lab.set_visible(False)
        fig.canvas.draw()
        
    def reset(self,event):
        # Clean the view when clicking on the title
        if event.artist!=title: return True
        if event.mouseevent.button!=3: return True
        #We need a local copy of labs to iterate while popping
        labs_local = dict.copy(labs)
        for lab in labs_local:
            lab.remove()
            labs.pop(lab)
        fig.canvas.draw()
    
    
def export_xmgrace(event):
    """
    DESCRIPTION
    ------------
    Function to convert the current data into a xmgrace plot, including
    labels currently on the screen
    
    Public variables taken from __main__
    * labs,  dict     : labels in xmgr format (arg: labels as annotation Class)
    * ind,   dict     : array indices (arg: stick spectra as 
                        matplotlib.collections.LineCollection Class)
    * ax,    mpl.axes : graphic info
    * ener,  np.array : vector with the energy of all transitions
    * intens,np.array : vector with the intensity of all transitions
    * spc,   np.array : convoluted spectrum. x is spc[:,0], y is spc[:,1]
    """

    if event.artist!=title: return True
    if event.mouseevent.button!=2: return True

    xmgrfile = stickfile.split(".")[0]+".agr"
    print("\nExporting plot to xmgrace ("+xmgrfile+")...")

    f = open(xmgrfile,'w')
    
    print("# XMGRACE CREATED BY FCC_ANALYZER",file=f)
    print("# Only data and labels. Format will",file=f)
    print("# be added by your default xmgrace",file=f)
    print("# defaults (including colors, fonts...)",file=f)
    # Without the @version, it makes auto-zoom (instead of taking world coords) 
    print("@version 50123",file=f)
    print("@page size 792, 612",file=f)
    print("@default symbol size 0.010000",file=f)
    print("@default char size 0.800000",file=f)
    for lab in labs:
        print("@with line",file=f)
        print("@    line on",file=f)
        print("@    line loctype world",file=f)
        print("@    line ",lab.xy[0],",",lab.xy[1],",",lab.xyann[0],",",lab.xyann[1],file=f)
        print("@line def",file=f)
        print("@with string",file=f)
        print("@    string on",file=f)
        print("@    string loctype world",file=f)
        print("@    string ", lab.xyann[0],",",lab.xyann[1],file=f)
        print("@    string def \"",labs[lab],"\"",file=f)
    print("@with g0",file=f)
    # Set a large view
    print("@    view 0.150000, 0.150000, 1.2, 0.92",file=f)
    #Get plotting range from mplt
    x=ax.get_xbound()
    y=ax.get_ybound()
    print("@    world ",x[0],",",y[0],",",x[1],",",y[1],file=f)
    #Get xlabel from mplt
    print("@    xaxis  label \""+ax.get_xlabel()+"\"",file=f)
    #Get tick spacing from mplt
    x=ax.get_xticks()
    y=ax.get_yticks()
    print("@    xaxis  tick major", x[1]-x[0],file=f)
    print("@    yaxis  tick major", y[1]-y[0],file=f)
    #Legend
    print("@    legend loctype view",file=f)
    print("@    legend 0.95, 0.9",file=f)
    #Now include data
    counter=-1
    if (xc.size != 0):
        counter+=1
        print("# Spect",file=f)
        print("@    s"+str(counter),"line type 1",file=f)
        print("@    s"+str(counter),"line linestyle 3",file=f)
 #       print("@    s"+str(counter),"legend  \"Spec\"",file=f)
        for i in range(0,xc.size):
            print(xc[i], yc[i],file=f)
    if True:
        counter+=1
        print("& sticks",file=f)
        print("@type bar",file=f)
        print("@    s"+str(counter),"line type 0",file=f)
#        print("@    s"+str(counter),"legend  \"Stics\"",file=f)
        for i in range(len(xs)):
            print(xs[i], ys[i],file=f)
            
    f.close()
    print("Done\n")


def convolute(spc_stick,npoints=1000,hwhm=0.1,broad="Gau",input_bins=False):
    """
    Make a Gaussian convolution of the stick spectrum
    The spectrum must be in energy(eV) vs Intens (LS?)
    
    Arguments:
    spc_stick  list of list  stick spectrum as [x,y]
               list of array
    npoints    int           number of points
    hwhm       float         half width at half maximum
    
    Retunrs a list of arrays [xconv,yconv]
    """
    x = spc_stick[0]
    y = spc_stick[1]
    # Order data
    idx =  x.argsort()
    x = x[idx]
    y = y[idx]
   
    # ------------------------------------------------------------------------
    # Convert discrete sticks into a continuous function with an histogram
    # ------------------------------------------------------------------------
    # (generally valid, but the exact x values might not be recovered)
    # Make the histogram for an additional 20% (if the baseline is not recovered, enlarge this)
    extra_factor = 0.2
    recovered_baseline=False
    sigma = hwhm / np.sqrt(2.*np.log(2.))
    while not recovered_baseline:
        if input_bins:
            npoints = len(x)
            xhisto = x
            yhisto = y
            width = (x[1] - x[0])
        else:
            extra_x = (x[-1] - x[0])*extra_factor
            yhisto, bins =np.histogram(x,range=[x[0]-extra_x,x[-1]+extra_x],bins=npoints,weights=y)
            # Use bin centers as x points
            width = (bins[1] - bins[0])
            xhisto = bins[0:-1] + width/2
        # ----------------------------------------
        # Build Gaussian (centered around zero)
        # ----------------------------------------
        dxgau = width
        # The same range as xhisto should be used
        # this is bad. We can get the same using 
        # a narrower range and playing with sigma.. (TODO)
        if npoints%2 == 1:
            # Zero is included in range
            xgau_min = -dxgau*(npoints/2)
            xgau_max = +dxgau*(npoints/2)
        else:
            # Zero is not included
            xgau_min = -dxgau/2. - dxgau*((npoints/2)-1)
            xgau_max = +dxgau/2. + dxgau*((npoints/2)-1)
        xgau = np.linspace(xgau_min,xgau_max,npoints)
        if broad=="Gau":
            ygau = np.exp(-xgau**2/2./sigma**2)/sigma/np.sqrt(2.*np.pi)
        elif broad=="Lor":
            ygau = hwhm/(xgau**2+hwhm**2)/np.pi
        else:
            try:
                # Read from file
                data = np.loadtxt(broad,comments='#')
                xfile = data[:,0]
                yfile = data[:,1]
                del(data)
                # Interpolate to fit xhisto range
                func = interpolate.interp1d(xfile,yfile)
                xgau_min = xfile[0]
                xgau_max = xgau_min+dxgau*(npoints-1)
                xgau = np.linspace(xgau_min,xgau_max,npoints)
                ygau = func(xgau)
            except:
                sys.exit("ERROR: Unknown broadening function: "+broad)
        
        # ------------
        # Convolute
        # ------------
        # with mode="same", we get the original xhisto range.
        # Since the first moment of the Gaussian is zero, 
        # xconv is exactly xhisto (no shifts)
        yconv = np.convolve(yhisto,ygau,mode="same")
        xconv = xhisto

        # Check baseline recovery (only with automatic bins
        if yconv[0] < yconv.max()/1000.0 and yconv[-1] < yconv.max()/1000.0:
            recovered_baseline=True
        if input_bins:
            recovered_baseline=True

        extra_factor = extra_factor + 0.05

    return [xconv,yconv]


if __name__ == '__main__':
    import matplotlib.pyplot as plt
    import sys

    # Defaults
    hwhm    = 0.333 # eV (gview default)
    stickfile = False
    do_print = False
    outfile="convoluted.dat"
    factor=1.0
    broadening="Gau"
    input_bins=False
    # Read command line input
    i=1 # 0 is the script name
    while i < len(sys.argv):
        if      sys.argv[i] == "-f":
            i += 1
            try:
                stickfile = sys.argv[i]
            except:
                print("Error reading command line option after -f")
                sys.exit()
        elif    sys.argv[i] == "-o":
            i += 1
            try:
                outfile = sys.argv[i]
            except:
                print("Error reading command line option after -o")
                sys.exit()
        elif sys.argv[i] == "-hwhm":
            i += 1
            try:
                hwhm = float(sys.argv[i])
            except:
                print("Error reading command line option after -hwhm")
                sys.exit()
        elif sys.argv[i] == "-factor":
            i += 1
            try:
                factor = float(sys.argv[i])
            except:
                print("Error reading command line option after -factor")
                sys.exit()
        elif    sys.argv[i] == "-broad":
            i += 1
            try:
                broadening = sys.argv[i]
            except:
                print("Error reading command line option after -o")
                sys.exit()
        elif sys.argv[i] == "-print":
            do_print = True
        elif sys.argv[i] == "-inputBins":
            input_bins = True
        elif sys.argv[i] == "-h":
            print("Key-Vaue Options        Current value   ")
            print(" -f          filename       ",(stickfile))
            print(" -broad      broad[Gau|Lor] ",(broadening))
            print(" -hwhm       width(eV)      ",(hwhm))
            print(" -factor     mult. factor   ",(factor))
            print(" -inputBins  use input bins ",(input_bins))
            print(" -o      outfile        ",(outfile))
            print("Bool Options            ")
            print(" -print                 ",(do_print))
            print(" -h      (help)         ")
            print("Default  (no-flag)      ")
            print(" filename               ")
            print("")
            print("Useful -factor values   ")
            print("Abs spectrum: 703.300   ")
            print("ECD spectrum: 20.5288   ")
            sys.exit()
        else: #default is the filename 
            stickfile = sys.argv[i]
        i += 1

    # Some post-processing
    if not stickfile:
        sys.exit("Provide filename as input. Get help with -h")
    sgm  = hwhm/np.sqrt(2.*np.log(2))

    # Open and read file
    f = open(stickfile,'r')
    i = 0
    x = []
    y = []
    z = []
    ener = []
    lamb = []
    ints = []
    state_trans=[]
    for line in f:
        data = line.split()
        try:
            ener.append(float(data[0]))
            ints.append(float(data[1]))
        except:
            continue

    f.close()
    
    #Inialize variables
    xs = np.array(ener)
    ys = np.array(ints)
    zero = np.zeros(len(xs))

    #Convolution (in energy(eV))
    xc,yc = convolute([xs,ys],hwhm=hwhm,broad=broadening,input_bins=input_bins)
    yc *= factor
    
    f = open(outfile,'w')
    for i in range(len(xc)):
        print(xc[i], yc[i],file=f)
    f.close()

    if not do_print:
        sys.exit()

    #Plot specifications (labels and so)
    fig, ax = plt.subplots()
    title = ax.set_title('UV/Vis spectrum from TD-DFT',fontsize=18,picker=1)
    ax.set_xlabel('Wavelength (nm)',fontsize=16)
    ax.set_ylabel('Osc. Str.',fontsize=16)
    ax.tick_params(direction='out',top=False, right=False)

    # Plot stick spectrum
    line = ax.vlines(xs,zero,ys,linewidths=1,color='k',picker=5)

    # NOT Convert to eV to nm
    #xc = xc/1.23981e-4 # eV->cm-1
    #xc = 1e7/xc        # cm-1 -> nm
    # Plot convoluted
    ax.plot(xc,yc,'--')
    
    #Instance PointBrowser functions
    browser = PointBrowser()
    labset  = LabelSet()
    
    #Make the connection of events with callbacks
    #Highlight sticks
    fig.canvas.mpl_connect('pick_event', browser.onpick)
    fig.canvas.mpl_connect('pick_event', browser.reset)
    fig.canvas.mpl_connect('key_press_event', browser.onpress)
    #Manage labels
    fig.canvas.mpl_connect('pick_event', labset.onpick)
    fig.canvas.mpl_connect('pick_event', labset.getlabel)
    fig.canvas.mpl_connect('button_press_event', labset.onpress)
    fig.canvas.mpl_connect('button_release_event', labset.onrelease)
    fig.canvas.mpl_connect('motion_notify_event', labset.onmove)
    fig.canvas.mpl_connect('button_press_event', labset.delete)
    #Delete all labels
    fig.canvas.mpl_connect('pick_event', labset.reset)
    #Export to xmgrace function
    fig.canvas.mpl_connect('pick_event', export_xmgrace)
    
    plt.show()
    
    #print(intens)

