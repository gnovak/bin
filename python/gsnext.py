import scipy, pyx, pylab

# TODO: 
# Add a bit to pyx to allow things like this:
# pyx.graph.data.list(transpose(array([x,y])),x=1,y=2)
# becomes
# pyx.graph.data.alist([x,y], x=1,y=2)... or something

############################
# Add some stuff to the scipy namespace
#
def lrange(l, h, dex):
    """Log-spaced array"""
    return 10**scipy.arange(scipy.log10(l), scipy.log10(h), dex)
scipy.lrange = lrange

def nrange(l, h, n):
    """Array where you specify low, high, and n instead of dx"""
    dx = (h-l)/(1.0*(n-1))
    return scipy.arange(l, h+0.5*dx, dx)
scipy.nrange = nrange

def irange(l, h, n):
    """Incomplete nrange -- doesn't include the endpoint"""
    dx = (h-l)/(1.0*n)
    return scipy.arange(l, h, dx)
scipy.irange = irange

def lnrange(l, h, n):
    """Array where you specify low, high, and n instead of dx"""
    return 10**nrange(scipy.log10(l), scipy.log10(h),n)
scipy.lnrange = lnrange

def lirange(l, h, n):
    """Array where you specify low, high, and n instead of dx"""
    return 10**irange(scipy.log10(l),scipy.log10(h),n)
scipy.lirange = lirange

def at(a):
    """A better name for nonzero"""
    return scipy.nonzero(a)
scipy.at = at

######################################
# Pylab
######################################

pylab.cmap = pylab.cm

rhot_data = {'red':   ((1-1.0, 1.0, 1.0),
                        (1-0.365079, 1.000000, 1.000000),
                        (1-0., 0.0416, 0.0416)),
              'green': ((1-1.0, 1.0, 1.0),
                        (1-0.746032, 1.000000, 1.000000),
                        (1-0.365079, 0.000000, 0.000000),
                        (1-0., 0., 0.)),
              'blue':  ((1-1.0, 1.0, 1.0),
                        (1-0.746032, 0.000000, 0.000000),
                        (1-0., 0., 0.))}                  

rhot_cmap = pylab.cm.colors.LinearSegmentedColormap('rhot',
                                                    rhot_data,
                                                    pylab.rcParams['image.lut'])

def rhot ():
    """set the default colormap to rhot and apply to current
    image if any.  See help(colormaps) for more information"""
    rc('image', cmap='rhot')
    im = gci()
    if im is not None:
        im.set_cmap(rhot)
    draw_if_interactive()

pylab.cm.datad['rhot']= rhot_data
pylab.cm.rhot = rhot_cmap
pylab.rhot = rhot

redblue_data = {'red':   ((0., 1., 1.),
                      (.5, 0., 0.),
                      (1., 0., 0.)),
            'green': ((0., 0., 0.),
                      (1., 0., 0.)),
            'blue':  ((0., 0., 0.),
                      (.5, 0., 0.),
                      (1., 1., 1.))}

redblue_cmap = pylab.cm.colors.LinearSegmentedColormap('redblue',
                                             redblue_data,
                                             pylab.rcParams['image.lut'])

def redblue():
    """set the default colormap to redblue and apply to current image
    if any.  See help(colormaps) for more information"""
    rc('image', cmap='redblue')
    im = gci()
    if im is not None:
        im.set_cmap(redblue)
    draw_if_interactive()

pylab.cm.datad['redblue']= redblue_data
pylab.cm.redblue = redblue_cmap
pylab.redblue = redblue

rainbow_pts = [0., .15, .45, .5, .55, .85, 1.]
rainbow_data = {'red':((rainbow_pts[0], 0., 0.),
                       (rainbow_pts[1], 0., 0.),
                       (rainbow_pts[2], 0., 0.),
                       (rainbow_pts[3], 0., 0.),
                       (rainbow_pts[4], 1., 1.),
                       (rainbow_pts[5], 1., 1.),
                       (rainbow_pts[6], 1., 1.)),
                'green':((rainbow_pts[0], 0., 0.),
                        (rainbow_pts[1], 0., 0.),
                        (rainbow_pts[2], 1., 1.),
                        (rainbow_pts[3], 1., 1.),
                        (rainbow_pts[4], 1., 1.),
                        (rainbow_pts[5], 0., 0.),
                        (rainbow_pts[6], 1., 1.)),
                'blue':((rainbow_pts[0], 0., 0.),
                        (rainbow_pts[1], 1., 1.),
                        (rainbow_pts[2], 1., 1.),
                        (rainbow_pts[3], 0., 0.),
                        (rainbow_pts[4], 0., 0.),
                        (rainbow_pts[5], 0., 0.),
                        (rainbow_pts[6], 1., 1.))}

rainbow_cmap = pylab.cm.colors.LinearSegmentedColormap('rainbow',
                                                  rainbow_data,
                                                  pylab.rcParams['image.lut'])

def rainbow():
    """set the default colormap to rainbow and apply to current image
    if any.  See help(colormaps) for more information"""
    rc('image', cmap='rainbow')
    im = gci()
    if im is not None:
        im.set_cmap(rainbow)
    draw_if_interactive()

pylab.cm.datad['rainbow']= rainbow_data
pylab.cm.rainbow = rainbow_cmap
pylab.rainbow = rainbow

#######################################
# Pyx Stuff
#######################################

class none:
    "A texter for suppressing axis labels"
    
    __implements__ = pyx.graph.axis.texter._Itexter
    
    def __init__(self):
        r"""initializes the instance"""
        
    def labels(self, ticks):
        r"""Do nothing"""
        for tick in ticks:
            tick.label = ''        
pyx.graph.axis.texter.none = none

# This seems to be missing when you do from pyx.graph import *
pyx.graph.__all__ += ['graphxy', 'axis']

# Should morph this into something useful
# def plot_panel(pan, mn = None, mx=None, **kw):
#     """Take an array of numbers and make a color plot"""
#     n = len(pan)
# 
#     d = []
#     if mx is None: mx = max(ravel(pan))
#     if mn is None: mn = min(ravel(pan))
#     
#     for i in range(n):
#         for j in range(n):
#             xmin = 2*pi*i/n
#             xmax = xmin + 2*pi/n
#             ymin = 2*pi*j/n
#             ymax = ymin + 2*pi/n
#             d.append([xmin, xmax, ymin, ymax,
#                       log10(pan[i][j]/mn)/log10(mx/mn)])
#     
#     g = graph.graphxy(x=graph.axis.linear(min=0, max=2*pi, divisor=pi, 
#                       texter=graph.axis.texter.none()),
#                       y=graph.axis.linear(min=0, max=2*pi, divisor=pi,
#                       texter=graph.axis.texter.none()),
#                       **kw)
# 
#     g.plot(graph.data.list(d, xmin=1, xmax=2, ymin=3, ymax=4, color=5),
#            graph.style.rect(color.palette.ReverseRainbow))
#     g.dodata()
#    # plot data first, then axes
#    return g
