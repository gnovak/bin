import scipy, pyx

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
