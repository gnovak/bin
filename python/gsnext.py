import scipy
import scipy.gplt
import matplotlib.matlab
from scipy import *

############################
# Add some stuff to the scipy namespace
#
def lrange(l, h, dex):
    """Log-spaced array"""
    return 10**arange(log10(l), log10(h), dex)
scipy.lrange = lrange

def nrange(l, h, n):
    """Array where you specify low, high, and n instead of dx"""
    dx = (h-l)/(1.0*(n-1))
    return arange(l, h+0.5*dx, dx)
scipy.nrange = nrange

#def histo(a, bins=None):
#    if bins is None:
#        bins = nrange(min(a), max(a), 50.)
#
#    n = searchsorted(sort(a), bins)
#    n = concatenate([n, [len(a)]])
#    return (bins, n[1:]-n[:-1])
#scipy.gplt.histo = histo

