import scipy
from scipy import log10,arange

############################
# Add some stuff to the scipy namespace
#

"""Log-spaced array"""
def lrange(l, h, dex):
    return 10**arange(log10(l), log10(h), dex)
scipy.lrange = lrange

"""Array where you specify low, high, and n instead of dx"""
def nrange(l, h, n):
    dx = (h-l)/(1.0*(n-1))
    return arange(l, h+0.5*dx, dx)
scipy.nrange = nrange
