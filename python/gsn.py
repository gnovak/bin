import gsnext
import scipy
import matplotlib.matlab
import pyx

def iterable(obj):
    """See if you can iterate over an object"""
    try: len(obj)
    except: return 0
    return 1

def permutations(z):
    """Given a list of lists, generate all permutations
    ie, [[a], [b,c]] generates [[a,b], [a,c]]"""
    if len(z) == 1:
        return map(lambda x: [x], z[0])    
    subperms = permutations(z[:-1])
    out = []
    for e in z[-1]:
        for l in subperms:
            t=list(l)
            t.append(e)
            out.append(t)
    return out

def zipl(*args):
    """Zip a bunch of lists together as lists, not as tuples"""
    return map(list,zip(*args))

def make_histo(y,bins,weights,overflow,normed):
    """Make a possibly weighted histogram
    y = data
    b = bin edges
    w = weights (None if unweighted)
    overflow = whether or not to keep overflow bins
    normed = normalized to be a probability distribution

    returns (hist,bins) where hist is the counts
    and bins is the corresponding left bin edge
    """
    if not iterable(bins):
        ymin, ymax = min(y), max(y)
        if ymin==ymax:
            ymin -= 0.5
            ymax += 0.5
        bins = scipy.linspace(ymin, ymax, bins)
 
    if weights is None:               # unweighted if weights not
        weights=scipy.ones(len(y))       # specified
 
    idx = scipy.argsort(y)            # find indicies that will sort the data
    sy = scipy.take(y,idx)            # sort the data
    sw = scipy.take(weights,idx)            # sort the weights
    n = scipy.searchsorted(sy, bins)
    
    if overflow:
        n = scipy.concatenate([[0], n,[len(y)]])
        bins=scipy.concatenate([[2*bins[0]-bins[1]], bins]) 
    else:
        bins=bins[:-1]
        
    hist = scipy.zeros(len(n)-1)      

    for i in range(len(hist)):  # sum up the weights that fall into
        l=n[i]                  # the range b/t each bin
        h=n[i+1]                # ...
        hist[i] = scipy.sum(sw[l:h])  # ...

    if normed:
        db = bins[1]-bins[0]
        hist /= len(y)*db

    return hist,bins

def make_pyx_histo_data(bins,hist):
    """Take a list of bin edges and counts and make it into something
    that PyX can plot nicely as a histogram"""

    # first bin
    d = [[bins[0],0]]

    # all intermediate bins
    for i in range(len(bins)-1):         
        d.append([bins[i  ],hist[i  ]])
        d.append([bins[i+1],hist[i  ]])

    #last bin
    n=len(bins)
    db=bins[n-1]-bins[n-2]
    d.append([bins[n-1],hist[n-1]])
    d.append([bins[n-1]+db,hist[n-1]])
    d.append([bins[n-1]+db,0])
    return d
    
def histo(x, bins=50, weights=None, normed=0, overflow=True, noplot=False, Pyx=False, width=8, **kw):
    """Make a possibly weighted histogram
    y = data
    b = number of bins or list of bin edges
    w = weights (None if unweighted)
    overflow = whether or not to keep overflow bins
    normed = normalized to be a probability distribution
    noplot = don't draw a plot, just return the bins and counts
    pyx = True => make a PyX graph, otherwise use matplotlib
    width = width of the pyx graph

    returns: if noplot: (bins,hist) where
                  hist is the counts and bins are the
                  corresponding left bin edges
             elif pyx: pyx graph
             else: None
    """

    hist,bins = make_histo(x,bins,weights,overflow,normed)
    if noplot:
        return hist,bins
    
    if Pyx:
        data = make_pyx_histo_data(bins,hist)        
        g = pyx.graph.graphxy(width=width, **kw)
        g.plot(pyx.graph.data.list(data, x=1, y=2),pyx.graph.style.line())
        return g
    else:
        width = 0.9*(bins[1]-bins[0])
        matplotlib.matlab.gca().bar(bins, hist, width=width, **kw)        
        matplotlib.matlab.show()
        return None
