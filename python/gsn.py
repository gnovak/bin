import gsnext, scipy, matplotlib.matlab, pyx

############################
# Random stuff
############################

def flatten(L):
    if type(L) != type([]): return [L]
    if L == []: return L
    return flatten(L[0]) + flatten(L[1:])

def iterable(obj):
    """See if you can iterate over an object"""
    try: len(obj)
    except: return False
    return True

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

def wrap(x, low=-scipy.pi, high=scipy.pi):
    """Wrap x into range low to high"""
    range = high-low
    m = scipy.floor((low-x)/range)+1
    return x + m*range

#############################
# Numerical Stuff
#############################

def euler_matrix(phi, the, psi):
    """Make an Euler transformation matrix"""
    cpsi=scipy.cos(psi)
    spsi=scipy.sin(psi)
    cphi=scipy.cos(phi)
    sphi=scipy.sin(phi)
    cthe=scipy.cos(the)
    sthe=scipy.sin(the)
    
    m = scipy.reshape(scipy.arange(9.),(3,3))
    m[0,0] = cpsi*cphi - cthe*sphi*spsi
    m[0,1] = cpsi*sphi + cthe*cphi*spsi
    m[0,2] = spsi*sthe
    m[1,0] = -spsi*cphi - cthe*sphi*cpsi
    m[1,1] = -spsi*sphi + cthe*cphi*cpsi 
    m[1,2] = cpsi*sthe
    m[2,0] = sthe*sphi
    m[2,1] = -sthe*cphi
    m[2,2] = cthe

    return scipy.mat(m)

def euler_pass(v,  phi,  the,  psi):
    """Passive Euler transform"""
    m = euler_matrix(phi, the, psi)
    return (m*v).asarray()[0]

def euler_pass_inv(v,  phi,  the,  psi):
    """Inverse of passive Euler transform"""
    m = scipy.mat(scipy.transpose(euler_matrix(phi, the, psi)))
    return (m*v).asarray()[0]

# Goldstein constructs euler x-form as passive 
# I want it to be active.  Turns out that this means
# I want his inverse.  
def euler_act(v,  phi,  the,  psi):
    """Active Euler transform"""
    m = scipy.mat(scipy.transpose(euler_matrix(phi, the, psi)))
    return (m*v).asarray()[0]

# Goldstein constructs euler x-form as passive 
# I want it to be active.  Turns out that this means
# I want his inverse.  This is *my* inverse, which means
# it's his forward transform.
def euler_act_inv( v,  phi,  the,  psi):
    """Active Euler transform inverse"""
    m = euler_matrix(phi, the, psi)
    return (m*v).asarray()[0]

def SolveNewton(f, fprime, x0, tol, maxiter = 50):
    """Solve an equation w/ newton's method"""
    x=x0
    iter = 0
    xs = []
    while (abs(f(x)) > tol and iter < maxiter):
        x = x - f(x)/fprime(x)
        xs.append(x)
        iter = iter+1

    if (iter == maxiter):
        print "Maximum iterations reached, x=", x, ", f(x) =", f(x)
        
    return x;

def SolveBisection(f, xl, xh, tol, maxiter=50, lowexp=None, highexp=None):
    """Solve an equation w/ bisection"""
    if (lowexp is None):
        lowexp = lambda xl, xh : 2*xl - xh 
    if (highexp is None):
        highexp = lambda xl, xh : 2*xh - xl        

    iter = 0
    x = 0.5*(xl + xh)
    v = f(x)

    while (abs(v) > tol):
        x = 0.5*(xl + xh)
        
        vl = f(xl)
        v = f(x)
        vh = f(xh)
        
        if (vl*vh > 0 and vl*v < 0): 
            # don't know what to do
            print "Covex/concave function encountered!"
            return None
        elif (vl*vh > 0):
            # try to expand range
            if (abs(vl) > abs(vh)): xh = highexp(xl, xh)
	    else:                   xl = lowexp(xl, xh)            
        else:
            # contract range
            if (vl*v > 0):  xl = x;
            else:           xh = x;

        iter = iter+1
        
    if (iter == maxiter):
        print "Maximum iterations reached"
    return x

###########################
# Plotting stuff
###########################

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
    
def histo(x, bins=50, weights=None,
          normed=False, overflow=True, noplot=False,
          matplotlib=False, Pyx=False, width=8, **kw):
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
    elif matplotlib:
        width = 0.9*(bins[1]-bins[0])
        matplotlib.matlab.gca().bar(bins, hist, width=width, **kw)        
        matplotlib.matlab.show()
        return None
    else:
        gplt.plot(hist,bins,'notitle w impulses')

def colorbar(xmin, xmax, 
             palette=pyx.color.palette.ReverseRainbow,
             log=False, vertical=False, n=100,
             long=10, short=.5, **graphkw):    
    """Make a PyX colorbar"""
    if log:
        xs = scipy.lnrange(xmin, xmax, n)
        longaxis = pyx.graph.axis.log(min=xmin, max=xmax)
    else:
        xs = scipy.nrange(xmin, xmax, n)
        longaxis = pyx.graph.axis.lin(min=xmin, max=xmax)
    shortaxis = pyx.graph.axis.lin(parter=None)

    if vertical:
        datakw = {'xmin':3, 'xmax':4, 'ymin':1, 'ymax':2}
        graphkw['x'] = shortaxis
        graphkw['y'] = longaxis
        graphkw['width'] = short
        graphkw['height'] = long
    else:
        datakw = {'xmin':1, 'xmax':2, 'ymin':3, 'ymax':4}
        graphkw['x'] = longaxis
        graphkw['y'] = shortaxis
        graphkw['width'] = long
        graphkw['height'] = short
        
    xl = xs[:-1]
    xh = xs[1:]
    yl = scipy.zeros(len(xl), 'd')
    yh = scipy.ones(len(xh), 'd')
    color = scipy.nrange(0., 1., n)

    data = [[xl[i], xmax, yl[i], yh[i], color[i]] for i in range(len(xl))]

    g = pyx.graph.graphxy(**graphkw)
    g.plot(pyx.graph.data.list(data, color=5, **datakw),
           pyx.graph.style.rect(palette))
    g.dodata()
    
    return g