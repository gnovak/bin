# First crack at pseudo color plots for PyX
from scipy import *
import pyx

COLORS = 256

def bwmap():
    return array([chr(i) for i in range(COLORS)], 'O')

def cmap():
    return [chr(i)+chr(i)+chr(i) for i in range(COLORS)]

def applymap(img, map=None, mx=None, mn=None):
    s=shape(img)
    if mx is None: mx = max(ravel(img))
    if mn is None: mn = min(ravel(img))
    if map is None: map=bwmap()
    # clip image
    cimg = clip(img, mn, mx)
    # scale and cast to int
    simg = (COLORS-1)*(cimg-mn)/(1.*mx-mn)
    simg = simg.astype(Int)
    return reshape(take(map,ravel(simg)), s)

def array_to_image(a):
    s=shape(a)
    mapped = applymap(a)
    packed = reduce(lambda x,y: x+y, ravel(mapped))
    return pyx.bitmap.image(s[0], s[1], "L", packed)
    

    g.finish()    
    x1,y1 = g.pos(b, ymax)
    x2,y2 = g.pos(b, 0.75*ymax)    
    g.stroke(path.line(x1,y1,x2,y2), [style.linestyle.solid])

def plot(a):
    b=array_to_image(a)
    c=pyx.bitmap.bitmap(0,1,b,height=5)
    d=pyx.canvas.canvas()
    d.insert(c)
    return d
    
