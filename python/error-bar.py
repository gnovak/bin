


from pyx import *
from pyx.graph.axis import linear

g = graph.graphxy(width=15, 
	x=linear(title="Lookback Time (GYr)",min=0,max=11),
	y=linear(title=r"$ln(M_B/L)-ln(M_B/L)_{Coma}$",min=-2,max=0.0))

df = graph.data.file("z.ml.dat",x=1,y=3,sig=5)
xs = df.getcolumn("x")
ys = df.getcolumn("y")
sigs = df.getcolumn("sig")

g.plot(graph.data.function("y=log((13.7-x)/11.6-.15)"))
g.finish()


for i in range(len(xs)):
	ewidth = 0.05
	x = xs[i]
	y = ys[i]
	s = sigs[i]
	x1,y1 = g.pos(x,y-s)
	x2,y2 = g.pos(x,y+s)
	g.stroke(path.line(x1,y1,x2,y2))
	g.stroke(path.line(x1-ewidth,y1,x1+ewidth,y1))
	g.stroke(path.line(x1-ewidth,y2,x1+ewidth,y2))

g.writeEPSfile('figure2')
