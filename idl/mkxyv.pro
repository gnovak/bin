PRO mkxyv,x,y,xmin,xmax,xpts,ymin,ymax,ypts
	if      (n_params() lt 8) then begin 
	print,''
	print,'USAGE mkxyv,x,y,xmin,xmax,xpts,ymin,ymax,ypts'
	print,'INPUT xmin - min x value'
	print,'      xmax - max x value'
	print,'      xpts - number of x points'
	print,'      ymin - min y value'
	print,'      ymax - max y value'
	print,'      ypts - number of y points'
	print,'OUTPUT x - output x vector'
	print,'       y - output y vector'
	print,''
	print,'Given information about the boundaries and number of points in the you want, generate vectors of x values and y values.  These are useful for passing to plotting and fitting routines to tell them where some function is evaluated.'
	return
	endif

	x = findgen(xpts)    ;X values at each point
	x=(x/(xpts-1))*(xmax-xmin) +xmin
	y = findgen(ypts)
	y=(y/(ypts-1))*(ymax-ymin) +ymin
end
