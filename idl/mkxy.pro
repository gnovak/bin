PRO mkxy,x,y,xmin,xmax,xpts,ymin,ymax,ypts
	if      (n_params() lt 8) then begin 
	print,''
	print,'USAGE mkxy,x,y,xmin,xmax,xpts,ymin,ymax,ypts'
	print,'INPUT xmin - min x value'
	print,'      xmax - max x value'
	print,'      xpts - number of x points'
	print,'      ymin - min y value'
	print,'      ymax - max y value'
	print,'      ypts - number of y points'
	print,'OUTPUT x - output x array'
	print,'       y - output y array'
	print,''
	print,'Given information about the boundaries and number of points in the you want, generate matricies of x values and y values suitable for computations of the form z=x*x + y*y to generate a paraboloid.'
	return
	endif

	x = findgen(xpts) # replicate(1., ypts)   ;X values at each point
	x=(x/(xpts-1))*(xmax-xmin) +xmin
	y = replicate(1.,xpts) # findgen(ypts)
	y=(y/(ypts-1))*(ymax-ymin) +ymin
end
