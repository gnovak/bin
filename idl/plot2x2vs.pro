PRO plot2x2vs,fname,scale,fx,fy,x,y
	if      (n_params() lt 1) then begin 
	print,''
	print,'USAGE plot2x1,fname'
	print,'INPUT fname'
	print,'OUTPUT fx, fx - NxM array of x and y components of function'
	print,'       x,y - x and y N and M vectors of points at which function is evaluated'
	print,''
	print,'Given a file name, read fname.x and fname.y to the the x and y values at which the function is evaluated.  Read x.fname and y.fname to get the x and y components of the function.  Finally plot the vector field using svelovect.  This is for functions form R^2 to R^2.'
	return
	endif

	x=fread(fname+'.x')
	y=fread(fname+'.y')
	fx=fread('x.'+fname)
	fy=fread('y.'+fname)
	svelovect,fx,fy,x,y,window length=scale
end

