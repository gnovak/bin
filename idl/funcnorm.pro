function FUNCNORM,X,Y
	if      (n_params() lt 2) then begin 
	print,''
	print,'USAGE funcnorm,x,y'
	print,'INPUT x - N vector of x values'
	print,'      y - N vector of y values'
	print,''
	print,'Find the norm of a function by integrating its absolute value.  The discrete integration scheme takes the first difference of the x values, averages adjacent y values, and takes the inner product of the resulting two vectors.'
	return
	endif
	avg,abs(y),yvals
	fdiff,x,xvals
	return,transpose(xvals)#yvals
end