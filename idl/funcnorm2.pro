FUNCTION FUNCNORM2,X,Y,Z
	if      (n_params() lt 3) then begin 
	print,''
	print,'USAGE funcnorm2,x,y'
	print,'INPUT x - N vector of x values'
	print,'      y - M vector of y values'
	print,'      z - NxM array of z values
	print,''
	print,'Find the norm of a function function from R2 to R by integrating its absolute value.  The integration scheme averages four adjacent z values and multiplies the result by dx*dy.'
	return,0
	endif
	avg2,abs(z),zvals
	fdiff,x,xvals
	fdiff,y,yvals
	RETURN,yvals##(zvals##xvals)
end