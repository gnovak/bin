PRO FUNCDIFF2,X,Y,zgood,ztest
	if      (n_params() lt 4) then begin 
	print,''
	print,'USAGE funcdiff2,x,y,zgood,ztest'
	print,'INPUT x - NxM matrix of x values'
	print,'      y - NxM matrix of y values'
	print,'      zgood - NxM matrix of reference z values'
	print,'      ztest - NxM matrix of z values of function to test'
	print,''
	print,'Compute the fractional difference in magnitude between two functions.  Print the value |ztest-zgood|/|zgood| where the operation || is defined by funcnorm2.pro'
	return
	endif

	print,'Fractional difference - ',FUNCNORM2(X,Y,zgood-ztest)/funcnorm2(x,y,zgood)
end




