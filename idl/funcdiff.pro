PRO FUNCDIFF,X,ygood,ytest
	if      (n_params() lt 3) then begin 
	print,''
	print,'USAGE funcdiff,x,ygood,ytest'
	print,'INPUT x - N vector of x values'
	print,'      ygood - N vector of reference y values'
	print,'      ytest - N vector of y values of function to test'
	print,''
	print,'Compute the fractional difference in magnitude between two functions.  Print the value |ytest-ygood|/|ygood| where the operation || is defined by funcnorm.pro'
	return
	endif

	print,FUNCNORM(X,ygood-ytest)/funcnorm(x,ygood)
end
