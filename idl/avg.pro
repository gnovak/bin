PRO avg,x,ax
	if      (n_params() lt 2) then begin 
	print,''
	print,'USAGE avg,x,ax'
	print,'INPUT x - n vector to be averaged'
	print,'OUTPUT ax - n-1 vector of averages'
	print,''
	print,'Given an N element vectory, return a vector with N-1 elements where each; entry is the average of two adjacent entries in the original vector so out_i = 0.5*(in_i + in_(i-1))'
	return
	endif

	ax=findgen(n_elements(x)-1,1)
	for i=0,n_elements(x)-2 do ax[i]=0.5*(x[i+1]+x[i])
end
