PRO FDIFF,X,DX
	if      (n_params() lt 2) then begin 
	print,''
	print,'USAGE fdiff,x,dx'
	print,'INPUT x - N element vector '
	print,'OUTPUT dx - (N-1) element vector of first differences'
	print,''
	print,'Find the first differences of the input vector.  So out_i = in_i-in_(i-1)'
	return
	endif

	DX=findgen(n_elements(X)-1,1)
	for i=0,n_elements(X)-2 do DX[i]=X[i+1]-X[i]
end
