PRO fdiff2,x,dx
	if      (n_params() lt 2) then begin 
	print,''
	print,'USAGE fdiff2,x,dx'
	print,'INPUT x - NxM array'
	print,'OUTPUT dx - (N-1)x(M-1) array of differential areas'
	print,''
	print,'Compute a matrix where each entry is equal to the differential area enclosed by four adjacent entries of the input matrix.  So out_ij = (in_(i+1)j-in_ij)*(in_i(j+1)-in(ij))'
	return
	endif

	c=(size(x))[1]
	r=(size(x))[2]
	DX=findgen(c-1,r-1)
	for i=0,c-2 do begin
		for j=0,r-2 do begin
			DX[i,j]=(X[i+1,j]-X[i,j])*(X[i,j+1]-X[i,j])
		end
	end
end




