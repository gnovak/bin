PRO avg2,x,ax
	if      (n_params() lt 2) then begin 
	print,''
	print,'USAGE avg2,x,ax'
	print,'INPUT x - NxN array to be averaged'
	print,'OUTPUT ax - (N-1)x(N-1) array of averages'
	print,''
	print,'Given an NxN matrix, return an (N-1)x(N-1) matrix where each entry is the average of four adjacent entries in the input matrix.  so out_ij = 0.25*(in_ij + in_(i-1)j + in_i(j-1) + in_(i-1)(j-1))'
	return
	endif

	c=(size(x))[1]
	r=(size(x))[2]
	AX=findgen(c-1,r-1)
	for i=0,c-2 do begin
		for j=0,r-2 do begin
			AX[i,j]=0.25*(X[i,j]+X[i+1,j]+X[i,j+1]+X[i+1,j+1])
		end
	end
end
