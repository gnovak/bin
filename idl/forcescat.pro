PRO FORCESCAT,X,Y,fgx,fgy,ftx,fty
	if      (n_params() lt 6) then begin 
	print,''
	print,'USAGE forcescat,x,y,fgx,fgy,ftx,fty'
	print,'INPUT x - N vector of x values'
	print,'      y - M vector of y values'
	print,'      fgx - NxM matrix with X component of reference force'
	print,'      fgy - NxM matrix with Y component of reference force'
	print,'      ftx - NxM matrix with X component of test force'
	print,'      fty - NxM matrix with Y component of test force'
	print,''
	print,'Given information about two force calculations, plot the fractional error in magnitude and the directional error in radians in an xy plot.  Each entry in the matrix of force values becomes a point in the plot.'
	return
	endif


	; good force magnitude
	fgm = sqrt(fgx*fgx+fgy*fgy)
	; good force direction in radians up to multiples of pi
	fgd = -atan(fgy/fgx)
	; test force magnitude
	ftm = sqrt(ftx*ftx+fty*fty)
	; test force direction
	ftd = -atan(fty/ftx)
	; fractional difference in force magnitudes
	dmag=findgen(n_elements(fgm))
	; difference in force direction in radians
	ddir=findgen(n_elements(fgm))
	; distance from origin of force evaluation point
	dist=findgen(n_elements(fgm))
	; x,y index bounds
	xsize=(size(fgm))[1]
	ysize=(size(fgm))[2]
	for i=0,xsize-1 do begin
		for j=0,ysize-1 do begin
			dist[i*ysize + j] = sqrt(x[i]*x[i]+y[j]*y[j])
			dmag[i*ysize + j] = (ftm[i,j]-fgm[i,j])/fgm[i,j]
			ddir[i*ysize + j] = fgd[i,j]-ftd[i,j]
		endfor
	endfor
	window,0
	plot,dist,dmag,ps=3
	window,1
	plot,dist,ddir,ps=3
end
