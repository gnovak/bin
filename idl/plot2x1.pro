PRO plot2x1,fname
	if      (n_params() lt 1) then begin 
	print,''
	print,'USAGE plot2x1,fname'
	print,'INPUT fname'
	print,''
	print,'Given a file name, read the file to find a matrix and plot the function from R^2 to R^1 using surface.
	return
	endif

	pot=(read_ascii(fname)).field001
	surface,pot
end

