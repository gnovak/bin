; returns matrix 
; mat(0..ncols-1, 0..nlines-1)
; to be accessed as
; mat(col,line)
function vector,fname
if (n_params() lt 1) then begin 
    print,''
    print,'USAGE var = matrix(fname)'
    print,'INPUT fname -- name of a text file with a 2d array of numbers'
    print,'OUTPUT var -- 2d array with data oriented as var(col,row)'
    print,'  print,var prints the data exactly as it appears in the file'
    return,null
endif

lines = nlines(fname)
; cols = ncols(fname)
mat = fltarr(lines)
openr,lun,fname,/Get_Lun
readf,lun,mat
free_lun,lun
return,mat
end
