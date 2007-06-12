; returns matrix 
; mat(0..ncols-1, 0..nlines-1)
; to be accessed as
; mat(col,line)
pro wmatrix,mat, fname
if (n_params() lt 2) then begin 
    print,''
    print,'USAGE var = matrix(fname)'
    print,'INPUT fname -- name of a text file with a 2d array of numbers'
    print,'OUTPUT var -- 2d array with data oriented as var(col,row)'
    print,'  print,var prints the data exactly as it appears in the file'
endif

openw,lun,fname,/Get_Lun
sz = size(mat)
for i=0, sz(1)-1 do begin
    for j=0, sz(2)-1 do begin
        printf,lun,format = '(2(F))',4.,5.
    endfor
endfor    
free_lun,lun
end
