function tensor,fname
if (n_params() lt 1) then begin 
    print,''
    print,'USAGE var = tensor(fname, d1, d2, d3)'
    print,'INPUT fname -- name of a text file with a 3d array of numbers'
    print,'OUTPUT var -- 3d array of data.'
    return,0
endif
totallines=nlines(fname)
cols=ncols(fname)
rows=ntensorrows(fname)
matricies=totallines/rows
print,"Creating ",matricies,"x",rows,"x",cols," tensor"
tens = fltarr(cols, rows, matricies)
openr,lun,fname,/Get_Lun
readf,lun,tens
free_lun,lun
return,tens
end
