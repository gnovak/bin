function ntensorrows,fname
if (n_params() lt 1) then begin 
    print,''
    print,'USAGE num = ntensorrows(fname)'
    print,'INPUT fname -- name of a text file.'
    print,'OUTPUT num = number of rows per in each matrix, for reading in tensors'
    return,null
endif
spawn,"tensorlinecount < "+fname,result
reads,result,rows
return,rows
end
