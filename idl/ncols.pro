function ncols,fname
if (n_params() lt 1) then begin 
    print,''
    print,'USAGE num = ncols(fname)'
    print,'INPUT fname -- name of a text file.'
    print,'OUTPUT num = the number of columns in the first line of valid'
    print,'  data in the file.'
    return,null
endif
lines=1
cols=1
spawn,"head -1 " + fname + " | wc",result
reads,result,lines,cols
return,cols
end
