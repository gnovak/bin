function nlines,fname
if (n_params() lt 1) then begin 
    print,''
    print,'USAGE num = nlines(fname)'
    print,'INPUT fname -- name of a text file.'
    print,'OUTPUT num = the number of lines with data in the text file'
    return,null
endif
lines=1
spawn,"grep '[0-9]' " + fname + " | wc",result
reads,result,lines
return,lines
end
