pro mtv,mat,quiet=q,min=min,max=max,_EXTRA = e
if (not keyword_set(q)) then q=0
if n_elements(min) le 0 then min=min(mat)
if n_elements(max) le 0 then max=max(mat)
tvimage,bytscl(mat,min=min,max=max),/NOINTERPOLATION,_EXTRA = e
if not q then  print,min(mat),max(mat)
end
