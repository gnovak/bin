pro mtv,mat,quiet=q,min=min,max=max,_EXTRA = e
if (not keyword_set(q)) then q=0
if n_elements(min) le 0 then min=min(mat)
if n_elements(max) le 0 then max=max(mat)
themax = max+(max-min)/5.0
themin = min-(max-min)/5.0
tvimage,bytscl(mat,min=themin,max=themax),/NOINTERPOLATION,_EXTRA = e
if not q then  print,min(mat),max(mat)
end
