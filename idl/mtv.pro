pro mtv,mat,quiet=q,_EXTRA = e
if (not keyword_set(q)) then q=0
tvimage,bytscl(mat,min=min(mat),max=max(mat)),/NOINTERPOLATION,_EXTRA = e
if not q then  print,min(mat),max(mat)
end
