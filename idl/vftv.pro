pro vftv,uxf,uyf,nx=nx,ny=ny,quiet=q,_EXTRA = e
ux=matrix(uxf)
uy=matrix(uyf)
if (not keyword_set(q)) then q=0
if not q then  print,"ux",min(ux),max(ux)
if not q then  print,"uy",min(uy),max(uy)
if (keyword_set(nx) and keyword_set(ny)) then begin
    ux=congrid(ux,nx,ny)
    uy=congrid(uy,nx,ny)
end 
help,ux
help,uy
velovect,ux,uy
end
