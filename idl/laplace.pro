function laplace,phi,dx,dy
if (n_params() lt 1) then begin 
    print,''
    print,'USAGE var = laplace(mat)'
    print,'INPUT mat -- matrix'
    print,'OUTPUT var -- Laplacian of mat. edges set to zero'
    return,0
endif
sz=SIZE(phi)
sx=sz[1]
sy=sz[2]
phixl=dblarr(sx,sy)
phixh=dblarr(sx,sy)
phiyl=dblarr(sx,sy)
phiyh=dblarr(sx,sy)
lap=dblarr(sx,sy)
phixl[0:sx-2,*]=phi[1:sx-1,*]
phixh[1:sx-1,*]=phi[0:sx-2,*]
phiyl[*,0:sy-2]=phi[*,1:sy-1]
phiyh[*,1:sy-1]=phi[*,0:sy-2]
lap=(phixl+phixh-2*phi)/dx^2+(phiyl+phiyh-2*phi)/dy^2
return,lap
end
