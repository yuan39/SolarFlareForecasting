function nave_multi, fwhm,  fim, sim, sample=smp,adv=adv, source=source, iter=itermax, par1=par1
;+
;   NAME: NAVE_MULTI  =  Non-linear Affine Velocity Estimator
;                       for all the spatial points
;
;   PURPOSE:  Determine 7 parameters defining the affine velocity field
;            at a small area at all the points
;
;   CALLING SEQUENCE:
;
;            Result = Nave_Multi (fwhm, first_image, second_image)
;   INPUTS:
;
;            fwhm       FWHM of the window function (shoud be 2, 4, 6, and so on )
;            first_image      Image at t1
;            second_image     Image at t2
;
;
;   KEYWORDS:
;            advection        if set,  the advection is solved rather than the continuity eqaution
;
;   OUTPUTS:
;            Result           parameters at  all the pixels of the images
;              Result(0,*,*) : x-component of velocity (U_0)
;              Result(1,*,*) : y-component of velocity (V_0)
;              Result(2,*,*) : x-derivative of x-component (U_x)
;              Result(3,*,*) : y-derivative of y-component (V_y)
;              Result(4,*,*) : y-derivative of x-component (U_y)
;              Result(5,*,*) : x-derivative of y-component (V_x)
;              Result(6,*,*) : nu
;
;              It is assumed that the velocity field around (x_0, y_0) is of the form
;
;                              vx = U_0 + U_x * (x-x_0) + U_y * (y-y_0)
;                              vy = V_0 + V_x * (x-x_0) + V_y * (y-y_0)
;
;
;   REMARKS:
;
;        -.  The time unit is the time interval between two successive images,
;            and the length unit is the pixel size.
;        -.  For the best performance, the pixel size and the time step should
;            be selected to satisfy the Courant condition
;
;                      |v_x| =< Dx/Dt and |V_y| =< Dy/Dt
;        -.  Using the time-centered spatial derivatives based on three images
;            works better than using the time-averaged spatial derivatives based on
;            two images.
;
;
;   HISTORY
;
;              2007 Jan, J. Chae,  first coded.
;
;-
s=size(fim)
nx=s(1)
ny=s(2)

if n_elements(par) ne (7*nx*ny) then par=fltarr(7, nx, ny)  ; U_0, V_0, U_x, V_y, U_y, V_x, nu



hh = fix(fwhm/2.)
nxs=2*hh+1
nys=2*hh+1
nx1= nx-1-hh+1
ny1= ny-1-hh+1

if n_elements(smp) eq 0 then smp=hh


nxmac = nx1/smp
nymac = ny1/smp
xmac=(hh+indgen(nxmac)*smp)#replicate(1, nymac)
ymac=replicate(1, nxmac)#(hh+indgen(nymac)*smp)


par1=nave_point( fim, sim, reform(xmac, nxmac*nymac), reform(ymac, nxmac*nymac), $
 fwhm,  iter=iter, adv=adv, source=source)

npar=n_elements(par1[*,0,0])
par1=reform(par1, npar, nxmac, nymac)

par = replicate(0.,npar, nx, ny)

x=indgen(nx)#replicate(1, ny)
y=replicate(1, nx)#indgen(ny)


x1= -smp/2  & x2= smp-1-smp/2
xs = (indgen(smp)+x1)#replicate(1, smp)
y1=-smp/2  & y2= smp-1 -smp/2
ys = replicate(1, smp)#(indgen(smp)-smp/2)


for im=0, nxmac-1 do for jm=0, nymac-1 do begin

x0 = xmac[im,jm]  & y0= ymac[im,jm]
U0=  par1[0,im,jm] & Ux = par1[2,im,jm] & Uy = par1[4,im,jm]
Uxx = (par1[2,(im+1)<(nxmac-1), jm]-par1[2, (im-1)>0, jm])/(2*smp)
Uxy = (par1[2,im, (jm+1)<(nymac-1)]-par1[2, im, (jm-1)>0])/(2*smp)
Uyy = (par1[4,im, (jm+1)<(nymac-1)]-par1[4, im, (jm-1)>0])/(2*smp)
Uyx = (par1[4,(im+1)<(nxmac-1), jm]-par1[4, (im-1)>0, jm])/(2*smp)

V0= par1[1,im,jm] & Vx = par1[5,im,jm] & Vy = par1[3,im,jm]
Vxx = (par1[5,(im+1)<(nxmac-1), jm]-par1[5, (im-1)>0, jm])/(2*smp)
Vxy = (par1[5,im, (jm+1)<(nymac-1)]-par1[5, im, (jm-1)>0])/(2*smp)
Vyy = (par1[3,im, (jm+1)<(nymac-1)]-par1[3, im, (jm-1)>0])/(2*smp)
Vyx = (par1[3,(im+1)<(nxmac-1), jm]-par1[3, (im-1)>0, jm])/(2*smp)

par[0, x1+x0:x2+x0,y1+y0:y2+y0] = U0 + Ux*xs + Uy*ys+0.5*Uxx*xs^2+ 0.5*Uyy*ys^2+0.5*(Uxy+Uyx)*xs*ys
par[1, x1+x0:x2+x0,y1+y0:y2+y0] = V0 + Vx*xs + Vy*ys+0.5*Vxx*xs^2+ 0.5*Vyy*ys^2+0.5*(Vxy+Vyx)*xs*ys
par[2, x1+x0:x2+x0,y1+y0:y2+y0] = Ux
par[3, x1+x0:x2+x0,y1+y0:y2+y0] = Vy
par[4, x1+x0:x2+x0,y1+y0:y2+y0] = Uy
par[5, x1+x0:x2+x0,y1+y0:y2+y0] = Vx
if npar gt 6 then begin
nux = (par1[6,(im+1)<(nxmac-1), jm]-par1[6, (im-1)>0, jm])/(2*smp)
nuy = (par1[6,im, (jm+1)<(nymac-1)]-par1[6, im, (jm-1)>0])/(2*smp)
par[6, x1+x0:x2+x0,y1+y0:y2+y0] = par1[6,im,jm]+nux*xs+nuy*ys
endif
endfor

return, par
end

