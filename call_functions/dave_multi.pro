function dave_multi, fwhm,  first_image, second_image, middle_image, $
     adv = adv, source=source, np_deriv=np_deriv, sigma=sigma, chisq=chisq, noise=noise
;+
;   NAME: DAVE_MULTI  = Differential Affine Velocity Estimator
;                       for all the spatial points
;
;   PURPOSE:  Determine 6 parameters defining the affine velocity field
;            at a small area at all the points
;
;   CALLING SEQUENCE:
;
;            Result = Dave_Multi (fwhm, first_image, second_image, middle_image)
;   INPUTS:
;
;            fwhm       FWHM of the window function (shoud be 2, 4, 6, and so on )
;            first_image      Image at t1
;            second_image     Image at t2
;
;   OPTIONAL INPUTS:
;            middle_image      Image at (t1+t2)/2
;
;
;
;   OUTPUTS:
;            Result           parameters at  all the pixels of the images
;              Result(0,*,*) : x-component of velocity (U_0)
;              Result(1,*,*) : y-component of velocity (V_0)
;              Result(2,*,*) : x-derivative of x-component (U_x)
;              Result(3,*,*) : y-derivative of y-component (V_y)
;              Result(4,*,*) : y-derivative of x-component (U_y)
;              Result(5,*,*) : x-derivative of y-component (V_x)
;              Result(6,*,*) : nu ;  image varies with time in proportion to exp(nu*t)
;
;              It is assumed that the velocity field around (x_0, y_0) is of the form
;
;                              vx = U_0 + U_x * (x-x_0) + U_y * (y-y_0)
;                              vy = V_0 + V_x * (x-x_0) + V_y * (y-y_0)
;
;
;   REMARKS:
;
;        -.  The time unit is the time interval between the first and second images,
;            and the length unit is the pixel size.
;
;
;   REFERENCES
;                       Schuck, P. W. 2005, ApJL, 632, L53
;                       Schuck, P. W. 2006 ApJ
;                       Chae, J. et al.  2008
;   HISTORY
;              2006 June:  firstly coded by J. Chae
;              2007 Jan, J. Chae,  modified the window function array "w"
;              2007 April, J. Chae,  introduced a new free parameter  "nu"
;              2009 Feburary, J. Chae
;
;-
if keyword_set(adv)  then psw = 0 else psw = 1
if keyword_set(source) then qsw =1 else qsw = 0
if n_elements(np_deriv) eq 0 then np_deriv=3
if n_elements(noise) eq 0 then noise=1.


s=size(first_image)
nx=s(1)
ny=s(2)

;  Constructing derivatives
if n_elements(middle_image) eq n_elements(first_image) then begin
 im = middle_image
 im_t = (second_image - first_image)/2.
endif  else begin
im =  0.5*(first_image+second_image)
im_t = (second_image - first_image)
endelse



if np_deriv eq 3 then kernel=[-1., 0,1.]/2. ; three-point differentiation
if np_deriv eq 5 then kernel =[0.12019, -0.74038, 0, 0.74038, -0.12019]   ; five-point differentiation
im_x =convol(im, kernel)
im_y= convol(im, transpose(kernel))

;im_x= derivatives(im)
;im_y =transpose(derivatives(transpose(im))) ;


npar = 6+qsw



; Constructing window function
wfunction = 'gaussian'

hh = fix(fwhm/2.)
if n_elements(hh) eq 1 then hh=[hh, hh]
if wfunction eq 'square' then mf=1 else mf=2
nxs = 2*hh[0]*mf+1
nys = 2*hh[1]*mf+1
xs=(indgen(nxs)-nxs/2) # replicate(1, nys)
ys=replicate(1, nxs) # (indgen(nys)-nys/2)

case wfunction of
   'square':   w = replicate(1., nxs, nys)
   'gaussian': w = exp(-alog(2.)*((xs/float(hh[0]))^2+(ys/float(hh(1)))^2))
   'hanning':  w = (1+cos(!pi*xs/hh[0]/2.))*(1+cos(!pi*ys/hh[1]/2.))/4.
end

w=w/noise^2
; Constructing coefficent arrays


A = fltarr(nx, ny, npar, npar, /nozero)

A(*,*,0, 0) = convol(im_x*im_x, w)   ; U0, U0
A(*,*,0, 1) = convol(im_x*im_y, w)   ; U0, V0
A(*,*,1, 1) = convol(im_y*im_y, w)   ; V0, V0
A(*,*,0, 2) = convol(im_x*im_x, xs*w) + psw* convol(im_x*im, w) ; U0, Ux
A(*,*,1, 2) = convol(im_y*im_x, xs*w) + psw* convol(im_y*im, w) ; V0, Ux
A(*,*,2, 2) = convol(im_x*im_x, xs*xs*w)+2*psw* convol(im_x*im, xs*w)+psw^2*convol(im*im, w); Ux, Ux
A(*,*,0, 3) = convol(im_x*im_y, ys*w) + psw* convol(im_x*im, w); U0, Vy
A(*,*,1, 3) = convol(im_y*im_y, ys*w) + psw* convol(im_y*im, w) ; V0, Vy
A(*,*,2, 3) = convol(im_x*im_y, xs*ys*w) + psw*convol(im*im_y, ys*w) + psw*convol(im_x*im, xs*w) $
                            +psw^2*convol(im*im,  w) ; Ux, Vy
A(*,*,3, 3) = convol(im_y*im_y, ys*ys*w )+2*psw*convol(im_y*im, ys*w)+psw^2*convol(im*im, w)  ; Vy, Vy
A(*,*,0, 4) = convol(im_x*im_x, ys*w)  ; U0, Uy
A(*,*,1, 4) = convol(im_y*im_x, ys*w) ; V0, Uy
A(*,*,2, 4) = convol(im_x*im_x, xs*ys*w) +psw*convol(im*im_x, ys*w) ; Ux, Uy
A(*,*,3, 4) = convol(im_y*im_x, ys*ys*w) +psw*convol(im*im_x, ys*w); Vy, Uy
A(*,*,4, 4) = convol(im_x*im_x, ys*ys*w) ; Uy, Uy
A(*,*,0, 5) = convol(im_x*im_y, xs*w)  ; U0, Vx
A(*,*,1, 5) = convol(im_y*im_y, xs*w)  ; V0, Vx
A(*,*,2, 5) = convol(im_x*im_y, xs*xs*w) +psw*convol(im*im_y, xs*w); Ux, Vx
A(*,*,3, 5) = convol(im_y*im_y, ys*xs*w) +psw*convol(im*im_y, xs*w) ; Vy, Vx
A(*,*,4, 5) = convol(im_x*im_y, ys*xs*w) ; Uy, Vx
A(*,*,5, 5) = convol(im_y*im_y, xs*xs*w)




; Vx, Vx
if qsw then begin
A(*,*,0, 6) = -qsw*convol(im_x*im,  w) ; U0, mu
A(*,*,1, 6) = -qsw*convol(im_y*im, w) ; V0, mu
A(*,*,2, 6) = -qsw*convol(im_x*im, xs*w)-qsw*psw*convol(im*im, w) ; Ux, mu
A(*,*,3, 6) = -qsw*convol(im_y*im, ys*w)-psw*qsw*convol(im*im, w); Vy, mu
A(*,*,4, 6) = -qsw*convol(im_x*im, ys*w) ; Uy, mu
A(*,*,5, 6) = -qsw*convol(im_y*im, xs*w) ; Vx, mu
A( *,*,6,6) = qsw^2*convol(im*im, w) ; mu, mu
endif
for i=1, npar-1 do for j=0, i-1 do A(*,*,i,j)=A(*,*,j,i)

B=fltarr( nx, ny,npar, /nozero)
B(*,*,0) = convol(im_t*im_x, -w)
B(*,*,1) = convol(im_t*im_y, -w)
B(*,*,2) = convol(im_t*im, -w)*psw + convol(im_t*im_x, -xs*w)
B(*,*,3) = convol(im_t*im, -w)*psw + convol(im_t*im_y, -ys*w)
B(*,*,4) = convol(im_t*im_x, -ys*w)
B(*,*,5) = convol(im_t*im_y, -xs*w)

if qsw then B(*,*,6)= qsw*convol(im_t*(-im), -w)

; Solving the lienear equations

result=replicate(0.,npar, nx, ny)
sigmacal=0
if arg_present(chisq) or arg_present(sigma) then begin
chisq=fltarr(nx, ny)
sigma=fltarr(npar, nx, ny, /nozero)
sigmacal=1
endif
for xx=hh[0], nx-1-hh[0] do for yy=hh[1], ny-1-hh[1] do begin
AA = reform(A(xx, yy,*,*))
BB = reform(B(xx, yy,*))
svdc, aa, ww, uu, vv
pp = svsol(uu, ww, vv, bb)
result(*,xx,yy)=pp

if sigmacal then begin

delxh = 0.5*( result[0,xx,yy] + result[2,xx,yy]*xs + result[4,xx,yy]*ys )
delyh = 0.5*( result[1,xx,yy] + result[5,xx,yy]*xs + result[3,xx,yy]*ys )

i=xx+xs
j=yy+ys
sx = fix(delxh)-(delxh lt 0.)
sy = fix(delyh)-(delyh lt 0.)
ex = delxh-sx
ey = delyh-sy

Fv = first_image[i-sx, j-sy]*(1-ex)*(1-ey)+ first_image[i-sx-1, j-sy]*ex*(1-ey) $
  + first_image[i-sx,j-sy-1]*(1-ex)*ey+first_image[i-sx-1,j-sy-1]*ex*ey

Sv = second_image[i+sx, j+sy]*(1-ex)*(1-ey)+ second_image[i+sx+1, j+sy]*ex*(1-ey) $
  + second_image[i+sx,j+sy+1]*(1-ex)*ey+second_image[i+sx+1,j+sy+1]*ex*ey


nu = -(result[2,xx,yy]+result[3,xx,yy])*psw
if qsw then nu = nu+qsw*result[6,xx,yy]

sdiv =exp(-nu/2.)
fdiv =1./sdiv
gv = Sv*sdiv- Fv*fdiv

chisq[xx,yy] =total(gv^2*w)

sigma[*, xx,yy]=sqrt(chisq[xx,yy]*diag_matrix((vv##diag_matrix(1./ww^2)##transpose(vv))))
endif
endfor



return, result
end

