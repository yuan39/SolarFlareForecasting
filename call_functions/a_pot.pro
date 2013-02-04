;+
;
; NAME :
;          A_POT
; PURPOSE :
;          Vector Potential  of Current-Free Magnetic Field  at the surface
; CALLING SEQUENCE:
;          A_POT, bz0, ax, ay, pad_width = pad_width, window_no=window_no
;
; INPUT :
;           bz0 : 2-d array of vertical field at z=0 plane
; OUTPUT :
;           ax, ay : 2-d arrays of vector potential  components
; OPTIONAL INPUT:
;          pad_width    if set and non-zero, then the surrounding area
;                             with a given width is padded with zeros.
; MODIFICATION HISTORY:
;        July 2000  Jongchul Chae
;-
pro a_pot,  bz0, ax, ay , pad_width=pad_width


s=size(bz0)
if n_elements(pad_width) eq 0 then pad_width=0
bz1 = fltarr(s(1)+2*pad_width, s(2)+2*pad_width)
bz1(pad_width:pad_width+s(1)-1, pad_width:pad_width+s(2)-1)=bz0

nx =  2^fix(alog(s(1)+2*pad_width)/alog(2.))
nx = nx*(1+(s(1)+2*pad_width gt nx))
ny =  2^fix(alog(s(2)+2*pad_width)/alog(2.))
ny = ny*(1+(s(2)+2*pad_width gt ny))

if nx ne s(1) or ny ne s(2) then   bz1 = congrid(bz1, nx, ny, cubic=-0.5)


kx = 2*!pi*[findgen(nx/2+1),reverse(-1-findgen(nx-nx/2-1))]/nx
ky = 2*!pi*[findgen(ny/2+1),reverse(-1-findgen(ny-ny/2-1))]/ny
kx=kx#replicate(1, ny)
ky=replicate(1,nx)#ky
fbz0 = fft(bz1, 1)
ksq=(kx^2+ky^2)  >kx(1,0)^2 > ky(0,1)^2
argx=fbz0*complex(0,-1.)*ky/ksq
argy=fbz0*complex(0, 1.)*kx/ksq
ax=fltarr(nx,ny)
ay=fltarr(nx,ny)
ax = congrid(float(fft(argx,-1)), s(1)+2*pad_width, s(2)+2*pad_width, cubic=-0.5)
ax=ax( pad_width:pad_width+s(1)-1, pad_width:pad_width+s(2)-1)
ay = congrid(float(fft(argy,-1)), s(1)+2*pad_width, s(2)+2*pad_width, cubic=-0.5)
ay  = ay(pad_width:pad_width+s(1)-1, pad_width:pad_width+s(2)-1)

end


