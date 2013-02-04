function lct_multi,  FWHM,first, second,  Range = Range, fwhmy=fwhmy, noise=noise, chisq=chisq
;+
;  Name:  LCT_multi
;  Purpose:
;          Calculate local displacements between two successive images
;          by maximizing local cross-correlation
;
;  Calling sequence:
;
;          Result = lct_multi (HWHM, First_image, Second_image, $
;                        Range = Range)
;
;  Inputs:
;
;           FWHM     FWHM width of the apodizing window function (should be 2, 4, 6, and so on)
;           First_image, Second_image: two images to be compared
;
;
;
;  Keyword input:
;
;           Range: the maximum range of x- and y- displacements
;                  For the best results, displacement in each direction should
;                  be smaller than Range.

;  Output:
;         Result :
;               result(0,*, *)   x-displacement
;               result(1,*, *)   y-displacement
;               result(2,*, *)   correlation
;
;  HISTORY
;
;        2006 June, J. Chae.   Fist coding (with addation from LCT)
;        2007 Jan, J. Chae,  modified the window function array "w"
;                            removed the constraint on the maximum range of 2
;
;-


s=size(first)
nx = s(1)
ny = s(2)

if n_elements(fwhmy) eq 0 then fwhmy=fwhm
nxs = fwhm +   (fwhm mod 2 eq 0)
nys = fwhmy+  (fwhmy mod 2 eq 0)
;nxs = 2*fwhm+1
;nys= 2*fwhmy+1
xs=(findgen(nxs)-nxs/2) # replicate(1., nys)
ys=replicate(1., nxs) # (findgen(nys)-nys/2)

;w=replicate(1., nxs, nys)
;w= (float(xs)/(fwhm/2))^2+(float(ys)/(fwhmy/2))^2 le 1.
w= exp(-0.5*(xs/fwhm*2.36)^2-0.5*(ys/fwhmy*2.36)^2)
w = w/total(w)
;LW=w/total(W)
;LW2=LW ; w^2/total(w^2)

if n_elements(Range) eq 0 then Range = 1

hw_box =  Range>1
nb = 2*hw_box+1
delx = (indgen(nb)-hw_box)#replicate(1, nb)
dely = replicate(1, nb)#(indgen(nb)-hw_box)
correlation_array = fltarr(nb, nb, nx, ny )

result=fltarr(3, nx, ny)



f_av0 = convol(first, w) ;/nw
s_av0 = convol(second, w)
f2_av0 = convol(first^2, w)
s2_av0 = convol(second^2, w)


for i=0, nb-1 do for j=0, nb-1 do begin
dx = delx(i,j)
dy = dely(i,j)

f_av =shift(f_av0, dx, dy)
s_av =shift(s_av0, -dx, -dy)

f2_av = shift(f2_av0, dx, dy)
s2_av = shift(s2_av0, -dx, -dy)


first1=shift(first, dx, dy)
second1 =shift(second, -dx, -dy)

fs_av = convol(first1*second1, w) ;/nw

correlation_array(i,j, *, *) = (fs_av - f_av*s_av)/sqrt((f2_av-f_av^2)*(s2_av-s_av^2))
endfor



 ; Preparation of Arrays required for Surface Fitting

     delx0 = [ [-1, 0, 1], [-1, 0,1], [-1,0,1]]
     dely0 = [ [-1, -1, -1], [0, 0, 0], [1, 1, 1]]

     h = dblarr(9, 6, /nozero)
     h(*,0) = 1.
     h(*,1) = reform(delx0*2.)
     h(*,2) = reform(dely0*2.)
     h(*,3) = reform(delx0^2*4.)
     h(*,4) = reform(dely0^2*4.)
     h(*,5) = reform(delx0*dely0*4.)
     ht = transpose(h)
     hh = invert(ht#h)

margin_width= (nb+nxs)/2 ;

for xx=margin_width, nx-1-margin_width do for yy=margin_width, ny-1-margin_width do begin

  correlation = correlation_array(*,*,xx, yy)
       if nb gt 3 then begin
       ss= where(correlation eq max(correlation))
       ic = round(mean(ss mod nb))
       jc = round(mean(ss / nb))
       endif else begin
        ic = 1
        jc = 1
       endelse

       ic=ic>1<(nb-2)
       jc=jc>1<(nb-2)
      if (ic-1) ge 0 and (ic+1) lt nb and $
          (jc-1) ge 0 and (jc+1) lt nb then begin

       a = hh#(ht#reform(correlation(ic-1:ic+1, jc-1:jc+1),9))

       peak = float(invert([[2*a(3), a(5)], [a(5), 2*a(4)]]) $
           # [-a(1), -a(2)])

      ; peak=peak >(-1.5)<1.5  ; to avoid too unrealistic solution

          result(0, xx, yy) = peak(0)+delx(ic, jc)*2
          result(1, xx, yy) = peak(1)+dely(ic, jc)*2
          result(2, xx, yy) = float( a(0)+a(1)*peak(0)+ a(2)*peak(1) $
             + a(3)*peak(0)^2+a(4)*peak(1)^2 $
             + a(5)*peak(0)*peak(1))
        ;   if xx eq 100 and yy eq 100 then stop

          ;if (abs(peak(0)) gt 1.5) or (abs(peak(1)) gt 1.5) then result[2,x,y]=0.
          ;if result[2,x,y] lt 0.2 then result[0:2,x,y]=0
       endif
 endfor

if arg_present(chisq)  then begin

chisq=fltarr(nx, ny)

if n_elements(noise) eq 0 then noise=1.0
for xx=margin_width, nx-1-margin_width do for yy=margin_width, ny-1-margin_width do begin

delxh = 0.5* result[0,xx,yy]
delyh = 0.5* result[1,xx,yy]

i=xx+xs
j=yy+ys
sx = fix(delxh)-(delxh lt 0.)
sy = fix(delyh)-(delyh lt 0.)
ex = delxh-sx
ey = delyh-sy
Fv = first[i-sx, j-sy]*(1-ex)*(1-ey)+ first[i-sx-1, j-sy]*ex*(1-ey) $
  + first[i-sx,j-sy-1]*(1-ex)*ey+first[i-sx-1,j-sy-1]*ex*ey
Sv = second[i+sx, j+sy]*(1-ex)*(1-ey)+ second[i+sx+1, j+sy]*ex*(1-ey) $
  + second[i+sx,j+sy+1]*(1-ex)*ey+second[i+sx+1,j+sy+1]*ex*ey
gv = Sv- Fv

chisq[xx,yy] =total(gv^2*w/noise^2)

endfor

endif

return, result

end

