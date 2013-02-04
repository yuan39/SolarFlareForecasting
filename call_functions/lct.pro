function lct, first, second, x, y, gauss_sigma, Range = Range
;+
;  Name:  LCT
;  Purpose:
;          Calculate local displacements between two successive images
;          by maximizing local cross-correlation
;
;  Calling sequence:
;
;          Result = lct (First_image, Second_image, Xpositions, Ypositions, $
;                        Gauss_sigma, Range = Range)
;
;  Inputs:
;           First_image, Second_image: two images to be compared
;           Xpositions, Ypositions:   1-d arrays of x- and y- pixel positions
;                            where displacements are to be determined.
;           Gauss_sigma: Gaussian width of the apodizing window function
;
;  Keyword input:
;
;           Range: the maximum range of x- and y- displacements
;                  For the best results, displacement in each direction should
;                  be smaller than Range.
;                  (Range+1)*(Range+1) area is searched.
;                  (default value = 2 pixels)
;  Output:
;         Result : 3 by Npoint array
;            Result(0, *)  x-displacements in pixels
;            Result(1, *)  y-displacements in pixels
;            Result(2, *)  cross-correlations (ranging from -1 to 1)
;                          These are set to zero at the pixels where
;                          determined displacements are bigger than Range.
;
;-

on_error, 2
window_size = 2*fix(gauss_sigma*3.)+1

window = shift(exp(-0.5*(dist(window_size)/gauss_sigma)^2 ), $
                window_size/2, window_size/2)
window2 = sqrt(window)
window = window/total(window)
window2 = window2/total(window2)

if n_elements(Range) eq 0 then Range = 2
hw_box = (Range+1)/2 > 1
nb = 2*hw_box+1
delx = (indgen(nb)-hw_box)#replicate(1, nb)
dely = replicate(1, nb)#(indgen(nb)-hw_box)

delx0 = [ [-1, 0, 1], [-1, 0,1], [-1,0,1]]
dely0 = [ [-1, -1, -1], [0, 0, 0], [1, 1, 1]]
correlation = fltarr(2*hw_box+1, 2*hw_box+1)

   ; Preparation of Arrays required for Surface Fitting

     h = dblarr(9, 6, /nozero)
     h(*,0) = 1.
     h(*,1) = reform(delx0*2.)
     h(*,2) = reform(dely0*2.)
     h(*,3) = reform(delx0^2*4.)
     h(*,4) = reform(dely0^2*4.)
     h(*,5) = reform(delx0*dely0*4.)
     ht = transpose(h)
     hh = invert(ht#h)

   ;

npoint = n_elements(x)
d      = fltarr(3, npoint)

s=size(first)
n = s(1)
m = s(2)


for k=0, npoint-1 do begin

    xc =  x(k)
    yc =  y(k)

    x1 = xc - window_size/2
    x2 = xc + window_size/2
    y1 = yc - window_size/2
    y2 = yc + window_size/2

    inside  = (x1-hw_box) ge 0 and (x2+hw_box) lt n $
            and (y1-hw_box) ge 0 and (y2+hw_box) lt m

  if inside then begin

    for i=0, nb-1 do for j=0, nb-1 do begin
       dx = delx(i,j)
       dy = dely(i,j)

       first_sub  = reform(first(x1-dx:x2-dx,y1-dy:y2-dy )*window2, $
                              window_size^2)
       second_sub = reform(second(x1+dx:x2+dx, y1+dy:y2+dy)*window2, $
                              window_size^2)

       correlation(i,j) = c_correlate(first_sub,second_sub,0)

    endfor


       if nb gt 3 then begin
       ss= where(correlation eq max(correlation))
       ic = ss(0) mod nb
       jc = ss(0) / nb
       endif else begin
        ic = 1
        jc = 1
       endelse
      if (ic-1) ge 0 and (ic+1) lt nb and $
         (jc-1) ge 0 and (jc+1) lt nb  then begin


       a = hh#(ht#reform(correlation(ic-1:ic+1, jc-1:jc+1),9))

       peak = float(invert([[2*a(3), a(5)], [a(5), 2*a(4)]]) $
           # [-a(1), -a(2)])

       d(0, k) = peak(0)+delx(ic, jc)*2
       d(1, k) = peak(1)+dely(ic, jc)*2
       d(2, k) = float( a(0)+a(1)*peak(0)+ a(2)*peak(1) $
             + a(3)*peak(0)^2+a(4)*peak(1)^2 $
             + a(5)*peak(0)*peak(1))
       if (abs(d(0,k)) gt 1.2*hw_box*2) or (abs(d(1,k)) gt 1.2*hw_box*2) then d(2,k)=0.
       if d(2, k) lt 0.5 then begin
        d(0,k)=0.
        d(1,k)=0.
        d(2,k)=0.
       endif

            endif
   endif

endfor

return, d

end

