;+
;
; NAME :
;          IMAGE_FLOW_DISPLAY
; PURPOSE :
;          Superpose  2-D Field (like flow) on an image
; CALLING SEQUENCE:
;          IMAGE_FLOW_DISPLAY, image,  vx, vy, vunit=vunit,   sample=sample, condition = condition
;                              mag_factor = mag_factor
;
; INPUT :
;           image : an byte image to be displayed
;           vx, vy : 2-d arrays of  flow fields
; OPTIONAL INPUT:
;          mag_factor            magnification factor for image (for enlarged displayed in window)
;                                          default=1
;          position                  4-element array of display position in device units
;
;          sample                  the pixel interval for display flow fields (default=1)
;          vunit                    the magnitude of flow field correspoding to on pixel in image
;          condition            a binary array. Flow fields are drawn only at those pixels with condition=1
;                                     (default =1 at all pixels)
;
; MODIFICATION HISTORY:
;        Januaryt 2002  Jongchul Chae
;-
pro  IMAGE_FLOW_display,  image, vx, vy , vunit=vunit,  bar_size=bar_size, sample=sample, $
             condition = condition, mag_factor=mag_factor, position=pos, $
             byte_margin=byte_margin, thick=thick

s=size(image)

if not isvalid(mag_factor) then mag_factor=1
if not isvalid(condition) then condition=1
if not isvalid(byte_margin) then byte_margin=20
if not isvalid(sample) then sample=(3/mag_factor)>1
if n_elements(pos) ne 4 then pos=[0,0, s(1)*mag_factor, s(2)*mag_factor]


   tv,  congrid(image,  s(1)*mag_factor, s(2)*mag_factor), /dev, pos(0), pos(1), xsize=pos(2)-pos(0)+1, ysize=pos(3)-pos(1)+1
   plot, [-0.5, s(1)-1+0.5], [-0.5, s(2)-1+0.5], /noerase, /nodata, xst=5, yst=5, $
        xr=[-0.5, s(1)-1+0.5], yr=[-0.5, s(2)-1+0.5], pos=pos, /dev

    xa=indgen(s(1))#replicate(1, s(2))
    ya=replicate(1,s(1)) # indgen(1, s(2))

    if not isvalid(vunit) then begin
     ss= where(xa mod sample eq 0 and ya mod sample eq 0 and condition)
     vunit  = sqrt(stdev(vx(ss))^2 + stdev(vy(ss))^2)
   endif

    ss= where(xa mod sample eq 0 and ya mod sample eq 0 and condition and image ge byte_margin)
    if ss(0) ne -1 then plot_vel, xa(ss), ya(ss), vx(ss)/vunit, vy(ss)/vunit,  1., /data, hsize=-0.4,color=0, thick=thick
    ss= where(xa mod sample eq 0 and ya mod sample eq 0 and condition and image lt byte_margin)
;    ss= [ss, (s(1)*s(2)*0.9+s(1)*0.1)/1]
;    vx((s(1)*s(2)*0.9+s(1)*0.1)/1)=0.5
;    vy((s(1)*s(2)*0.9+s(1)*0.1)/1)=0.
    if ss(0) ne -1 then plot_vel, xa(ss), ya(ss), vx(ss)/vunit, vy(ss)/vunit, 1., /data, hsize=-0.4, color=255, thick=thick



end