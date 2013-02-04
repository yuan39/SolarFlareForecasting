pro mdi_vel_apot1,  mdicubefile, xr, yr, del,  cubefile,  fwhm = fwhm, bin_time=bin_time, $
     display=display, bcrit=bcrit, maxvel=maxvel, mag_factor=mag_factor, $
     cor_crit=cor_crit, pad_apot=pad_apot, renew=renew, flux_correct=flux_correct

;+
;  NAME: mdi_vel
;  PURPOSE: make a cube for horizontal velocity data
;  CALLING SEQUENCE:
;
;  INPUTS:
;     mdicubefile  a cube file of MDI magnetograms
;          xr         x-range (in arcseconds)
;          yr         y-range (in arcseconds)
;          del        sampling size (in arcseconds)
;          cubefile      a  cube file for velocity data
;                            it contains three floating images per each set
;                               data(5*k+0) :  magnetogram in unit G
;                               data(5*k+1) :  v_x in unit of     km /s
;                               data(5*k+2) :  v_y in unit of     km/s
;                               data(5*k+3) :  A_x in unit of G Mm
;                               data(5*k+4) :  A_y in unit  of G M
;
; KEYWORD INPUTS:
;           fwhm            Full width half maximum of apodizing window in arc seconds
;           bin_time        time duration for averaging  magnetograms for velocity fields
;                           equal to the time interval for cross-correlation (in minutes)
;
; HISTORY
;
;            2004 Nov,  J. Chae:
;                            - 'delt' is calculated for each pair of two successive images
;                            - the number of selected points for LCT can exceed the limit
;                               imposed by the IDL FOR statement
;
;
; -
if not isvalid(bcrit) then bcrit=5.
if not isvalid(bbig) then bbig=50.
if not isvalid(vunit) then vunit=0.1
if not isvalid(mag_factor) then mag_factor=3
if not isvalid(cor_crit) then cor_crit=0.9

nx = round((xr(1)-xr(0))/del)+1
ny = round((yr(1)-yr(0))/del)+1
x= (findgen(nx)*del+xr(0))#replicate(1., ny)
y= replicate(1., nx)#(findgen(ny)*del+yr(0))

a=assoreadfits(mdicubefile, bz, bs, h,unit=unit1,  nf=nf)
t=fxpar(h, 'COMMENT')
if isvalid(bin_time) then $
  bin =round(bin_time/ ( mdi_time_interval(t(nf-1), t(0), /min)/(nf-1))) $
  else bin=1
ix = (x - fxpar(h, 'XCEN'))/fxpar(h,'XSCALE')+fxpar(h, 'PLATE_X')
iy = (y - fxpar(h, 'YCEN'))/fxpar(h,'YSCALE')+fxpar(h, 'PLATE_Y')

nf1=nf/bin-1+(nf mod bin eq 0)
fxhmake, hdr, fltarr(nx, ny)
fxaddpar, hdr, 'NAXIS', 3
fxaddpar, hdr, 'NAXIS3', (nf1-1)*7
fxaddpar, hdr, 'XSCALE', del, 'in arc seconds'
fxaddpar, hdr, 'YSCALE', del, 'in arc seconds'
fxaddpar, hdr, 'XCEN',  xr(0)+del*nx*0.5, 'center position (arcsec) from reference'
fxaddpar, hdr, 'PLATE_X', nx*0.5, 'center position (pixel) in image'
fxaddpar, hdr, 'YCEN', yr(0)+del*ny*0.5
fxaddpar, hdr, 'PLATE_Y', ny*0.5
fxaddpar, hdr, 'NSR', fxpar(h, 'nsr'), 'lattiude of reference point in degrees'
fxaddpar, hdr, 'EWR', fxpar(h, 'ewr'), 'longitude of reference in degrees'
fxaddpar, hdr, 'REFTIME', fxpar(h, 'reftime'), 'the epoch'
time=strarr(nf1)
time(0)=t(0)
for k=1, nf1-1 do begin
time(k) = t(k*bin)
fxaddpar, hdr, 'COMMENT', time(k)
endfor

delt =   mdi_time_interval(time(nf1-1), time(1), /min)/(nf1-2)
vx=x*0
vy=y*0
delarcsec=fxpar(h, 'XSCALE')

gauss_width = fwhm/2.35/delarcsec   ; in pixels

tstr = fxpar(h, 'COMMENT')
if strtrim(fxpar(h, 'rot_cor'),2) eq 'YES' then tref=fxpar(h, 'REFTIME') else tref=tstr(0)
t_in_min =mdi_time_interval(tstr, tref, /min)
s=size(a(0))
b=fltarr(s(1), s(2), bin+1)
if isvalid(pad_apot) then pad=pad_apot*(s(1)>s(2))


data=fltarr(nx, ny, 7)
; write header part
assowritefits, unit, file=cubefile, header=hdr, /open


if isvalid(maxvel) then begin
pixelrange =  maxvel/(delarcsec*725./(delt*60.))
range =  round(pixelrange)
print, 'range=', range
endif

for k=0, nf1-1 do begin

        if bin gt 1 then begin
        for j=0, bin-1 do b(*,*,j) = assoget(a, k*bin+j, bz, bs)

        for ii=0, s(1)-1 do for jj=0, s(2)-1 do $
         b(ii,jj, bin) = median(b(ii, jj, 0:bin-1))
        tmp=0.
        weight=0.

        if not isvalid(sigma) then sigma = stdev(b(*,*,0)-b(*,*,bin))
        for j=0, bin-1 do begin
          w = abs(b(*,*,j) - b(*,*,bin)) le 3.*sigma
          weight = weight+w
          tmp = tmp + w*b(*,*,j)
        endfor
          mag2 = tmp/weight
        endif else mag2=assoget(a, k, bz, bs)


;  MDI flux density correction

     if keyword_set(flux_correct) then $
              mag2 = mag2/0.64 ; Berger \& Lites 2003
;============================================================


     if k ge 1 then begin

        mag= (mag1+mag2)/2.

        ss=where(abs((mag)(ix,iy)) ge bcrit, count)

        nseg = count/30000
        for iss=0, nseg do begin
        if iss lt nseg then ssseg = ss[iss*30000:(iss+1)*30000-1] $
              else  ssseg=ss[iss*30000:*]
        r = lct(mag1, mag2, ix(ssseg), iy(ssseg), gauss_width, range=range)
        delt =   mdi_time_interval(time(k), time(k-1), /min)
        vx(ssseg) = r(0,*) * (r(2,*) gt cor_crit)*(delarcsec*725./(delt*60.))   ;  km/s
        vy(ssseg) = r(1,*) * (r(2,*) gt cor_crit)*(delarcsec*725./(delt*60.))   ;  km/s
        endfor



    data(*, *,1) = vx
    data(*, *,2) = vy
    data(*, *,0) = interpolate(mag,ix,iy,cubic=-0.5)

    a_pot, mag, ax, ay, pad=pad

    data(*,*,3)  = interpolate(ax, ix, iy, cubic=-0.5)*(delarcsec*0.725)
    data(*,*,4)  = interpolate(ay, ix, iy, cubic=-0.5)*(delarcsec*0.725)

   ax=data(*,*,3)
   ay=data(*,*,4)


;========================================================================
     dbdt = interpolate(mag2-mag1,ix,iy,cubic=-0.5)/(delt*60.)   ; G/s
     bzux = data(*,*,0)*data(*,*,1)/(del*725.)       ; G macropix/s
     bzuy = data(*,*,0)*data(*,*,2)/(del*725.)       ; G macropix/s
     if keyword_set(renew) then bu_from_bt, /renew, dbdt, bzux, bzuy
     data(*,*,5)=bzux*(del*725.)     ; in G km/s
     data(*,*,6)=bzuy*(del*725.)     ;    G km/s
;========================================================================


    if keyword_set(display) then begin

    if k eq 1 then window, 1, xs=nx*mag_factor+1, ys=ny*mag_factor+1
    if k eq 1 then window, 2, xs=nx*mag_factor+1, ys=ny*mag_factor+1
    if k eq 1 then window, 3, xs=nx*mag_factor+1, ys=ny*mag_factor+1

    device,decomposed=0


    wset, 1

    vel_x=data(*,*,1)
    vel_y=data(*,*,2)

    ;vel_x((nx/10),(ny/10)*9)=0.5
    ;vel_y((nx/10),(ny/10)*9)=0.

    v=sqrt((vel_x)^2+(vel_y)^2)
    vunit=0.03
    tmp=data(*,*,0)
    tmp1=stdev(tmp)*4.
    image=bytscl(tmp,-tmp1,tmp1)

;    image_flow_display, image, $
;                    data(*,*,1), data(*, *,2), mag_factor=mag_factor , $
;                    vunit=vunit, condition=abs(data(*,*,0)) ge bcrit and v ge vunit*0.2, sample=2, thick=1
    image_flow_display, image, $
                    vel_x, vel_y, mag_factor=mag_factor , $
                    vunit=vunit, condition= v ge vunit*5, sample=2, thick=1
    tmp_hel_rate=-2*(data(*,*,5)*data(*,*,3)+data(*,*,6)*data(*,*,4))
    tmp1_hel_rate=stdev(tmp_hel_rate)*2
    tmp_hel_rate=bytscl(tmp_hel_rate,-tmp1_hel_rate,tmp1_hel_rate)
    contour,tmp_hel_rate,/overplot, levels=[0.1,0.3,0.5,0.7,0.9]*max(tmp_hel_rate)

;    HAMA=tmp_hel_rate*0.0
;    ok=where(tmp gt 5.0)
;    HAMA[ok]=tmp_hel_rate[ok]/tmp[ok]^2
;    HAMA1=stdev(HAMA)*2
;    HAMA=bytscl(HAMA,-HAMA1,HAMA1)
;    contour,HAMA, /overplot, levels=[0.6,0.7,0.8,0.9]*max(HAMA)

    ;xyouts, 0.8, 0.9, '0.5km/s', charsize=1.2, charthick=2, /norm, font=0
    xyouts, 0.03, 0.93, 'Gray Scale: B_n, Arrows: v_LCT, Contour: G_A', charsize=1.5, charthick=2, /norm, font=0
    xyouts, 0.4, 0.03, strmid(strtrim(t(k),2),0,19), charsize=1.5, charthick=2, /norm, font=0


    wset, 2

    v=sqrt(ax^2+ay^2)
    vunit=0.5*sqrt(stdev(ax)^2+stdev(ay)^2)
     image_flow_display, image, $
                    data(*,*,3), data(*,*,4), mag_factor=mag_factor, $
                    vunit= vunit, $
                    condition=abs(data(*,*,0)) ge bcrit and v ge vunit*0.5, sample=2

    xyouts, 0.03, 0.93, 'Gray Scale: B_z, Arrows: A_p', charsize=1.5, charthick=2, /norm, font=0
    xyouts, 0.4, 0.03, strmid(strtrim(t(k),2),0,19), charsize=1.5, charthick=2, /norm, font=0


    wset, 3

    v=sqrt((vel_x)^2+(vel_y)^2)
    vunit=0.03
    tmp_hel_rate=-2*(data(*,*,5)*data(*,*,3)+data(*,*,6)*data(*,*,4))
    tmp1_hel_rate=stdev(tmp_hel_rate)*4
    image2=bytscl(tmp_hel_rate,-tmp1_hel_rate,tmp1_hel_rate)
    image_flow_display, image2, $
                    vel_x, vel_y, mag_factor=mag_factor , $
                    vunit=vunit, condition= v ge vunit*5, sample=2, thick=1

    ;xyouts, 0.8, 0.9, '0.5km/s', charsize=1.2, charthick=2, /norm, font=0
    xyouts, 0.03, 0.93, 'Gray Scale: G_A, Arrows: v_LCT', charsize=1.5, charthick=2, /norm, font=0
    xyouts, 0.4, 0.03, strmid(strtrim(t(k),2),0,19), charsize=1.5, charthick=2, /norm, font=0
    ;xyouts, 0.8, 0.9, STRCOMPRESS(string(k+1),/remove_all)+'/'+STRCOMPRESS(string(nf1),/remove_all), charsize=0.8, charthick=1.2, /norm
    wait, 0.2



    ;; To make plots in the form of eps

      if k eq 2 then begin
;
;       Set_plot, 'ps'
;
;       Device,  /bold, /times, /encapsulated,$
;               filename = 'C:\Documents and Settings\Sung-Hong Park\My Documents\My Documents\Helicity\Result Data\10030\vel_mag_hel.eps',$
;               /color, xsize=21, ysize=21*ny/nx, xoff = 0, yoff = 0
;
;        data(12,18,1)=0.5
;        data(12,18,2)=0.
;        v=sqrt(vx^2+vy^2)
;        v(12,18)=0.5
;        vunit=0.05
;        tmp=data(*,*,0)
;        tmp1=stdev(tmp)*2
;        data(12,18,0)=-30
;        tmp (12,18)=-30
;        tmp (11,18)=-30
;        tmp (10,18)=-30
;
;        image_flow_display, bytscl(tmp,-tmp1,tmp1), $
;                    data(*,*,1), data(*, *,2), mag_factor=70 , $
;                    vunit=vunit, condition=abs(data(*,*,0)) ge bcrit and v ge vunit*0.2, sample=2, thick=1
;
;        tmp_hel_rate=-2*(data(*,*,5)*data(*,*,3)+data(*,*,6)*data(*,*,4))
;        tmp1_hel_rate=stdev(tmp_hel_rate)*2
;        tmp_hel_rate=bytscl(tmp_hel_rate,-tmp1_hel_rate,tmp1_hel_rate)
;        contour,tmp_hel_rate,/overplot, levels=[0.1,0.3,0.5,0.7,0.9]*max(tmp_hel_rate)

;        HAMA=tmp_hel_rate*0.0
;        ok=where(tmp gt 5.0)
;        HAMA[ok]=tmp_hel_rate[ok]/tmp[ok]^2
;        HAMA1=stdev(HAMA)*2
;        HAMA=bytscl(HAMA,-HAMA1,HAMA1)
;        contour,HAMA, /overplot, levels=[0.6,0.7,0.8,0.9]*max(HAMA)
;        xyouts, 0.01, 0.1, '0.5km/s', charsize=1.3, charthick=2.5, color=255, /norm
;        xyouts, 0.01, 0.68, 'Gray scale: B!dz!n, Arrows: v!dLCT!n, Contour: n', charsize=1.5, charthick=2.8, color=255, /norm
;        xyouts, 0.4, 0.03, strmid(strtrim(t(k),2),0,19), charsize=1.5, charthick=2.8, color=255, /norm

;        xyouts, 0.01, 0.1, '0.5km/s', charsize=1.2, charthick=2.5, color=255, /norm
;        xyouts, 0.01, 0.55, 'Gray scale: B!dz!n, Arrows: v!dLCT!n, Contour: G', charsize=1.5, charthick=2.8, color=255, /norm
;        xyouts, 0.25, 0.02, strmid(strtrim(t(k),2),0,19), charsize=1.5, charthick=2.8, color=255, /norm
;
;
;        device, /close
;        set_plot, 'win'


    ;   Set_plot, 'ps'

    ;   Device, Bits_per_pixel = 8, /bold, /times, /encapsulated,$
    ;           filename = 'C:\Documents and Settings\Sung-Hong Park\My Documents\My Documents\Helicity\Result Data\10030\vpot_mag_image'+strtrim(k+1,2)+'.eps', /color, xsize = 11,$
    ;           ysize = 12*ny/nx

    ;    v=sqrt(ax^2+ay^2)
    ;    vunit=0.5*sqrt(stdev(ax)^2+stdev(ay)^2)
    ;    tmp=data(*,*,0)
    ;    tmp1=stdev(tmp)*2

    ;    image_flow_display, bytscl(tmp, -tmp1, tmp1), $
    ;                    data(*,*,3), data(*,*,4), mag_factor=50, $
    ;                    vunit= vunit, condition=abs(data(*,*,0)) ge bcrit and v ge vunit*0.5, sample=2, thick=1

    ;    xyouts, 0.05, 0.95, 'Gray Scale: B!dz!n, Arrows: A!dp!n', charsize=1, charthick=2.5,color=255, /norm
    ;    xyouts, 0.6, 0.03, strmid(strtrim(t(k),2),0,19), charsize=1, charthick=2.5,color=255, /norm

    ;    device, /close
    ;    set_plot, 'win'



;       Set_plot, 'ps'
;
;       Device, Bits_per_pixel = 8, /bold, /times, /encapsulated,$
;               filename = 'C:\Documents and Settings\Sung-Hong Park\My Documents\My Documents\Helicity\Result Data\vel_mag_image'+strtrim(k+1,2)+'_2.eps', /color, xs=1.5, ys= 1.3
;        data(6,18,1)=0.5
;        data(6,18,2)=0
;        vunit=0.05
;        v=sqrt(vx^2+vy^2)
;        v(6,18)=0.5
;        data(6,18,0)=-80
;        data(6,19,0)=-80
;        tmp=data(*,*,0)
;        tmp(6,18)=-10
;        tmp(6,19)=-8
;        tmp(6,17)=-7
;        tmp1=stdev(tmp)*2
;        image=bytscl(tmp,-tmp1,tmp1)
;        image(6,18)=100
;        image_flow_display, image, $
;                        data(*,*,1), data(*, *,2), mag_factor=20 , $
;                        vunit=vunit, condition=abs(data(*,*,0)) ge bcrit and v ge vunit*0.5, sample=2, thick=1
;
;        xyouts, 0.06, 0.2, '0.5km/s', charsize=0.2, charthick=1, color=255, /norm
;        xyouts, 0.02, 0.85, 'Gray Scale: B!dz!n, Arrows: v!dh!n', charsize=0.25, charthick=1, color=255, /norm
;        xyouts, 0.25, 0.04, strmid(strtrim(t(k),2),0,19), charsize=0.25, charthick=1, color=255, /norm
;
;        device, /close
;        set_plot, 'win'
;
;       Set_plot, 'ps'
;
;       Device, Bits_per_pixel = 8, /bold, /times, /encapsulated,$
;               filename = 'C:\Documents and Settings\Sung-Hong Park\My Documents\My Documents\Helicity\Result Data\vel_mag_image'+strtrim(k+1,2)+'_1.eps', /color, xs=4.7, ys= 4
;        data(12,30,1)=0.5
;        data(12,30,2)=0
;        vunit=0.05
;        v=sqrt(vx^2+vy^2)
;        v(12,30)=0.5
;        data(12,30,0)=-80
;        data(12,31,0)=-70
;        data(11,29,0)=-60
;        tmp=data(*,*,0)
;        tmp(12,30)=-30
;        tmp(12,31)=-20
;        tmp(12,29)=-10
;        tmp1=stdev(tmp)*2
;        image=bytscl(tmp,-tmp1,tmp1)
;        image(12,30)=100
;        image_flow_display, image, $
;                        data(*,*,1), data(*, *,2), mag_factor=20 , $
;                        vunit=vunit, condition=abs(data(*,*,0)) ge bcrit and v ge vunit*0.5, sample=2, thick=1
;
;        xyouts, 0.02, 0.10, '0.5km/s', charsize=0.4, charthick=2, color=255, /norm
;        xyouts, 0.02, 0.9, 'Gray Scale: B!dz!n, Arrows: v!dh!n', charsize=0.5, charthick=2, color=255, /norm
;        xyouts, 0.4, 0.04, strmid(strtrim(t(k),2),0,19), charsize=0.5, charthick=2, color=255, /norm
;
;        device, /close
;        set_plot, 'win'



        endif


    endif



    assowritefits, unit, data=data

    endif

   mag1 = mag2

   endfor
   assowritefits, unit, header=hdr, /close
   close, unit1  & free_lun, unit1 & free_lun, unit


end