pro mdi_remap,  mdifiles, xr, yr, del,  cubefile, nsr=nsr, ewr=ewr, reftime=reftime, $
          xc=xc, yc=yc,  display=display, ar_num=ar_num
;+
;  NAME:  MDI_REMAP
;  PURPOSE:  make  a data cube from a series of MDI files
;  CALLING SEQUENCE:
;          MDI_REMAP, mdifile, xr, yr, del,  cubefile
;  INPUTS:
;          mdifiles      an array of MDI magnetogram files
;          xr         x-range (in arcseconds)
;          yr         y-range (in arcseconds)
;          del        sampling size (in arcseconds)
;          cubefile      a data cube file
;
;  KEYWORD INPUTS:
;         reftime    the epoch
;         xc, yc     the x- and y- positions from disk center at the epoch (arcseconds)
;                        if set and specified, the following nsr and ewr are calculated as outputs
;         nsr, ewr    latitude and longitude of the reference point at the epoch
;                      inputs when xc and yc are not specified
;                      outputs when xc ans yc are specified
;         DISPLAY      if set, display images
; HISTORY
;       2002 January :   Remapping into the local surface frame
;                                  x is in the l-increasing direction
;                                  y is in the b-increasing direction
;                                  z is in the local vertical direction
;                              the reference point is tracked down taking
;                              into account the differential rotation of the point
;       2002 July
;                                Do not  the host-to-ieee conversion in "mid_remap"
;                                 New optional keyword "swap" is defined in function "assoget"
;                                Instead,  new keyword  "swap_if_little_endian" is set  in
;                                               the function "assoreadfits"
;
;-


nf = n_elements(mdifiles)
if not isvalid(reftime) then reftime=fxpar(headfits(mdifiles(0)), 'T_OBS')

nx = round((xr(1)-xr(0))/del)+1
ny = round((yr(1)-yr(0))/del)+1
x= reform((findgen(nx)*del+xr(0))#replicate(1., ny), nx*ny)
y= reform(replicate(1., nx)#(findgen(ny)*del+yr(0)), nx*ny)

if isvalid(xc) and isvalid(yc) then begin

tmp = get_sun(mdi_time_ccsds(reftime), he_lat=b0, he_lon=l0, sd=sd, pa=p)
hel = arcmin2hel(xc/60., yc/60., b0=b0(0), l0=l0(0), r0=206265./sd, p=p(0))
nsr=hel(0,0)
ewr=hel(1,0) ;+l0(0)
;print, 'ewr =', ewr, ' nsr =', nsr
endif


bscale=0.1

; make header

fxhmake, hdr, intarr(nx, ny)
fxaddpar, hdr, 'NAXIS', 3
fxaddpar, hdr, 'NAXIS3', nf
fxaddpar, hdr, 'BZERO', 0.
fxaddpar, hdr, 'BSCALE', BSCALE
fxaddpar, hdr, 'XSCALE', del, 'in arc seconds'
fxaddpar, hdr, 'YSCALE', del, 'in arc seconds'
fxaddpar, hdr, 'XCEN',  xr(0)+del*nx*0.5, 'center position (arcsec) from reference'
fxaddpar, hdr, 'PLATE_X', nx*0.5, 'center position (pixel) in image'
fxaddpar, hdr, 'YCEN', yr(0)+del*ny*0.5
fxaddpar, hdr, 'PLATE_Y', ny*0.5
fxaddpar, hdr, 'NSR', nsr, 'lattiude of reference point in degrees'
fxaddpar, hdr, 'EWR', ewr, 'longitude of reference in degrees'
fxaddpar, hdr, 'REFTIME',  reftime
fxaddpar, hdr, 'ROT_COR', 'YES', 'for the center of field of view'
fxaddpar, hdr, 'VERTICAL', 'YES', 'Geometric correction'

time=strarr(nf)
bb=fltarr(nx,ny,nf)

for k=0, nf-1 do begin
tmp=headfits(mdifiles(k))
time(k) = fxpar(tmp,'T_OBS')
fxaddpar, hdr, 'COMMENT', time(k)
endfor

;  write header part
assowritefits, unit, file=cubefile, header=hdr, /open

;  write data part

;if keyword_set(display) then window, /free, xs=nx, ys=ny

window,10, xs=nx, ys=ny

for k=0, nf-1 do begin

 a=readfits(mdifiles(k), /sil, h) ;using print,k to check the file
 if round(fxpar(h, 'SOLAR_P')) eq -180 or   round(fxpar(h, 'SOLAR_P')) eq 180 then $
   a= rotate(a, 2)
tmp = get_sun(mdi_time_ccsds(fxpar(h, 'T_OBS')), he_lat=b0, he_lon=l0, sd=sd)
b0=b0(0) & l0=l0(0) & sd=sd(0)
interval= mdi_time_interval(time(k), reftime, /sec)
   ; (strmid(time(k), 8, 2)*1.-strmid(reftime, 8, 2))*24.*3600. + $
   ;                (strmid(time(k), 11, 2)*1.-strmid(reftime, 11, 2))*3600. + $
   ;           (strmid(time(k), 14, 2)*1.-strmid(reftime, 14, 2))*60. + $
   ;                (strmid(time(k), 17, 2)*1.-strmid(reftime, 17, 2))
ddays = interval/(24*3600.)
;print, 'ddays=', ddays
ewr1 = ewr +1*(diff_rot(ddays, nsr, /howard)    - diff_rot(ddays, 0, /howard) )
snsr = sin(nsr*!dtor)
hel = arcmin2hel(x/60.,y/60., b0=nsr, l0=ewr1, r0=206265./sd, p=0.)
arcmin = hel2arcmin(hel(0,*), hel(1,*), b0=b0, l0=l0, p=0., r0=206265./sd)
cospsi = cos(b0*!dtor)*cos(hel(0,*)*!dtor)*cos((hel(1,*)-l0)*!dtor) + $
                sin(b0*!dtor)*sin(hel(0,*)*!dtor)
 xsolar = arcmin(0,*)*60.
 ysolar = arcmin(1,*)*60.
 xx=(xsolar-fxpar(h,'XCEN'))/fxpar(h, 'XSCALE')+fxpar(h, 'PLATE_X')
 yy=(ysolar-fxpar(h,'YCEN'))/fxpar(h, 'YSCALE')+fxpar(h, 'PLATE_Y')
 xx=reform(xx, nx, ny)
 yy=reform(yy, nx, ny)
cospsi = reform(cospsi, nx, ny)
data = fix(round(interpolate(a, xx, yy, missing=0.)/(bscale*cospsi)))

bb(*,*,k)=data

;to display
wset,10

if keyword_set(display) then begin
 tv, bytscl(data, -100/bscale, 100/bscale)
 xyouts, 0.1, 0.05, strmid(strtrim(time(k),2),0,19), charsize=0.7, charthick=3, /norm, $
         font = 0
 xyouts, /norm, 0.72, 0.9, strtrim(string(k+1),2)+'/'+strtrim(string(nf),2)+' '

wait,0.8 ;for test
endif

assowritefits, unit, data=data

endfor

;save, bb, filename='C:\Documents and Settings\SUNG-HONG PARK\Desktop\test\AR10930\b_map_10930.sav'
assowritefits, unit, header=hdr, /close


end
