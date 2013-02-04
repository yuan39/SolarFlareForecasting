function Gtheta_M_from_u, Bn, u, v, delarcsec
s=size(Bn)
nx=s(1)
ny=s(2)


nx2=nx*2
ny2=ny*2


xx = shift((findgen(nx2)-nx2/2) # replicate(1., ny2), -nx2/2, -ny2/2)
yy = shift(replicate(1., nx2) # (findgen(ny2)-ny2/2), -nx2/2, -ny2/2)
r2=(xx^2 + yy^2)>1.


ff=fft(xx/r2, 1)
fg=fft(yy/r2, 1)

buffer = fltarr(nx2, ny2)
result=0.

buffer(0:nx-1, 0:ny-1) = Bn
fb = fft(buffer, 1)

result = v* (float(fft(ff*fb, -1)))[0:nx-1, 0:ny-1]
result=result - u*(float(fft(fg*fb, -1)))[0:nx-1, 0:ny-1]
buffer(0:nx-1, 0:ny-1) = Bn*v
fbv = fft(buffer, 1)
result=result -(float(fft(ff*fbv, -1)))[0:nx-1, 0:ny-1]
buffer(0:nx-1, 0:ny-1) = Bn*u
fbu = fft(buffer, 1)
result=result +(float(fft(fg*fbu, -1)))[0:nx-1, 0:ny-1]


result = -(bn/(2*!pi))*result*(delarcsec*0.725) ; in G^2 km/s Mm
return, result

end
pro mdi_vel_gtheta,  mdicubefile,  cubefile,  fwhm = fwhm, bin_time=bin_time, $
     display=display, bcrit=bcrit, maxvel=maxvel, mag_factor=mag_factor, $
     cor_crit=cor_crit, pad_apot=pad_apot, renew=renew, flux_correct=flux_correct, method=method

;+
;  NAME: mdi_vel_getheta
;  PURPOSE: make a cube for horizontal velocity and gtheta
;  CALLING SEQUENCE:
;
;  INPUTS:
;      mdicubefile    a cube file of MDI magnetograms
;      cubefile       a cube file for velocity data
;                            it contains the floating 5 images per each set
;                               data(5*k+0) :  magnetogram in unit G
;                               data(5*k+1) :  v_x in unit of     km /s
;                               data(5*k+2) :  v_y in unit of     km/s
;                               data(5*k+3) :  G_theta^M in unit of G^2 km/s Mm
;                               data(5*k+4) :  G_theta^S in unit of G^2 km/s Mm
;
;
; KEYWORD INPUTS:
;           fwhm        Full width half maximum of apodizing window in arc seconds
;           bin_time    time duration for averaging  magnetograms for velocity fields
;                                   equal to the time interval for cross-correlation (in minutes)
;           method      LCT or DAVE (default=DAVE)
;
; HISTORY
;
;
;            2006 November, J. Chae:   first coded.
;                                  References: 2006 COSPAR paper to be submitted in Advances in Space Research
;
;
;
; -
if not isvalid(bcrit) then bcrit=5.
if not isvalid(bbig) then bbig=200.
if not isvalid(vunit) then vunit=0.1
if not isvalid(mag_factor) then mag_factor=3
if not isvalid(cor_crit) then cor_crit=0.7
if not isvalid(method) then method='DAVE'


;; to make save files of g_theta_M and g_theta_S

path='C:\Documents and Settings\SUNG-HONG PARK\Desktop\test\AR10696\gtheta_'



a=assoreadfits(mdicubefile, bz, bs, h,unit=unit1,  nf=nf)

t=fxpar(h, 'COMMENT')

nx = fxpar(h, 'NAXIS1') ;round((xr(1)-xr(0))/del)+1
ny = fxpar(h, 'NAXIS2') ; round((yr(1)-yr(0))/del)+1
del = fxpar(h, 'XSCALE'); x= (findgen(nx)*del+xr(0))#replicate(1., ny)
;    y= replicate(1., nx)#(findgen(ny)*del+yr(0))
if isvalid(bin_time) then $
  bin =round(bin_time/ ( mdi_time_interval(t(nf-1), t(0), /min)/(nf-1))) $
  else bin=1
;ix = (x - fxpar(h, 'XCEN'))/fxpar(h,'XSCALE')+fxpar(h, 'PLATE_X')
;iy= (y - fxpar(h, 'YCEN'))/fxpar(h,'YSCALE')+fxpar(h, 'PLATE_Y')

nf1=nf/bin-1+(nf mod bin eq 0)
fxhmake, hdr, fltarr(nx, ny)
fxaddpar, hdr, 'NAXIS', 3
fxaddpar, hdr, 'NAXIS3', (nf1-1)*7
fxaddpar, hdr, 'XSCALE', del, 'in arc seconds'
fxaddpar, hdr, 'YSCALE', del, 'in arc seconds'
fxaddpar, hdr, 'XCEN', fxpar(h, 'XCEN'), 'center position (arcsec) from reference'
fxaddpar, hdr, 'PLATE_X',fxpar(h, 'PLATE_X'), 'center position (pixel) in image'
fxaddpar, hdr, 'YCEN', fxpar(h, 'YCEN')
fxaddpar, hdr, 'PLATE_Y', fxpar(h, 'PLATE_Y')
fxaddpar, hdr, 'NSR', fxpar(h, 'nsr'), 'lattiude of reference point in degrees'
fxaddpar, hdr, 'EWR', fxpar(h, 'ewr'), 'longitude of reference in degrees'
fxaddpar, hdr, 'REFTIME', fxpar(h, 'reftime'), 'the epoch'
time=strarr(nf1)
for k=1, nf1-1 do begin
time(k) = t(k*bin)
fxaddpar, hdr, 'COMMENT', time(k)
endfor
time(0)=t(0)
delt =   mdi_time_interval(time(nf1-1), time(1), /min)/(nf1-2)

tflt= mdi_time_interval(t, time(0), /min)
vx=fltarr(nx, ny)
vy=fltarr(nx, ny)
delarcsec=fxpar(h, 'XSCALE')


gauss_width = fwhm/2.35/delarcsec   ; in pixels

tstr = fxpar(h, 'COMMENT')
if strtrim(fxpar(h, 'rot_cor'),2) eq 'YES' then tref=fxpar(h, 'REFTIME') else tref=tstr(0)
t_in_min =mdi_time_interval(tstr, tref, /min)
s=size(a(0))
b=fltarr(s(1), s(2), bin+1)
if isvalid(pad_apot) then pad=pad_apot*(s(1)>s(2))


data=fltarr(nx, ny, 5)
; write header part
assowritefits, unit, file=cubefile, header=hdr, /open


if isvalid(maxvel) then begin
pixelrange =  maxvel/(delarcsec*725./(delt*60.))
range =  round(pixelrange)>1
print, 'range=', range
endif


tt1=fltarr(nx, ny)


for k=0, nf1-1 do begin

        if bin gt 1 then begin
        for j=0, bin-1 do b(*,*,j) = assoget(a, k*bin+j, bz, bs)
        for ii=0, s(1)-1 do for jj=0, s(2)-1 do $
         b(ii,jj, bin) = median(b(ii, jj, 0:bin-1))
         tmp=0.
        weight=0.
         t_tmp=0.
         if not isvalid(sigma) then sigma = stdev(b(*,*,0)-b(*,*,bin))
        for j=0, bin-1 do begin
          w = abs(b(*,*,j) - b(*,*,bin)) le 3.*sigma
          weight = weight+w
          tmp = tmp + w*b(*,*,j)
          t_tmp = t_tmp + w*tflt(k*bin+j)
        endfor

         mag2 = tmp/weight
         tt2 = median(t_tmp/weight)
        endif else begin
        mag2=  assoget(a, k*bin, bz, bs)
        tt2=tflt(k*bin)
        endelse

;  MDI flux density correction

     if keyword_set(flux_correct) then $
              mag2 = mag2/0.64 ; Berger \& Lites 2003
;============================================================

     if k ge 1 then begin


        mag= (mag1+mag2)/2.

        if method eq 'LCT' then  r=lct_multi(fwhm/delarcsec, mag1, mag2, range=range)
        if method eq 'DAVE' then r=dave_multi(fwhm/delarcsec, mag1, mag2)
        if method eq 'NAVE' then r=nave_multi(fwhm/delarcsec, mag1, mag2, samp=1)

        delt =  (tt2-tt1)
        u =  r(0,*,*) * (delarcsec*725./(delt*60.))  ;  km/s
        v = r(1,*,*) * (delarcsec*725./(delt*60.))   ;  km/s

        if method eq 'LCT' then begin
        uy=0.
        vx=0.
        endif else begin
        uy = r(4,*,*) /(delt*60.) * 1000 ;  km/s/Mm
        vx = r(5,*,*)/(delt*60.)  * 1000  ; km/s/Mm
        endelse
    data(*, *,1) = u
    data(*, *,2) = v
    data(*, *,0) = mag ; interpolate(mag,ix,iy,cubic=-0.5)



    data(*,*,3)  = Gtheta_M_from_u(mag, u, v, delarcsec)  ; G^2 km/s Mm
    data(*,*,4)  = - mag^2*(vx-uy)*(delarcsec*0.725)^2 ;  G^2  km/s Mm





    if keyword_set(display) then begin

    if k eq 1 then window, 0, xs=nx*mag_factor+1, ys=ny*mag_factor+1
    if k eq 1 then window, 1, xs=nx*mag_factor+1, ys=ny*mag_factor+1
    if k eq 1 then window, 2, xs=nx*mag_factor+1, ys=ny*mag_factor+1

;    if k eq 3 then begin

    wset, 0

    v=sqrt(vx^2+vy^2)
    vunit=0.1
    image_flow_display, bytscl(data(*,*,0), -bbig,  bbig), $
                     data(*,*,1), data(*, *,2), mag_factor=mag_factor , $
                     vunit=vunit, $
                     condition=abs(data(*,*,0)) ge bcrit and v ge vunit*0.2, sample=2

    xyouts, 0.03, 0.93, 'Gray Scale: B_n, Arrows: v_DAVE', charsize=1.5, charthick=2, /norm, font=0
    xyouts, 0.4, 0.03, strmid(strtrim(t(k),2),0,19), charsize=1.5, charthick=2, /norm, font=0


;    Set_plot, 'ps'
;
;       Device,  /bold, /times, /encapsulated,$
;               filename = 'C:\Documents and Settings\SUNG-HONG PARK\Desktop\vel_mag_hel.eps',$
;               /color, xsize=514*3, ysize=426*3, xoff = 1, yoff = 1
;
;
;
;    v=sqrt((data(*,*,1))^2+(data(*,*,2))^2)
;    vunit=0.02
;
;
;    tmp=data(*,*,0)
;    tmp1=stdev(tmp)*2.
;    image=bytscl(tmp,-tmp1, tmp1)
;
;    image_flow_display, image, $
;                    data(*,*,1), data(*,*,2), mag_factor=mag_factor , $
;                    vunit=vunit, condition=abs(data(*,*,0)) ge 150 and v ge vunit*0.8, sample=3, thick=0.8
;
;        device, /close
;        set_plot, 'win'

    wset, 1
    tv, congrid(bytscl(data(*,*,3), max=max(abs(data(*,*,3))), min=-max(abs(data(*,*,3)))), nx*mag_factor+1, ny*mag_factor+1)
    xyouts, 0.03, 0.93, 'Gray Scale: Mutual G_theta', charsize=1.5, charthick=2, /norm, font=0
    xyouts, 0.4, 0.03, strmid(strtrim(t(k),2),0,19), charsize=1.5, charthick=2, /norm, font=0

    wset, 2
    tv, congrid(bytscl(data(*,*,4), max=max(abs(data(*,*,4))), min=-max(abs(data(*,*,4)))), nx*mag_factor+1, ny*mag_factor+1)
    xyouts, 0.03, 0.93, 'Gray Scale: Self G_theta', charsize=1.5, charthick=2, /norm, font=0
    xyouts, 0.4, 0.03, strmid(strtrim(t(k),2),0,19), charsize=1.5, charthick=2, /norm, font=0
    wait, 0.2
;    print, t(k)

;    save, data, t, filename=path+strtrim(string(k),2)+'.sav'
    endif
;    endif



    assowritefits, unit, data=data



     endif
   mag1 = mag2
   tt1  = tt2
   endfor
   assowritefits, unit, header=hdr, /close
   close, unit1  & free_lun, unit1 & free_lun, unit





end