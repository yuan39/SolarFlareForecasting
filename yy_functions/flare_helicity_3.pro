pro flare_helicity_3, ar_num,lat,lon,path1,flareIndex_24,flareIndex_48,avg_dhdt,avg_f, $
                    pro_24_c,pro_48_c,pro_24_m,pro_48_m,pro_24_x,pro_48_x, del_map=del_map, method=method, fwhm=fwhm

;+
;
;
; PURPOSE:  1. To calculate "helicity injection rate" and "helicity accumulation"
;              through the photospheric surface of an interested active region
;           2. To calculate "flare index" and "flare-productive probability" in the active region
;
;
; CALLING SEQUENCE:
;           flare_helicity, ar_num
;
; INPUT:   ** ar_num is just for making a name of cube files. **
;          1. ar_num : NOAA active region number (a string) (e.g. if the active region is NOAA 10491, then ar_num is '10491')
;          2. year : four digit integer
;          3. month : 1 or 2 digit integer
;          4. day : 1 or 2 digit integer
; OPTIONAL CHANGE
;
;
; KEYWORD INPUT:
;         1. del_map : the sampling size in remapping the magnetograms of the active region (in arcseconds; default=2.)
;         2. method  : the method for caculating helicity injection rate (1=LCT+G_A, 2=DAVE+G_theta; default=1)
;         3. fwhm    : full width half maximum of apodizing window for LCT or DAVE (in arcseconds; default=10.)
;         
; OUTPUT:
;         1. two cube files:
;             a. helicity_'ar_num'_first.cube: the aligned magnetogram maps of the active region used for mdi_vel_apot1.pro
;             b. helicity_'ar_num'_second.cube: the magnetic field, vector potential field, and velocity field used for mdi_helicity_rate2.pro
;
; WRITTEN: August 2010  Sung-Hong Park
;
; MODIFICATION HISTORY
;
;
;-

if not isvalid(del_map) then del_map=2.
if not isvalid(method) then method=1
if not isvalid(fwhm) then fwhm=10.
; available fits file search
f=findfile(path1+'*.fits')
;f=select_available_file(f)

; active region location and size selection
a=readfits(f[0],hr)
s=size(a)
if round(fxpar(hr, 'SOLAR_P')) eq -180 or   round(fxpar(hr, 'SOLAR_P')) eq 180 then  a= rotate(a, 2)
reftime=fxpar(hr,'t_obs')

;window,0,xsize=s(1)/2,ysize=s(2)/2
;tvscl,rebin(a,s(1)/2,s(2)/2)>(-200.)<(200.)
;box_cursor,x,y,nx,ny,/message
;x=(x+nx/2)*2 & y=(y+ny/2)*2 ; AR center coordinates in pixel, with respect to the lower-left corner of the image



;xc=(x-fxpar(hr,'plate_x'))*fxpar(hr,'xscale')+fxpar(hr,'xcen') ; in arsec with respect to disk center
;yc=(y-fxpar(hr,'plate_y'))*fxpar(hr,'yscale')+fxpar(hr,'ycen') ; in arsec with respect to disk center
xc = lon
yc = lat
nx = 100
ny = 100
nx=nx*4 & ny=ny*4 ; in arcsecond (1pix=2arc)
xr=[-nx/2,nx/2] & yr=[-ny/2,ny/2]


path2='/var/www/flares/MDI/helicity_' ; directory for saving output files

; to make the aligned magnetograms of the active region
mdi_remap, f, xr, yr, del_map, path2+ar_num+'_first.cube', reftime=reftime, xc=xc, yc=yc, /display

del=del_map
fwhm=10


if method eq 1 then begin

mdi_vel_apot1, path2+ar_num+'_first.cube', xr, yr, del, path2+ar_num+'_second.cube', fwhm=fwhm, bin_time=bin_time, bcrit=bcrit, $
              cor_crit=cor_crit;, /display

mdi_helicity_rate2, path2+ar_num+'_second.cube', time,  rate, reftime=reftime, n=n

endif



if method eq 2 then begin

mdi_vel_gtheta, path2+ar_num+'_first.cube', path2+ar_num+'_second.cube', fwhm=fwhm, bin_time=bin_time, $
      bcrit=bcrit, cor_crit=cor_crit;, /display

mdi_helicity_rate_gtheta, path2+ar_num+'_second.cube', time,  rate, reftime=reftime, n=n

endif


refd=float(strmid(reftime,8,2))+float(strmid(reftime,11,2))/24.+float(strmid(reftime,14,2))/60./24.

;t=[refd,refd+time/24./60.] ; day
t=[0.,0.+time/24./60.] ; day

; helicity accumulation calculution
  n=n_elements(time)

  h=fltarr(n+1)
  h[0]=0
  new_t=fltarr(n+1)
  new_t=[0,time]

  for k=0, n-2 do begin
  if k eq 0 then begin
   h[k+1]=h[k]+(rate[k,0])*(new_t[k+1]-new_t[k])/60.
  endif
     h[k+2]=h[k+1]+0.5*(rate[k+1,0]+rate[k,0])*(new_t[k+2]-new_t[k+1])/60. ; 10^40 Mx^2
  endfor

; unsigned flux calculation
  uf=abs(rate(*,1))+abs(rate(*,2)) ; in 10^20 Mx
  flux=[uf(0), uf]

; average helicity injection rate and average unsigned flux
  avg=moment(rate(*,0))
  avg_dhdt=abs(avg(0))
  avg=moment(flux)
  avg_f=abs(avg(0))

; to plot the time profile of helicity accumulation
;window, 11
;plot, t, h
;xyouts, 0.6, 0.8, reftime

print, ''
print, '============= Average Magnetic Parameters ============='
print, 'Average helicity injection rate [10^40 Mx^2/hr]=', avg_dhdt
print, 'Average unsigned flux [10^20 Mx]=', avg_f

; to calculate flare index
path3='/var/www/flares/data_samples/' ; directory of the save file 'dhdt_f_idx.sav'
restore,path3+'dhdt_f_idx.sav'

x = dhdt
y = f_idx_24
index = WHERE(y, count)
xx = x[index]
yy = y[index]
logx = alog10(xx)
logy = alog10(yy)
result = LINFIT(logx, logy)
today_x = avg_dhdt
today_y = 10^result[0]*today_x^result[1]
flareIndex_24 = today_y
print, ''
print, '============= Flare Index ============='
print,'0-24 hr flare index:', today_y

x = dhdt
y = f_idx_48
index = WHERE(y, count)
xx = x[index]
yy = y[index]
logx = alog10(xx)
logy = alog10(yy)
result = LINFIT(logx, logy)
today_y = 10^result[0]*today_x^result[1]
print,'24-48 hr flare index:', today_y
flareIndex_48 = today_y
; to calculate flare-productive probability
p=avg_dhdt
n=n_elements(p)

pro_24_c=1.
pro_24_m=1.
pro_24_x=1.
pro_48_c=1.
pro_48_m=1.
pro_48_x=1.

for i=0, n-1 do begin

pro_24_1=1./(1.+exp(1.5156-0.0626*p(i)))
pro_24_2=1./(1.+exp(3.0971-0.0423*p(i)))
pro_24_3=1./(1.+exp(5.5683-0.038*p(i)))
pro_48_1=1./(1.+exp(1.6419-0.0667*p(i)))
pro_48_2=1./(1.+exp(3.086-0.0392*p(i)))
pro_48_3=1./(1.+exp(5.7648-0.0579*p(i)))

pro_24_c=(1-pro_24_1)*pro_24_c
pro_24_m=(1-pro_24_2)*pro_24_m
pro_24_x=(1-pro_24_3)*pro_24_x
pro_48_c=(1-pro_48_1)*pro_48_c
pro_48_m=(1-pro_48_2)*pro_48_m
pro_48_x=(1-pro_48_3)*pro_48_x

endfor

pro_24_c=1-pro_24_c
pro_24_m=1-pro_24_m
pro_24_x=1-pro_24_x
pro_48_c=1-pro_48_c
pro_48_m=1-pro_48_m
pro_48_x=1-pro_48_x

print, ''
print, '============= Flare-Productive Probability ============='
print, 'C_24_Prob [%] =', round(100*pro_24_c)
print, 'M_24_Prob [%] =', round(100*pro_24_m)
print, 'X_24_Prob [%] =', round(100*pro_24_x)
print, 'C_48_Prob [%] =', round(100*pro_48_c)
print, 'M_48_Prob [%] =', round(100*pro_48_m)
print, 'X_48_Prob [%] =', round(100*pro_48_x)

end