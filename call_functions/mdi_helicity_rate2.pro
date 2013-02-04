pro mdi_helicity_rate2,  vacubefile, t,  rate, reftime=reftime, n=n
;
;
;

v=assoreadfits(vacubefile, bzv, bsv, hv, nf=nfv, unit=unitv)

if not isvalid(reftime) then reftime= fxpar(hv, 'REFTIME')
t=mdi_time_interval(fxpar(hv, 'comment'), reftime, /min)
if n_elements(n) eq 0 then n=n_elements(t)

rate=fltarr(n, 5)
t=t[0:n-1]
area_unit =fxpar(hv, 'xscale')*fxpar(hv, 'yscale')

for k=0, n-1 do begin

bz = assoget(v, k*7, bzv, bsv)    ; in G

vx = assoget(v, k*7+1, bzv, bsv)  ; in km/s

vy = assoget(v, k*7+2,bzv, bsv)   ; in km/s

ax = assoget(v, k*7+3, bzv, bsv)  ; in G Mm

ay = assoget(v, k*7+4, bzv, bsv)  ; in G Mm

bzux = bz*vx ; assoget(v, k*7+5, bzv, bsv) ; in G km/s
bzuy = bz*vy ; assoget(v, k*7+6, bzv, bsv) ; in G km/s



factor = area_unit*(0.725e8)^2/1.e20
factor = factor*(1.e5*3600.)*(1.e8)/1.e20
g=-2.*(bzux*ax+bzuy*ay)*factor
sm_g=smooth(g,10)
sm_g1=stdev(sm_g)*2
sm_g=bytscl(sm_g,-sm_g1,sm_g1)
;tv,sm_g
;wait, 0.3

;       Set_plot, 'ps'
;
;       Device, Bits_per_pixel = 8, /bold, /times, /encapsulated,$
;               filename = 'C:\Documents and Settings\Sung-Hong Park\My Documents\My Documents\Helicity\Result Data\10696\helicity_rate_map_10696_'+strtrim(k+1,2)+'.eps', /color, xs=8, ys= 7
;
;       factor = area_unit*(0.725e8)^2/1.e20
;       factor = factor*(1.e5*3600.)*(1.e8)/1.e20
;       g=-2.*(bzux*ax+bzuy*ay)*factor
;       sm_g=smooth(g,10)
;       sm_g1=stdev(sm_g)*2
;       sm_g=bytscl(sm_g,-sm_g1,sm_g1)
;       tv,sm_g
;       xyouts, 0.85, 0.65, strmid(strtrim(k+1,2),0,19), charsize=1.7, charthick=2.5, color=255, /norm
;
;       device, /close
;       set_plot, 'win'


rate(k,0) =total(-2.*(bzux*ax+bzuy*ay),/nan)*factor    ; in 10^40 Mx^2 h^-1
rate(k,1) =total(bz>0.)*area_unit*(0.725e8)^2/1.e20    ; in 10^20 Mx
rate(k,2) =total(bz<0.)*area_unit*(0.725e8)^2/1.e20    ; in 10^20 Mx
;rate(k,3) =total(g[where(g gt 0)],/nan)         ; in 10^40 Mx^2 h^-1
;rate(k,4) =total(g[where(-g gt 0)],/nan)       ; in 10^40 Mx^2 h^-1

endfor


close, unitv  & free_lun, unitv

end