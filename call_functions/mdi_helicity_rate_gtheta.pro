pro mdi_helicity_rate_gtheta,  vgcubefile, t,  rate, reftime=reftime, n=n

;+
;;
;    History:   2006 Novemeber J. Chae first coded.
;-


v=assoreadfits(vgcubefile, bzv, bsv, hv, nf=nfv, unit=unitv)

if not isvalid(reftime) then reftime= fxpar(hv, 'REFTIME')
t=mdi_time_interval(fxpar(hv, 'comment'), reftime, /min)
if n_elements(n) eq 0 then n=n_elements(t)
rate=fltarr( n, 3)
t=t[0:n-1]
area_unit =fxpar(hv, 'xscale')*fxpar(hv, 'yscale')*(0.725)^2 ; Mm^2
for k=0, n-1 do begin

bz = assoget(v, k*5, bzv, bsv)    ; in G
vx = assoget(v, k*5+1, bzv, bsv)  ; in km/s

vy = assoget(v,k*5+2,bzv, bsv)    ; in km/s

G_theta_M = assoget(v, k*5+3, bzv, bsv)  ; in G^2 km/s Mm

G_theta_S = assoget(v, k*5+4, bzv, bsv)  ; in G^2 km/s Mm

factor = area_unit*3600. *1.e-11

rate(k, 0) = total(G_theta_M+G_theta_S)*factor   ;   in 10^40 Mx^2 h^-1
;rate(k,1) =total(G_theta_S)*factor ; in 10^40 M^2 h^-1
;rate(k) = total(-2*(bzux*ax+bzuy*ay))*factor   ;   in 10^40 Mx^2 h^-1
rate(k,1) =total(bz>0.)*(area_unit*(1.e8)^2/1.e20)    ; in 10^20 Mx
rate(k,2) =total(bz<0.)*(area_unit*(1.e8)^2/1.e20)    ; in 10^20 Mx
endfor

close, unitv  & free_lun, unitv

end