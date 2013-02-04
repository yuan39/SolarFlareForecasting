function mdi_time_interval, time,  ref,  min=min, sec=sec

t0 = anytim2jd(mdi_time_ccsds(ref))


nf=n_elements(time)
out = fltarr(nf)
for k=0, nf-1 do begin
t1 = anytim2jd(mdi_time_ccsds(time(k)))
out(k) = ((t1.int-t0.int)+(t1.frac-t0.frac))*24.
endfor
if keyword_set(min) then out=out*60.
if keyword_set(sec) then out=out*3600.
if n_elements(out) eq 1 then out=out(0)
return, out
end
