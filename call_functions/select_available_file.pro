
function select_available_file, f

j=n_elements(f)
k=0
s=0

a=readfits(f[0],hr)
reftime=fxpar(hr,'t_obs')
   if round(fxpar(hr, 'SOLAR_P')) eq -180 or   round(fxpar(hr, 'SOLAR_P')) eq 180 then $
   a= rotate(a, 2)
tvscl,rebin(a,512,512)>(-200.)<(200.)
cursor,x,y,/device & x=x*2 & y=y*2

xc=(x-fxpar(hr,'plate_x'))*fxpar(hr,'xscale')+fxpar(hr,'xcen') ; in arsec relative to disk center
yc=(y-fxpar(hr,'plate_y'))*fxpar(hr,'yscale')+fxpar(hr,'ycen') ; in arsec relative to disk center

for i=0,(j-1) do begin

   b0=0.
   l0=0.
   sd=0.
   p=0.
   r0=0.
   r=0.

   ;tmp = get_sun(mdi_time_ccsds(fxpar(headfits(f[i]), 'T_OBS')), he_lat=b0, he_lon=l0, s=sd, pa=p)
   ;hel = arcmin2hel(xc/60., yc/60., b0=b0, l0=l0, r0=206265./sd, p=p) ; longitude and latitue of AR center position from the disk center

   ;arcmin = hel2arcmin(hel[0],hel[1], b0=b0, l0=l0, r0=206265./sd, p=p)

   r=sqrt((xc/80.)^2+(yc/80.)^2) ; distance of AR center from the disk center [in armin]

    if r ge 9. then begin ; 9 armin is 0.6 solar radius == 5.5 arcmin

      print,f[i]+' is not available!'
      k=k+1

      a=readfits(f[i+1],hr)
       if round(fxpar(hr, 'SOLAR_P')) eq -180 or   round(fxpar(hr, 'SOLAR_P')) eq 180 then $
       a= rotate(a, 2)
      reftime=fxpar(hr,'t_obs')
      tvscl,rebin(a,512,512)>(-200.)<(200.)
      cursor,x,y,/device & x=x*2 & y=y*2
      xc=(x-fxpar(hr,'plate_x'))*fxpar(hr,'xscale')+fxpar(hr,'xcen')
      yc=(y-fxpar(hr,'plate_y'))*fxpar(hr,'yscale')+fxpar(hr,'ycen')

    endif

    if i eq j-1 then begin

      a=readfits(f[j-1],hr)
       if round(fxpar(hr, 'SOLAR_P')) eq -180 or   round(fxpar(hr, 'SOLAR_P')) eq 180 then $
       a= rotate(a, 2)
       reftime=fxpar(hr,'t_obs')
      tvscl,rebin(a,512,512)>(-200.)<(200.)
      cursor,x,y,/device & x=x*2 & y=y*2
      xc=(x-fxpar(hr,'plate_x'))*fxpar(hr,'xscale')+fxpar(hr,'xcen')
      yc=(y-fxpar(hr,'plate_y'))*fxpar(hr,'yscale')+fxpar(hr,'ycen')


        b0=0.
        l0=0.
        sd=0.
        p=0.
        r0=0.
        r=0.

        r=sqrt((xc/80.)^2+(yc/80.)^2)

         if r ge 9. then begin

          for w=j-1,k,-1 do begin

           r=sqrt((xc/80.)^2+(yc/80.)^2)
            
            if r ge 9. then begin

             print,f[w]+' is not available!'

              s=s+1

              a=readfits(f[w-1],hr)
               if round(fxpar(hr, 'SOLAR_P')) eq -180 or   round(fxpar(hr, 'SOLAR_P')) eq 180 then $
               a= rotate(a, 2)
              reftime=fxpar(hr,'t_obs')
              tvscl,rebin(a,512,512)>(-200.)<(200.)
              cursor,x,y,/device & x=x*2 & y=y*2
              xc=(x-fxpar(hr,'plate_x'))*fxpar(hr,'xscale')+fxpar(hr,'xcen')
              yc=(y-fxpar(hr,'plate_y'))*fxpar(hr,'yscale')+fxpar(hr,'ycen')

             b0=0.
             l0=0.
             sd=0.
             p=0.
             r0=0.
             r=0.

             endif

          endfor

         endif

    endif

endfor


park=sindgen(j-k-s)


for i=0,(j-k-s-1) do begin

      park[i]=f[i+k]

endfor

print,'--------------------------------------------------'
print,string(j-k-s)+' files are available!'

return,park

end