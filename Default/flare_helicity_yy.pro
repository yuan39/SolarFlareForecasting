Pro flare_helicity_yy
close,/all
caldat,systime(/julian)-1,month,day,year
;caldat,systime(/julian),month,day,year
;year = 2010
;month = 11
;day = 3
ARLocation,year,month,day,arnum,lat,lon,number
print,year
print,month
print,day
print,arnum
print,lat
print,lon
if month eq 1 then theDate = STRING(day, FORMAT='(I2.2)')+'-Jan-'+STRING(year, FORMAT='(I4.4)')
if month eq 2 then theDate = STRING(day, FORMAT='(I2.2)')+'-Feb-'+STRING(year, FORMAT='(I4.4)')
if month eq 3 then theDate = STRING(day, FORMAT='(I2.2)')+'-Mar-'+STRING(year, FORMAT='(I4.4)')
if month eq 4 then theDate = STRING(day, FORMAT='(I2.2)')+'-Apr-'+STRING(year, FORMAT='(I4.4)')
if month eq 5 then theDate = STRING(day, FORMAT='(I2.2)')+'-May-'+STRING(year, FORMAT='(I4.4)')
if month eq 6 then theDate = STRING(day, FORMAT='(I2.2)')+'-Jun-'+STRING(year, FORMAT='(I4.4)')
if month eq 7 then theDate = STRING(day, FORMAT='(I2.2)')+'-Jul-'+STRING(year, FORMAT='(I4.4)')
if month eq 8 then theDate = STRING(day, FORMAT='(I2.2)')+'-Aug-'+STRING(year, FORMAT='(I4.4)')
if month eq 9 then theDate = STRING(day, FORMAT='(I2.2)')+'-Sep-'+STRING(year, FORMAT='(I4.4)')
if month eq 10 then theDate = STRING(day, FORMAT='(I2.2)')+'-Oct-'+STRING(year, FORMAT='(I4.4)')
if month eq 11 then theDate = STRING(day, FORMAT='(I2.2)')+'-Nov-'+STRING(year, FORMAT='(I4.4)')
if month eq 12 then theDate = STRING(day, FORMAT='(I2.2)')+'-Dec-'+STRING(year, FORMAT='(I4.4)')
;print,lon(1),lat(1)
;arcsecs = conv_h2a([lon(0),lat(0)],STRING(year, FORMAT='(I4.4)')+ '-'+$
;                         STRING(month, FORMAT='(I2.2)')+ '-' +$
;                         STRING(day, FORMAT='(I2.2)'))


  today_jul = julday(month,day,year)
        ;yesterday_jul = today_jul - 1
        reference_jul = julday(8,19,2010) ; the Julian date for 2010-08-19  
        today_soho_mdi = today_jul - reference_jul + 6439 
        print,today_soho_mdi
        ;yeserday_soho_mdi = yesterday_jul - reference_jul + 6439
        ;print,yeserday_soho_mdi

        files = sock_find('http://soi.stanford.edu','fd_M_96m_01d.'+strtrim(today_soho_mdi,2)+'.00*.fits', $
                   path='/magnetic/synoptic/eof/fd_M_96m_01d/fd_M_96m_01d.00'+strtrim(today_soho_mdi,2))
                   ;files = sock_find('http://soi.stanford.edu','fd_M_96m_01d.' + strtrim(today_soho_mdi,2) +'.00*.fits',$
                   ;  path='/magnetic/mag/fd_M_96m_01d.00'+strtrim(today_soho_mdi,2))
                   ;close,/all
                   ;file_delete,'C:\flare_helicity\MDI',/RECURSIVE, /VERBOSE
        FILE_MKDIR,'/var/www/flares/MDI/'+strtrim(today_soho_mdi,2)
        if (size(files,/dimension) gt 2) then begin
          for i = 0,n_elements(files)-1 do begin
			filepath = '/var/www/flares/MDI/'+strtrim(today_soho_mdi,2)+'/'+STRING(i, FORMAT='(I2.2)')+'.fits'
			sock_copy,files[i],filepath,/verb,/outdir
			;sock_fits,files[0],data,header=header
			data = readfits(filepath,hr)
          	pangle = sxpar(hr, 'P_ANGLE')
    		radius = sxpar(hr,'R_SUN')
    		x0 = sxpar(hr, 'X0')
    		y0 = sxpar(hr, 'Y0')
          	;cropped = circle_mask(data, x0, y0, 'GE', radius, mask=0)
          	rotated = rot(data, pangle, 1, x0, y0)
          	sxaddpar, hr, 'P_ANGLE',0
            writefits, filepath, rotated, hr
          endfor
        endif else begin
          print,'no file for today';+ yearstr+monthstr+daystr
          return
        endelse




if number gt 0 then begin
  openw, 2, '/var/www/daily.txt'
  for id = 0,number-1 do begin
    if( abs(lon(id)) lt 60 and  abs(lat(id)) lt 60 ) then begin
        
      

        path1='/var/www/flares/MDI/'+strtrim(today_soho_mdi,2)+'/' ; directory of fits files of today's magnetograms
  
    
    
        arcsecs = conv_h2a([lon(id),lat(id)],theDate)
      
        flare_helicity_3, arnum(id),arcsecs(1),arcsecs(0),path1,flareIndex_24,flareIndex_48,avg_dhdt,avg_f, $
                    pro_24_c,pro_48_c,pro_24_m,pro_48_m,pro_24_x,pro_48_x
  
        ;openw, 1, 'c:\flare_helicity\para.txt'
        ;printf,1, arnum(0)+','+strtrim(flareIndex_24,2)+','+strtrim(flareIndex_48,2)+','+strtrim(avg_dhdt,2)+','+strtrim(avg_f,2)
        ;close, 1

        ;openw, 1, 'c:\flare_helicity\prob.txt'
        ;printf,1, strtrim(round(100*pro_24_c),2)+','+strtrim(round(100*pro_48_c),2) 
        ;printf,1, strtrim(round(100*pro_24_m),2)+','+strtrim(round(100*pro_48_m),2)
        ;printf,1, strtrim(round(100*pro_24_x),2)+','+strtrim(round(100*pro_48_x),2)
        ;close, 1

        mydate = string(year,format='(I4.4)')+string(month,format='(I2.2)')+string(day,format='(I2.2)')
        ar_num = arnum(id)
        ;openw, 1, '/var/www/flare_forecasting.csv',/APPEND 
        ;printf,1, mydate+',1'+ar_num+','+strtrim(avg_dhdt,2)+','+strtrim(avg_f,2)+','$
         ;+strtrim(flareIndex_24,2)+','+strtrim( flareIndex(ar_num,mydate,1),2 )+','$
         ;+strtrim(flareIndex_48,2)+','+strtrim( flareIndex(ar_num,mydate,2),2 )+','$
         ;+strtrim(round(100*pro_24_c),2)+'%,'+strtrim(round(100*pro_48_c),2)+'%,' $
         ;+strtrim(round(100*pro_24_m),2)+'%,'+strtrim(round(100*pro_48_m),2)+'%,' $
         ;+strtrim(round(100*pro_24_x),2)+'%,'+strtrim(round(100*pro_48_x),2)+'%,'$
         ;+strtrim(flareNumber(ar_num,mydate,1,'c'),2)+','$
         ;+strtrim(flareNumber(ar_num,mydate,1,'m'),2)+','$
         ;+strtrim(flareNumber(ar_num,mydate,1,'x'),2)
        ;close, 1
        
        mydate2 = string(year,format='(I4.4)')+'-'+string(month,format='(I2.2)')+'-'+string(day,format='(I2.2)')
        ;printf,2, mydate2+',1'+ar_num+','+strtrim(avg_dhdt,2)+','+strtrim(avg_f,2)+','$
        ; +strtrim(flareIndex_24,2)+','$
        ; +strtrim(flareIndex_48,2)+','$
        ; +strtrim(round(100*pro_24_c),2)+','+strtrim(round(100*pro_48_c),2)+',' $
        ; +strtrim(round(100*pro_24_m),2)+','+strtrim(round(100*pro_48_m),2)+',' $
        ; +strtrim(round(100*pro_24_x),2)+','+strtrim(round(100*pro_48_x),2)+''
        printf,2,mydate2,ar_num,avg_dhdt,avg_f,flareIndex_24,flareIndex_48,$
		100*pro_24_c,100*pro_48_c,100*pro_24_m,100*pro_48_m,100*pro_24_x,$
                100*pro_48_x,format=$
                '(a,",1",a,",",f0.2,",",f0.1,",",f0.1,",",f0.1,",",i,",",i,",",i,",",i,",",i,",",i)'
        
    endif    
  endfor
  close, 2
  SPAWN, '/var/www/flares/Default/DBDemo.sh'
endif

end
