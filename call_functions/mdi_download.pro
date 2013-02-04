caldat,systime(/julian),month,day,year
today_jul = julday(month,day,year)
yesterday_jul = today_jul - 1

reference_jul = julday(8,19,2010) ; the Julian date for 2010-08-19 
yeserday_soho_mdi = yesterday_jul - reference_jul + 6439
print,yeserday_soho_mdi

files = sock_find('http://soi.stanford.edu','fd_M_96m_01d.'+strtrim(yeserday_soho_mdi,2)+'.00*.fits', $
                   path='/magnetic/synoptic/eof/fd_M_96m_01d/fd_M_96m_01d.00'+strtrim(yeserday_soho_mdi,2))
;close,/all
;file_delete,'C:\flare_helicity\MDI',/RECURSIVE, /VERBOSE
FILE_MKDIR,'C:\flare_helicity\MDI\'+strtrim(yeserday_soho_mdi,2)
if (size(files,/dimension) gt 0) then begin
  for i = 0,n_elements(files)-1 do begin
    filepath = 'C:\flare_helicity\MDI\'+strtrim(yeserday_soho_mdi,2)+'\'+STRING(i, FORMAT='(I2.2)')+'.fits'
    sock_copy,files[i],filepath,/verb,/outdir
  ;sock_fits,files[0],data,header=header
  end
endif else begin
  print,'no file for today';+ yearstr+monthstr+daystr
  return
endelse

end