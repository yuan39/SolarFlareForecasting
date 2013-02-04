;+
; :Description:
;    Describe the procedure.
;    To run the program using: arlocation,2010,9,16,arnum,lat,lon,number
; :Params:
;    year : (input) A four digit number
;    month : (input) A one or two digit number
;    day : (input) A one or two digital number
;    arnum   : (output)An string array containing all the active region numbers
;    lat     : (output)An integer array containing the latitudes of the active regions (North Positive)
;    lon     : (output)An integer array containing the longitudes of the active regions (West Positive)
;    number : (output)A scalar indicating how many active regions on the solar disk for the date requested
;
;
;
; :Author: YUAN YUAN
;-
Pro ARLocation,year,month,day,arnum,lat,lon,number
print,day
sock_list,'http://www.swpc.noaa.gov/ftpdir/forecasts/SRS/'+ STRING(month, FORMAT='(I2.2)') + STRING(day, FORMAT='(I2.2)') +'SRS.txt',srs
number = 0
arnum = strarr(20)
lat = intarr(20)
lon = intarr(20)
for i = 0,n_elements(srs) do begin
    if STRCMP('I.', srs[i], 2, /FOLD_CASE) gt 0 then begin
        for j = i+1,i+20 do begin
            if STRCMP('Nmbr', srs[j], 4, /FOLD_CASE) gt 0 then begin
                continue
            endif 
            if STRCMP('None', srs[j], 4, /FOLD_CASE) gt 0 then begin
                goto, myend
            endif 
            if STRCMP('IA.', srs[j], 3, /FOLD_CASE) gt 0 then begin
                goto, myend
            endif 
            tmpstr = strsplit(srs[j],/extract)
            arnum(number) = tmpstr[0]
            print,tmpstr[0]
            print,tmpstr[1]
            if STRCMP(strmid(tmpstr[1],0,1), 'n', 1, /FOLD_CASE) gt 0 then begin
                lat(number) = fix(strmid(tmpstr[1],1,2))
            endif else begin
                lat(number) = -fix(strmid(tmpstr[1],1,2))
            endelse
                        
            if STRCMP(strmid(tmpstr[1],3,1), 'w', 1, /FOLD_CASE) gt 0 then begin
                 lon(number) = fix(strmid(tmpstr[1],4,2))
            endif else begin
                 lon(number) = -fix(strmid(tmpstr[1],4,2))
            endelse
                        
            number = number + 1
            print,number
      
        endfor            
      
    endif 
endfor
myend:
end