FUNCTION flareNumber, region_no, datetime, days, class
;This function is to figure out the flare index in the window of 24*days hours after
;the specified time (start time in the window)
;region_no is something like '0528'
;datetime is something like '20040101u162300'
;days is an integer in the range [1,2,3,4,....]
;class is a string such as 'c' or 'm' or 'x' or 'b'
datetime = strtrim(datetime);
juldatetime = julday(strmid(datetime,4,2),strmid(datetime,6,2),strmid(datetime,0,4),$
           strmid(datetime,9,2),strmid(datetime,11,2),strmid(datetime,13,2));
left = juldatetime
right = juldatetime + days

number = 0;
for k = 0L,days do begin
  CALDAT, juldatetime+k, Month, Day, Year, Hour, Minute, Second
  currentdate = String(Year, Format='(I0.4)')+'-'+String(Month, Format='(I0.2)')+'-'+String(Day, Format='(I0.2)');
  print,currentdate
  event_struct = yevents2struct(currentdate,'/var/www/flaredb/', region = region_no, /PRINT, err = err)
  IF err NE '' THEN  continue;

  for i = 0L, n_elements(event_struct)-1 do begin
    stime = time2file(strtrim(event_struct[i].start_time),/sec)
    sjul = julday(strmid(stime,4,2),strmid(stime,6,2),strmid(stime,0,4),$
            strmid(stime,9,2),strmid(stime,11,2),strmid(stime,13,2))
    if(sjul ge left && sjul le right) then begin
      if(strcmp(class,event_struct[i].fclass, /FOLD_CASE) ) then begin
        number = number + 1;
      endif
    endif
  endfor
  ;print,index
endfor
return,number
end

