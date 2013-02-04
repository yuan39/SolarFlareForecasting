function mdi_time_ccsds, time

return, strmid(time,0,4)+'-'+strmid(time, 5,2)+'-'+strmid(time, 8,2)+$
   'T'+strmid(time, 11, 8)+'Z'
 end  
