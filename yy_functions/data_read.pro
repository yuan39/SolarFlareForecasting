PRO data_read, filename, list

num = file_line(filename)

list = sindgen(num)

OPENR, lun, filename, /GET_LUN
tmp = ''
for i = 0, num-1 do begin
	readf, lun, tmp
	;print,list[i]
	;print,tmp
	list[i] = tmp
endfor

FREE_LUN, lun

END