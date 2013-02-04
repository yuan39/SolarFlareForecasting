;
;+
;NAME:
;	anytim
;PURPOSE:
;       Given a time in the form of a (1) structure, (2) 7-element time
;       representation, or (3) a string representation, or (4) an array
;       2xN where the first dimension holds (MSOD, DS79), or 
;	(5) a double or float array of seconds from 1-jan-79
;	convert to  any of the 5 representations including both varieties
;	of strings, dd-mon-yr or yy/mm/dd.
;CALLING SEQUENCE:
;	xx = anytim(roadmap, out_styl='ints')
;	xx = anytim("12:33 5-Nov-91", out_sty='ex')
;	xx = anytim([0, 4000], out_style= 'sec')
;CATEGORY:
;	time conversions
;INPUT:
;	item	- The input time
;		  Form can be (1) structure with a .time and .day
;		  field, (2) the standard 7-element external representation
;		  or (3) a string of the format "hh:mm dd-mmm-yy"
;		  or (4) 2xN where the first dimension holds (MSOD, DS79), or 
;		  or (5) a double or float array of seconds from 1-jan-79
;       The UTC procedures, utc2int, str2utc, and int2utc are included using
;	CALL_FUNCTION so anytim will compile and work without these routines
;	and their subsequent calls to procedures in the utc directory.
;	Allowed formats include those supported under UTC  code, see STR2UTC.PRO:
; 			= A character string containing the date and time.  The
;			  target format is the CCSDS ASCII Calendar Segmented
;			  Time Code format (ISO 8601), e.g.
;
;				"1988-01-18T17:20:43.123Z"
;
;			  The "Z" is optional.  The month and day can be
;			  replaced with the day-of-year, e.g.
;
;				"1988-018T17:20:43.123Z"
;
;			  Other variations include
;
;				"1988-01-18T17:20:43.12345"
;				"1988-01-18T17:20:43"
;				"1988-01-18"
;				"17:20:43.123"
;
;			  Also, the "T" can be replaced by a blank, and the
;			  dashes "-" can be replaced by a slash "/".  This is
;			  the format used by the SOHO ECS.
;
;			  In addition this routine can parse dates where only
;			  two digits of the year is given--the year is assumed
;			  to be between 1950 and 2049.
;
;			  Character string months, e.g. "JAN" or "January", can
;			  be used instead of the number.  In that case, it
;			  assumes that the date is either in day-month-year or
;			  month-day-year format, e.g. "18-JAN-1988" or
;			  "Jan-18-1988".  However, if the first parameter is
;			  four digits, then year-month-day is assumed, e.g.
;			  "1988-Jan-18".
;
;			  Dates in a different order than year-month-day are
;			  supported, but unless the month is given as a
;			  character string, then these are only supported
;			  through the /MDY and /DMY keywords.
;
;	End UTC documentation	
;
;OPTIONAL KEYWORD INPUT:
;	out_style - Output representation, specified by a string:
;      		INTS   	- structure with [msod, ds79]
;		STC     - same as INTS
;      		2XN    	- longword array [msod,ds79] X N
;      		EX     	- 7 element external representation (hh,mm,ss,msec,dd,mm,yy)
;		UTIME	- Utime format, Real*8 seconds since 1-jan-79, DEFAULT!!!!
;               SEC     - same as Utime format
;               SECONDS - same as Utime format
;		ATIME   - Variable Atime format, Yohkoh
;			  Yohkoh style - 'dd-mon-yy hh:mm:ss.xxx'   or
;			  HXRBS pub style  - 'yy/mm/dd, hh:mm:ss.xxx'
;			  depending on atime_format set by 
;			  hxrbs_format or yohkoh_format
;		YOHKOH  - yohkoh style string 
;		HXRBS   - HXRBS Atime format /pub, 'yy/mm/dd, hh:mm:ss.xxx'
;               YY/MM/DD- same as HXRBS
;		MJD     - UTC-type structure
;			= The UTC date/time as a data structure with the
;			  elements:
;
;				MJD	= The Modified Julian Day number
;				TIME	= The time of day, in milliseconds
;					  since the start of the day.
;
;			  Both are long integers.
;		UTC_INT - Same as MJD
;		UTC_EXT - UTC external format, a structure
;		          containing the elements, YEAR, MONTH, DAY, HOUR, MINUTE,
;		          SECOND, and MILLISECOND as shortword integers.
;		CCSDS   - A string variable containing the calendar date in the
;			 format recommended by the Consultative Committee for
;			 Space Data Systems (ISO 8601), e.g.
;
;				"1988-01-18T17:20:43.123Z"
;
;		ECS     - A variation on the CCSDS format used by the EOF Core
;			 System.  The "T" and "Z" separators are eliminated, and
;			 slashes are used instead of dashes in the date, e.g.
;
;				"1988/01/18 17:20:43.123"
;
;		VMS     - Similar to that used by the VMS operating system, this
;			 format uses a three-character abbreviation for the
;			 month, and rearranges the day and the year, e.g.
;
;				"18-JAN-1988 17:20:43.123"
;
;		STIME   - Based on !STIME in IDL, this format is the same as the
;			 second accuracy, e.g.
;			 VMS format, except that the time is only given to 0.01 
;			 second accuracy, e.g.
;
;				"18-JAN-1988 17:20:43.12"
;;			
;	or by keywords
;		/ints   - 
;	        /stc
;		/_2xn
;		/external
;		/utime
;		/seconds
;		/atimes
;		/yohkoh
;		/hxrbs
;		/yymmdd	
;		/mjd
;		/utc_int
;		/utc_ext
;		/ccsds
;		/ecs
;		/vms
;		/stime
;
;	mdy	- If set, use the MM/DD/YY order for converting the string date
;		
;	date 	- return only the calendar date portion, 
;			e.g. anytime('93/6/1, 20:00:00',/date,/hxrbs) ==> '93/06/01'
;	time    - return only the time of day portion
;			e.g. anytime('93/6/1, 20:00:00',/date,/hxrbs) ==> '20:00:00.000'
;keyword output:
;	error	- set if an error, dummy for now, ras 18-Nov-93
;restrictions:
;	one dimensional or scalar longwords will be interpreted as
;	double precision seconds unless they have either two or seven
;	elements
;       Should not be used to interpret an array of mixed string formats.
;       If the formats are mixed then the array should be processed element by element.
;HISTORY:
;	Written 31-Oct-93 ras
;	modified 4-jan-94 ras, made argument recognition more robust
;		also made output dimensions similar for /yohkoh  and /hxrbs
;	modified 25-jan-94 ras, made SEC or SECONDS work
;	ras 30-jan-94, fixed string outputs for /date and /time
;	ras 9-feb-94, fixed typo
;	ras 15-jun-1995, integrated Bill Thompson's UTC formats, for input and output 
;		start adjusting to 4 digit years
;	ras, 20-jun-1995, put calls to utc functions inside call_function to reduce need
;	to include utc directories, default structure is Yohkoh as before,
;	utc structures are tested for explicitly
;       ras, 23-jun-1995, stopped searching for 'T' to identify CCSDS format, now 'Z'
;	   made sear for 4 digit year more exacting
;	ras, 28-jul-95, restored item using item_old if necessary
;-
;

function anytim, item, out_style=out_style, mdy=mdy, $
ints=ints, stc=stc, _2xn=_2xn, external=external, utime=utimes, $
seconds=sec, atimes=atimes,yohkoh=yohkoh,  hxrbs=hxrbs, yymmdd=yymmdd, $
date=date, time=time, mjd=mjd, utc_int=utc_int, utc_ext=utc_ext, ccsds=ccsds, $
ecs=ecs, vms=vms, stime=stime, error=error

on_error, 2
error = 1			;ras 18-nov-93; tbd

;offset between Modified Julian Days and days since 1-jan-1979
mjd_fiducial = 43873L		

;error checking on EX vector
;ex is hh,mm,ss,msec,dd,mm,yy
;exrange= reform( [0,23,0,59,0,59,0,999,1,31,1,12,0,99], 2,7)
;4 digit years in future, ras, 15-jun-1995
exrange= reform( [0,23,0,59,0,59,0,999,1,31,1,12,0,9999], 2,7)	

siz = size(item)
if siz(0) eq 0 then scalar = 1 else scalar =0
typ = datatype(item(0))
;Convert to EX representation

case 1 of
    (typ eq 'STC'): begin
        
        ;To support the UTC formats, we must check for and convert the two UTC structure formats
        ;their internal format with tags MJD and TIME and
        ;their external format with 7 tags.
	;Check for Yohkoh structure tag names (day, time) $
	;vs CDS structure tag names, (mjd,time), or (year,month,day,hour,minute,second,millisecond)
        result = item
	tags = tag_names(item(0))
	 case 1 of
 	    ((where(strpos(tags,'YEAR') ne -1))(0) ne -1 ) and ((where(strpos(tags,'MONTH') ne -1))(0) ne -1 ) and $
	    ((where(strpos(tags,'DAY') ne -1))(0) ne -1 ) and ((where(strpos(tags,'HOUR') ne -1))(0) ne -1 ) and $
 	    ((where(strpos(tags,'MINUTE') ne -1))(0) ne -1 ) and ((where(strpos(tags,'SECOND') ne -1))(0) ne -1 ) and $
	    ((where(strpos(tags,'MILLISECOND') ne -1))(0) ne -1 ) : begin
                ex = call_function('utc2int', item)	;convert external to internal
                ;It's Modified Julian Day!
                result = utime2str(fltarr( n_elements(item)))
                result.time= ex.time
                result.day = ex.mjd - mjd_fiducial 
               end
            ((where(strpos(tags,'MJD') ne -1))(0) ne -1 ) and ((where(strpos(tags,'TIME') ne -1))(0) ne -1 ) : begin
                if (where( strpos(strupcase(tag_names(item)),'MJD') ne -1))(0) ne -1 then begin
                    ;It's Modified Julian Day!
                    result = utime2str(fltarr( n_elements(item)))
                    result.time = item.time
                    result.day = item.mjd - mjd_fiducial 
                    endif
                end
            else: 
            endcase
        int2ex, gt_time(result), gt_day(result), ex
        end
    (typ eq 'DOU' or typ eq 'FLO') or $
    ( (typ eq 'INT' or typ eq 'LON') and ( (n_elements(item) eq 1) or $ 
    (siz(0) eq 1 and (siz(1) ne 2 and siz(1) ne 7)))):  begin
        ;	ustr = utime2str( item, utbase=0.0) 
        ustr = utime2str( item(*), utbase=0.0)  ;ras, 4-jan-94
        int2ex, ustr.time, ustr.day, ex
        end
    
    (typ eq 'INT' or typ eq 'LON' and n_elements(item) ge 2): begin
        case siz(1) of
            7: ex = item 
            2: int2ex, item(0,*), item(1,*), ex
            else: begin
                Print, 'Not a valid input to Anytim! Error!'
                goto, error_out 
                end
            endcase
        end
    
    (typ eq 'STR'): begin
        use_utc= 0	;default is not to use UTC processing
        ;Look for UTC formats, check the first entry and assume the rest match!
        ;First look for the year month day delimiter, '-' or '/'
        delim = '-'
        nslash = 0
        delim_pos = strpos( item, delim)
        wdelim_pos = where( delim_pos ne -1, ndash)
        if ndash eq 0 then begin 		;check for alternate delimiter '/'
            delim = '/'
            delim_pos = strpos( item, delim)
            wdelim_pos = where( delim_pos ne -1, nslash)
            endif
        ;If there is a delimiter, then we must send 4 digit dates onto the UTC string converters
        ;until the Yohkoh time string converter, timstr2ex.pro, supports 4 digits completely
        ;and the HXRBS converter, utime.pro, supports 4 digits completely, ras 18-jun-1995
        ;Search for comma's first and convert them to spaces.
        result = byte( item )
        comma_pos  = where( result eq (byte(','))(0), ncomma )
        if ncomma ge 1 then begin
            result(comma_pos) = (byte(' '))(0) ;replace with a space
	    item_old = item
            item = strcompress( result )
            endif
        year_4_digits = 0
        if ndash ge 1 or nslash ge 1 then begin
            result = str2arr( strcompress( strtrim( item( wdelim_pos(0) ), 2) ), delim=' ')
            ;There can't be more than 2 elements separated by a space.
            if n_elements(result) gt 2 then goto, error_out   else begin
                test = result(0)
                if strpos(test,delim)  eq -1 then test=result(1)
		result = str2arr( test, delim=delim)
                lresult = strlen(result)

                w4 = where( lresult ge 4, n4more)
                if n4more ge 1 then begin
                    result = result( w4)
                    for i=0,n4more - 1 do begin
                       test = (byte(result(i)))(0:3)
                       wdigits = where( test ge 48 and test le 57, nwdigits)
                       if nwdigits eq 4 then year_4_digits =1
                    endfor
                endif
            endelse
            result = str2arr( item(wdelim_pos(0)), delim=delim)
            any_zs = where( strpos( strupcase(item),(byte('Z'))(0)) ne -1, n_any_zs)   
            if year_4_digits or n_any_zs ge 1 then begin 	
                ;Use UTC parsing until Yohkoh is upgraded to handle 4 digit numbers
                ;When Yohkoh software has 4 digit capability then pipe the "/" 
                ;"-" formats to their more vector oriented routines instead of STR2UTC
                ;Look for commas, and make them blanks
                comma_pos  = strpos( item, ',' )
                any_commas = (where(comma_pos ne -1, ncomma))(0) ne -1
                ;if commas and 4 digits, clear the commas and process under UTC                
                if any_commas then begin     
                    wcomma = where(comma_pos ne -1)
                    result = item(wcomma)
                    comma_pos = comma_pos(wcomma)
                    for i=0,ncomma-1 do result(i) = $
                    strmid( result(i),0,comma_pos(i)) $
                    + ' ' + strmid( result(i),comma_pos(i)+1,strlen(result(i)))
                    item(wcomma) = result
                    endif
                item_utc = call_function('str2utc', item )	
                result = utime2str(fltarr(n_elements(item_utc)))
                result.time = item_utc.time
                result.day = item_utc.mjd - mjd_fiducial
                int2ex, result.time, result.day, ex
                use_utc = 1
		;The time has been converted using UTC codes!!
                endif
            endif 
         if not use_utc then begin
            
            if keyword_set(mdy) then begin; Special format!
                wyo_count = n_elements(item)
                wno_count = 0
                wyohkoh= indgen(wyo_count)
                endif else begin
                test = strpos(item,'-') ne -1  
                wyohkoh = where( test, wyo_count)
                wnot    = where( test ne 1, wno_count)
                endelse
            
            ;Interpret all Yohkoh strings,
            ;	Although Utime will support simple Yohkoh strings,
            ;	it doesn't support reverse order and 4 digits for the year, ie 1993
            ;	For the moment, 1-Nov-93, all Yohkoh strings interpreted here
            
            if wyo_count ge 1 then begin
                ex1 = timstr2ex( item( wyohkoh ),mdy=mdy )
                ;Check for errors in Yohkoh string interpretation
                for i=0,6 do begin
                    out_of_range= where( ex1(i,*) lt exrange(0,i) or ex1(i,*) gt $
                    exrange(1,i), num_out)
                    if num_out ge 1 then begin
                        Print, 'Error in Yohkoh string interpretation out of Timstr2ex,'
                        Print, 'Could not interpret - ',(item(wyohkoh))(out_of_range)
                        Print, 'Correct input format:'
                        Print, '4-Jan-91 22:00:15.234, range is 1-jan-(19)50 to 31-dec-(20)49'
                        goto, error_out
                        endif
                    endfor
                endif
            
            
            ;Interpret HXRBS style strings and strings w/o dates, 'yy/mm/dd, hh:mm:ss.xxx'
            if wno_count ge 1 then begin
                ut = utime( item(wnot), error=error_utime ) 		;not yet if ever, mdy=mdy )
                if error_utime then begin
                    Print, 'Error in HXRBS string interpretation by Utime'
                    Print, 'Could not interpret - ',(item(wnot))(0)		;ras, 9-feb-94
                    Print, 'Correct input format:'
                    Print, '89/12/15, 22:00:15.234, range is 1-jan-(19)50 to 31-dec-(20)49'
                    goto, error_out
                    Print,'goto, error_out
                    endif
                ustr= utime2str(ut, utbase = 0.0)
                int2ex, ustr.time, ustr.day, ex2
                endif
            
            if wyo_count eq 0 then ex = ex2 else $
            if wno_count eq 0 then ex = ex1 else ex=[ex1,ex2]
            endif               ;close non-UTC string processing
        end
    1: begin
        Print, 'Not a valid input to Anytim! Error!'
        goto, error_out 
        end
    endcase

if n_elements(item_old) ge 1 then item=item_old
wcount = n_elements(ex) / 7

case 1 of
    keyword_set(date): $
    if wcount eq 1 then ex(0:3) = 0 else ex(0:3,*) = 0 
    keyword_set(time): $
    if wcount eq 1 then ex(4:6) = [1,1,79] else ex(4:6,*) = rebin([1,1,79],3,wcount) 
    1: ;NOACTION
    endcase

;Now we have the time in the 7xN external format, Choose the output!

checkvar, out_style, 'UTIME'

out = strupcase(out_style)

if keyword_set(utimes) then out = 'UTIME'
if keyword_set(sec) then out = 'SEC'
if keyword_set(atimes) then out = 'ATIME'
if keyword_set(external) then out = 'EX'
if keyword_set(ints) then out = 'INTS'
if keyword_set(stc) then out = 'STC'
if keyword_set(_2xn) then out = '2XN'
if keyword_set(hxrbs) then out = 'HXRBS'
if keyword_set(yymmdd) then out = 'YY/MM/DD'
if keyword_set(yohkoh) then out = 'YOHKOH'
if keyword_set(mjd)  or keyword_set(utc_int) then out = 'UTC_INT'
if keyword_set(utc_ext) then out='UTC_EXT'
if keyword_set(ccsds) then out='CCSDS'
if keyword_set(ecs) then out='ECS'
if keyword_set(vms) then out='VMS'
if keyword_set(stime) then out='STIME'


if out eq  'UTIME' or out eq 'SEC' or out eq 'SECONDS' then begin 

    result = int2sec( anytim2ints( ex ) )
    if (typ eq 'DOU' or typ eq 'FLO' or typ eq 'STR') then $
    result = double(strmid(item,0,0)+'0') + result
    endif

if out eq 'EX' then result = ex

if out eq 'INTS' or out eq 'STC' then result = anytim2ints( ex )

if out eq '2XN' then begin 
    result = anytim2ints(ex)
    result = transpose( [[result.time],[result.day]] )
    endif

if out eq 'ATIME' then begin
    result = atime(/pub, ex, date=date, time=time)
    endif

if out eq 'YOHKOH' then begin
    result = atime(/yohkoh, ex, date=date, time=time) 
    if (typ eq 'DOU' or typ eq 'FLO' or typ eq 'STR') then $ ;ras, 4-jan-94
    result = strmid(item,0,0) + result
    endif

if out eq 'HXRBS' or out eq 'YY/MM/DD' then begin
    result = int2sec( anytim2ints( ex ) )
    result = atime( result,/hxrbs,/pub,date=date, time=time )
    if (typ eq 'DOU' or typ eq 'FLO' or typ eq 'STR') then $
    result = strmid(item,0,0) + result
    endif
if scalar and n_elements(result) eq 1 then result= result(0)
if out eq 'UTC_INT' or out eq 'UTC_EXT' or out eq 'CCSDS' or out eq 'ECS' or $
out eq 'VMS' or out eq 'STIME' then begin
    ;use UTC converters
    ex2int, ex, msod, ds79
    result = replicate(call_function('str2utc','01-jan-1979'), n_elements(ds79))
    result.mjd = result.mjd-1+ds79
    result.time = msod
    if out eq 'UTC_EXT' or out eq 'CCSDS' or out eq 'ECS' or $
    out eq 'VMS' or out eq 'STIME' then result=call_function('int2utc',result, $
    ccsds=(out eq 'CCSDS'), ecs=(out eq 'ECS'), vms=(out eq 'VMS'), stime=(out eq 'STIME'), $
    date_only=date, time_only=time)
    endif


error = 0
return, result
error_out: return, item

end
