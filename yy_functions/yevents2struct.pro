;+
; FUNCTION:   yevents2struct
;
; PURPOSE:    Read in NOAA SEC events list using sockets and return a structure
;
; USEAGE:     result = yevents2struct( date ,folder [, region_no = region_no, PRINT = PRINT ] )
;
; INPUT:
;  date       Date in any SSW FORmat
;  folder     Root Path to search for flare data base
; KEYWORDS:   region_no = The 4-digit NOAA region number as a string
;             PRINT = PRINT the events
;
; OUTPUT:
;  result     A structure containing the event list
;
; EXAMPLE:    event_struct = yevents2struct( '1-jan-2004' )
;               or
;             event_struct = yevents2struct( '1-jan-2004', /PRINT )
;               or
;             event_struct = yevents2struct( '1-jan-2004', region = '0528', /PRINT)
;
;
; AUTHOR:     1-Apr-2004 peter.t.gallagher@gsfc.nasa.gov - written
;             6-Jul-2004 Russ Hewett -- Made program return a struct
;   	      10-mar-2004 j mcateer-altered and cleaned to use sec list and include B&A flares
;			  23-Jun-2009 Yuan Yuan altered to use both local database and internet list
;-

FUNCTION yevents2struct, date, folder, region_no = region_no, PRINT = PRINT, $
                        a_flares = a_flares, b_flares = b_flares, c_flares = c_flares, m_flares = m_flares, x_flares = xflares, $
			err = err

IF KEYWORD_SET( region_no ) THEN region_no = arr2str( region_no, /trim)

err = ''

yyyymmdd = time2file( date, /date )

CASE 1 OF
    ( ( anytim( date ) GE anytim( '31-jul-1996' ) ) AND $
    ( anytim( date ) LT anytim( '1-jan-2010' ) ) ) : data_read, $
    	;'D:\allclear2\flareDB\' $
    	folder $
    	+ STRMID( yyyymmdd, 0, 4 ) + '_events/' + yyyymmdd + 'events.txt', events ;'http://hesperia.gsfc.nasa.gov/~jma/sec/events/'
    ( anytim( date ) ge anytim( '1-jan-2010' ) )   : sock_list, $
    	'http://www.swpc.noaa.gov/ftpdir/warehouse/2010/' $
    	+ STRMID( yyyymmdd, 0, 4 ) + '_events/' + yyyymmdd + 'events.txt', events                  ;'http://www.sec.noaa.gov/ftpdir/indices/'
ELSE :  BEGIN
    err  = '> No NOAA SEC Events data available FOR ' + date
    PRINT, err
    RETURN, -1
END

ENDCASE

;Find C-, M-, and X-class flares

goes_flares = [WHERE (stregex( events, 'GO8' ) NE -1),WHERE (stregex( events, 'GO9' ) NE -1), WHERE(stregex( events, 'G10' ) NE -1), WHERE(stregex( events, 'G11' ) NE -1), WHERE(stregex( events, 'G12' ) NE -1),WHERE(stregex( events, 'G13' ) NE -1),WHERE(stregex( events, 'G14' ) NE -1),WHERE(stregex( events, 'G15' ) NE -1),WHERE(stregex( events, 'G16' ) NE -1)]
goes_flares=goes_flares(sort(goes_flares))

a_index = WHERE( stregex( events(goes_flares), 'A[0-9]' ) NE -1)
b_index = WHERE( stregex( events(goes_flares), 'B[0-9]' ) NE -1)
c_index = WHERE( stregex( events(goes_flares), 'C[0-9]' ) NE -1)
m_index = WHERE( stregex( events(goes_flares), 'M[0-9]' ) NE -1)
x_index = WHERE( stregex( events(goes_flares), 'X[0-9]' ) NE -1)

IF ( a_index[ 0 ] NE -1 ) THEN a_flares = events( goes_flares( a_index )) ELSE a_flares = ''
IF ( b_index[ 0 ] NE -1 ) THEN b_flares = events( goes_flares( b_index )) ELSE b_flares = ''
IF ( c_index[ 0 ] NE -1 ) THEN c_flares = events( goes_flares( c_index )) ELSE c_flares = ''
IF ( m_index[ 0 ] NE -1 ) THEN m_flares = events( goes_flares( m_index )) ELSE m_flares = ''
IF ( x_index[ 0 ] NE -1 ) THEN x_flares = events( goes_flares( x_index )) ELSE x_flares = ''

flares = [ a_flares, b_flares, c_flares, m_flares, x_flares ]

;Find regions

IF keyword_set( region_no ) THEN BEGIN
    index = WHERE( stregex( a_flares, region_no ) NE -1 )
    IF ( index[ 0 ] NE -1 ) THEN region_a = a_flares[ index ] ELSE region_a = ''

    index = WHERE( stregex( b_flares, region_no ) NE -1 )
    IF ( index[ 0 ] NE -1 ) THEN region_b = b_flares[ index ] ELSE region_b = ''

    index = WHERE( stregex( c_flares, region_no ) NE -1 )
    IF ( index[ 0 ] NE -1 ) THEN region_c = c_flares[ index ] ELSE region_c = ''

    index = WHERE( stregex( m_flares, region_no ) NE -1 )
    IF ( index[ 0 ] NE -1 ) THEN region_m = m_flares[ index ] ELSE region_m = ''

    index = WHERE( stregex( x_flares, region_no ) NE -1 )
    IF ( index[ 0 ] NE -1 ) THEN region_x = x_flares[ index ] ELSE region_x = ''

    regions = [ region_a, region_b, region_c, region_m, region_x ]

    IF ( TOTAL( strlen( regions ) ) EQ 0 ) THEN BEGIN
    	err = '> No flares associated with NOAA ' + arr2str( region_no, /trim ) + ' on ' + date
    	PRINT, '  '
    	PRINT, err
    ENDIF

ENDIF

IF KEYWORD_SET( PRINT ) THEN BEGIN
    IF keyword_set( region_no ) THEN BEGIN
    	IF ( region_a[ 0 ] NE '' ) THEN PRINT, ' ' + arr2str( N_ELEMENTS( region_a ), /trim ) + ' A-class flares'
    	IF ( region_b[ 0 ] NE '' ) THEN PRINT, ' ' + arr2str( N_ELEMENTS( region_b ), /trim ) + ' B-class flares'
    	IF ( region_c[ 0 ] NE '' ) THEN PRINT, ' ' + arr2str( N_ELEMENTS( region_c ), /trim ) + ' C-class flares'
    	IF ( region_m[ 0 ] NE '' ) THEN PRINT, ' ' + arr2str( N_ELEMENTS( region_m ), /trim ) + ' M-class flares'
    	IF ( region_x[ 0 ] NE '' ) THEN PRINT, ' ' + arr2str( N_ELEMENTS( region_x ), /trim ) + ' X-class flares'
    	PRINT, regions
    ENDIF ELSE BEGIN
    	IF ( a_flares[ 0 ] NE '' ) THEN PRINT, ' ' + arr2str( N_ELEMENTS( a_flares ), /trim ) + ' A-class flares'
    	IF ( b_flares[ 0 ] NE '' ) THEN PRINT, ' ' + arr2str( N_ELEMENTS( b_flares ), /trim ) + ' B-class flares'
    	IF ( c_flares[ 0 ] NE '' ) THEN PRINT, ' ' + arr2str( N_ELEMENTS( c_flares ), /trim ) + ' C-class flares'
    	IF ( m_flares[ 0 ] NE '' ) THEN PRINT, ' ' + arr2str( N_ELEMENTS( m_flares ), /trim ) + ' M-class flares'
    	IF ( x_flares[ 0 ] NE '' ) THEN PRINT, ' ' + arr2str( N_ELEMENTS( x_flares ), /trim ) + ' X-class flares'

    	IF ( a_flares[ 0 ] NE '' ) THEN PRINT, a_flares
    	IF ( b_flares[ 0 ] NE '' ) THEN PRINT, b_flares
    	IF ( c_flares[ 0 ] NE '' ) THEN PRINT, c_flares
    	IF ( m_flares[ 0 ] NE '' ) THEN PRINT, m_flares
    	IF ( x_flares[ 0 ] NE '' ) THEN PRINT, x_flares
    ENDELSE
ENDIF

IF keyword_set( regions ) THEN data=regions else data=flares

    w=WHERE(data NE '')
    IF (w eq [-1]) THEN BEGIN
    	err = '> No flares on ' + date
    	RETURN, -1
    ENDIF

    data = data(w)
    data_size = N_ELEMENTS(data)
    goes_struct = CREATE_STRUCT('eventnum', 0L, 'start_time', '', 'max_time', '', 'end_time', '', $
    	'satellite', '', 'q', '', 'type', '', 'freq', '', 'fclass', '', 'fbase', 0.0, 'flux', 0.0, 'p1', 0.0, $
    	    'region', '', name='event_struct')
    output = REPLICATE(goes_struct, data_size)

    FOR i=0,data_size-1 DO BEGIN
    	data_arr = strsplit(data[i], /extract)
    	IF (data_arr[1] eq '+') THEN modifier=1 ELSE modifier = 0

	output[i].eventnum = long(data_arr[0])

        date_string = STRMID(yyyymmdd,0,4) + '-' + STRMID(yyyymmdd,4,2) + '-' + STRMID(yyyymmdd,6,2)

        start_string = STRMID(data_arr[1+modifier],0,2) + ':' + STRMID(data_arr[1+modifier],2,2)
        output[i].start_time = anytim(date_string + ' ' + start_string, /vms)

        max_string = STRMID(data_arr[2+modifier],0,2) + ':' + STRMID(data_arr[2+modifier],2,2)
        output[i].max_time = anytim(date_string + ' ' + max_string, /vms)

        end_string = STRMID(data_arr[3+modifier],0,2) + ':' + STRMID(data_arr[3+modifier],2,2)
        output[i].end_time=anytim(date_string + ' ' + end_string, /vms)

        output[i].satellite=data_arr[4+modifier]
        output[i].q=data_arr[5+modifier]
        output[i].type=data_arr[6+modifier]
        output[i].freq=data_arr[7+modifier]
        output[i].fclass=STRMID(data_arr[8+modifier],0,1)
        output[i].fbase=double(STRMID(data_arr[8+modifier],1,3))

        factor=0.0
        IF (output[i].fclass eq 'A') THEN factor=10.0^(-8.0)
        IF (output[i].fclass eq 'B') THEN factor=10.0^(-7.0)
        IF (output[i].fclass eq 'C') THEN factor=10.0^(-6.0)
        IF (output[i].fclass eq 'M') THEN factor=10.0^(-5.0)
        IF (output[i].fclass eq 'X') THEN factor=10.0^(-4.0)
        output[i].flux = factor * output[i].fbase
        ;removed 030405 ,jma
        ;IF N_ELEMENTS(data_arr) LE (9+modifier) THEN BEGIN
        ;    STOP
        ;    return, -1
        ;ENDIF
        IF N_ELEMENTS(data_arr) gt (9+modifier) THEN output[i].p1=double(data_arr[9+modifier])

        IF (N_ELEMENTS(data_arr) eq 12) or ((N_ELEMENTS(data_arr) eq 11) and (data_arr[1] NE '+')) THEN $
    	    output[i].region=string(data_arr[10+modifier])
    ENDFOR

    RETURN,output

END
