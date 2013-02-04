pro host_to_ieee, data, IDLTYPE = idltype
;+
; NAME:
;	HOST_TO_IEEE
; PURPOSE:
;	Translate an IDL variable from host to IEEE representation 
; EXPLANATION:
;	The variable is converted from the format used by the host architecture
;	into IEEE-754 representation (as used, for example, in FITS data ).
;
; CALLING SEQUENCE:
;	HOST_TO_IEEE, data, [ IDLTYPE = , ]
;
; INPUT-OUTPUT PARAMETERS:
;	data - any IDL variable, scalar or vector.   It will be modified by
;		HOST_TO_IEEE to convert from host to IEEE representation.  Byte 
;		and string variables are returned by HOST_TO_IEEE unchanged
;
; OPTIONAL KEYWORD INPUTS:
;	IDLTYPE - scalar integer (1-9) specifying the IDL datatype according
;		to the code given by the SIZE function.      This keyword
;		will usually be used when suppying a byte array that needs
;		to be interpreted as another data type (e.g. FLOAT).
;
; EXAMPLE:
;	Suppose FITARR is a 2880 element byte array to be converted to a FITS
;	record and interpreted a FLOAT data.
;
;	IDL> host_to_ieee, FITARR, IDLTYPE = 4
;
; METHOD:
;	The BYTEORDER procedure is called with the appropriate keywords
;
; RESTRICTION:
;	Assumes the IDL version is since 2.2.2 when the /XDRTOF keyword 
;	became available to BYTEORDER.    There were two bad implementations
;	in BYTEORDER for double precision: (1) in IDL V3.* for DecStations
;	(!VERSION.ARCH = 'mipsel') and (2) on Dec Alpha OSF machines.
;	IEEE_TO_HOST works around these cases by swapping the byte order
;	directly.
;
; MODIFICATION HISTORY:
;	Adapted from CONV_UNIX_VAX, W. Landsman   Hughes/STX    January, 1992
;	Fixed Case statement for Float and Double      September, 1992
;	Workaround for /DTOXDR on DecStations          January, 1993
;	Workaround for /DTOXDR on Alpha OSF            July 1994
;	Assume since Version 2.2.2, Ultrix problems persist   November 1994
;	Add support for double complex        July, 1995
;	Workaround for VAX VMS bug in BYTEORDER,/FTOXDR in V4.0   August 1995
;	Workaround for VMS bug in BYTEORDER,/FTOXDR and /DTOXDR in
;		V4.0.1 (sigh...)  W. Landsman   August 1995
;	Workaround for /FTOXDR bug in OSF V4.0.1 September 1995
;	Don't change double scalars to 1 element arrays November 1997
;       Make to work explicitly with V5; compute DCOMPLEX values
;               correctly, C. Markwardt, Apr 1999
;-
 On_error,2 

 if N_params() EQ 0 then begin
    print,'Syntax - HOST_TO_IEEE, data, [IDLTYPE = ]
    return
 endif  

 npts = N_elements( data )
 if npts EQ 0 then $
     message,'ERROR - IDL data variable (first parameter) not defined'

 sz = size(data)
 if not keyword_set( idltype) then idltype = sz( sz(0)+1)

 if idltype EQ 6 then idltype = 4     ;Treat complex as float
 if idltype EQ 9 then idltype = 5     ;Treat double complex as double

 case idltype of

      1: return                             ;byte

      2: byteorder, data, /HTONS            ;integer

      3: byteorder, data, /HTONL            ;long

      4: begin                              ;float

         bad_vms = 0
	 little_endian = 0

         if !VERSION.RELEASE LT 5 then begin
             if (!VERSION.OS EQ 'linux')  or (!VERSION.OS EQ 'Win32') or $
               (!VERSION.OS EQ 'OSF')  or (!VERSION.OS EQ 'ultrix') then $
               little_endian = 1
             if (!VERSION.ARCH EQ 'vax') and $
               (!VERSION.RELEASE EQ '4.0') then bad_vms = 1
             if (!VERSION.RELEASE EQ '4.0.1') then $
               if !VERSION.OS EQ 'vms' then bad_vms = 1
         endif
         if little_endian then byteorder, data,/LSWAP else $
         if bad_vms then data = conv_vax_unix(data, target = 'sparc') $
         else byteorder, data, /FTOXDR

         end

      5: begin                              ;double

     bad_ultrix =   (!VERSION.ARCH EQ 'mipsel') and $
                    strmid(!VERSION.RELEASE,0,1) EQ '3'
     bad_windows = (!VERSION.RELEASE EQ '4.0.1') and ((!VERSION.OS EQ 'ultrix')$
		  or (!VERSION.OS EQ 'linux') or (!VERSION.OS EQ 'Win32') )
     bad_osf  =     (!VERSION.ARCH EQ 'alpha') and (!VERSION.OS EQ 'OSF') $
                  and (!VERSION.RELEASE LT 5)
     bad_vms  =     (!VERSION.OS EQ 'vms') and (!VERSION.RELEASE EQ '4.0.1')

     if bad_ultrix or bad_osf or bad_windows then begin  ;Swap byte order directly

                    dtype = sz( sz(0) + 1)
                    if ( dtype EQ 5 ) then begin
                        data = byte(data, 0, npts*8)
                        data = reform( data, 8 , npts ,/OVER)
                    endif else if (dtype EQ 9) then begin
                        data = byte(data, 0, npts*16)
                        data = reform( data, 8 , npts*2, /OVER)
                    endif else begin
                        npts = npts/8
                        data = reform( data, 8, npts, /OVER)
                    endelse
                    data = rotate( data, 5)
                    if ( dtype EQ 5 ) then data = double(data, 0, npts) else $
                    if ( dtype EQ 9 ) then data = dcomplex(data,0,npts)
                    if sz(0) gt 0 then data = reform( data, sz(1:sz(0)), /OVER)$
			else data = data(0)

      endif else if bad_vms then data = conv_vax_unix(data, target = 'sparc') $

      else  byteorder, data, /DTOXDR

      end
     
; 6: Complex treated as FLOAT 

      7: return                             ;string

       8: BEGIN				    ;structure

	Ntag = N_tags( data )

	for t=0,Ntag-1 do  begin
          temp = data.(t)
          host_to_ieee, temp
          data.(t) = temp
        endfor 
       END

; 9: Double complex treated as double

     else: message,'Unrecognized dataype ' + strtrim(idltype,2)

 ENDCASE

 return
 end 

