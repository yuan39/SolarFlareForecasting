pro  plot_vel,  x, y, vx, vy, scale,  HSIZE = hsize, COLOR = color, HTHICK = hthick, $
	THICK = thick, DATA = data, DEVICE = device, NORMALIZED = norm, $
	SOLID = solid, HANGLE=hangle
;+
;  NAME: plot_vel
;  PURPOSE:
;       Draw velocity vectors at given spatial points	
;  CALLING SEQUENCE:
;      plot_vel, x, y, vx, vy, scale
;  INPUTS:
;    x, y    positions
;   vx, vy	velocity vectors
;   scale      velocity to displacement conversion factor (scalar or a two-element array)
;  KEYWORDS
;    HSIZE, COLOR, HTHICK, THICK, DATA, DEVICE, NORMALIZED, SOLID, HANGLE
;           See ARROW for the explnations of each keyword
;-


;if n_elements(scale) eq 1 then caley=scale else scaley=scale(1)
scalex=scale
scaley=scale	
	 arrow_c, x, y, x+vx*scalex, y+ vy*scaley, $
	 HSIZE = hsize, COLOR = color, HTHICK = hthick, $
	 THICK = thick, DATA = data, DEVICE = device, NORMALIZED = norm, $
       	SOLID = solid
	
end	
	
	
