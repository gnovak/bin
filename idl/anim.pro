pro anim,data, delay=delay
if (n_params() lt 1) then begin 
    print,''
    print,' Animate 1d data.' 
    print,''
    print,'USAGE anim,data, /delay=delay'
    print,'INPUT anim - 2d array of data where each 1d array data[*,i] is a snapshot'
    print,'   of the system.  Read with matrix() where each row of the text file is'
    print,'   a snapshot.'
    print,'      delay = delay between frames.  default 0'
    return
endif
if n_elements(delay) le 0 then delay=0
max=max(data)
min=min(data)
plot,data[*,0],YRANGE=[min,max],PSYM=4
sz = size(data)
for i=0, sz(2)-1 do begin
	oplot,data[*,i],PSYM=4
	wait,delay
	oplot,data[*,i],COLOR=!P.BACKGROUND,PSYM=4
endfor
end
