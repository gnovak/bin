pro anim2,data, delay=delay
if (n_params() lt 1) then begin 
    print,''
    print,' Animate 2d data.' 
    print,''
    print,'USAGE anim2,data, /delay=delay'
    print,'INPUT data - 3d array of data where each 1d array data(*,*,i) is a snapshot'
    print,'   of the system.  Read with matrix() where each row of the text file is'
    print,'   a snapshot.'
    print,'      delay = delay between frames.  default 0'
    return
endif
if n_elements(delay) le 0 then delay=0
sz = size(data)
min=min(data)
max=max(data)
print,"min=",min," max=",max
for i=0, sz(3)-1 do begin
	mtv,data[*,*,i],min=min,max=max,/quiet
	wait,delay
endfor
end
