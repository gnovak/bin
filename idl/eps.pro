pro eps,fname,bpp=bitsperpixel,bw=blackandwhite
IF (not Keyword_Set(bitsperpixel)) THEN bpp=8
set_plot,'ps'
device,filename=fname
IF (not Keyword_Set(blackandwhite)) THEN begin
    device,/color
    device,bits_per_pixel=bpp
end
device,/encapsulated
end
