SOURCE = util.c d_bssstep.c d_mmid.c d_odeint.c d_pzextr.c d_rzextr.c d_rkck.c d_rkqs.c d_mnewt.c d_ludcmp.c d_lubksb.c d_fdjac.c

CC = gcc
CFLAGS = -g -I /home/hyde/novak/bin/local/include -O3
OUTPUT = ../libutil.a
LDOUT = -o

OBJECT = $(SOURCE:.c=.o)

all: $(OBJECT)
	ar rcs $(OUTPUT) $(OBJECT) 

clean: 
	rm -f $(OBJECT) $(OUTPUT) core 

