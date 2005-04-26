
EXEC   = Swap


SRCS   = main.c 

OBJS   = $(SRCS:.c=.o)

INCL   = 


CC     =  cc

CFLAGS =  -O2


LIBS   =  -lm  


%.o: %.c
	$(CC)  -c $(CFLAGS) $< -o $@



$(EXEC): $(OBJS) 
	$(CC) $(OBJS) $(LIBS)  -o  $(EXEC)

$(OBJS): $(INCL) 


