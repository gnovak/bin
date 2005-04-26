#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <sys/types.h>
#include <sys/stat.h>

  
int main(int argc,char **argv)
{
  int  i, a, b, size, n;
  char *ap, *bp;
  struct stat buffer; 
  FILE *fdin, *fdout;


  if(argc!=3)
    {
      fprintf(stdout,"Parameters are: <infile> <outfile>\n");
      exit(0);
    }

  if(!(fdin=fopen(argv[1],"r")))
    {
      printf("can't open `%s' for reading.\n", argv[1]);
      exit(0);
    }
  
  if(!(fdout=fopen(argv[2],"w")))
    {
      printf("can't open `%s' for writing.\n", argv[2]);
      exit(0);
    }

  ap=(char *)&a;
  bp=(char *)&b;


  stat(argv[1], &buffer);
  size= buffer.st_size;
  
  printf("size= %d\n", size);
  size/=4;

  for(n=0; n<size; n++)
    {
      /*
      if((n%10)==0)
	printf("%d  %d\n", n, size);
      */

      fread(&a, sizeof(int), 1, fdin);
      for(i=0;i<4;i++)
	bp[i]=ap[3-i];
      fwrite(&b, sizeof(int), 1, fdout);
    }

  fclose(fdin);
  fclose(fdout);

}




