#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>


int main(int argc,char **argv)
{
  int  i, a, b;

  int buf[8][2];

  FILE *fdin, *fdout;


  if(argc!=2)
    {
      fprintf(stdout,"Parameters are: <snapfile>\n");
      exit(0);
    }

  if(!(fdin=fopen(argv[1],"r+")))
    {
      printf("can't open `%s' for reading and writing.\n", argv[1]);
      exit(0);
    }
 


  fseek(fdin, 7*4, SEEK_SET);
  fread(&buf[0][0], sizeof(double), 8, fdin);
  
  for(i=0;i<8;i++)
    {
      a=buf[i][0]; buf[i][0]=buf[i][1]; buf[i][1]=a;
    }

  fseek(fdin, 7*4, SEEK_SET);
  fwrite(&buf[0][0], sizeof(double), 8, fdin);
  



  fseek(fdin, 7*4 + 8*8 + 10*4, SEEK_SET);
  fread(&buf[0][0], sizeof(double), 4, fdin);
  
  for(i=0;i<4;i++)
    {
      a=buf[i][0]; buf[i][0]=buf[i][1]; buf[i][1]=a;
    }

  fseek(fdin, 7*4 + 8*8 +10*4, SEEK_SET);
  fwrite(&buf[0][0], sizeof(double), 4, fdin);
  







  fclose(fdin);

}




