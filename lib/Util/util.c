#include <malloc.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <nr.h>
#include <nrutil.h>

#include "util.h"

#define TINY 1e-9
#define SOURCEFILE "util.c"
// #define WHITESPACE " \t"
#define WHITESPACE " \t\n"
#define COMMENT '#'
#define NR_END 0
#define FREE_ARG char*
#define NEWTON_MAX_X 1e10
#define NEWTON_MAX_ITER 1000

void 
get_params(const char fname[], const int npar, 
	   char **names, char **types, void **locs)
{
  FILE *inf;
  inf = efopen(fname, "r");
  get_params_file(inf, npar, names, types, locs);
}

void 
get_params_file(FILE *inf, const int npar, 
		char **names, char **types, void **locs)
{
  
  int end;
  char *buf;
  char *success;
  char *name;
  char *data;
  char *ptr;
  int i, j, k, found;
  int temp;
  char *arrformat, *arrdata;
  int arrmax, arrnumfound;
  char *arrnumname;

  success = cvector(0,npar-1);
  for (i=0; i<npar; i++) 
    success[i]=0;
  
  end = get_line(inf, &buf);
  while (!feof(inf)) {
    name = cvector(0,end);
    data = cvector(0,end);    
    ptr = buf;
    // skip whitespace 
    ptr += strspn(ptr, WHITESPACE);
    strcpy(name, ptr);
    // skip up to whitespace
    ptr += strcspn(ptr, WHITESPACE);
    strcpy(data, ptr);
    
    ptr=name;
    // skip up to whitespace
    ptr += strcspn(ptr, WHITESPACE);
    *ptr = '\0';

    found=0;
    for (i=0; i<npar; i++) {
      if(strcmp(name, names[i])==0) {
	if (success[i]!=0) {
	  fprintf(stderr, SOURCEFILE ":get_params_file():"
		  "Warning - resetting %s.\n", names[i]);
	}
	// instructions to read into an array
	if (types[i][0]=='#') {
	  if (sscanf(types[i], "#%d", &arrmax) != 1) {
	    fprintf(stderr, SOURCEFILE 
		    ":get_params_file():No max array size given for %s\n", names[i]);
	    continue;
	  }
	  arrformat=types[i];
	  // skip up to format spec
	  arrformat += strcspn(arrformat, WHITESPACE);
	  arrformat += strspn(arrformat, WHITESPACE);
	  arrdata = data;
	  for (j=0; j<arrmax; j++) {
	    if (strcmp(arrformat, "%lf")==0) 
	      temp = sscanf(arrdata, arrformat, ((double *) locs[i]) + j);
	    else if (strcmp(arrformat, "%f")==0) 
	      temp = sscanf(arrdata, arrformat, ((float *) locs[i]) + j);
	    else if (strcmp(arrformat, "%d")==0) 
	      temp = sscanf(arrdata, arrformat, ((int *) locs[i]) + j);
	    else 
	      fprintf(stderr, SOURCEFILE 
		      ":get_params_file():Sorry, not set up to deal with array formats %s in %s\n", 
		      arrformat, names[i]);	      
	    // skip to next token whitespace 
	    arrdata += strspn(arrdata, WHITESPACE);
	    arrdata += strcspn(arrdata, WHITESPACE);
	    if (1 == temp) {
	      success[i]=1;
	      found=1;
	    }
	    else 
	      break;
	  }
	  arrnumfound=0;
	  arrnumname = cvector(0,strlen(names[i])+1);
	  strcpy(arrnumname, "n");
	  strcat(arrnumname, names[i]);
	  for (k=0; k<npar; k++) {
	    if(strcmp(arrnumname, names[k])==0) {
	      // check to see if it's already been set
	      if (success[k]!=0) {
		fprintf(stderr, SOURCEFILE ":get_params_file():"
			"Warning - resetting %s.\n", names[k]);
	      }
	      * ((int *)locs[k])=j;
	      success[k]=1;
	      arrnumfound=1;
	      break;
	    }
	  }
	  //	  if (arrnumfound==0) {
	  //fprintf(stderr, SOURCEFILE 
	  //":get_params_file():Warning -- you should provide a variable named "
	  //"%s to hold number of elements read into %s\n", arrnumname, names[i]);
	  //	  }
	  //	  fprintf(stderr, SOURCEFILE 
	  //		  ":get_params_file():Read %d values for %s\n", j, names[i]);
	  free_cvector(arrnumname, 0, strlen(names[i]));
	}
	else {
	  temp = sscanf(data, types[i], locs[i]);
	  if (1 == temp) {
	    success[i]=1;
	    found=1;
	  }
	  else 
	    fprintf(stderr, SOURCEFILE 
		    ":get_params_file():Malformed line:%s\n", buf);     
	}
      }
    }

    if (found==0 && strlen(name) > 0)       
      fprintf(stderr, SOURCEFILE ":get_params_file():Malformed line:%s\n", buf);     
 
    free_cvector(data, 0, end);    
    free_cvector(name, 0, end);
    free_cvector(buf, 0, end);
    end = get_line(inf, &buf);    
  }  
  free_cvector(buf, 0, end);

  for (i=0; i<npar; i++) 
    if (success[i] == 0) 
      fprintf(stderr, SOURCEFILE ":get_params_file():%s not set.\n", names[i]);      

  free_cvector(success, 0,npar-1);
}

void * 
emalloc(const int size)
{
  void *ptr;
  ptr = malloc(size);
  if (ptr==NULL) {
    fprintf(stderr, SOURCEFILE ":emalloc():Couldn't allocate memory! aborting...\n");
    abort();
  }
  return ptr;
}

FILE *
efopen(const char fname[], const char mode[]) 
{
  FILE *fp;
  if ((fp=fopen(fname, mode))==NULL) {
    fprintf(stderr, SOURCEFILE ":efopen():Couldn't open file %s for mode %s!\n"
	    SOURCEFILE ":efopen():Dumping core.\n", fname, mode);
    abort();  // generate core dump
  }
  return fp;
}

void 
cvector_copy(char dest[], const char src[], 
	     const int beg, const int end) 
{
  int i;
  for (i=beg; i<= end; i++) 
    dest[i]=src[i];
}
    
// **********************
// Coordinate related stuff
// **********************

void 
f_euler_pass(float v[], const float phi, const float the, const float psi) 
{
  float newx, newy, newz;  
  float cpsi, spsi, cthe,sthe,cphi,sphi; // only compute trig funcs once
 
  cpsi=cos(psi);
  spsi=sin(psi);
  cphi=cos(phi);
  sphi=sin(phi);
  cthe=cos(the);
  sthe=sin(the);

  newx = v[0]*( cpsi*cphi - cthe*sphi*spsi) +
         v[1]*( cpsi*sphi + cthe*cphi*spsi) +
         v[3]*( spsi*sthe                     );

  newy = v[0]*(-spsi*cphi - cthe*sphi*cpsi) +
         v[1]*(-spsi*sphi + cthe*cphi*cpsi) +
         v[3]*( cpsi*sthe                     );

  newz = v[0]*  sthe*sphi +
         v[1]* -sthe*cphi +
         v[3]*  cthe                           ;
  v[0]=newx;
  v[1]=newy;
  v[2]=newz;
}

void 
f_euler_pass_inv(float v[], const float phi, const float the, const float psi) 
{
  float newx, newy, newz;
  float cpsi, spsi, cthe,sthe,cphi,sphi; // only compute trig funcs once

  cpsi=cos(psi);
  spsi=sin(psi);
  cphi=cos(phi);
  sphi=sin(phi);
  cthe=cos(the);
  sthe=sin(the);

  newx = v[0]*( cpsi*cphi - cthe*sphi*spsi) +
         v[1]*(-spsi*cphi - cthe*sphi*cpsi) +
         v[2]*( sthe*sphi                     );

  newy = v[0]*( cpsi*sphi + cthe*cphi*spsi) +
         v[1]*(-spsi*sphi + cthe*cphi*cpsi) +
         v[2]*(-sthe*cphi                     );

  newz = v[0]*  sthe*spsi +
         v[1]*  sthe*cpsi +
         v[2]*  cthe                           ;
  v[0]=newx;
  v[1]=newy;
  v[2]=newz;
}

// Goldstein constructs euler x-form as passive 
// I want it to be active.  Turns out that this means
// I want his inverse.  This is *my* inverse, which means
// it's his forward transform.
void 
f_euler_act(float v[], const float phi, const float the, const float psi) 
{
  f_euler_pass_inv(v,phi,the,psi);
}

// Goldstein constructs euler x-form as passive 
// I want it to be active.  Turns out that this means
// I want his inverse.  This is *my* inverse, which means
// it's his forward transform.
void 
f_euler_act_inv(float v[], const float phi, const float the, const float psi) 
{
  f_euler_pass(v,phi,the,psi);
}

void 
d_euler_pass(double v[], const double phi, const double the, const double psi) 
{
  double newx, newy, newz;  
  double cpsi, spsi, cthe,sthe,cphi,sphi; // only compute trig funcs once
 
  cpsi=cos(psi);
  spsi=sin(psi);
  cphi=cos(phi);
  sphi=sin(phi);
  cthe=cos(the);
  sthe=sin(the);

  newx = v[0]*( cpsi*cphi - cthe*sphi*spsi) +
         v[1]*( cpsi*sphi + cthe*cphi*spsi) +
         v[3]*( spsi*sthe                             )    ;

  newy = v[0]*(-spsi*cphi - cthe*sphi*cpsi) +
         v[1]*(-spsi*sphi + cthe*cphi*cpsi) +
         v[3]*( cpsi*sthe                             )    ;

  newz = v[0]*  sthe*sphi +
         v[1]* -sthe*cphi +
         v[3]*  cthe                                           ;
  v[0]=newx;
  v[1]=newy;
  v[2]=newz;
}

void 
d_euler_pass_inv(double v[], const double phi, const double the, const double psi) 
{
  double newx, newy, newz;
  double cpsi, spsi, cthe,sthe,cphi,sphi; // only compute trig funcs once

  cpsi=cos(psi);
  spsi=sin(psi);
  cphi=cos(phi);
  sphi=sin(phi);
  cthe=cos(the);
  sthe=sin(the);

  newx = v[0]*( cpsi*cphi - cthe*sphi*spsi) +
         v[1]*(-spsi*cphi - cthe*sphi*cpsi) +
         v[2]*( sthe*sphi                             )    ;

  newy = v[0]*( cpsi*sphi + cthe*cphi*spsi) +
         v[1]*(-spsi*sphi + cthe*cphi*cpsi) +
         v[2]*(-sthe*cphi                             )    ;

  newz = v[0]*  sthe*spsi +
         v[1]*  sthe*cpsi +
         v[2]*  cthe                                           ;
  v[0]=newx;
  v[1]=newy;
  v[2]=newz;
}

// Goldstein constructs euler x-form as passive 
// I want it to be active.  Turns out that this means
// I want his inverse.  This is *my* inverse, which means
// it's his forward transform.
void 
d_euler_act(double v[], const double phi, const double the, const double psi) 
{
  d_euler_pass_inv(v,phi,the,psi);
}

// Goldstein constructs euler x-form as passive 
// I want it to be active.  Turns out that this means
// I want his inverse.  This is *my* inverse, which means
// it's his forward transform.
void 
d_euler_act_inv(double v[], const double phi, const double the, const double psi) 
{
  d_euler_pass(v,phi,the,psi);
}

float 
f_sphere_r(const float pos[]) 
{
  return sqrt(pos[0]*pos[0]+pos[1]*pos[1]+pos[2]*pos[2]);
}

float 
f_sphere_theta(const float pos[]) 
{
  float r;
  r=f_sphere_r(pos);
  //  point is at the origin
  if (r==0) 
    return 0;
  else 
    return acos(pos[2]/r);
}

float 
f_sphere_phi(const float pos[]) 
{
 return atan2(pos[1],pos[0]);
}

float 
f_polar_r(const float pos[]) 
{
  return hypot(pos[1],pos[0]);
}

float 
f_polar_theta(const float pos[]) 
{
  return atan2(pos[1],pos[0]);
}

// these functions are somewhat redundant.  They're here
// to make other routines more transparent
float 
f_cylinTheta(const float pos[]) 
{
  return atan2(pos[1],pos[0]);
}

float 
f_cylinR(const float pos[]) 
{
  return hypot(pos[1],pos[0]);
}


double 
d_sphere_r(const double pos[]) 
{
  return sqrt(pos[0]*pos[0]+pos[1]*pos[1]+pos[2]*pos[2]);
}

double 
d_sphere_theta(const double pos[]) 
{
  double r;
  r=d_sphere_r(pos);
  //  point is at the origin
  if (r==0) 
    return 0;
  else 
    return acos(pos[2]/r);
}

double 
d_sphere_phi(const double pos[]) 
{
  // there's a lib finction for this
 return atan2(pos[1],pos[0]);
}

double 
d_polar_r(const double pos[]) 
{
  return hypot(pos[1],pos[0]);
}

double 
d_polar_theta(const double pos[]) 
{
  return atan2(pos[1],pos[0]);
}

// these functions are somewhat redundant.  They're here
// to make other routines more transparent
double 
d_cylinTheta(const double pos[]) 
{
  return atan2(pos[1],pos[0]);
}

double 
d_cylinR(const double pos[]) 
{
  return hypot(pos[1],pos[0]);
}

// **********************
// Vectors
// **********************

void 
dvector_dump(const double *v, const int beg, const int end, const char s[]) 
{
  int i;

  printf("%s = (", s);
  // it's i<end not i<=end b/c the last entry is a special case (no comma after it)
  for(i=beg;i<end;i++) 
    printf("%g , ", v[i]);
  // note that last entry hasn't been printed yet (to avoid superfluous comma)
  printf("%.3f)\n", v[end]);
  return;
}

void 
dvector_dump_file(const double v[], const int beg, const int end, const char fn[]) 
{
  int i;
  FILE *f;
  f=efopen(fn, "w");
  for (i=beg;i<=end;i++) 
    fprintf(f,"%g \n", v[i]);
  fclose(f);
}

void 
dvector_pair_dump_file(const double v[], const double *w, const int beg, const int end, const char fn[]) 
{
  int i;
  FILE *f;
  f=efopen(fn, "w");
  for (i=beg;i<=end;i++) 
    fprintf(f,"%g %g\n", v[i],w[i]);
  fclose(f);
}

void 
dvector_pair_dump_file_append(const double v[], const double w[], 
			      const int beg, const int end, const char fn[]) 
{
  int i;
  FILE *f;
  f=efopen(fn, "a");
  for (i=beg;i<=end;i++) 
    fprintf(f,"%g %g\n", v[i],w[i]);
  fclose(f);
}

void 
vector_dump(const float v[], const int beg, const int end, const char s[]) 
{
  int i;

  printf("%s = (", s);
  // it's i<end not i<=end b/c the last entry is a special case (no comma after it)
  for(i=beg;i<end;i++) 
    printf("%g , ", v[i]);
  // note that last entry hasn't been printed yet (to avoid superfluous comma)
  printf("%.3f)\n", v[end]);
  return;
}

void 
vector_dump_file(const float v[], const int beg, const int end, const char fn[]) 
{
  int i;
  FILE *f;
  f=efopen(fn, "w");
  for (i=beg;i<=end;i++) 
    fprintf(f,"%g \n", v[i]);
  fclose(f);
}

void 
vector_pair_dump_file(const float v[], const float w[], const int beg, const int end, const char fn[]) 
{
  int i;
  FILE *f;
  f=efopen(fn, "w");
  for (i=beg;i<=end;i++) 
    fprintf(f,"%g %g\n", v[i],w[i]);
  fclose(f);
}

void 
vector_pair_dump_file_append(const float v[], const float w[], const int beg, const int end, const char fn[]) 
{
  int i;
  FILE *f;
  f=efopen(fn, "a");
  for (i=beg;i<=end;i++) 
    fprintf(f,"%g %g\n", v[i],w[i]);
  fclose(f);
}

float 
f_dot(const float a[], const float b[], const int beg, const int end) 
{
  float result=0;
  int i;
  for (i=beg;i<=end;i++) 
    result += a[i]*b[i];
  return result;
}

 // cross(a,b,c) => a=bxc
void 
f_cross(float res[], const float a[], const float b[], const int beg, const int end) 
{
  if (end-beg+1 != 3) {
    fprintf(stderr, SOURCEFILE ":f_cross():Dimension != 3!\n");
    return;
  }
  res[beg] =   a[beg+1]*b[beg+2]-a[beg+2]*b[beg+1];
  res[beg+1] = a[beg+2]*b[beg  ]-a[beg  ]*b[beg+2];
  res[beg+2] = a[beg  ]*b[beg+1]-a[beg+1]*b[beg  ];
}

void 
vector_add(float res[], const float a[], const float b[], const int beg, const int end) 
{ 
  // add(a,b,c) => a=b+c  
  int i;
  for (i=beg;i<=end;i++) 
    res[i]=a[i]+b[i];
}

void 
vector_sub(float res[], const float a[], const float b[], const int beg, const int end) 
{ 
  // add(a,b,c) => a=b-c
  int i;
  for (i=beg;i<=end;i++) 
    res[i]=a[i]-b[i];
}

void 
vector_s_mul(float res[], const float s, const float a[], const int beg, const int end) 
{ 
  // vSMul(a,b,c) => a=b*c
  int i;
  for(i=beg;i<=end;i++) 
    res[i]=s*a[i];
}

float 
f_norm(const float v[], const int beg, const int end) 
{
  return sqrt(f_norm2(v, beg, end));
}

float 
f_norm2(const float v[], const int beg, const int end) 
{
  float d2=0;
  int i;
  for (i=beg;i<=end;i++) 
    d2 += v[i]*v[i];
  return d2;
}

float 
f_d(const float p1[], const float p2[], const int beg, const int end) 
{
  float d2=0;
  int i;
  for (i=beg;i<=end;i++) 
    d2 += (p1[i]-p2[i])*(p1[i]-p2[i]);
  return sqrt(d2);
}

double 
d_dot(const double a[], const double b[], const int beg, const int end) 
{
  double result=0;
  int i;
  for (i=beg;i<=end;i++) 
    result += a[i]*b[i];
  return result;
}

 // cross(a,b,c) => a=bxc
void 
d_cross(double res[], const double a[], const double b[], const int beg, const int end) 
{
  if (end-beg+1 != 3) {
    fprintf(stderr, SOURCEFILE ":d_cross():Dimension != 3!\n");
    return;
  }
  res[beg] =   a[beg+1]*b[beg+2]-a[beg+2]*b[beg+1];
  res[beg+1] = a[beg+2]*b[beg  ]-a[beg  ]*b[beg+2];
  res[beg+2] = a[beg  ]*b[beg+1]-a[beg+1]*b[beg  ];
}

void 
dvector_add(double res[], const double a[], const double b[], const int beg, const int end) 
{ 
  // add(a,b,c) => a=b+c  
  int i;
  for (i=beg;i<=end;i++) 
    res[i]=a[i]+b[i];
}

void 
dvector_sub(double res[], const double a[], const double b[], const int beg, const int end) 
{ 
  // add(a,b,c) => a=b-c
  int i;
  for (i=beg;i<=end;i++) 
    res[i]=a[i]-b[i];
}

void 
dvector_s_mul(double res[], const double s, const double a[], const int beg, const int end) 
{ 
  // vSMul(a,b,c) => a=b*c
  int i;
  for(i=beg;i<=end;i++) 
    res[i]=s*a[i];
}

double 
d_norm(const double v[], const int beg, const int end) 
{
  return sqrt(d_norm2(v, beg, end));
}

double 
d_norm2(const double v[], const int beg, const int end) 
{
  double d2=0;
  int i;
  for (i=beg;i<=end;i++) 
    d2 += v[i]*v[i];
  return d2;
}

double 
d_d(const double p1[], const double p2[], const int beg, const int end) 
{
  double d2=0;
  int i;
  for (i=beg;i<=end;i++) 
    d2 += (p1[i]-p2[i])*(p1[i]-p2[i]);
  return sqrt(d2);
}

// distance b/t two lines parameterized by two vectors: line=a*t+b, a,b vectors
// variables are named a-vector, etc.
float 
f_d_lines(const float av[], const float bv[], const float cv[], const float dv[],
	const int beg, const int end)
{
  float *p1; // point on line 1 closest to a given point on line 2
  float *p2; // point on line2 closest to point p1
  float result;

  p1=vector(beg,end);
  p2=vector(beg,end);
  result = f_d_lines_get_points(av,bv,cv,dv,p1,p2, beg, end);
  free_vector(p1,beg,end);
  free_vector(p2,beg,end);
  return result;
}

// distance b/t two lines parameterized by two vectors: line=a*t+b, a,b vectors
// variables are named a-vector, etc.
// also fills arrays p1 and p2 with the points of closest approach for particle
// 1 and particle 2
float 
f_d_lines_get_points(const float av[], const float bv[], const float cv[], const float dv[], 
		   float p1[], float p2[], const int beg, const int end) 
{
  float av2, cv2; // |av|^2 and |c|^2
  float t1,t2;  // two times to parameterize the lines
  float num, denom; // temp vars to hold numerator and denominator of ugly expressions
  float *bvMinusdv;  // variable to hold temp vector
  int i;

  // get norms and test for an error
  if ((av2=f_norm2(av,beg,end))<TINY) {
    // set p1 to be bv
    for (i=beg;i<=end;i++) 
      p1[i]=bv[i];    
    return f_d_line_to_point_get_point(cv, dv, bv, p2, beg, end);
  }
  if ((cv2=f_norm2(cv,beg,end))<TINY) {
    // set p2 to be dv
    for (i=beg;i<=end;i++) 
      p2[i]=dv[i];
    return f_d_line_to_point_get_point(av,bv,dv,p1, beg, end);
  }

  // Now we know that neither line is degernate.  
  // zero p1 and p2
  for(i=beg;i<=end;i++)
    p1[i]=p2[i]=0;

  //initialize bvMinusdv
  bvMinusdv = vector(beg, end);
  for(i=beg;i<=end;i++) 
    bvMinusdv[i]=bv[i]-dv[i];

  // here are the formulas.  They are not intuitive
  num=f_dot(av,cv, beg, end)*f_dot(av,bvMinusdv, beg, end) - 
    f_dot(av,av, beg, end)*f_dot(cv,bvMinusdv, beg, end);
  denom=f_dot(av,cv,beg,end)*f_dot(av,cv,beg,end) - 
    f_dot(av,av,beg,end)*f_dot(cv,cv,beg,end);

  // prevent divergence
  t2 = num/(fabs(denom) > TINY ? denom : TINY);

  // find t1
  t1 = (f_dot(av,cv,beg,end)*t2 + f_dot(av,dv,beg,end) - f_dot(av,bv,beg,end))/av2;

   // generate points of closest approach
  for (i=beg;i<=end;i++) {
    p1[i] = av[i]*t1 + bv[i];
    p2[i] = cv[i]*t2 + dv[i];
  }
 
  free_vector(bvMinusdv, beg, end);

  // return the distance
  return f_d(p1,p2, beg, end);
}

// distance b/t two lines specified by two points on each line
// notation is p<line><number>
float 
f_d_lines_points(const float p11[], const float p12[], const float p21[], const float p22[], 
		 const int beg, const int end) 
{
  float *a; // direction vector for line 1: line=a*t+offset
  float *c; // similarly for line 2
  int i;
  float result;
  
  a=vector(beg, end);
  c=vector(beg, end);

  // find vector giving direction of each line
  for (i=beg;i<=end;i++) {
    a[i]=p11[i]-p12[i];
    c[i]=p21[i]-p22[i];
  }

  result = f_d_lines(a,p11,c,p21, beg, end);

  free_vector(a,beg,end);
  free_vector(c,beg,end);
  return result;
}

float 
f_d_line_to_point(const float av[], const float bv[], const float p[], const int beg, const int end) 
{
  float *closePoint;
  float result;

  closePoint = vector(beg,end);
  result = f_d_line_to_point_get_point(av,bv,p,closePoint, beg, end);
  free_vector(closePoint, beg, end);
  return result;
}

float 
f_d_line_to_point_get_point(const float av[], const float bv[], const float p[], float pOut[],
			    const int beg, const int end) 
{
  int i,j;
  float av2; // |a|^2

  // get norms and test for a degenerate line.  If it is degenerate, 
  // only calculate the distance b/t the two points, p and b.
  if ((av2=f_norm2(av,beg,end))<TINY) {
    // the output point is by definition the offset vector b
    for(i=beg;i<=end;i++) 
      pOut[i]=bv[i];
    return f_d(pOut,p, beg, end);
  }

  // clear pOut
  for(i=beg;i<=end;i++) 
    pOut[i]=0;

  // calculate p1.  The formula is unintiutive, but (I believe) correct
  for(i=beg;i<=end;i++) 
    for(j=beg;j<=end;j++) 
      pOut[i]+=(av[i]*av[j]*p[j] - av[i]*av[j]*bv[j] + av[j]*av[j]*bv[i])/av2;
  return f_d(pOut,p,beg,end);
}

// distance b/t two lines parameterized by two vectors: line=a*t+b, a,b vectors
// variables are named a-vector, etc.
double 
d_d_lines(const double av[], const double bv[], const double cv[], const double dv[],
	const int beg, const int end)
{
  double *p1; // point on line 1 closest to a given point on line 2
  double *p2; // point on line2 closest to point p1
  double result;

  p1=dvector(beg,end);
  p2=dvector(beg,end);
  result = d_d_lines_get_points(av,bv,cv,dv,p1,p2, beg, end);
  free_dvector(p1,beg,end);
  free_dvector(p2,beg,end);
  return result;
}

// distance b/t two lines parameterized by two vectors: line=a*t+b, a,b vectors
// variables are named a-vector, etc.
// also fills arrays p1 and p2 with the points of closest approach for particle
// 1 and particle 2
double 
d_d_lines_get_points(const double av[], const double bv[], const double cv[], const double dv[], 
		   double p1[], double p2[], const int beg, const int end) 
{
  double av2, cv2; // |av|^2 and |c|^2
  double t1,t2;  // two times to parameterize the lines
  double num, denom; // temp vars to hold numerator and denominator of ugly expressions
  double *bvMinusdv;  // variable to hold temp vector
  int i;

  // get norms and test for an error
  if ((av2=d_norm2(av,beg,end))<TINY) {
    // set p1 to be bv
    for (i=beg;i<=end;i++) 
      p1[i]=bv[i];    
    return d_d_line_to_point_get_point(cv, dv, bv, p2,beg,end);
  }
  if ((cv2=d_norm2(cv,beg,end))<TINY) {
    // set p2 to be dv
    for (i=beg;i<=end;i++) 
      p2[i]=dv[i];
    return d_d_line_to_point_get_point(av,bv,dv,p1,beg,end);
  }

  // Now we know that neither line is degernate.  
  // zero p1 and p2
  for(i=beg;i<=end;i++)
    p1[i]=p2[i]=0;

  //initialize bvMinusdv
  bvMinusdv = dvector(beg, end);
  for(i=beg;i<=end;i++) 
    bvMinusdv[i]=bv[i]-dv[i];

  // here are the formulas.  They are not intuitive
  num=d_dot(av,cv,beg,end)*d_dot(av,bvMinusdv,beg,end) - d_dot(av,av,beg,end)*d_dot(cv,bvMinusdv,beg,end);
  denom=d_dot(av,cv,beg,end)*d_dot(av,cv,beg,end) - d_dot(av,av,beg,end)*d_dot(cv,cv,beg,end);

  // prevent divergence
  t2 = num/(fabs(denom) > TINY ? denom : TINY);

  // find t1
  t1 = (d_dot(av,cv,beg,end)*t2 + d_dot(av,dv,beg,end) - d_dot(av,bv,beg,end))/av2;

   // generate points of closest approach
  for (i=beg;i<=end;i++) {
    p1[i] = av[i]*t1 + bv[i];
    p2[i] = cv[i]*t2 + dv[i];
  }
 
  free_dvector(bvMinusdv, beg, end);

  // return the distance
  return d_d(p1,p2,beg,end);
}

// distance b/t two lines specified by two points on each line
// notation is p<line><number>
double 
d_d_lines_points(const double p11[], const double p12[], const double p21[], const double p22[],
		 const int beg, const int end) 
{
  double *a; // direction vector for line 1: line=a*t+offset
  double *c; // similarly for line 2
  double result;
  int i;
  
  a=dvector(beg,end);
  c=dvector(beg,end);

  // find vector giving direction of each line
  for (i=beg;i<=end;i++) {
    a[i]=p11[i]-p12[i];
    c[i]=p21[i]-p22[i];
  }

  result = d_d_lines(a,p11,c,p21, beg, end);
  free_dvector(a,beg,end);
  free_dvector(c,beg,end);  
  return result;
}

double 
d_d_line_to_point(const double av[], const double bv[], const double p[],
		  const int beg, const int end) 
{
  double *closePoint;
  float result;
  closePoint = dvector(beg,end);
  result = d_d_line_to_point_get_point(av,bv,p,closePoint,beg,end);
  free_dvector(closePoint,beg,end);
  return result;
}

double 
d_d_line_to_point_get_point(const double av[], const double bv[], const double p[], double pOut[],
			    const int beg, const int end) 
{
  int i,j;
  double av2; // |a|^2

  // get norms and test for a degenerate line.  If it is degenerate, 
  // only calculate the distance b/t the two points, p and b.
  if ((av2=d_norm2(av,beg,end))<TINY) {
    // the output point is by definition the offset vector b
    for(i=beg;i<=end;i++) 
      pOut[i]=bv[i];
    return d_d(pOut,p,beg,end);
  }

  // clear pOut
  for(i=beg;i<=end;i++) 
    pOut[i]=0;

  // calculate p1.  The formula is unintiutive, but (I believe) correct
  for(i=beg;i<=end;i++) 
    for(j=beg;j<=end;j++) 
      pOut[i]+=(av[i]*av[j]*p[j] - av[i]*av[j]*bv[j] + av[j]*av[j]*bv[i])/av2;

  return d_d(pOut,p,beg,end);
}


void 
matrix_dump(float **mat, const int rstart, const int rend, 
	    const int cstart, const int cend) 
{
  int i,j;
  
  for (i=rstart;i<=rend;i++) {
    for (j=cstart;j<=cend;j++) 
      printf("%9.4f",mat[i][j]);
    printf("\n");
  }
}

void 
matrix_dump_file(float **mat, const int rstart, const int rend, 
		 const int cstart, const int cend, const char fname[]) 
{
  int i,j;
  FILE *outf;

  outf = efopen(fname, "w");
  for (i=rstart;i<=rend;i++) {
    for (j=cstart;j<=cend;j++) 
      fprintf(outf, "%g ",mat[i][j]);
    fprintf(outf,"\n");
  }
}

void 
matrix_dump_transpose_file(float **mat, const int rstart, const int rend, 
			   const int cstart, const int cend, const char fname[]) 
{
  int i,j;
  FILE *outf;

  outf = efopen(fname, "w");
  for (j=cstart;j<=cend;j++) {
    for (i=rstart;i<=rend;i++) 
      fprintf(outf, "%g ",mat[i][j]);
    fprintf(outf,"\n");
  }
}

void 
matrix_dump_sm_file(float **mat, const int rstart, const int rend, 
		    const int cstart, const int cend, const char fname[],
		    const char header[]) 
{
  int i,j;
  FILE *outf;
  outf = efopen(fname, "w");
  fprintf(outf, header);  
  for (j=cstart;j<=cend;j++) {
    for (i=rstart;i<=rend;i++) 
      fprintf(outf, "%g ",mat[i][j]);
    fprintf(outf,"\n");
  }
}

void 
matrix_dump_transpose(float **mat, const int rstart, const int rend, 
		      const int cstart, const int cend) 
{
  int i,j;

  for (j=cstart;j<=cend;j++) {
    for (i=rstart;i<=rend;i++) 
      printf("%9.4f",mat[i][j]);
    printf("\n");
  }
}

int 
file_lines(const char infn[]) {
  FILE *inf;
  char *buf;
  int end;
  int lines = 0;
  float junk;
  
  inf = efopen(infn, "r");
  
  //  end = get_line(inf, &buf);
  //while (!feof(inf)) {
  //  // make sure there's at least one number on the line
  //  if (sscanf(buf, "%f", &junk) == 1) 
  //    lines++;
  //  free_cvector(buf, 0, end);
  //  end = get_line(inf, &buf);    
  //}

  do {
    end = get_line(inf, &buf);
    // make sure there's at least one number on the line
    if (sscanf(buf, "%f", &junk) == 1) 
      lines++;
    free_cvector(buf, 0, end);
  } while (!feof(inf));

  fclose(inf);
  return lines;
}

int 
file_cols(const char infn[]) {
  FILE *inf;
  char *buf;
  char *token;
  int end;
  int cols=0;
  float junk;
  inf = efopen(infn, "r");

  end = get_line(inf, &buf);

  // make sure there's at least one number in the line
  while (sscanf(buf, "%f", &junk) != 1 && !feof(inf)) {
    free_cvector(buf, 0, end);    
    end = get_line(inf, &buf);
  }

  // print error message if there are no lines w/ numbers
  if (feof(inf)) {
    fprintf(stderr, SOURCEFILE ":file_cols():No data in input file %s\n",
	    infn);
    return 0;
  }

  token = buf;
  while (sscanf(token, "%f", &junk) == 1) {
    cols++;
    // skip up to whitespace
    token += strcspn(token, WHITESPACE);
    // skip the whitespace itself
    token += strspn(token, WHITESPACE);
  }

  fclose(inf);
  return cols;
}

int 
get_line(FILE *inf, char ** output) {
  const int size_inc = 256;
  int size=0;
  char *buf;
  char *newbuf;
  int newsize;
  
  size = size_inc;
  buf = cvector(0,size-1);
  buf[0] = '\0';
  fgets(buf, size_inc, inf);
  
  while (strlen(buf) == size-1 && buf[size-2] != '\n') {
    newsize = size+size_inc;
    newbuf = cvector(0,newsize-1);
    cvector_copy(newbuf, buf, 0, size-1);
    free_cvector(buf, 0, size-1);    
    buf = newbuf;
    // the size_inc+1 is correct b/c we're overwriting the 
    // null character from the previous call to fgets
    fgets(buf+size-1, size_inc+1, inf);
    size = newsize;
  }
  if (buf[0] == COMMENT) {
    free_cvector(buf, 0, size-1);
    return get_line(inf, output);
  }
  else {
    *output = buf;
    return size-1;
  }
}

float 
matrix_interp(float **mat, float *rval, float *cval, 
	      int rstart, int rend, int cstart, int cend, 
	      float xr, float xc) 
{
  unsigned long ir_pre, ic_pre;
  float tr, tc;
  float result;
  int ir, ic;

  //  if (xr < rval[rstart] || xr > rval[rend] ||
  //    xc < cval[cstart] || xc > cval[cend]) 
  //  fprintf(stderr, SOURCEFILE ":matrix_interp():"
  //	    "Being asked to extrapolate!\n");

  // tmp tmp tmp think this is right.
  f_locate(rval+rstart-1, rend-rstart+1, xr, &ir_pre);
  f_locate(cval+cstart-1, cend-cstart+1, xc, &ic_pre);

  ir_pre = IMIN(IMAX(ir_pre, 1), rend-1);
  ic_pre = IMIN(IMAX(ic_pre, 1), cend-1);
  ir = ir_pre + rstart - 1;
  ic = ic_pre + cstart - 1;

  tr = (xr-rval[ir])/(rval[ir+1]-rval[ir]);
  tc = (xc-cval[ic])/(cval[ic+1]-cval[ic]);
  
  result = 
    (1-tr)*(1-tc)*mat[ir  ][ic  ] +
       tr *(1-tc)*mat[ir+1][ic  ] +
       tr *   tc *mat[ir+1][ic+1] +
    (1-tr)*   tc *mat[ir  ][ic+1] ;

  return result;
}

void 
matrix_copy(float **dest, float **src, 
	    const int rstart, const int rend,
	    const int cstart, const int cend)
{
  int ir, ic;
  for (ir=rstart; ir <= rend; ir++) 
    for (ic=cstart; ic <= cend; ic++) 
      dest[ir][ic] = src[ir][ic];
}

void 
matrix_transpose(float **dest, float **src, 
		 const int rstart, const int rend,
		 const int cstart, const int cend) 
{
  int ir, ic;
  for (ir=rstart; ir <= rend; ir++) 
    for (ic=cstart; ic <= cend; ic++) 
      dest[ic][ir] = src[ir][ic];
}

void
matrix_read_afile_lc(float ***mat, const int rstart, int *rend, 
		  const int cstart, int *cend, const char infn[]) 
{
  int lines, cols;
  lines = file_lines(infn);
  cols = file_cols(infn);
  *rend = rstart + lines - 1;
  *cend = cstart + cols - 1;
  *mat = matrix(rstart, *rend, cstart, *cend);
  matrix_fill_afile(*mat, rstart, *rend, cstart, *cend, infn);
}

void
matrix_read_afile_cl(float ***mat, const int rstart, int *rend, 
		  const int cstart, int *cend, const char infn[]) 
{
  float **pre_mat;
  int pre_cend;
  int pre_rend;

  matrix_read_afile_lc(&pre_mat, cstart, &pre_cend, 
		       rstart, &pre_rend, infn);

  *mat = matrix(rstart, pre_rend, cstart, pre_cend);
  matrix_transpose(*mat, pre_mat, cstart, pre_cend, rstart, pre_rend);
  *rend = pre_rend;
  *cend = pre_cend;
}

void
matrix_fill_afile_cl(float **mat, const int rstart, const int rend, 
		  const int cstart, const int cend, 
		  const char infn[]) 
{
  // do this in an extremely stupid way...
  float **mat2;

  mat2 = matrix(cstart,cend,rstart,rend);
  matrix_fill_afile_lc(mat2, cstart, cend, rstart, rend, infn);
  matrix_transpose(mat, mat2, cstart, cend, rstart, rend);
}

void
matrix_fill_afile(float **mat, const int rstart, const int rend, 
		  const int cstart, const int cend, 
		  const char infn[]) 
{  
  matrix_fill_afile_lc(mat, rstart, rend, cstart, cend, infn);
}

void
matrix_fill_afile_lc(float **mat, const int rstart, const int rend, 
		  const int cstart, const int cend, 
		  const char infn[]) 
{
  FILE *inf;
  int ir, ic;
  int line;
  char *buf;
  char *token;
  float value;
  int bufend;
  int success;
  int premature_eof = 0;

  if (strcmp(infn, "stdin")==0) 
    inf = stdin;
  else 
    inf = efopen(infn, "r");

  for (ir=rstart, line=1; ir<=rend; ir++, line++) {
    bufend = get_line(inf, &buf);
    token=buf;
    // skip comment lines
    if (sscanf(token, "%f", &value) != 1) {
      if (!feof(inf)) {
	ir--;
	continue;
      }
      else {
	// this eof flag needs a special case because if the file is one
	// line short and there's whitespace at the end, the code will 
	// skip the whitespace as comments, hit eof, and then try to convert
	// the line (because an eof at the end of the line is legal).  
	// Then you get conversion errors instead of eof errors, which could
	// possibly be misleading.
	fprintf(stderr, SOURCEFILE ":matrix_fill_afile_lc():"
		"Ran out of data in file %s\n",
		infn);
	premature_eof = 1;
	break;	
      }
    }
    for (ic=cstart; ic<=cend; ic++) {
      success = sscanf(token, "%f", &value);
      if (success==1)
	mat[ir][ic] = value;
      else {
	fprintf(stderr, SOURCEFILE ":matrix_fill_afile():"
		"Conversion error at line %d in %s\n",
		line, infn);	
      }
      // skip up to whitespace
      token += strcspn(token, WHITESPACE);
      // skip the whitespace itself
      token += strspn(token, WHITESPACE);
    }
    // check to make sure that there are no other numbers on the line
    if (sscanf(token, "%f", &value) == 1) {
	fprintf(stderr, SOURCEFILE ":matrix_fill_afile():"
		"Extra data on line %d in %s\n",
		line, infn);	
    }

    // must test for eof after reading the line, or else you 
    // lost the last line of the file
    // An eof is only an error if it occurs before the last line of 
    // the file
    if (feof(inf) && ir != rend) {
      fprintf(stderr, SOURCEFILE ":matrix_fill_afile_lc():"
	      "Ran out of data in file %s\n",
	      infn);
      premature_eof = 1;
      break;
    }

    free_cvector(buf, 0, bufend);
  }

  // And at last, make sure there are no lines with data remaining in the file
  while (!feof(inf)) {
    bufend = get_line(inf, &buf);
    if (sscanf(buf, "%f", &value) == 1) {
    fprintf(stderr, SOURCEFILE ":matrix_fill_afile():"
	    "Extra data beyond line %d in %s\n", 
	    line, infn);      
    }
    free_cvector(buf, 0, bufend);    
  }
}

void 
matrix_write_afile_lc(float **mat, const int rstart, const int rend, 
		       const int cstart, const int cend, const char fname[])
{
  matrix_dump_file(mat, rstart, rend, cstart, cend, fname);
}

void 
matrix_write_afile_cl(float **mat, const int rstart, const int rend, 
		       const int cstart, const int cend, const char fname[])
{
  matrix_dump_transpose_file(mat, rstart, rend, cstart, cend, fname);
}

void 
matrix_read_file(float **mat, const int rstart, const int rend, 
		 const int cstart, const int cend, FILE *inf) 
{
  int entries;
  entries = (rend-rstart+1)*(cend-cstart+1);

  if (entries != fread(&(mat[rstart][cstart]), sizeof(float), entries, inf)) 
    fprintf(stderr, SOURCEFILE ":matrix_read_file():Error reading matrix!\n");
}

void 
matrix_write_file(float **mat, const int rstart, const int rend, 
		  const int cstart, const int cend, FILE *of) 
{
  int entries;
  entries = (rend-rstart+1)*(cend-cstart+1);

  if (entries != fwrite(&(mat[rstart][cstart]), sizeof(float), entries, of)) 
    fprintf(stderr, SOURCEFILE ":matrix_write_file():Error writing matrix!\n");
}

// fills an arbitrary (not necessarily newly allocated) matrix with
// the identity matrix
void 
id_matrix(float **mat, const int rstart, const int rend, const int cstart, const int cend) 
{
  int i,j;
  for (i=rstart;i<=rend;i++) 
    for (j=cstart; j<=cend; j++) 
      if (i==j) 
	mat[i][j]=1;
      else 
	mat[i][j]=0;
}

// calculate the mean value of a matrix
float 
matrix_mean(float **mat, const int rstart, const int rend, const int cstart, const int cend) 
{
  int i,j;
  float total=0.0;

  for (i=rstart;i<=rend;i++) 
    for (j=cstart;j<=cend;j++) 
      total+=mat[i][j];

  return total/((rend-rstart+1)*(cend-cstart+1));
}

// calculate the mean value of a matrix
float 
matrix_rms(float **mat, const int rstart, const int rend, 
	   const int cstart, const int cend, const float mean) 
{
  int i,j;
  float total=0.0;

  for (i=rstart;i<=rend;i++) 
    for (j=cstart;j<=cend;j++) 
      total+=(mat[i][j]-mean)*(mat[i][j]-mean);

  return sqrt(total/((rend-rstart+1)*(cend-cstart+1)));
}

// recalculate RMS throwing away points more than n sigma 
// away from the mean
float 
matrix_nsig_rms(float **mat, const int rstart, const int rend, 
		const int cstart, const int cend,
		const float mean, const int n, const float RMS) 
{
  int i,j;
  float diff;
  float cutoff;
  int count=0;
  float total=0.0;

  // square the cutoff value for easy comparison
  cutoff=n*n*RMS*RMS;
  for (i=rstart;i<=rend;i++) 
    for (j=cstart;j<=cend;j++) {
      diff=(mat[i][j]-mean)*(mat[i][j]-mean);
      if (diff<=cutoff) {
	total+=diff;
	count++;
	printf("incl %g \n", mat[i][j]);
      }
    }
  return sqrt(total/((float)count));
}
// c=a+b
void 
matrix_add(float **matc, float **mata, float **matb, 
	  const int rstart, const int rend, const int cstart, const int cend) 
{
  int i,j;

  for (i=rstart;i<=rend;i++) 
    for (j=cstart;j<=cend;j++) 
      matc[i][j]=mata[i][j]+matb[i][j];
}

// c = a-b
void 
matrix_sub(float **matc, float **mata, float **matb, 
	  const int rstart, const int rend, const int cstart, const int cend) 
{
  int i,j;

  for (i=rstart;i<=rend;i++) 
    for (j=cstart;j<=cend;j++) 
      matc[i][j]=mata[i][j]-matb[i][j];
}


void 
dmatrix_dump(double **mat, const int rstart, const int rend, const int cstart, const int cend) 
{
  int i,j;
  
  for (i=rstart;i<=rend;i++) {
    for (j=cstart;j<=cend;j++) 
      printf("%9.4f",mat[i][j]);
    printf("\n");
  }
}

void 
dmatrix_dump_file(double **mat, const int rstart, const int rend, 
		 const int cstart, const int cend, const char fname[]) 
{
  int i,j;
  FILE *outf;

  outf = efopen(fname, "w");
  for (i=rstart;i<=rend;i++) {
    for (j=cstart;j<=cend;j++) 
      fprintf(outf, "%g ",mat[i][j]);
    fprintf(outf,"\n");
  }
}

void 
dmatrix_dump_transpose_file(double **mat, const int rstart, const int rend, 
			   const int cstart, const int cend, const char fname[]) 
{
  int i,j;
  FILE *outf;

  outf = efopen(fname, "w");
  for (j=cstart;j<=cend;j++) {
    for (i=rstart;i<=rend;i++) 
      fprintf(outf, "%g ",mat[i][j]);
    fprintf(outf,"\n");
  }
}

void 
dmatrix_dump_sm_file(double **mat, const int rstart, const int rend, 
		    const int cstart, const int cend, const char fname[],
		    const char header[]) 
{
  int i,j;
  FILE *outf;
  outf = efopen(fname, "w");
  fprintf(outf, header);  
  for (j=cstart;j<=cend;j++) {
    for (i=rstart;i<=rend;i++) 
      fprintf(outf, "%g ",mat[i][j]);
    fprintf(outf,"\n");
  }
}

void 
dmatrix_dump_transpose(double **mat, const int rstart, const int rend, 
		      const int cstart, const int cend) 
{
  int i,j;

  for (j=cstart;j<=cend;j++) {
    for (i=rstart;i<=rend;i++) 
      printf("%9.4f",mat[i][j]);
    printf("\n");
  }
}

double
dmatrix_interp(double **mat, double *rval, double *cval, 
	      int rstart, int rend, int cstart, int cend, 
	      double xr, double xc) 
{
  unsigned long ir_pre, ic_pre;
  double tr, tc;
  double result;
  int ir, ic;

  //  if (xr < rval[rstart] || xr > rval[rend] ||
  //    xc < cval[cstart] || xc > cval[cend]) 
  //  fprintf(stderr, SOURCEFILE ":matrix_interp():"
  //	    "Being asked to extrapolate!\n");

  // tmp tmp tmp correct?
  d_locate(rval+rstart-1, rend-rstart+1, xr, &ir_pre);
  d_locate(cval+cstart-1, cend-cstart+1, xc, &ic_pre);

  ir_pre = IMIN(IMAX(ir_pre, 1), rend-1);
  ic_pre = IMIN(IMAX(ic_pre, 1), cend-1);
  ir = ir_pre + rstart - 1;
  ic = ic_pre + cstart - 1;

  tr = (xr-rval[ir])/(rval[ir+1]-rval[ir]);
  tc = (xc-cval[ic])/(cval[ic+1]-cval[ic]);
  
  result = 
    (1-tr)*(1-tc)*mat[ir  ][ic  ] +
       tr *(1-tc)*mat[ir+1][ic  ] +
       tr *   tc *mat[ir+1][ic+1] +
    (1-tr)*   tc *mat[ir  ][ic+1] ;

  return result;
}

void 
dmatrix_copy(double **dest, double **src, 
	     const int rstart, const int rend,
	     const int cstart, const int cend)
{
  int ir, ic;
  for (ir=rstart; ir <= rend; ir++) 
    for (ic=cstart; ic <= cend; ic++) 
      dest[ir][ic] = src[ir][ic];
}

void 
dmatrix_transpose(double **dest, double **src, 
		 const int rstart, const int rend,
		 const int cstart, const int cend) 
{
  int ir, ic;
  for (ir=rstart; ir <= rend; ir++) 
    for (ic=cstart; ic <= cend; ic++) 
      dest[ic][ir] = src[ir][ic];
}

void
dmatrix_read_afile_lc(double ***mat, const int rstart, int *rend, 
		  const int cstart, int *cend, const char infn[]) 
{
  int lines, cols;
  lines = file_lines(infn);
  cols = file_cols(infn);
  *rend = rstart + lines - 1;
  *cend = cstart + cols - 1;
  *mat = dmatrix(rstart, *rend, cstart, *cend);
  dmatrix_fill_afile(*mat, rstart, *rend, cstart, *cend, infn);
}

void
dmatrix_read_afile_cl(double ***mat, const int rstart, int *rend, 
		  const int cstart, int *cend, const char infn[]) 
{
  double **pre_mat;
  int pre_cend;
  int pre_rend;

  dmatrix_read_afile_lc(&pre_mat, cstart, &pre_cend, 
			rstart, &pre_rend, infn);

  *mat = dmatrix(rstart, pre_rend, cstart, pre_cend);
  dmatrix_transpose(*mat, pre_mat, cstart, pre_cend, rstart, pre_rend);
  *rend = pre_rend;
  *cend = pre_cend;
}

void
dmatrix_fill_afile_cl(double **mat, const int rstart, const int rend, 
		  const int cstart, const int cend, 
		  const char infn[]) 
{
  // do this in an extremely stupid way...
  double **mat2;

  mat2 = dmatrix(cstart,cend,rstart,rend);
  dmatrix_fill_afile_lc(mat2, cstart, cend, rstart, rend, infn);
  dmatrix_transpose(mat, mat2, cstart, cend, rstart, rend);
}

void
dmatrix_fill_afile(double **mat, const int rstart, const int rend, 
		  const int cstart, const int cend, 
		  const char infn[]) 
{  
  dmatrix_fill_afile_lc(mat, rstart, rend, cstart, cend, infn);
}


void
dmatrix_fill_afile_lc(double **mat, const int rstart, const int rend, 
		  const int cstart, const int cend, 
		  const char infn[]) 
{
  FILE *inf;
  int ir, ic;
  int line;
  char *buf;
  char *token;
  double value;
  int bufend;
  int success;
  int premature_eof = 0;
  
  if (strcmp(infn, "stdin")==0) 
    inf = stdin;
  else 
    inf = efopen(infn, "r");
  
  for (ir=rstart, line=1; ir<=rend; ir++, line++) {
    bufend = get_line(inf, &buf);
    token=buf;
    // skip comment lines
    if (sscanf(token, "%lf", &value) != 1) {
      if (!feof(inf)) {
	ir--;
	continue;
      }
      else {
	// this eof flag needs a special case because if the file is one
	// line short and there's whitespace at the end, the code will 
	// skip the whitespace as comments, hit eof, and then try to convert
	// the line (because an eof at the end of the line is legal).  
	// Then you get conversion errors instead of eof errors, which could
	// possibly be misleading.
	fprintf(stderr, SOURCEFILE ":matrix_fill_afile_lc():"
		"Ran out of data in file %s\n",
		infn);
	premature_eof = 1;
	break;	
      }
    }
    for (ic=cstart; ic<=cend; ic++) {
      success = sscanf(token, "%lf", &value);
      if (success==1)
	mat[ir][ic] = value;
      else {
	fprintf(stderr, SOURCEFILE ":matrix_fill_afile():"
		"Conversion error at line %d in %s\n",
		line, infn);	
      }
      // skip up to whitespace
      token += strcspn(token, WHITESPACE);
      // skip the whitespace itself
      token += strspn(token, WHITESPACE);
    }
    // check to make sure that there are no other numbers on the line
    if (sscanf(token, "%lf", &value) == 1) {
	fprintf(stderr, SOURCEFILE ":matrix_fill_afile():"
		"Extra data on line %d in %s\n",
		line, infn);	
    }

    // must test for eof after reading the line, or else you 
    // lost the last line of the file
    // An eof is only an error if it occurs before the last line of 
    // the file
    if (feof(inf) && ir != rend) {
      fprintf(stderr, SOURCEFILE ":matrix_fill_afile_lc():"
	      "Ran out of data in file %s\n",
	      infn);
      premature_eof = 1;
      break;
    }

    free_cvector(buf, 0, bufend);
  }

  // And at last, make sure there are no lines with data remaining in the file
  while (!feof(inf)) {
    bufend = get_line(inf, &buf);
    if (sscanf(buf, "%lf", &value) == 1) {
    fprintf(stderr, SOURCEFILE ":matrix_fill_afile():"
	    "Extra data beyond line %d in %s\n", 
	    line, infn);      
    }
    free_cvector(buf, 0, bufend);    
  }
}

void 
dmatrix_write_afile_lc(double **mat, const int rstart, const int rend, 
		       const int cstart, const int cend, const char fname[])
{
  dmatrix_dump_file(mat, rstart, rend, cstart, cend, fname);
}

void 
dmatrix_write_afile_cl(double **mat, const int rstart, const int rend, 
		       const int cstart, const int cend, const char fname[])
{
  dmatrix_dump_transpose_file(mat, rstart, rend, cstart, cend, fname);
}

void 
dmatrix_read_file(double **mat, const int rstart, const int rend, 
		 const int cstart, const int cend, FILE *inf) 
{
  int entries;
  entries = (rend-rstart+1)*(cend-cstart+1);

  if (entries != fread(&(mat[rstart][cstart]), sizeof(double), entries, inf)) 
    fprintf(stderr, SOURCEFILE ":dmatrix_read_file():Error reading matrix!\n");
}

void 
dmatrix_write_file(double **mat, const int rstart, const int rend, 
		  const int cstart, const int cend, FILE *of) 
{
  int entries;
  entries = (rend-rstart+1)*(cend-cstart+1);

  if (of==NULL) {
    printf("Null pointer in matrixreadfile\n");
    return;
  }

  if (entries != fwrite(&(mat[rstart][cstart]), sizeof(double), entries, of)) 
    fprintf(stderr, SOURCEFILE ":dmatrix_write_file():Error writing matrix!\n");
}

// fills an arbitrary (not necessarily newly allocated) matrix with
// the identity matrix
void 
id_dmatrix(double **mat, const int rstart, const int rend, const int cstart, const int cend) 
{
  int i,j;
  for (i=rstart;i<=rend;i++) 
    for (j=cstart; j<=cend; j++) 
      if (i==j) 
	mat[i][j]=1;
      else 
	mat[i][j]=0;
}

// calculate the mean value of a matrix
double 
dmatrix_mean(double **mat, const int rstart, const int rend, const int cstart, const int cend) 
{
  int i,j;
  double total=0.0;

  for (i=rstart;i<=rend;i++) 
    for (j=cstart;j<=cend;j++) 
      total+=mat[i][j];

  return total/((rend-rstart+1)*(cend-cstart+1));
}

// calculate the mean value of a matrix
double 
dmatrix_rms(double **mat, const int rstart, const int rend, 
	   const int cstart, const int cend, const double mean) 
{
  int i,j;
  double total=0.0;

  for (i=rstart;i<=rend;i++) 
    for (j=cstart;j<=cend;j++) 
      total+=(mat[i][j]-mean)*(mat[i][j]-mean);

  return sqrt(total/((rend-rstart+1)*(cend-cstart+1)));
}

// recalculate RMS throwing away points more than n sigma 
// away from the mean
double 
dmatrix_nsig_rms(double **mat, const int rstart, const int rend, 
		const int cstart, const int cend,
		const double mean, const int n, const double RMS) 
{
  int i,j;
  double diff;
  double cutoff;
  int count=0;
  double total=0.0;

  // square the cutoff value for easy comparison
  cutoff=n*n*RMS*RMS;
  for (i=rstart;i<=rend;i++) 
    for (j=cstart;j<=cend;j++) {
      diff=(mat[i][j]-mean)*(mat[i][j]-mean);
      if (diff<=cutoff) {
	total+=diff;
	count++;
	printf("incl %g \n", mat[i][j]);
      }
    }
  return sqrt(total/((double)count));
}
// c=a+b
void 
dmatrix_add(double **matc, double **mata, double **matb, 
	  const int rstart, const int rend, const int cstart, const int cend) 
{
  int i,j;

  for (i=rstart;i<=rend;i++) 
    for (j=cstart;j<=cend;j++) 
      matc[i][j]=mata[i][j]+matb[i][j];
}

// c = a-b
void 
dmatrix_sub(double **matc, double **mata, double **matb, 
	  const int rstart, const int rend, const int cstart, const int cend) 
{
  int i,j;

  for (i=rstart;i<=rend;i++) 
    for (j=cstart;j<=cend;j++) 
      matc[i][j]=mata[i][j]-matb[i][j];
}

void 
f3tensor_read_file(float ***tens, const int sstart, const int send, const int rstart,
		   const int rend, const int cstart, const int cend, FILE *inf) 
{
  int s;
  for (s=sstart;s<=send;s++) 
    matrix_read_file(tens[s],rstart,rend,cstart,cend, inf); 
}

void 
f3tensor_write_file(float ***tens, const int sstart, const int send, const int rstart, 
		    const int rend, const int cstart, const int cend, FILE *of) 
{
  int s;
  for (s=sstart;s<=send;s++) 
    matrix_write_file(tens[s],rstart,rend,cstart,cend, of);
}
 
float 
f3tensor_interp(float ***tens, 
		float *sval, float *rval, float *cval, 
		int sstart, int send, 
		int rstart, int rend, 
		int cstart, int cend, 
		float xs, float xr, float xc) 
{
  unsigned long is_pre, ir_pre, ic_pre;
  float ts, tr, tc;
  float result;
  int is, ir, ic;

  if (xs < sval[sstart] || xs > sval[send] ||
      xr < rval[rstart] || xr > rval[rend] ||
      xc < cval[cstart] || xc > cval[cend]) 
    fprintf(stderr, SOURCEFILE ":f3tensor_interp():"
  	    "Being asked to extrapolate!\n");

  f_locate(sval-sstart+1, send-sstart+1, xs, &is_pre);
  f_locate(rval-rstart+1, rend-rstart+1, xr, &ir_pre);
  f_locate(cval-cstart+1, cend-cstart+1, xc, &ic_pre);

  is_pre = IMIN(IMAX(is_pre, 1), send-1);
  ir_pre = IMIN(IMAX(ir_pre, 1), rend-1);
  ic_pre = IMIN(IMAX(ic_pre, 1), cend-1);

  is = is_pre + sstart - 1;
  ir = ir_pre + rstart - 1;
  ic = ic_pre + cstart - 1;

  ts = (xs-sval[is])/(sval[is+1]-sval[is]);
  tr = (xr-rval[ir])/(rval[ir+1]-rval[ir]);
  tc = (xc-cval[ic])/(cval[ic+1]-cval[ic]);
  
  result = 
    (1-ts)*(1-tr)*(1-tc)*tens[is  ][ir  ][ic  ] +
    (1-ts)*   tr *(1-tc)*tens[is  ][ir+1][ic  ] +
    (1-ts)*   tr *   tc *tens[is  ][ir+1][ic+1] +
    (1-ts)*(1-tr)*   tc *tens[is  ][ir  ][ic+1] +
       ts *(1-tr)*(1-tc)*tens[is+1][ir  ][ic  ] +
       ts *   tr *(1-tc)*tens[is+1][ir+1][ic  ] +
       ts *   tr *   tc *tens[is+1][ir+1][ic+1] +
       ts *(1-tr)*   tc *tens[is+1][ir  ][ic+1] ;

  return result;
}

void 
d3tensor_read_file(double ***tens, const int sstart, const int send, const int rstart,
		   const int rend, const int cstart, const int cend, FILE *inf) 
{
  int s;
  for (s=sstart;s<=send;s++) 
    dmatrix_read_file(tens[s],rstart,rend,cstart,cend, inf); 
}

void 
f3tensor_fill_afile(float ***tens, const int sstart, const int send, 
		    const int rstart, const int rend, 
		    const int cstart, const int cend, const char fname[]) 
{
  FILE *inf;
  int is, ir, ic;

  inf = efopen(fname, "r");

  for (ic = cstart; ic <= cend; ic++) {	
    for (ir = rstart; ir <= rend; ir++) {
      for (is = sstart; is <= send; is++) {
	if (1 != fscanf(inf, "%f", &(tens[is][ir][ic]))) {
	  fprintf(stderr, SOURCEFILE ":f3tensor_fill_afile():Failed conversion!\n"); 
	  return;
	}
      }      
    }
  }
  fclose(inf);
}

void 
f3tensor_copy(float ***dest, float ***src, const int sstart, const int send, const int rstart,
		   const int rend, const int cstart, const int cend)
{
  int is, ir, ic;
  
  for (is = sstart; is <= send; is++) 
    for (ir = rstart; ir <= rend; ir++) 
      for (ic = cstart; ic <= cend; ic++) 
	dest[is][ir][ic]=src[is][ir][ic];
}

void 
f3tensor_dump(float ***tens, const int sstart, const int send, const int rstart,
		   const int rend, const int cstart, const int cend)
{
  int is, ir, ic;

  for (is = sstart; is <= send; is++) {
    for (ir = rstart; ir <= rend; ir++) {
      for (ic = cstart; ic <= cend; ic++) {	
	printf("%g ", tens[is][ir][ic]);
      }      
      printf("\n");
    }
    printf("\n");
  }  
}

double 
***d3tensor(long nrl, long nrh, long ncl, long nch, long ndl, long ndh)
/* allocate a double 3tensor with range t[nrl..nrh][ncl..nch][ndl..ndh] */
{
	long i,j,nrow=nrh-nrl+1,ncol=nch-ncl+1,ndep=ndh-ndl+1;
	double ***t;

	/* allocate pointers to pointers to rows */
	t=(double ***) malloc((size_t)((nrow+NR_END)*sizeof(double**)));
	if (!t) nrerror("allocation failure 1 in d3tensor()");
	t += NR_END;
	t -= nrl;

	/* allocate pointers to rows and set pointers to them */
	t[nrl]=(double **) malloc((size_t)((nrow*ncol+NR_END)*sizeof(double*)));
	if (!t[nrl]) nrerror("allocation failure 2 in d3tensor()");
	t[nrl] += NR_END;
	t[nrl] -= ncl;

	/* allocate rows and set pointers to them */
	t[nrl][ncl]=(double *) malloc((size_t)((nrow*ncol*ndep+NR_END)*sizeof(double)));
	if (!t[nrl][ncl]) nrerror("allocation failure 3 in d3tensor()");
	t[nrl][ncl] += NR_END;
	t[nrl][ncl] -= ndl;

	for(j=ncl+1;j<=nch;j++) t[nrl][j]=t[nrl][j-1]+ndep;
	for(i=nrl+1;i<=nrh;i++) {
		t[i]=t[i-1]+ncol;
		t[i][ncl]=t[i-1][ncl]+ncol*ndep;
		for(j=ncl+1;j<=nch;j++) t[i][j]=t[i][j-1]+ndep;
	}

	/* return pointer to array of pointers to rows */
	return t;
}

void 
free_d3tensor(double ***t, long nrl, long nrh, long ncl, long nch,
	long ndl, long ndh)
/* free a double d3tensor allocated by d3tensor() */
{
	free((FREE_ARG) (t[nrl][ncl]+ndl-NR_END));
	free((FREE_ARG) (t[nrl]+ncl-NR_END));
	free((FREE_ARG) (t+nrl-NR_END));
}

double 
d3tensor_interp(double ***tens, 
		double *sval, double *rval, double *cval, 
		int sstart, int send, 
		int rstart, int rend, 
		int cstart, int cend, 
		double xs, double xr, double xc) 
{
  unsigned long is_pre, ir_pre, ic_pre;
  double ts, tr, tc;
  double result;
  int is, ir, ic;

  if (xs < sval[sstart] || xs > sval[send] ||
      xr < rval[rstart] || xr > rval[rend] ||
      xc < cval[cstart] || xc > cval[cend]) 
    fprintf(stderr, SOURCEFILE ":d3tensor_interp():"
  	    "Being asked to extrapolate!\n");

  d_locate(sval-sstart+1, send-sstart+1, xs, &is_pre);
  d_locate(rval-rstart+1, rend-rstart+1, xr, &ir_pre);
  d_locate(cval-cstart+1, cend-cstart+1, xc, &ic_pre);

  is_pre = IMIN(IMAX(is_pre, 1), send-1);
  ir_pre = IMIN(IMAX(ir_pre, 1), rend-1);
  ic_pre = IMIN(IMAX(ic_pre, 1), cend-1);

  is = is_pre + sstart - 1;
  ir = ir_pre + rstart - 1;
  ic = ic_pre + cstart - 1;

  ts = (xs-sval[is])/(sval[is+1]-sval[is]);
  tr = (xr-rval[ir])/(rval[ir+1]-rval[ir]);
  tc = (xc-cval[ic])/(cval[ic+1]-cval[ic]);
  
  result = 
    (1-ts)*(1-tr)*(1-tc)*tens[is  ][ir  ][ic  ] +
    (1-ts)*   tr *(1-tc)*tens[is  ][ir+1][ic  ] +
    (1-ts)*   tr *   tc *tens[is  ][ir+1][ic+1] +
    (1-ts)*(1-tr)*   tc *tens[is  ][ir  ][ic+1] +
       ts *(1-tr)*(1-tc)*tens[is+1][ir  ][ic  ] +
       ts *   tr *(1-tc)*tens[is+1][ir+1][ic  ] +
       ts *   tr *   tc *tens[is+1][ir+1][ic+1] +
       ts *(1-tr)*   tc *tens[is+1][ir  ][ic+1] ;

  return result;
}


void 
f_locate(float xx[], unsigned long n, double x, unsigned long *j)
{
  locate(xx,n,x,j);
}

void 
d_locate(double xx[], unsigned long n, double x, unsigned long *j)
{
	unsigned long ju,jm,jl;
	int ascnd;

	jl=0;
	ju=n+1;
	ascnd=(xx[n] > xx[1]);
	while (ju-jl > 1) {
		jm=(ju+jl) >> 1;
		if (x > xx[jm] == ascnd)
			jl=jm;
		else
			ju=jm;
	}
	*j=jl;
}

void 
d3tensor_write_file(double ***tens, const int sstart, const int send, const int rstart, 
		    const int rend, const int cstart, const int cend, FILE *of) 
{
  int s;
  for (s=sstart;s<=send;s++) 
    dmatrix_write_file(tens[s],rstart,rend,cstart,cend, of);
}

void 
d3tensor_fill_afile(double ***tens, const int sstart, const int send, 
		    const int rstart, const int rend, 
		    const int cstart, const int cend, const char fname[]) 
{
  FILE *inf;
  int is, ir, ic;

  inf = efopen(fname, "r");

  for (ic = cstart; ic <= cend; ic++) {	
    for (ir = rstart; ir <= rend; ir++) {
      for (is = sstart; is <= send; is++) {
	if (1 != fscanf(inf, "%lf", &(tens[is][ir][ic]))) {
	  fprintf(stderr, SOURCEFILE ":d3tensor_fill_afile():Failed conversion!\n"); 
	  return;
	}
      }      
    }
  }
  fclose(inf);
}

void 
d3tensor_copy(double ***dest, double ***src, const int sstart, const int send, const int rstart,
		   const int rend, const int cstart, const int cend)
{
  int is, ir, ic;
  
  for (is = sstart; is <= send; is++) 
    for (ir = rstart; ir <= rend; ir++) 
      for (ic = cstart; ic <= cend; ic++) 
	dest[is][ir][ic]=src[is][ir][ic];
}

void 
d3tensor_dump(double ***tens, const int sstart, const int send, const int rstart,
		   const int rend, const int cstart, const int cend)
{
  int is, ir, ic;

  for (is = sstart; is <= send; is++) {
    for (ir = rstart; ir <= rend; ir++) {
      for (ic = cstart; ic <= cend; ic++) {	
	printf("%g ", tens[is][ir][ic]);
      }      
      printf("\n");
    }
    printf("\n");
  }  
}

void 
f_plot1x1(float (*func)(float), const float min, const float max, int npts, const char fn[]) 
{
  FILE *of;
  float x;
  int i;

  of=efopen(fn,"w");
  for (i=0;i<npts;i++) {
    x=min+(max-min)*i/((float)npts-1);
    fprintf(of,"%g %g\n", x, (*func)(x));
  }

  fclose(of);
}

// plot a polar function in 2dimensions
// write to file w/ indep variables x and y for ease of gnuplotting
void 
f_plot2x1polar(float (*func)(float,float), 
	       const float xmin, const float xmax, const int xpts,
	       const float ymin, const float ymax, const int ypts, 
	       const char fn[]) 
{
  FILE *of;
  float x,y;
  float r,th;
  int i,j;

  of=efopen(fn,"w");
  for (i=0;i<xpts;i++) {
    x=xmin+(xmax-xmin)*i/((float)xpts-1);
    for(j=0;j<ypts;j++) {
      y=ymin+(ymax-ymin)*j/((float)ypts-1);
      r=sqrt(x*x+y*y);
      th=atan2(y,x);
      fprintf(of,"%g %g %g\n", x, y, (*func)(r,th));
    }
  }
  
  fclose(of);
}

// plot a real valued function that takes two real arguments.
// Write a file a la gnuplot, w/ x,y,z triples on each line
void 
f_plot2x1(float (*func)(float,float), 
	  const float xmin, const float xmax, const int xpts,
	  const float ymin, const float ymax, const int ypts, 
	  const char *fn) 
{
  FILE *of;
  float x,y;
  int i,j;

  of=efopen(fn,"w");
  for (i=0;i<xpts;i++) {
    x=xmin+(xmax-xmin)*i/((float)xpts-1);
    for(j=0;j<ypts;j++) {
      y=ymin+(ymax-ymin)*j/((float)ypts-1);
      fprintf(of,"%g %g %g\n", x, y, (*func)(x,y));
    }
  }
  fclose(of);
}

// Plot a 2d function.  Write a "box" of numbers w/ the z values at each 
// position.  This if for later consumption by IDL
void 
f_plot2x1idl(float (*func)(float,float), 
	     const float xmin, const float xmax, const int xpts,
	     const float ymin, const float ymax, const int ypts, 
	     const char fn[]) 
{
  FILE *of;
  FILE *xf;
  FILE *yf;
  char *yfn;
  char *xfn;
  float x,y;
  int i,j;
  int cvect_len;
  
  cvect_len = strlen(fn)+2;
  xfn = cvector(0,cvect_len);
  yfn = cvector(0,cvect_len);

  sprintf(xfn, "%s.x", fn);
  sprintf(yfn, "%s.y", fn);
  of=efopen(fn,"w");
  xf=efopen(xfn,"w");
  yf=efopen(yfn,"w");

  for(j=0;j<ypts;j++) {
    y=ymin+(ymax-ymin)*j/((float)ypts-1);
    for (i=0;i<xpts;i++) {
      x=xmin+(xmax-xmin)*i/((float)xpts-1);
      fprintf(of,"%g ", (*func)(x,y));
    }
    fprintf(of,"\n");
  }

  // write x, y coords as vectors to facilitate curve fitting
  for(i=0;i<ypts;i++) {
    y=ymin+(ymax-ymin)*i/((float)ypts-1);
    fprintf(yf,"%g\n",y);
  }
  for(i=0;i<xpts;i++) {
    x=xmin+(xmax-xmin)*i/((float)xpts-1);
    fprintf(xf,"%g\n",x);
  }

  free_cvector(xfn, 0, cvect_len);
  free_cvector(yfn, 0, cvect_len);
  fclose(xf);
  fclose(yf);
  fclose(of);
}

// Plot a function from R^2 to R^1, and fill a matrix with the values
// in the plot.  This is for ease of post-processing.  Also, 
// write the output in IDL format (a square of numbers)
void 
f_plot2x1idl_mat(float (*func)(float,float), 
		 const float xmin, const float xmax, const int xpts,
		 const float ymin, const float ymax, const int ypts, 
		 const char fn[], float **mat) 
{
  FILE *of;
  float x,y;
  float val;
  int i,j;

  of=efopen(fn,"w");
  for(j=0;j<ypts;j++) {
    y=ymin+(ymax-ymin)*j/((float)ypts-1);
    for (i=0;i<xpts;i++) {
      x=xmin+(xmax-xmin)*i/((float)xpts-1);
      val=(*func)(x,y);
      fprintf(of,"%g ", val);
      mat[i][j]=val;
    }
    fprintf(of,"\n");
  }

  fclose(of);
}

// Routine to write a vector field to two files for consumption by IDL
// This could in principle handle an arbitrary number of in and out 
// variables, but for now I'm only interested in 2x2 and that's 
// probably all IDL can handle sensibly
void 
f_plot2x2idl(void (*func)(float,float,float *,float *), 
	     const float xmin, const float xmax, const int xpts,
	     const float ymin, const float ymax, const int ypts, 
	     const char fn[]) 
{
  FILE *ofx;
  FILE *ofy;
  char *fnx;
  char *fny;
  FILE *xf;
  FILE *yf;
  char *yfn;
  char *xfn;

  float x,y;
  float fx, fy;
  int i,j;
  int cvect_len;

  cvect_len = strlen(fn)+2;
  xfn = cvector(0,cvect_len);
  yfn = cvector(0,cvect_len);
  fnx = cvector(0,cvect_len);
  fny = cvector(0,cvect_len);

  // open data files
  sprintf(fnx,"x.%s",fn);
  sprintf(fny,"y.%s",fn);
  ofx=efopen(fnx,"w");
  ofy=efopen(fny,"w");

  // open vector files for x,y points of evaluation
  sprintf(yfn, "%s.y", fn);
  sprintf(xfn, "%s.x", fn);
  xf=efopen(xfn,"w");
  yf=efopen(yfn,"w");


  for(j=0;j<ypts;j++) {
    y=ymin+(ymax-ymin)*j/((float)ypts-1);
    for (i=0;i<xpts;i++) {
      x=xmin+(xmax-xmin)*i/((float)xpts-1);
      (*func)(x,y,&fx, &fy);
      fprintf(ofx, "%g ", fx);
      fprintf(ofy, "%g ", fy);
    }
    fprintf(ofx,"\n");
    fprintf(ofy,"\n");
  }

  // write x, y coords as vectors to facilitate curve fitting
  for(i=0;i<ypts;i++) {
    y=ymin+(ymax-ymin)*i/((float)ypts-1);
    fprintf(yf,"%g\n",y);
  }
  for(i=0;i<xpts;i++) {
    x=xmin+(xmax-xmin)*i/((float)xpts-1);
    fprintf(xf,"%g\n",x);
  }
  
  free_cvector(xfn, 0, cvect_len);
  free_cvector(yfn, 0, cvect_len);
  free_cvector(fnx, 0, cvect_len);
  free_cvector(fny, 0, cvect_len);
  fclose(ofx);
  fclose(ofy);
}

void 
f_plot1xn(void (*func)(float, float *), const int n, const float min, const float max, 
	  const int npts, const char fn[]) 
{
  FILE *of;
  float x;
  float *res;
  int i,j;

  res=vector(0,n-1);
  of=efopen(fn,"w");
  for (i=0;i<npts;i++) {
    x=min+(max-min)*i/((float)npts-1);
    (*func)(x,res);
    fprintf(of,"%g ", x);
    for (j=0;j<n;j++) 
      fprintf(of,"%g ", res[j]);
    fprintf(of,"\n");
  }
  free_vector(res,0,n-1);
  fclose(of);
}

float 
f_newton(float (*f)(const float,const float *), 
	 float (*fp)(const float, const float *), 
	 const float *par, const float start, const float eps)
{
  float xn;
  int iter=0;
  
  xn=start;
  while (fabs((*f)(xn,par))>=eps) {
    xn-=((*f)(xn,par))/((*fp)(xn,par));
    if (xn>=NEWTON_MAX_X) {
      fprintf(stderr, SOURCEFILE ":%s:Root at infinity or zero derivative?\n", "f_newton()");
      exit(1);
    }
    if (iter>NEWTON_MAX_ITER) {
      fprintf(stderr, SOURCEFILE ":%s:Too many iterations.\n", "f_newtion()");
      exit(1);
    }
    iter++;
  }
  return xn;
}

void 
d_plot1x1(double (*func)(double), const double min, const double max, int npts, const char fn[]) 
{
  FILE *of;
  double x;
  int i;

  of=efopen(fn,"w");
  for (i=0;i<npts;i++) {
    x=min+(max-min)*i/((double)npts-1);
    fprintf(of,"%g %g\n", x, (*func)(x));
  }

  fclose(of);
}

// plot a polar function in 2dimensions
// write to file w/ indep variables x and y for ease of gnuplotting
void 
d_plot2x1polar(double (*func)(double,double), 
	       const double xmin, const double xmax, const int xpts,
	       const double ymin, const double ymax, const int ypts, 
	       const char fn[]) 
{
  FILE *of;
  double x,y;
  double r,th;
  int i,j;

  of=efopen(fn,"w");
  for (i=0;i<xpts;i++) {
    x=xmin+(xmax-xmin)*i/((double)xpts-1);
    for(j=0;j<ypts;j++) {
      y=ymin+(ymax-ymin)*j/((double)ypts-1);
      r=sqrt(x*x+y*y);
      th=atan2(y,x);
      fprintf(of,"%g %g %g\n", x, y, (*func)(r,th));
    }
  }
  
  fclose(of);
}

// plot a real valued function that takes two real arguments.
// Write a file a la gnuplot, w/ x,y,z triples on each line
void 
d_plot2x1(double (*func)(double,double), 
	  const double xmin, const double xmax, const int xpts,
	  const double ymin, const double ymax, const int ypts, 
	  const char *fn) 
{
  FILE *of;
  double x,y;
  int i,j;

  of=efopen(fn,"w");
  for (i=0;i<xpts;i++) {
    x=xmin+(xmax-xmin)*i/((double)xpts-1);
    for(j=0;j<ypts;j++) {
      y=ymin+(ymax-ymin)*j/((double)ypts-1);
      fprintf(of,"%g %g %g\n", x, y, (*func)(x,y));
    }
  }
  fclose(of);
}

// Plot a 2d function.  Write a "box" of numbers w/ the z values at each 
// position.  This if for later consumption by IDL
void 
d_plot2x1idl(double (*func)(double,double), 
	     const double xmin, const double xmax, const int xpts,
	     const double ymin, const double ymax, const int ypts, 
	     const char fn[]) 
{
  FILE *of;
  FILE *xf;
  FILE *yf;
  char *yfn;
  char *xfn;
  double x,y;
  int i,j;
  int cvect_len;

  cvect_len = strlen(fn)+2;
  xfn = cvector(0,cvect_len);
  yfn = cvector(0,cvect_len);

  sprintf(yfn, "%s.y", fn);
  sprintf(xfn, "%s.x", fn);
  of=efopen(fn,"w");
  xf=efopen(xfn,"w");
  yf=efopen(yfn,"w");

  for(j=0;j<ypts;j++) {
    y=ymin+(ymax-ymin)*j/((double)ypts-1);
    for (i=0;i<xpts;i++) {
      x=xmin+(xmax-xmin)*i/((double)xpts-1);
      fprintf(of,"%g ", (*func)(x,y));
    }
    fprintf(of,"\n");
  }

  // write x, y coords as vectors to facilitate curve fitting
  for(i=0;i<ypts;i++) {
    y=ymin+(ymax-ymin)*i/((double)ypts-1);
    fprintf(yf,"%g\n",y);
  }
  for(i=0;i<xpts;i++) {
    x=xmin+(xmax-xmin)*i/((double)xpts-1);
    fprintf(xf,"%g\n",x);
  }

  free_cvector(xfn, 0, cvect_len);
  free_cvector(yfn, 0, cvect_len);
  fclose(xf);
  fclose(yf);
  fclose(of);
}

// Plot a function from R^2 to R^1, and fill a matrix with the values
// in the plot.  This is for ease of post-processing.  Also, 
// write the output in IDL format (a square of numbers)
void 
d_plot2x1idl_mat(double (*func)(double,double), 
		 const double xmin, const double xmax, const int xpts,
		 const double ymin, const double ymax, const int ypts, 
		 const char fn[], double **mat) 
{
  FILE *of;
  double x,y;
  double val;
  int i,j;

  of=efopen(fn,"w");
  for(j=0;j<ypts;j++) {
    y=ymin+(ymax-ymin)*j/((double)ypts-1);
    for (i=0;i<xpts;i++) {
      x=xmin+(xmax-xmin)*i/((double)xpts-1);
      val=(*func)(x,y);
      fprintf(of,"%g ", val);
      mat[i][j]=val;
    }
    fprintf(of,"\n");
  }

  fclose(of);
}

// Routine to write a vector field to two files for consumption by IDL
// This could in principle handle an arbitrary number of in and out 
// variables, but for now I'm only interested in 2x2 and that's 
// probably all IDL can handle sensibly
void 
d_plot2x2idl(void (*func)(double,double,double *,double *), 
	     const double xmin, const double xmax, const int xpts,
	     const double ymin, const double ymax, const int ypts, 
	     const char fn[]) 
{
  FILE *ofx;
  FILE *ofy;
  char *fnx;
  char *fny;
  FILE *xf;
  FILE *yf;
  char *yfn;
  char *xfn;

  double x,y;
  double fx, fy;
  int i,j;
  int cvect_len;

  cvect_len = strlen(fn)+2;
  xfn = cvector(0,cvect_len);
  yfn = cvector(0,cvect_len);
  fnx = cvector(0,cvect_len);
  fny = cvector(0,cvect_len);

  sprintf(fnx,"x.%s",fn);
  sprintf(fny,"y.%s",fn);
  ofx=efopen(fnx,"w");
  ofy=efopen(fny,"w");

  // open vector files for x,y points of evaluation
  sprintf(yfn, "%s.y", fn);
  sprintf(xfn, "%s.x", fn);
  xf=efopen(xfn,"w");
  yf=efopen(yfn,"w");


  for(j=0;j<ypts;j++) {
    y=ymin+(ymax-ymin)*j/((double)ypts-1);
    for (i=0;i<xpts;i++) {
      x=xmin+(xmax-xmin)*i/((double)xpts-1);
      (*func)(x,y,&fx, &fy);
      fprintf(ofx, "%g ", fx);
      fprintf(ofy, "%g ", fy);
    }
    fprintf(ofx,"\n");
    fprintf(ofy,"\n");
  }

  // write x, y coords as vectors to facilitate curve fitting
  for(i=0;i<ypts;i++) {
    y=ymin+(ymax-ymin)*i/((double)ypts-1);
    fprintf(yf,"%g\n",y);
  }
  for(i=0;i<xpts;i++) {
    x=xmin+(xmax-xmin)*i/((double)xpts-1);
    fprintf(xf,"%g\n",x);
  }
  
  free_cvector(xfn, 0, cvect_len);
  free_cvector(yfn, 0, cvect_len);
  free_cvector(fnx, 0, cvect_len);
  free_cvector(fny, 0, cvect_len);
  fclose(ofx);
  fclose(ofy);
}

void 
d_plot1xn(void (*func)(double, double *), const int n, const double min, const double max, 
	  const int npts, const char fn[]) 
{
  FILE *of;
  double x;
  double *res;
  int i,j;

  res=dvector(0,n-1);
  of=efopen(fn,"w");
  for (i=0;i<npts;i++) {
    x=min+(max-min)*i/((double)npts-1);
    (*func)(x,res);
    fprintf(of,"%g ", x);
    for (j=0;j<n;j++) 
      fprintf(of,"%g ", res[j]);
    fprintf(of,"\n");
  }
  free_dvector(res,0,n-1);
  fclose(of);
}

double 
d_newton(double (*f)(const double,const double *), 
	 double (*fp)(const double, const double *), 
	 const double *par, const double start, const double eps)
{
  double xn;
  int iter=0;
  
  xn=start;
  while (fabs((*f)(xn,par))>=eps) {
    xn-=((*f)(xn,par))/((*fp)(xn,par));
    if (xn>=NEWTON_MAX_X) {
      fprintf(stderr, SOURCEFILE ":%s:Root at infinity or zero derivative?\n", "d_newton()");
      exit(1);
    }
    if (iter>NEWTON_MAX_ITER) {
      fprintf(stderr, SOURCEFILE ":%s:Too many iterations.\n", "d_newton()");
      exit(1);
    }
    iter++;
  }
  return xn;
}

