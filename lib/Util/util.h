#include <stdio.h>

void d_fdjac(int n, double x[], double fvec[], double **df,
	   void (*vecfunc)(int, double [], double []));
void d_lubksb(double **a, int n, int *indx, double b[]);
void d_ludcmp(double **a, int n, int *indx, double *d);
void d_mnewt(int ntrial, double x[], int n, double tolx, double tolf);
void d_rkqs(double y[], double dydx[], int n, double *x, double htry, double eps,
	double yscal[], double *hdid, double *hnext,
	    void (*derivs)(double, double [], double []));
void d_rkck(double y[], double dydx[], int n, double x, double h, double yout[],
	    double yerr[], void (*derivs)(double, double [], double []));
void d_bsstep(double y[], double dydx[], int nv, double *xx, double htry, double eps,
	      double yscal[], double *hdid, double *hnext,
	      void (*derivs)(double, double [], double []));
void d_mmid(double y[], double dydx[], int nvar, double xs, double htot, int nstep,
	    double yout[], void (*derivs)(double, double[], double[]));
void d_odeint(double ystart[], int nvar, double x1, double x2, double eps, double h1,
	      double hmin, int *nok, int *nbad,
	      void (*derivs)(double, double [], double []),
	      void (*rkqs)(double [], double [], int, double *, double, double, double [],
			   double *, double *, void (*)(double, double [], double [])));
void d_pzextr(int iest, double xest, double yest[], double yz[], double dy[], int nv);


// *********************************
// non float/double stuff
// *********************************

void get_params(const char fname[], const int npar, 
		//		char * names[], char **types, void **locs);
		char *name[], char *types[], void *locs[]);
void get_params_file(FILE *inf, const int npar, 
		     char **names, char **types, void **locs);


// Open a file with error reporting.  Saves you from testing 
// for null pointers all the time.  Writes a message to stderr
// and exits the program if a file isn't opened.
FILE * efopen(const char fname[], const char mode[]);

void * emalloc(const int size);

// copy character vector from src to dest.  Used for buffered file reads
void cvector_copy(char dest[], const char src[], 
		  const int beg, const int end);

// Buffered read of file *inf.  
// ***** This routine allocates memory!!! ******
// get_line reads up to a newline, reading chunks of the file and 
// allocating new memory as needed to accomodate the entire line.
// Points *output[0..return_value-1] at the allocated array.
// You *must* call free_cvector(*output, 0, return_val) to free
// the memory.
// Access the array with: for (i=0; i<=return_val; i++)
int get_line(FILE *inf, char ** output);
// get the number of lines with at least one number on them in a file
// So, allocating matrix(1,file_lines("fname"), 1, rows) will give you 
// enough memory to read the entire file into memory.
int file_lines(const char infn[]);
// get the number of numbers on the first line that actually includes
// numbersa in the file infn.  That is, lines that start with text and 
// comments at the beginning of the file are skipped.
// So, allocating matrix(1,file_cols("fname"), 1, cols) will give you enough 
// memory to read the entire file into memory.
int file_cols(const char infn[]);

// **********************
// Coordinate related stuff
// **********************

// Goldstein constructs euler x-form as passive 
void f_euler_pass(float v[], const float phi, const float the, const float psi);
void f_euler_pass_inv(float v[], const float phi, const float the, const float psi);
// Active transformation is the inverse of the passive transformation
void f_euler_act(float v[], const float phi, const float the, const float psi);
void f_euler_act_inv(float v[], const float phi, const float the, const float psi);

// converts pos[0..2] euclidean coordinates to spherical r
float f_sphere_r(const float pos[]);
// converts pos[0..2] euclidean coordinates to spherical theta, defined as the angle
// down from the z axis.  Theta is in the range 0 <= theta <= PI
float f_sphere_theta(const float pos[]);
// converts pos[0..2] euclidean coordinates to spherical phi, defined as the angle of 
// rotation about the z axis where phi==0 => point is on x axis.  Phi is in the
// range -PI < phi <= PI
float f_sphere_phi(const float pos[]);
float f_polar_r(const float pos[]);
float f_polar_theta(const float pos[]);
float f_cylinTheta(const float pos[]);
float f_cylinR(const float pos[]);

void vector_dump(const float v[], const int beg, const int end, const char s[]);
void vector_dump_file(const float v[], const int beg, const int end, const char fn[]);
void vector_pair_dump_file(const float v[], const float w[], 
			   const int beg, const int end, const char fn[]);
void vector_pair_dump_file_append(const float v[], const float w[], 
				  const int beg, const int end, const char fn[]);

float f_dot(const float a[], const float b[], const int beg, const int end);
// cross(a,b,c) => a=bxc
void f_cross(float res[], const float a[], const float b[], const int beg, const int end);
void vector_add(float res[], const float a[], const float b[], const int beg, const int end);
void vector_sub(float res[], const float a[], const float b[], const int beg, const int end);
void vector_s_mul(float res[], const float s, const float a[], const int beg, const int end);

float f_norm(const float v[], const int beg, const int end);
float f_norm2(const float v[], const int beg, const int end);
float f_d(const float p1[], const float p2[], const int beg, const int end);

// distance b/t two lines parameterized by two vectors: line=a*t+b, a,b vectors
// variables are named a-vector, etc.
float f_d_lines(const float av[], const float bv[], const float cv[], const float dv[],
		const int beg, const int end);
// distance b/t two lines parameterized by two vectors: line=a*t+b, a,b vectors
// variables are named a-vector, etc.
// also fills arrays p1 and p2 with the points of closest approach for particle
// 1 and particle 2
float f_d_lines_get_points(const float av[], const float bv[], const float cv[], const float dv[], 
			   float p1[], float p2[], const int beg, const int end);

// distance b/t two lines specified by two points on each line
// notation is p<line><number>
float f_d_lines_points(const float p11[], const float p12[], const float p21[], const float p22[], 
		       const int beg, const int end);

float f_d_line_to_point(const float av[], const float bv[], const float p[], 
			const int beg, const int end);

float f_d_line_to_point_get_point(const float av[], const float bv[], 
				  const float p[], float pOut[],
				  const int beg, const int end);
void f_locate(float xx[], unsigned long n, double x, unsigned long *j);

void matrix_copy(float **dest, float **src, 
		 const int rstart, const int rend,
		 const int cstart, const int cend);
void matrix_transpose(float **dest, float **src, 
		      const int rstart, const int rend,
		      const int cstart, const int cend);
void matrix_dump(float **mat, const int rstart, const int rend, 
		 const int cstart, const int cend);
void matrix_dump_file(float **mat, const int rstart, const int rend, 
		       const int cstart, const int cend, const char fname[]);
void matrix_dump_transpose_file(float **mat, const int rstart, const int rend, 
				const int cstart, const int cend, const char fname[]);
void matrix_dump_sm_file(float **mat, const int rstart, const int rend, 
			 const int cstart, const int cend, const char fname[],
		      const char header[]);
void matrix_dump_transpose(float **mat, const int rstart, const int rend, 
			   const int cstart, const int cend);
void matrix_read_file(float **mat, const int rstart, const int rend, 
		      const int cstart, const int cend, FILE *inf);
void matrix_write_file(float **mat, const int rstart, const int rend, 
		       const int cstart, const int cend, FILE *of);
void matrix_fill_afile_lc(float **mat, const int rstart, const int rend, 
		       const int cstart, const int cend, const char infn[]);
void matrix_fill_afile_cl(float **mat, const int rstart, const int rend, 
		       const int cstart, const int cend, const char infn[]);
void matrix_fill_afile(float **mat, const int rstart, const int rend, 
		       const int cstart, const int cend, const char infn[]);
// Fully dynamic reading of a file into memory.  Get the number 
// of lines in infn with at least one floating point number on the line
// (ie, blank lines, lines with text, etc, are skipped automatically).
// Read to the first line with numbers, and get the number of values
// on that line.  Allocate an appropriately sized matrix and point *mat at it.
// Fill *rend and *cend with the appropriate values indicating the size
// of the matrix.  Finally read the actual values into the matrix.  
// Print messages for stderr if a line contains too many values,
// not enough values, or if the program reaches eof before
// reading the correct number of lines (this would of course be a bug
// since this function is supposed to figure out how many lines to read
// by itself.
//
// Finally, you're left with a matrix:
// (*mat)[rstart..*rend][cstart..*cend] where you access values according to
// (*mat)[line][column] in the file. 
//
// ***** Note that this routine allocates memory! *****
// Free it with free_matrix(*mat, rstart, *rend, cstart, *cend);
void matrix_read_afile_lc(float ***mat, const int rstart, int *rend, 
			  const int cstart, int *cend, const char infn[]);
// Same as matrix_read_afile_lc, except that this routine reads the transpose 
// of the matrix.  So you access the matrix with (*mat)[column][line].
// ***** Note that this routine allocates memory! *****
// Free it with free_matrix(*mat, rstart, *rend, cstart, *cend);
void matrix_read_afile_cl(float ***mat, const int rstart, int *rend, 
			  const int cstart, int *cend, const char infn[]);
void matrix_write_afile_lc(float **mat, const int rstart, const int rend, 
			    const int cstart, const int cend, const char fname[]);
void matrix_write_afile_cl(float **mat, const int rstart, const int rend, 
			    const int cstart, const int cend, const char fname[]);

void id_matrix(float **mat, const int rstart, const int rend, 
	       const int cstart, const int cend);

float matrix_interp(float **mat, float *rval, float *cval, 
		    int rstart, int rend, int cstart, int cend, 
		    float xr, float xc);
// calculate the mean value of a matrix
float matrix_mean(float **mat, const int rstart, const int rend, 
		  const int cstart, const int cend);

// calculate the mean value of a matrix
float matrix_rms(float **mat, const int rstart, const int rend, 
		 const int cstart, const int cend, const float mean);

// recalculate RMS throwing away points more than n sigma 
// away from the mean
float matrix_nsig_rms(float **mat, const int rstart, const int rend, 
		      const int cstart, const int cend,
		      const float mean, const int n, const float RMS);

// c=a+b
void matrix_add(float **matc, float **mata, float **matb, 
		const int rstart, const int rend, const int cstart, const int cend);
// c = a-b
void matrix_sub(float **matc, float **mata, float **matb, 
		const int rstart, const int rend, const int cstart, const int cend);

void f3tensor_copy(float ***dest, float ***src, 
		   const int sstart, const int send, const int rstart,
		   const int rend, const int cstart, const int cend);
void f3tensor_dump(float ***tens, const int sstart, const int send, const int rstart,
		   const int rend, const int cstart, const int cend);

void f3tensor_read_file(float ***tens, const int sstart, const int send, const int rstart,
			const int rend, const int cstart, const int cend, FILE *inf);

void f3tensor_write_file(float ***tens, const int sstart, const int send, const int rstart, 
			 const int rend, const int cstart, const int cend, FILE *of); 

// needs work!
void f3tensor_fill_afile(float ***tens, const int sstart, const int send, 
			    const int rstart, const int rend, 
			    const int cstart, const int cend, const char fname[]);
float f3tensor_interp(float ***tens, 
		      float *sval, float *rval, float *cval, 
		      int sstart, int send, 
		      int rstart, int rend, 
		      int cstart, int cend, 
		      float xs, float xr, float xc);

void f_plot1x1(float (*func)(float), const float min, const float max, int npts, const char fn[]);
// plot a polar function in 2dimensions
// write to file w/ indep variables x and y for ease of gnuplotting
void f_plot2x1polar(float (*func)(float,float), 
		    const float xmin, const float xmax, const int xpts,
		    const float ymin, const float ymax, const int ypts, 
		    const char fn[]);

// plot a real valued function that takes two real arguments.
// Write a file a la gnuplot, w/ x,y,z triples on each line
void f_plot2x1(float (*func)(float,float), 
	       const float xmin, const float xmax, const int xpts,
	       const float ymin, const float ymax, const int ypts, 
	       const char *fn);

// Plot a 2d function.  Write a "box" of numbers w/ the z values at each 
// position.  This if for later consumption by IDL
void f_plot2x1idl(float (*func)(float,float), 
		  const float xmin, const float xmax, const int xpts,
		  const float ymin, const float ymax, const int ypts, 
		  const char fn[]);

// Plot a function from R^2 to R^1, and fill a matrix with the values
// in the plot.  This is for ease of post-processing.  Also, 
// write the output in IDL format (a square of numbers)
void f_plot2x1idl_mat(float (*func)(float,float), 
		      const float xmin, const float xmax, const int xpts,
		      const float ymin, const float ymax, const int ypts, 
		      const char fn[], float **mat);

// Routine to write a vector field to two files for consumption by IDL
// This could in principle handle an arbitrary number of in and out 
// variables, but for now I'm only interested in 2x2 and that's 
// probably all IDL can handle sensibly
void f_plot2x2idl(void (*func)(float,float,float *,float *), 
		  const float xmin, const float xmax, const int xpts,
		  const float ymin, const float ymax, const int ypts, 
		  const char fn[]);

void f_plot1xn(void (*func)(float, float *), const int n, const float min, const float max, 
	       const int npts, const char fn[]);

float f_newton(float (*f)(const float, const float *), 
	       float (*fp)(const float, const float *), 
	       const float *par, const float start, const float eps);


void d_euler_pass(double v[], const double phi, const double the, const double psi);
void d_euler_pass_inv(double v[], const double phi, const double the, const double psi);
void d_euler_act(double v[], const double phi, const double the, const double psi);
void d_euler_act_inv(double v[], const double phi, const double the, const double psi);
double d_sphere_r(const double pos[]);
double d_sphere_theta(const double pos[]);
double d_sphere_phi(const double pos[]);
double d_polar_r(const double pos[]);
double d_polar_theta(const double pos[]);
double d_cylinTheta(const double pos[]);
double d_cylinR(const double pos[]);
void dvector_dump(const double v[], const int beg, const int end, const char s[]);
void dvector_dump_file(const double v[], const int beg, const int end, const char fn[]);
void dvector_pair_dump_file(const double v[], const double w[], 
			   const int beg, const int end, const char fn[]);
void dvector_pair_dump_file_append(const double v[], const double w[], 
				   const int beg, const int end, const char fn[]);
double d_dot(const double a[], const double b[], const int beg, const int end);
void d_cross(double res[], const double a[], const double b[], const int beg, const int end);
void dvector_add(double res[], const double a[], const double b[], const int beg, const int end);
void dvector_sub(double res[], const double a[], const double b[], const int beg, const int end);
void dvector_s_mul(double res[], const double s, const double a[], const int beg, const int end);
double d_norm(const double v[], const int beg, const int end);
double d_norm2(const double v[], const int beg, const int end);
double d_d(const double p1[], const double p2[], const int beg, const int end);
double d_d_lines(const double av[], const double bv[], const double cv[], const double dv[],
		const int beg, const int end);
double d_d_lines_get_points(const double av[], const double bv[], const double cv[], const double dv[], 
			   double p1[], double p2[], const int beg, const int end);
double d_d_lines_points(const double p11[], const double p12[], const double p21[], const double p22[], 
		       const int beg, const int end);
double d_d_line_to_point(const double av[], const double bv[], const double p[], 
			const int beg, const int end);
double d_d_line_to_point_get_point(const double av[], const double bv[], const double p[], double pOut[],
				  const int beg, const int end);

void dmatrix_copy(double **dest, double **src, 
		 const int rstart, const int rend,
		 const int cstart, const int cend);
void dmatrix_transpose(double **dest, double **src, 
		      const int rstart, const int rend,
		      const int cstart, const int cend);
void dmatrix_dump(double **mat, const int rstart, const int rend, 
		 const int cstart, const int cend);
void dmatrix_dump_file(double **mat, const int rstart, const int rend, 
		       const int cstart, const int cend, const char fname[]);
void dmatrix_dump_transpose_file(double **mat, const int rstart, const int rend, 
				const int cstart, const int cend, const char fname[]);
void dmatrix_dump_sm_file(double **mat, const int rstart, const int rend, 
			  const int cstart, const int cend, const char fname[],
			  const char header[]);
void dmatrix_dump_transpose(double **mat, const int rstart, const int rend, 
			   const int cstart, const int cend);
void dmatrix_read_file(double **mat, const int rstart, const int rend, 
		      const int cstart, const int cend, FILE *inf);
void dmatrix_write_file(double **mat, const int rstart, const int rend, 
		       const int cstart, const int cend, FILE *of);
void dmatrix_fill_afile_lc(double **mat, const int rstart, const int rend, 
		       const int cstart, const int cend, const char infn[]);
void dmatrix_fill_afile_cl(double **mat, const int rstart, const int rend, 
		       const int cstart, const int cend, const char infn[]);
void dmatrix_fill_afile(double **mat, const int rstart, const int rend, 
		       const int cstart, const int cend, const char infn[]);
void dmatrix_read_afile_lc(double ***mat, const int rstart, int *rend, 
			  const int cstart, int *cend, const char infn[]);
void dmatrix_read_afile_cl(double ***mat, const int rstart, int *rend, 
			  const int cstart, int *cend, const char infn[]);
void dmatrix_write_afile_lc(double **mat, const int rstart, const int rend, 
			    const int cstart, const int cend, const char fname[]);
void dmatrix_write_afile_cl(double **mat, const int rstart, const int rend, 
			    const int cstart, const int cend, const char fname[]);
void id_dmatrix(double **mat, const int rstart, const int rend, const int cstart, const int cend);
double dmatrix_interp(double **mat, double *rval, double *cval, 
		      int rstart, int rend, int cstart, int cend, 
		      double xr, double xc);
double dmatrix_mean(double **mat, const int rstart, const int rend, 
		  const int cstart, const int cend);
double dmatrix_rms(double **mat, const int rstart, const int rend, 
		 const int cstart, const int cend, const double mean);
double dmatrix_nsig_rms(double **mat, const int rstart, const int rend, 
		      const int cstart, const int cend,
		      const double mean, const int n, const double RMS);
void dmatrix_add(double **matc, double **mata, double **matb, 
		const int rstart, const int rend, const int cstart, const int cend);
void dmatrix_sub(double **matc, double **mata, double **matb, 
		const int rstart, const int rend, const int cstart, const int cend);


void d_locate(double xx[], unsigned long n, double x, unsigned long *j);

double ***d3tensor(long nrl, long nrh, long ncl, long nch, long ndl, long ndh);
void free_d3tensor(double ***t, long nrl, long nrh, long ncl, long nch,
		   long ndl, long ndh);

void d3tensor_dump(double ***tens, const int sstart, const int send, const int rstart,
		   const int rend, const int cstart, const int cend);
void d3tensor_copy(double ***dest, double ***src,
		   const int sstart, const int send, const int rstart,
		   const int rend, const int cstart, const int cend);
void d3tensor_read_file(double ***tens, 
			const int sstart, const int send, const int rstart,
			const int rend, const int cstart, const int cend, FILE *inf);
void d3tensor_write_file(double ***tens, const int sstart, const int send, const int rstart, 
			 const int rend, const int cstart, const int cend, FILE *of); 
// needs work!
void d3tensor_fill_afile(double ***tens, const int sstart, const int send, 
			    const int rstart, const int rend, 
			    const int cstart, const int cend, const char fname[]);
double d3tensor_interp(double ***tens, 
		      double *sval, double *rval, double *cval, 
		      int sstart, int send, 
		      int rstart, int rend, 
		      int cstart, int cend, 
		      double xs, double xr, double xc);

void d_plot1x1(double (*func)(double), const double min, const double max, int npts, const char fn[]);
void d_plot2x1polar(double (*func)(double,double), 
		    const double xmin, const double xmax, const int xpts,
		    const double ymin, const double ymax, const int ypts, 
		    const char fn[]);
void d_plot2x1(double (*func)(double,double), 
	       const double xmin, const double xmax, const int xpts,
	       const double ymin, const double ymax, const int ypts, 
	       const char *fn);
void d_plot2x1idl(double (*func)(double,double), 
		  const double xmin, const double xmax, const int xpts,
		  const double ymin, const double ymax, const int ypts, 
		  const char fn[]);
void d_plot2x1idl_mat(double (*func)(double,double), 
		      const double xmin, const double xmax, const int xpts,
		      const double ymin, const double ymax, const int ypts, 
		      const char fn[], double **mat);
void d_plot2x2idl(void (*func)(double,double,double *,double *), 
		  const double xmin, const double xmax, const int xpts,
		  const double ymin, const double ymax, const int ypts, 
		  const char fn[]);
void d_plot1xn(void (*func)(double, double *), const int n, const double min, const double max, 
	       const int npts, const char fn[]);
double d_newton(double (*f)(const double, const double *), 
		double (*fp)(const double, const double *), 
		const double *par, const double start, const double eps);
