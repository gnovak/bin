#include <math.h>
#define NRANSI
#include "nrutil.h"

void usrfun(double *x,int n,double *fvec,double **fjac);
#define FREERETURN {free_dmatrix(fjac,1,n,1,n);free_dvector(fvec,1,n); free_dvector(p,1,n);free_ivector(indx,1,n);return;}

void d_mnewt(int ntrial, double x[], int n, double tolx, double tolf)
{
	void d_lubksb(double **a, int n, int *indx, double b[]);
	void d_ludcmp(double **a, int n, int *indx, double *d);
	int k,i,*indx;
	double errx,errf,d,*fvec,**fjac,*p;

	indx=ivector(1,n);
	p=dvector(1,n);
	fvec=dvector(1,n);
	fjac=dmatrix(1,n,1,n);
	for (k=1;k<=ntrial;k++) {
		usrfun(x,n,fvec,fjac);
		// TMP TMP TMP
		//		printf("%d : %g\t%g\t%g\t%g\t: %g\t%g\t%g\t%g\n", k, x[1], x[2], x[3], x[4],
		//		       fvec[1], fvec[2], fvec[3], fvec[4]);
		errf=0.0;
		for (i=1;i<=n;i++) errf += fabs(fvec[i]);
		if (errf <= tolf) FREERETURN
		for (i=1;i<=n;i++) p[i] = -fvec[i];
		d_ludcmp(fjac,n,indx,&d);
		d_lubksb(fjac,n,indx,p);
		errx=0.0;
		for (i=1;i<=n;i++) {
			errx += fabs(p[i]);
			// TMP TMP TMP
			// x[i] += 0.1*p[i];
			x[i] += p[i];
		}
		if (errx <= tolx) FREERETURN
	}
	FREERETURN
}
#undef FREERETURN
#undef NRANSI
/* (C) Copr. 1986-92 Numerical Recipes Software 'N;,5%. */
