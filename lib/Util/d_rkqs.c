#include <math.h>
#define NRANSI
#include "nrutil.h"
#define SAFETY 0.9
#define PGROW -0.2
#define PSHRNK -0.25
#define ERRCON 1.89e-4

void d_rkqs(double y[], double dydx[], int n, double *x, double htry, double eps,
	double yscal[], double *hdid, double *hnext,
	void (*derivs)(double, double [], double []))
{
	void d_rkck(double y[], double dydx[], int n, double x, double h,
		double yout[], double yerr[], void (*derivs)(double, double [], double []));
	int i;
	double errmax,h,xnew,*yerr,*ytemp;

	yerr=dvector(1,n);
	ytemp=dvector(1,n);
	h=htry;
	for (;;) {
		d_rkck(y,dydx,n,*x,h,ytemp,yerr,derivs);
		errmax=0.0;
		for (i=1;i<=n;i++) errmax=FMAX(errmax,fabs(yerr[i]/yscal[i]));
		errmax /= eps;
		if (errmax > 1.0) {
			h=SAFETY*h*pow(errmax,PSHRNK);
			if (h < 0.1*h) h *= 0.1;
			xnew=(*x)+h;
			if (xnew == *x) nrerror("stepsize underflow in rkqs");
			continue;
		} else {
			if (errmax > ERRCON) *hnext=SAFETY*h*pow(errmax,PGROW);
			else *hnext=5.0*h;
			*x += (*hdid=h);
			for (i=1;i<=n;i++) y[i]=ytemp[i];
			break;
		}
	}
	free_dvector(ytemp,1,n);
	free_dvector(yerr,1,n);
}
#undef SAFETY
#undef PGROW
#undef PSHRNK
#undef ERRCON
#undef NRANSI
/* (C) Copr. 1986-92 Numerical Recipes Software 'N;,5%. */
