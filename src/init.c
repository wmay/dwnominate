#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Fortran calls */
extern void F77_NAME(dwnom)(int *nomstartin, double *weightsin, int *nbills,
			    int *icongin, double *dynin, double *zmidin,
			    int *mcongin, int *nrowrct, int *ncolrct,
			    int *rcvotet1in, int *rcvotet9in, int *nlegs,
			    int *ncongin, int *id1in, double *xdatain,
			    int *nrowrc, int *ncolrc, int *rcvote1in,
			    int *rcvote9in, double *xdataout, double *sdx1out,
			    double *sdx2out, double *varx1out, double *varx2out,
			    double *xbiglogout, int *kbiglogout, double *gmpaout,
			    double *gmpbout, double *dynout, double *zmidout,
			    double *weightsout);

static const R_FortranMethodDef FortranEntries[] = {
    {"dwnom", (DL_FUNC) &F77_NAME(dwnom), 31},
    {NULL, NULL, 0}
};

void R_init_dwnominate(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
