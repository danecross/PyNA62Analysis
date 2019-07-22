#include "q.hh"

void q_(long long *i,double *qb)
{
  double* addr = (double*)(*i);
  for (int j=0; j<8; j++) qb[j] = *(addr+j);
}
