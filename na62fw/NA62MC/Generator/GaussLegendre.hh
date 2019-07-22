#ifndef GAUSS_LEGENDRE_H
#define GAUSS_LEGENDRE_H

/*
  Copyright (c)2007-2010, 2015 Pavel Holoborodko, Tomas Husek
  This is a modification of Gauss-Legendre Quadrature package
  by Holoborodko. Original copyright/license copied below.
  Modification by Husek is covered by the same license.
*/

/*
  Numerical Integration by Gauss-Legendre Quadrature Formulas of high orders.
  High-precision abscissas and weights are used.

  see license in GaussLegendre.cc
*/
double gauss_legendre(int n, double (*f)(double), double a, double b);

#endif /* GAUSS_LEGENDRE_H */
