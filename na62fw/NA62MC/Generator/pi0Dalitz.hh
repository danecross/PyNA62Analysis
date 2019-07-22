////////////////////////////////////////////////////////////////////////////
// Pi0 Dalitz decay generator: pi0 -> e+ e- gamma
// NA62 version: Michal Koval (michal.koval@cern.ch) 02/10/2015
//
// Radiative corrections by Tomas Husek,
// DOI: 10.1103/PhysRevD.92.054027
// arxiv: arXiv:1504.06178
//
// copy of the license:
//
// This program calculates for given (x,y) the radiative corrections
// (virtual & bremsstrahlung) to the Dalitz decay (p-scalar --> l l gamma)
// including (m_e)^4 terms with the result written in percent.
// The one-photon irreducible contribution is also included.
// For details see the paper by the author of this code
// or also the original work
// (K. O. Mikaelian & J. Smith, Phys.Rev. D5 (1972) 1763-1773))
//
// Contact e-mail: husek@ipnp.mff.cuni.cz
//
// Copyright (c)2014, 2015 Tomas Husek
// Institute of Particle and Nuclear Physics
// Faculty of Mathematics and Physics
// Charles University, Prague
//
// <LICENSE>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published
// by the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program. If not, see <http://www.gnu.org/licenses/>.
//
//
/////////////////////////////////////////////////////////////////////////////

#ifndef PI0DALITZ_H
#define PI0DALITZ_H

#include "masses.hh"
const double a         = 1/137.035999; // fine structure constant
const double v         = 2*MEL/MP0; // Mee/Mpi0 minimal value
const double v2        = v*v;       // x mininimal value
const double aslopeVMD = 0.032;     // VMD value of the form factor slope
const double chi       = 2.2;       // 1gIR contribution parameter, given by LMD model
const double mu        = 770.;      // 1gIR contribution parameter, given by LMD model
const int    ord       = 1024;      // used in Gauss Legendre numerical integration
const double zgammaCut = 0.01;      // cut-off parameter of the radiative decay generator
const double M42max    = 5e10;      // maximum M42 matrix element value
const double weightMax = 0.09732;   // max weight for the 4-body decay generation
const double maxRadDelta = 1.03;    // max. radiative correction delta of 3 body Dalitz
double xGlobal, yGlobal;            // needed to define global x,y Dalitz variables in order to pass JTr to Gauss-Legendre integration

class TLorentzVector;

extern "C" {
    void generate_pi0dal_(double ppi0[4], int * pzmode);
}

void generate_dalitz_3body(TLorentzVector &gamom,
                           TLorentzVector &epmom,
                           TLorentzVector &emmom,
                           bool radcor, double zcut = 1.0, double xlowcut = v2);
void generate_dalitz_4body(TLorentzVector &gamom1,
                           TLorentzVector &gamom2,
                           TLorentzVector &epmom,
                           TLorentzVector &emmom,
                           double zcut = 1.0, double xlowcut = v2);

double radcorr_full(double x, double y, double zcut);
double radcorr_MS(double x, double y, double zcut);
double radcorr_1gIR(double x, double y, double zcut);

double f_JTr(double z);
double JTr(double x, double y, double z);
double JTrv0(double x, double y, double z);
double JTrv2(double x, double y, double z);
double JTrv4(double x, double y, double z);
double IJTrDiv(double x, double y, double z_upper);
double dVirt(double x, double y);
double A(double x, double y);
double T(double x, double y);

double ratio_34body(double zcut);

// 3 functions provided by T. Husek, used in |M|^2 4-body matrix element calculation
double M42v0(double x, double z, double a, double b, double c, double d, double e, double f);
double M42v2(double x, double z, double a, double b, double c, double d, double e, double f);
double M42v4(double x, double z, double a, double b, double c, double d, double e, double f);
// final |M|^2 4-body matrix element built from previous 3 functions
double pi0dal_4body_M42(double x, double z, double a, double b, double c, double d, double e, double f);

#endif /* PI0DALITZ_H */
