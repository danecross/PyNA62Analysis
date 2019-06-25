////////////////////////////////////////////////////////////////////////////
// pi0 -> e+ e- (gamma) decay generator
// NA62 implementation: Michal Koval (michal.koval@cern.ch) 01/12/2016
//
// Theoretical computation of |M|^2(pi0 -> ee(g)) by Tomas Husek
// Contact e-mail: husek@ipnp.mff.cuni.cz
//
// Copyright (c)2014, 2015, 2016 Tomas Husek
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
////////////////////////////////////////////////////////////////////////////

#ifndef PI0EE_H
#define PI0EE_H

#include "masses.hh"
const double v         = 2*MEL/MP0; // Mee/Mpi0 minimal value
const double v2        = v*v;       // x mininimal value
const double chi       = 2.2;       // parameter given by LMD model
const double mu        = 770.;      // parameter given by LMD model
const double Fpi0      = 92.2;
const double wMaxPi0eeg = 0.385;     // max weight for the pi0eeg 3-body phase space decay
const double xcutPi0ee  = 0.98;      // TODO: use as macro parameter

class TLorentzVector;

extern "C" {
    void generate_pi0ee_(double ppi0[4], int * pzmode); // to be called from pi0decays.F
}
double get_3body_event_probability(double xcutoff);
double get_maximum_M2_value_3body(double xcutoff);

void generate_pi0ee_2body(TLorentzVector &epmom, TLorentzVector &emmom);
void generate_pi0ee_3body(TLorentzVector &epmom, TLorentzVector &emmom, TLorentzVector &gamom);

double M2_piee_brems(double x, double y);

double ReP(double x, double y);
double ImP(double x, double y);
double ReA(double x, double y);
double ImA(double x, double y);
double ReT(double x, double y);
double ImT(double x, double y);

#endif /* PI0EE_H */
