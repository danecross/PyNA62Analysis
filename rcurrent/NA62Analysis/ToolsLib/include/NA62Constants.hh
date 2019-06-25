/*
 * NA62Constants.hh
 *
 *  Created on: Aug 27, 2013
 *      Author: nlurkin
 */

#ifndef NA62CONSTANTS_HH_
#define NA62CONSTANTS_HH_

namespace NA62Constants {

  // Particle masses [MeV] according to PDG 2018
  static const double MKCH = 493.677;
  static const double MPI  = 139.57061;
  static const double MPI0 = 134.9770;
  static const double MEL  = 0.5109989461;
  static const double MMU  = 105.6583745;

  static const double SQMKCH = MKCH*MKCH;
  static const double SQMPI  = MPI*MPI;
  static const double SQMPI0 = MPI0*MPI0;
  static const double SQMEL  = MEL*MEL;
  static const double SQMMU  = MMU*MMU;
}

#endif /* CONSTANTSREPOSITORY_HH_ */
