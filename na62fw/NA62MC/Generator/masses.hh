#ifndef MASSES_H
#define MASSES_H

/////////////////////////////////////////////////////////////////////////
// Particle masses: they must correspond exactly to G4ParticleDefinition.
// Unit used by the decay generators: GeV.

const double MKCH   = 0.493677;
const double SQMKCH = MKCH*MKCH;

const double MK0    = 0.497614;
const double SQMK0  = MK0*MK0;

const double MPI    = 0.13957010;
const double SQMPI  = MPI*MPI;

const double MP0    = 0.1349766;
const double SQMP0  = MP0*MP0;

const double MEL    = 0.00051099891;
const double SQMEL  = MEL*MEL;

const double MMU    = 0.1056583715;
const double SQMMU  = MMU*MMU;

const double RPI    = MPI/MKCH;
const double RPI2   = RPI*RPI;

/////////////////////
// PDG particle codes

const int PDG_ID_pip =  211; // pi+
const int PDG_ID_pim = -211; // pi-
const int PDG_ID_pi0 =  111; // pi0
const int PDG_ID_elp = -11;  // e+
const int PDG_ID_elm =  11;  // e-
const int PDG_ID_mup = -13;  // mu+
const int PDG_ID_mum =  13;  // mu-
const int PDG_ID_gam =  22;  // gamma
const int PDG_ID_nu  =  12;  // nu_e
const int PDG_ID_kp  =  321; // K+
const int PDG_ID_km  = -321; // K-
const int PDG_ID_kl  =  130; // KL
const int PDG_ID_exo =  9000; // Exotic: NA62 custom particle

////////////////////
// Constants
const double AlphaQED  = 1/137.035999; // fine structure constant

#endif
