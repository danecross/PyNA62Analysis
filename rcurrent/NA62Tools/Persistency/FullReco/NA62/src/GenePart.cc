// --------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 6 Apr 2011
//
// --------------------------------------------------------------
#include "GenePart.hh"

#include <stdlib.h>
#include "Riostream.h"


ClassImp(GenePart)

GenePart::GenePart() {
  Clear();
}

void GenePart::Clear(Option_t* /*option*/) {
  fPDGcode           = 0;
  fCharge            = 0;
  fParticleName      = "";
  fInitialEnergy     = 0.0;
  fInitialMomentum   = TVector3(0.0, 0.0, 0.0);
  fParticleGroup     = -999;
  fParticleGroupName = "?";
}

/////////////////////////////////////////////////////////////
// Set particle PDG code and electrical charge.
// The charges are set correctly for K+-, pi+-, mu+- and e+-.

void GenePart::SetPDGcode(Int_t code) {
  fPDGcode = code;
  fCharge  = 0;
  if      (code==-11 || code==-13 || code==+211 || code==+321) fCharge = +1; // e,mu,pi,K
  else if (code==+11 || code==+13 || code==-211 || code==-321) fCharge = -1;
}

void GenePart::SetParticleGroup(Int_t val) {
  fParticleGroup = val;
  if      (fParticleGroup==0) fParticleGroupName = "K_decay";
  else if (fParticleGroup==1) fParticleGroupName = "pi0_decay";
  else if (fParticleGroup==2) fParticleGroupName = "K_decay_rad";
  else if (fParticleGroup==3) fParticleGroupName = "pi0_decay_rad";
}

void GenePart::Print(Option_t*) const {
 std::cout << "PDG " << fPDGcode << " Name " << fParticleName
      << " E "   << fInitialEnergy << " MeV"
      << " P "   << fInitialMomentum.X() << " " << fInitialMomentum.Y() << " " << fInitialMomentum.Z() << " MeV"
      << " Group " << fParticleGroupName
      << std::endl;
}
