// Created by Vito Palladino 20/1/11
// Modified by E.Leonardi 2014/06/13

#include "LAVPhotocathode.hh"
#include "LAVGeometry.hh"

#include "TRandom3.h"
#include "TMath.h"

LAVPhotocathode::LAVPhotocathode(TF1* gPhotocathode) :
  fBlockId(-1),
  fgPhotocathode(gPhotocathode)
{
}

void LAVPhotocathode::SetBlockId(Int_t blockId) {
  // Currently all blocks share the same efficiency curve
  // This will probably stay so, but just in case...
  fBlockId = blockId;
}

Bool_t LAVPhotocathode::ApplyQE(Double_t PhE){

  // Check if photon energy is in useful range
  if ( PhE < 2.15e-6 || PhE > 4.34e-6 ) return 0;

  // Compute Quantum Efficiency at current photon energy
  Double_t QE = fgPhotocathode->Eval(PhE)*0.01;

  // Apply binomial distribution with current QE
  if ( gRandom->Binomial(1,QE) ) return 1;

  return 0;

}
