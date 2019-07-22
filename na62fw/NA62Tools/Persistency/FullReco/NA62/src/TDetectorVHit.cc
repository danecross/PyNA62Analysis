// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-08
//
// --------------------------------------------------------------
#include "TDetectorVHit.hh"

#include "Riostream.h"


ClassImp(TDetectorVHit)

TDetectorVHit::TDetectorVHit() :
  TVHit(),
  fPosition(0.,0.,0.),
  fEnergy(0.),
  fTime(0.)
{
}

TDetectorVHit::TDetectorVHit(const TDetectorVHit & hit) :
  TVHit((TVHit &) hit),
  fPosition(hit.fPosition),
  fEnergy(hit.fEnergy),
  fTime(hit.fTime)
{
}

TDetectorVHit::TDetectorVHit(Int_t iCh) :
  TVHit(iCh),
  fPosition(0.,0.,0.),
  fEnergy(0.),
  fTime(0.)
{
}

void TDetectorVHit::Print(Option_t *) const {
    TVHit::Print();
    std::cout << "HitPosition = (" << fPosition.X() << "," << fPosition.Y() << "," << fPosition.Z() << ")" << std::endl
        << "Energy = " << fEnergy << std::endl
        << "Time = " << fTime << std::endl << std::endl;
}

void TDetectorVHit::Clear(Option_t* option) {
  TVHit::Clear(option);
  fPosition = TVector3(0.,0.,0.);
  fEnergy = 0.;
  fTime = 0.;
}
