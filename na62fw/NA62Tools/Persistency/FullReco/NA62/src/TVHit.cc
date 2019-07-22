// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-08
//
// --------------------------------------------------------------
#include "TVHit.hh"

#include "Riostream.h"


ClassImp(TVHit)

TVHit::TVHit() :
  TVChannelID(),
  fMCTrackID(-1),
  fDirectInteraction(kFALSE)
{
}

TVHit::TVHit(const TVHit & hit) :
  TObject(hit),
  TVChannelID(hit),
  fMCTrackID(hit.fMCTrackID),
  fDirectInteraction(hit.fDirectInteraction)
{
}

TVHit::TVHit(Int_t iCh) :
  TVChannelID(iCh),
  fMCTrackID(-1),
  fDirectInteraction(kFALSE)
{
}

void TVHit::Print(Option_t *) const {
    TVChannelID::Print();
    std::cout << "MCTrackID = " << fMCTrackID << std::endl
        << "DirectInteraction = " << fDirectInteraction << std::endl;
}

void TVHit::Clear(Option_t* option) {
  TVChannelID::Clear(option);
  fMCTrackID = -1;
  fDirectInteraction = kFALSE;
}
