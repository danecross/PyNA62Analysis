// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-23
//
// --------------------------------------------------------------
#include "TLKrEvent.hh"
#include "TLKrHit.hh"
#include "Riostream.h"


ClassImp(TLKrEvent)

TLKrEvent::TLKrEvent() : TDetectorVEvent(TLKrHit::Class()){

  //for(Int_t i = 0; i < 16384; i++)
  //fSeedIndexes[i]=-1;
  fNSeeds=0;
}

void TLKrEvent::Clear(Option_t* Option){
  TDetectorVEvent::Clear(Option );
  //for(Int_t i = 0; i < fNSeeds ; i++)
  //fSeedIndexes[i] = -1;
  fNSeeds=0;
}
//void TLKrEvent::AddSeed(Int_t iSeed){
//  cout <<" iSeed "<< iSeed << endl;
//fSeedIndexes[fNSeeds++] = iSeed;


//}
