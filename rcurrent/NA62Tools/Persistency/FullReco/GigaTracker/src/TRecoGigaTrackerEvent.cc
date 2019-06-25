// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#include "TRecoGigaTrackerEvent.hh"

ClassImp(TRecoGigaTrackerEvent)

TRecoGigaTrackerEvent::TRecoGigaTrackerEvent() :
TRecoVEvent(TRecoGigaTrackerCandidate::Class(), TRecoGigaTrackerHit::Class()) {}

TRecoGigaTrackerEvent::TRecoGigaTrackerEvent(const TRecoGigaTrackerEvent & event) : TRecoVEvent((TRecoVEvent &) event) {
  for (int s(0);s<3;s++){
    for (int c(0);c<10;c++){
      for (int h(0);h<2;h++){
        fHitNbFromPrevL0[s][c][h] = event.fHitNbFromPrevL0[s][c][h];
      }
    }
  }
}

TRecoGigaTrackerEvent::~TRecoGigaTrackerEvent() {}

void TRecoGigaTrackerEvent::Clear(Option_t* option){
  TRecoVEvent::Clear(option);
  for (int s(0);s<3;s++){
    for (int c(0);c<10;c++){
      for (int h(0);h<2;h++){
        fHitNbFromPrevL0[s][c][h] = 0;
      }
    }
  }
}

int TRecoGigaTrackerEvent::GetHitNbFromPrevL0(int gtk, int chip, int half){
  if(half < 0) return fHitNbFromPrevL0[gtk][chip][0]+fHitNbFromPrevL0[gtk][chip][1];
  else return fHitNbFromPrevL0[gtk][chip][half];
}

void TRecoGigaTrackerEvent::SetHitNbFromPrevL0(int gtk, int chip, int half, int n){
  fHitNbFromPrevL0[gtk][chip][half] = n;
  return;
}
