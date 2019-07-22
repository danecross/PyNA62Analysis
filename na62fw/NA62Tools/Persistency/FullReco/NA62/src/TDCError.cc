// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2007-03-30
//
// --------------------------------------------------------------
#include "TDCError.hh"
ClassImp(TDCError)

TDCError::TDCError():TDigiVError(){
  fTDCID = -1;
}

void TDCError::Clear(Option_t * option) {
  TDigiVError::Clear(option);
  fTDCID = -1;
}
