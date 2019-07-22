// --------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2015-12-31
//
// --------------------------------------------------------------
#include "TDigiVError.hh"
ClassImp(TDigiVError)

TDigiVError::TDigiVError(){
  fROMezzanineID = -1;
  fROBoardID = -1;
  fType = -1;
  fFatal = kFALSE;
}

void TDigiVError::Clear(Option_t * /*option*/) {
  fROMezzanineID = -1;
  fROBoardID = -1;
  fType = -1;
  fFatal = kFALSE;
}
