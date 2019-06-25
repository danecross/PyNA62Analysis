// --------------------------------------------------------------
// History:
//
// Created by Vito Palladino (Vito.Palladino@cern.ch) 2011-01-04
//
// --------------------------------------------------------------
#include "TRecoLAVDigi.hh"

ClassImp(TRecoLAVDigi)

TRecoLAVDigi::TRecoLAVDigi() : TObject(){
  fNToT=0;

  fThType = -1;
}

void TRecoLAVDigi::Clear(Option_t* /*option*/){
  fNToT=0;
  fThType = -1;
}
