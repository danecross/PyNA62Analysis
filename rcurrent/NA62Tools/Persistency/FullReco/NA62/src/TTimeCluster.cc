// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-04-24
//
// --------------------------------------------------------------
#include "TTimeCluster.hh"

#include "Riostream.h"

ClassImp(TTimeCluster)

TTimeCluster::TTimeCluster() : TDigiVCandidate(){
    fAverage = 0;
    fRMS     = 0;
    fMinTime = 1e5;
    fMaxTime = 0;
}

Bool_t TTimeCluster::AddDigi(Int_t DigiIndex){
    if(!TDigiVCandidate::AddDigi(DigiIndex))
        return kFALSE;

    if(GetNDigis() == 1)
        fStationID = GetDigi(0)->GetStationID();

    Double_t DigiTime = GetDigi(GetNHits() - 1)->GetTime();
    fMinTime = DigiTime < fMinTime ? DigiTime : fMinTime;
    fMaxTime = DigiTime > fMaxTime ? DigiTime : fMaxTime;

    return kTRUE;       
}

Int_t TTimeCluster::Compare(const TObject *obj) const {
  if (fAverage > static_cast<const TTimeCluster*>(obj)->GetAverage()) return +1;
  if (fAverage < static_cast<const TTimeCluster*>(obj)->GetAverage()) return -1;
  return 0;
}

void TTimeCluster::Clear(Option_t * option) {
  TDigiVCandidate::Clear(option);
  fAverage = 0;
  fRMS     = 0;
  fMinTime = 1e5;
  fMaxTime = 0;
}
