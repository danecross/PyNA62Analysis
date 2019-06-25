// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-08
//            Evelina Marinova (Evelina.Marinova@cern.ch)
//
// --------------------------------------------------------------
#include "TTimeCluster.hh"
#include "FADCEvent.hh"

ClassImp(FADCEvent)

FADCEvent::FADCEvent() :
  TDigiVEvent(),
  fFADCID(0),
  fNSamples(0),
  fEventFlag(0)
{}

FADCEvent::FADCEvent(TClass * HitClass) :
  TDigiVEvent(TTimeCluster::Class(), HitClass, TDigiVError::Class()),
  fFADCID(0),
  fNSamples(0),
  fEventFlag(0)
{}

FADCEvent::FADCEvent(TClass * HitClass, Int_t NMaxHits) :
  TDigiVEvent(TTimeCluster::Class(), HitClass, TDigiVError::Class(), NMaxHits),
  fFADCID(0),
  fNSamples(0),
  fEventFlag(0)
{}

FADCVHit* FADCEvent::GetHit(Int_t iHit) {
  return static_cast<FADCVHit*>(TDigiVEvent::GetHit(iHit));
}

void FADCEvent::Clear(Option_t * option) {
  TDigiVEvent::Clear(option);
  fFADCID = -1;
  fNSamples  = -1;
  fEventFlag = 0;
}
