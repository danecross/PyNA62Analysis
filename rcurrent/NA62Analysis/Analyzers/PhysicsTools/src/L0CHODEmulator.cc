// ---------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2017-10-10
//
// ---------------------------------------------------------

/// \class L0CHODEmulator
/// \Brief
/// Emulator of the CHOD L0 trigger-generating firmware. Inherits from L0RICHDetector.
/// Returns std::vector<EmulatedL0Primitive>
/// \EndBrief
/// \Detailed
/// Emulator of the CHOD L0 trigger-generating firmware. Inherits from L0RICHEmulator.
/// Returns std::vector<EmulatedL0Primitive>
/// See documentation of L0RICHEmulator for more details.
///
/// \author Chris Parkinson (chris.parkinson@cern.ch)
/// \EndDetailed

// add_preanalyzer L0RICHEmulator

#include "L0CHODEmulator.hh"
#include "TRecoCHODEvent.hh"

L0CHODEmulator::L0CHODEmulator(Core::BaseAnalysis *ba) : L0RICHEmulator(ba, "CHOD"){
  fL0Detector=kL0CHOD;
  RequestTree("CHOD", new TRecoCHODEvent, "Reco");
  AddParam("PP0Fraction", &fFracPP[0], 0.238);
  AddParam("PP1Fraction", &fFracPP[1], 0.470);
  AddParam("PP2Fraction", &fFracPP[2], 0.733);
}

void L0CHODEmulator::FillTimes(){

  TRecoCHODEvent* event = GetEvent<TRecoCHODEvent>("Reco");
  Int_t nHits = event->GetNHits();
  FillHisto("nCandidates_CHOD", nHits);

  fTimes.reserve(nHits);
  for(int i=0; i<nHits; ++i){
    TRecoCHODHit* hit = static_cast<TRecoCHODHit*>(event->GetHit(i));
    Double_t time         = hit->GetTime();
    if(GetWithMC()) time += fL0Reference;
    Double_t fulltime     = time+fEventTimeStamp;
    Int_t pp = hit->GetChannelID()/16;
    pp = pp%4;
    FillHisto("hitTimes_CHOD", time-fL0Reference);
    fTimes.push_back(EmulatedL0Primitive(fL0Detector, fulltime, pp));
  }
}
