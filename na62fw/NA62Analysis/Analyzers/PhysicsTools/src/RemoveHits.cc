// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2017-09-19
//
// ---------------------------------------------------------------

/// \class RemoveHits
/// \Brief
/// Remove hits from detector trees when filtering
/// \EndBrief
/// \Detailed
/// RecoHits are removed from all trees.
/// This analyzer should be included into the list of pre-analyzers when filtering.
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include "RemoveHits.hh"
#include "Event.hh"
#include "Persistency.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

RemoveHits::RemoveHits(Core::BaseAnalysis *ba) : Analyzer(ba, "RemoveHits") {
  RequestAllRecoTrees();
}

void RemoveHits::PostProcess() {

  TRecoCedarEvent        *CedarEvent        = static_cast<TRecoCedarEvent*>       (GetEvent("Cedar"));
  TRecoCHANTIEvent       *CHANTIEvent       = static_cast<TRecoCHANTIEvent*>      (GetEvent("CHANTI"));
  TRecoCHODEvent         *CHODEvent         = static_cast<TRecoCHODEvent*>        (GetEvent("CHOD"));
  TRecoGigaTrackerEvent  *GigaTrackerEvent  = static_cast<TRecoGigaTrackerEvent*> (GetEvent("GigaTracker"));
  TRecoHACEvent          *HACEvent          = static_cast<TRecoHACEvent*>         (GetEvent("HAC"));
  TRecoIRCEvent          *IRCEvent          = static_cast<TRecoIRCEvent*>         (GetEvent("IRC"));
  TRecoLAVEvent          *LAVEvent          = static_cast<TRecoLAVEvent*>         (GetEvent("LAV"));
  TRecoLKrEvent          *LKrEvent          = static_cast<TRecoLKrEvent*>         (GetEvent("LKr"));
  TRecoMUV0Event         *MUV0Event         = static_cast<TRecoMUV0Event*>        (GetEvent("MUV0"));
  TRecoMUV1Event         *MUV1Event         = static_cast<TRecoMUV1Event*>        (GetEvent("MUV1"));
  TRecoMUV2Event         *MUV2Event         = static_cast<TRecoMUV2Event*>        (GetEvent("MUV2"));
  TRecoMUV3Event         *MUV3Event         = static_cast<TRecoMUV3Event*>        (GetEvent("MUV3"));
  TRecoNewCHODEvent      *NewCHODEvent      = static_cast<TRecoNewCHODEvent*>     (GetEvent("NewCHOD"));
  TRecoRICHEvent         *RICHEvent         = static_cast<TRecoRICHEvent*>        (GetEvent("RICH"));
  TRecoSACEvent          *SACEvent          = static_cast<TRecoSACEvent*>         (GetEvent("SAC"));
  TRecoSpectrometerEvent *SpectrometerEvent = static_cast<TRecoSpectrometerEvent*>(GetEvent("Spectrometer"));
  TRecoSAVEvent          *SAVEvent          = static_cast<TRecoSAVEvent*>         (GetEvent("SAV"));

  CedarEvent->TDetectorVEvent::Clear();
  CHANTIEvent->TDetectorVEvent::Clear();
  CHODEvent->TDetectorVEvent::Clear();
  GigaTrackerEvent->TDetectorVEvent::Clear();
  HACEvent->TDetectorVEvent::Clear();
  IRCEvent->TDetectorVEvent::Clear();
  LAVEvent->TDetectorVEvent::Clear();
  LKrEvent->TDetectorVEvent::Clear();
  MUV0Event->TDetectorVEvent::Clear();
  MUV1Event->TDetectorVEvent::Clear();
  MUV2Event->TDetectorVEvent::Clear();
  MUV3Event->TDetectorVEvent::Clear();
  NewCHODEvent->TDetectorVEvent::Clear();
  RICHEvent->TDetectorVEvent::Clear();
  SACEvent->TDetectorVEvent::Clear();
  SpectrometerEvent->TDetectorVEvent::Clear();
  SAVEvent->TDetectorVEvent::Clear();
}
