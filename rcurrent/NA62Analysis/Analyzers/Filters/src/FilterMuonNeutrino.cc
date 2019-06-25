// ---------------------------------------------------------------
//
// History:
//
// Created by Mathieu Perrin-Terrin (mathieu.perrin-terrin@cern.ch) 2018-06-06
//
// ---------------------------------------------------------------

/// \class FilterMuonNeutrino
/// \Brief
/// Filter events triggered with the muon neutrino liine
/// \EndBrief
/// \Detailed
/// Events triggered with the muon neutrinos (data) or all events (MC) are filtered.
/// A downscaling is controllable by the user can be applied;
/// events with timestamp divisible by the downscaling are filtered out. Example of use:
/// \code
/// ./MyApplication -i <input_file> -o <output_file> -p "FilterMuonNeutrino:Downscaling=10" --filter
/// \endcode
/// \author Mathieu Perrin-Terrin (mathieu.perrin-terrin@cern.ch)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include "TriggerConditions.hh"
#include "FilterMuonNeutrino.hh"
#include "Event.hh"
#include "Persistency.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

FilterMuonNeutrino::FilterMuonNeutrino(Core::BaseAnalysis *ba) :
  Analyzer(ba, "FilterMuonNeutrino") {
  RequestAllMCTrees();
  RequestAllRecoTrees();
  AddParam("Downscaling", &fDownscaling, 1); // by default, no downscaling

  fTW = 6.25;


}

void FilterMuonNeutrino::Process(Int_t) {


  // Retain only events with trigger-mask 0xA
  L0TPData* L0TPDataEv = GetL0Data();
  UInt_t  DataType= (UInt_t) L0TPDataEv->GetDataType();
  UInt_t L0TriggerFlags = (UInt_t) L0TPDataEv->GetTriggerFlags();
  Int_t LevelZeroWord = L0TriggerFlags&0x0FFFF;

  Bool_t NuMuTrigger = ( ((DataType&0x01)) && (LevelZeroWord&(1<<10)));

  if (!NuMuTrigger) return;


  // Retain event with a MOQX candidate pair in time coincidence with 2 ns 
  fMUV3Evt = GetEvent<TRecoMUV3Event>();
  EventHeader* rawHeader = GetEventHeader();
  double finetime =  rawHeader->GetFineTime()*ClockPeriod/256.;
  double dtMOQX(999);
  for (int i(0);i<fMUV3Evt->GetNCandidates()-1; i++){
    TRecoMUV3Candidate* candidate1 = static_cast<TRecoMUV3Candidate*>(fMUV3Evt->GetCandidate(i));
    if(candidate1 == NULL) cout<<user_always()<<"BAD "<<__LINE__<<endl;
    if(abs(candidate1->GetTime() - finetime)>=fTW) continue;
    if( candidate1->IsInner() ) continue;
    for (int j(i+1);j<fMUV3Evt->GetNCandidates(); j++){
      TRecoMUV3Candidate* candidate2 = static_cast<TRecoMUV3Candidate*>(fMUV3Evt->GetCandidate(j));
      if(candidate2 == NULL) cout<<user_always()<<"BAD "<<__LINE__<<endl;
      if(abs(candidate2->GetTime() - finetime)>=fTW) continue;
      if( candidate2->IsInner() ) continue;
      if( !( candidate1->GetPosition().X() * candidate2->GetPosition().X() < 0 && candidate1->GetPosition().Y() * candidate2->GetPosition().Y() < 0) ) continue;  //check the QX condition
      double dt = candidate1->GetTime()-candidate2->GetTime();
      if(abs(dt)<abs(dtMOQX)) dtMOQX=dt;
    }
  }

  if(abs(dtMOQX) >= 2) return;


  if (GetEventHeader()->GetTimeStamp() % fDownscaling) return;
  FilterAccept();
}
