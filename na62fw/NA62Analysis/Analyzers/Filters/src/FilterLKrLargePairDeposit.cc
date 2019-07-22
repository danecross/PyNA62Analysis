// ---------------------------------------------------------------
//
// History:
//
// Created by Jurgen Engelfried (Jurgen.Engelfried@cern.ch) 2019-03-25
//
// ---------------------------------------------------------------

/// \class FilterLKrLargePairDeposit
/// \Brief
/// Filter events with a large energy deposit in the LKr
/// \EndBrief
/// \Detailed
/// Events with a large (>72GeV) energy deposit of an intime cluster pair are filtered.
/// A downscaling is controllable by the user can be applied;
/// events with timestamp divisible by the downscaling are filtered out. 
/// Also the minimum energy deposit can be changed.
/// Example of use:
/// \code
/// ./MyApplication -i <input_file> -o <output_file> -p "FilterLKrLargePairDeposit:Downscaling=10;MinimumEnergy=70000;TimeWindow=10" --filter
/// \endcode
/// \author  Jurgen Engelfried (Jurgen.Engelfried@cern.ch) 2019-03-25
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include "FilterLKrLargePairDeposit.hh"
#include "Event.hh"
#include "Persistency.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

FilterLKrLargePairDeposit::FilterLKrLargePairDeposit(Core::BaseAnalysis *ba) :
  Analyzer(ba, "FilterLKrLargePairDeposit") {
  RequestAllMCTrees();
  RequestAllRecoTrees();
  AddParam("Downscaling", &fDownscaling, 1); // by default, no downscaling
  AddParam("MinimumEnergy", &fMinimumEnergy, 72000.); // Default 72 GeV
  AddParam("TimeWindow", &fInTime, 7.); // Default 7nsec
  
}

void FilterLKrLargePairDeposit::InitHist() {

  BookHisto("TotalEnergy", new TH1F("TotalEnergy", "Total Energy Deposit in LKr", 160,0.,80000.));
  BookHisto("TotalInTimeEnergy", new TH1F("TotalInTimeEnergy", "Energy Deposit of InTime pairs in LKr", 160,0.,80000.));

  BookCounter("TotalEvents");
  BookCounter("FilteredEvents");
  BookCounter("LargeDepositEvents");
  NewEventFraction("LKrLargePairDeposit");
  AddCounterToEventFraction("LKrLargePairDeposit","TotalEvents") ;
  AddCounterToEventFraction("LKrLargePairDeposit","LargeDepositEvents") ;
  AddCounterToEventFraction("LKrLargePairDeposit","FilteredEvents") ;
  DefineSampleSizeCounter("LKrLargePairDeposit","TotalEvents");

  printf("\n\nRunning FilterLkrLargePairDeposit with MinimumEnergy %5.1f MeV, TimeWindow %3.1f nsec, Downscaling %d\n\n",fMinimumEnergy,fInTime,fDownscaling);

}

void FilterLKrLargePairDeposit::Process(Int_t) {


  // Retain all physics triggers
  L0TPData* L0TPDataEv = GetL0Data();
  UInt_t  DataType= (UInt_t) L0TPDataEv->GetDataType();

  Bool_t Trigger = ((DataType&0x01)) || ((DataType&0x10));

  if (!Trigger) return;

  IncrementCounter("TotalEvents");
  
  TRecoLKrEvent * LKrEvt = static_cast<TRecoLKrEvent*>(GetEvent("LKr"));
  Double_t TotalEnergy = LKrEvt->GetEnergyTotal();
  FillHisto("TotalEnergy",TotalEnergy);
  if (TotalEnergy>fMinimumEnergy) IncrementCounter("LargeDepositEvents");
  
  Bool_t FoundOne=false;
  for (Int_t i=0;i<LKrEvt->GetNCandidates()-1;i++) {
    TRecoLKrCandidate* candidate1 = static_cast<TRecoLKrCandidate*>(LKrEvt->GetCandidate(i));
    for (Int_t j=i+1;j<LKrEvt->GetNCandidates();j++) {
      TRecoLKrCandidate* candidate2 = static_cast<TRecoLKrCandidate*>(LKrEvt->GetCandidate(j));
      if (abs(candidate1->GetTime()-candidate2->GetTime())<fInTime) {
	TotalEnergy = candidate1->GetClusterEnergy()+candidate2->GetClusterEnergy();
	FillHisto("TotalInTimeEnergy",TotalEnergy);
	if (TotalEnergy > fMinimumEnergy) FoundOne = true;
      }
    }
  }
  
	
  if (!FoundOne) return;

  if (GetEventHeader()->GetTimeStamp() % fDownscaling) return;

  IncrementCounter("FilteredEvents");
  FilterAccept();
}

void FilterLKrLargePairDeposit::EndOfJobUser() {
  SaveAllPlots();
}
