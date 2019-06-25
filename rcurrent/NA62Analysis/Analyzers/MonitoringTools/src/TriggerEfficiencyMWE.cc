#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "TriggerEfficiencyMWE.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

#include "TEfficiency.h"

TriggerEfficiencyMWE::TriggerEfficiencyMWE(Core::BaseAnalysis *ba) : Analyzer(ba, "TriggerEfficiencyMWE")
{
  RequestTree("NewCHOD", new TRecoNewCHODEvent, "Reco");

  // Request the L0TP data packet to be read from the event.
  RequestL0Data();

  // Initialise the PrimitiveHandler.
  fPrimitiveHandler  = L0PrimitiveHandler::GetInstance();

  // The next two lines declare the L0Emulators to PrimitiveHandler.
  // The first (commented) line ensures the relevant L0Emulators are included as preanalyzers.
  //   -- you don't need it if you add them in your config file.
  // The second line tells PrimitiveHandler which L0Emulator(s) you will be using later.
  // PrimitiveHandler also needs the "fParent" pointer as the first argument.

  // add_preanalyzer L0NewCHODEmulator // This line is necessary
  fPrimitiveHandler->DeclareL0Emulators(fParent, kL0NewCHOD);
}

void TriggerEfficiencyMWE::InitOutput(){
}

void TriggerEfficiencyMWE::InitHist(){

  if(GetIsTree()){
    BookHisto(new TH1F("QXEfficiency_vs_NHits_Den", "QXEffNum", 128, -0.5, 127.5));
    BookHisto(new TH1F("QXEfficiency_vs_NHits_Num", "QXEffDen", 128, -0.5, 127.5));
    BookHisto(new TH1F("QXEfficiency_vs_NHits_Emu", "QXEffEmu", 128, -0.5, 127.5));
  }
  else{
    fMyHists.push_back( RequestHistogram(fAnalyzerName, "QXEfficiency_vs_NHits_Den", true) );
    fMyHists.push_back( RequestHistogram(fAnalyzerName, "QXEfficiency_vs_NHits_Num", true) );
    fMyHists.push_back( RequestHistogram(fAnalyzerName, "QXEfficiency_vs_NHits_Emu", true) );
  }
}

void TriggerEfficiencyMWE::DefineMCSimple(){
}

void TriggerEfficiencyMWE::StartOfRunUser(){

  if(!GetIsTree()) return;

  // Give PrimitiveHanlder the current run ID.
  fPrimitiveHandler->SetRunID( GetRunID() );

  // In MC you might want to overwrite the run ID from the event if the MC sample
  // was generated in different conditions to the data you are looking at.
  // For example: 6610 for 2016 data; 8215 for 2017 data; 8983 for 2018 data.
  if(GetWithMC()) fPrimitiveHandler->SetRunID( 8215 );
  
  // Configure the L0Emulators for the current run.
  fPrimitiveHandler->ConfigureL0Emulators();
}

void TriggerEfficiencyMWE::StartOfBurstUser(){
}

void TriggerEfficiencyMWE::ProcessSpecialTriggerUser(int /*iEvent*/, unsigned int /*triggerType*/){
}

void TriggerEfficiencyMWE::Process(int /*iEvent*/){

  // Don't run in HISTO mode
  if(!GetIsTree()) return;

  // don't run on simulated data.
  if(GetWithMC()) return;

  // Give PrimitiveHandler the L0TPData packet.
  fPrimitiveHandler->SetData(GetL0Data());
  
  // Check if this event was fired by the control trigger.
  Bool_t ControlData = (GetL0Data()->GetDataType() & 0x10);

  // Use PrimitiveHandler to confirm there is a CHOD primitive in the trigger slot.
  ControlData &= fPrimitiveHandler->IsPrimitiveInSlot(kL0CHOD, kL0TriggerSlot);

  // Don't run unless event was collected via the Control trigger.
  if(!ControlData) return;

  // Check RICH condition to ensure 1-to-1 matching of CHOD and RICH primitives.
  Bool_t RICHCondition = fPrimitiveHandler->CheckCondition(kL0RICH);

  // Don't run unless event has a 1-to-1 matching of CHOD and RICH primitives.
  if(!RICHCondition) return;

  // Get the time of the RICH primitive in the trigger slot.
  Int_t RICHTime = fPrimitiveHandler->GetTriggerTime(kL0RICH);

  // Get the number of NewCHOD hits in the  event
  TRecoNewCHODEvent* event = GetEvent<TRecoNewCHODEvent>("Reco");
  Int_t NHits = event->GetNHits();

  // Check that the event is expected to fire QX
  Bool_t isQXEvent = IsQXEvent(event, RICHTime);

  // Only run on events that are expected to fire QX.
  if(!isQXEvent) return;

  // Fill denominator histogram.
  FillHisto("QXEfficiency_vs_NHits_Den", NHits);

  // Check for a QX primitive coincident with the RICH primitive time.
  Bool_t HasQX = fPrimitiveHandler->CheckPrimitives("QX", RICHTime);
  
  // Fill numerator histogram for real primitives.
  if(HasQX) FillHisto("QXEfficiency_vs_NHits_Num", NHits);

  // Check for an emulated QX primitive coincident with the RICH primitive time.
  Bool_t EmuQX = fPrimitiveHandler->CheckEmulatedPrimitives("QX", RICHTime);

  // Fill numerator histogram for emulated primitives.
  if(EmuQX) FillHisto("QXEfficiency_vs_NHits_Emu", NHits);
}

Bool_t TriggerEfficiencyMWE::IsQXEvent(TRecoNewCHODEvent* event, Int_t RICHTime){

  // Convert time from 'online' TDC units to 'offline' ns units.
  // Involves converting from Int_t to Double_t
  Double_t RICHTimeF = RICHTime*TdcCalib; 

  // Get number of NewCHOD hits in event.
  Int_t NHits = event->GetNHits();

  // Simple QX algorithm.
  // Note that the timing cuts (at +/- 3ns) are quite tight, but not unreasonable.
  // 1. Check for a first hit close to the RICH trigger time, record its quadrant
  // 2. Check for a second hit close to the RICH trigger time, record its quadrant
  // 3. Check that the two hits are coincident
  // 4. Check that the two hits are in opposite quadrants.

  // loop through all hits
  for(int i=0; i<NHits; ++i){
    TRecoNewCHODHit* hit1 = static_cast<TRecoNewCHODHit*>(event->GetHit(i));

    // Step 1.
    if(fabs(hit1->GetTime()-RICHTimeF)>3.0) continue;
    Int_t q1 = hit1->GetQuadrantID();

    // loop through all OTHER hits
    for(int j=i+1; j<NHits; ++j){
      TRecoNewCHODHit* hit2 = static_cast<TRecoNewCHODHit*>(event->GetHit(j));

      // Step 2.
      if(fabs(hit2->GetTime()-RICHTimeF)>3.0) continue;
      Int_t q2 = hit2->GetQuadrantID();

      // Step 3.
      if(fabs(hit1->GetTime() - hit2->GetTime())>3.0) continue;

      // Step 4.
      if(fabs(q1-q2)!=2) continue;

      // This is a QX event.
      return true;
    }
  }
  
  // This is not a QX event.
  return false;
}

void TriggerEfficiencyMWE::PostProcess(){
}

void TriggerEfficiencyMWE::EndOfBurstUser(){
}

void TriggerEfficiencyMWE::EndOfRunUser(){
}

void TriggerEfficiencyMWE::EndOfJobUser(){

  // If running in HISTO mode, take the three histograms from the fHist container.
  // Create two TEfficiency objects, one each for the REAL and EMULATED trigger efficiency.
  if(!GetIsTree()){

    TH1* den = fMyHists[0];
    TH1* num = fMyHists[1];
    TH1* emu = fMyHists[2];

    if(den && num && emu){
      TEfficiency* eff1 = new TEfficiency(*num, *den);    
      TEfficiency* eff2 = new TEfficiency(*emu, *den);

      eff1->SetName("QXEfficiency_vs_NHits_Real");
      eff2->SetName("QXEfficiency_vs_NHits_Emulated");
      
      eff1->Write();
      eff2->Write();
    }
    else{
      if(!den) std::cout << user_normal() << " Den histogram not found!" << std::endl;
      if(!num) std::cout << user_normal() << " Num histogram not found!" << std::endl;
      if(!emu) std::cout << user_normal() << " Emu histogram not found!" << std::endl;
    }
  }

  // Save all plots to file.
  SaveAllPlots();
}

void TriggerEfficiencyMWE::DrawPlot(){
}

TriggerEfficiencyMWE::~TriggerEfficiencyMWE(){
}
