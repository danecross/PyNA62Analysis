// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-10-31
//
// ---------------------------------------------------------------

/// \class TriggerRates
/// \Brief
/// Print out L0 and L1 trigger rates for a burst
/// \EndBrief
/// \Detailed
/// Information from L0TPSpecialTrigger and L1TPSpecialTrigger is used.
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "TriggerRates.hh"

TriggerRates::TriggerRates(Core::BaseAnalysis *ba) : Analyzer(ba, "TriggerRates") {
  if (!GetIsTree()) return;
  RequestL0Data();
  RequestL1Data();
  RequestL0SpecialTrigger();
  RequestL1SpecialTrigger();
  RequestBeamSpecialTrigger();
}

void TriggerRates::ProcessEOBEvent() {
  Int_t Run = GetEventHeader()->GetRunID();
  Int_t Bur = GetEventHeader()->GetBurstID();

  // Pre-compute L1 input and output counts: sum over the PCs
  std::vector<L1PCSpecialBlock> L1PCs = GetL1SpecialTrigger()->GetL1PCsInfo();
  Int_t L1in[16], L1out[16], L1inTotal=0, L1outTotal=0;
  for (Int_t i=0; i<16; i++) L1in[i] = L1out[i] = 0;
  for (UInt_t i=0; i<L1PCs.size(); i++) { // loop over PCs
    std::vector<L1MaskSpecialBlock> L1Masks = L1PCs[i].GetL1MasksInfo();
    for (UInt_t j=0; j<L1Masks.size(); j++) { // loop over L0 trigger masks
      L1in[j]  += L1Masks[j].GetNL1InputEvents();
      L1out[j] += 0.98*L1Masks[j].GetNL1OutputEvents(); // regular events (recipe certified by Angela)
      L1out[j] += 0.02*L1Masks[j].GetNL1InputEvents();  // autopass events
    }
    L1inTotal  += L1PCs[i].GetNL1InputEvents();
    L1outTotal += L1PCs[i].GetNL1OutputEvents();
  }

  // Print out L0 and L1 counts
  std::cout << user_normal() << "Run " << Run <<", burst "<< Bur <<
    ", intensity = " << GetBeamSpecialTrigger()->GetIntensityT10() << " x 10^11 POT" << std::endl;
  std::cout << user_normal() << "Format: Bit Name | L0_generated L0_sent | L1_input L1_output" << std::endl;
  std::vector<L0Mask> L0Masks = GetL0SpecialTrigger()->GetL0Masks();
  for (UInt_t i=0; i<L0Masks.size(); i++) {
    Int_t condition = TriggerConditions::GetInstance()->L0TriggerCondition(Run, i);
    if (condition>=0) {
      std::cout <<user_normal() << "" << i <<" " << TriggerConditions::GetInstance()->GetL0ConditionName(condition);
      if(CanPrint()){
        if (L0Masks[i].GetDownscalingFactor()>1)
          std::cout << "/" << L0Masks[i].GetDownscalingFactor();
        std::cout <<" | "<<
          L0Masks[i].GetNTriggersGenerated() << " "<<
          L0Masks[i].GetNTriggersSent() << " | "<<
          L1in[i] <<" " << L1out[i] << std::endl;
      }
    }
  }
  std::cout <<user_normal() << "Control/" <<
    GetL0SpecialTrigger()->GetControlTriggerDownscalingFactor() << " | " <<
    GetL0SpecialTrigger()->GetNControlTriggersGenerated() <<" "<<
    GetL0SpecialTrigger()->GetNControlTriggersSent()<<std::endl;
  // According to Dario, there is no information about the total number of triggers in L0TPSpecialTrigger!
  std::cout <<user_normal() << "Total | x x | " << L1inTotal << " " << L1outTotal << std::endl;
}
