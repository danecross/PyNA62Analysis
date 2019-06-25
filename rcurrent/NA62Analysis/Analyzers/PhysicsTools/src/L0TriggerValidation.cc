// ---------------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson) 2017-08-05
// ---------------------------------------------------------------

/// \class L0TriggerValidation
/// \Brief
/// An analyzer to validate both the L0 trigger system and the L0PrimitiveHandler
/// tool, by comparing their results on physics and control events.
/// Plots are also made so that the number of events generated and sent can be
/// compared to the number processed by the analyzer.
/// \EndBrief

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "L0TriggerValidation.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

L0TriggerValidation::L0TriggerValidation(Core::BaseAnalysis *ba) : Analyzer(ba, "L0TriggerValidation")
{
  RequestL0Data();
  RequestL0SpecialTrigger();

  fL0PrimHandle = L0PrimitiveHandler::GetInstance();
  fL0PrimHandle->PrintDetectorBits();
  fL0PrimHandle->PrintDetectorKey();
  
  fNMasks = 16;
  fBadBurst = false;
}

void L0TriggerValidation::InitOutput(){
}

void L0TriggerValidation::InitHist(){
  BookHisto(new TH1F("MaskGenerated",     "Mask Generated;Mask ID;Count",fNMasks,-0.5,fNMasks-0.5));
  BookHisto(new TH1F("MaskSent",          "Mask Sent;Mask ID;Count",fNMasks,-0.5,fNMasks-0.5));
  BookHisto(new TH1F("MaskDownscale",     "Mask Downscale;Mask ID;Count",fNMasks,-0.5,fNMasks-0.5));
  BookHisto(new TH1F("MaskEffDownscale",  "Mask Effective Downscale;Mask ID;Count",fNMasks,-0.5,fNMasks-0.5));
  BookHisto(new TH1F("MaskObserved",      "Mask Observed;Mask ID;Count",fNMasks,-0.5,fNMasks-0.5));
  BookHisto(new TH1F("MaskObservedAP",    "Mask Observed (only AP);Mask ID;Count",fNMasks,-0.5,fNMasks-0.5));
  BookHisto(new TH1F("MaskDAQEfficiency", "Mask DAQ Efficiency;Mask ID;Count",fNMasks,-0.5,fNMasks-0.5));

  Int_t fNConts=1;
  BookHisto(new TH1F("ContGenerated",     "Cont Generated;Cont ID;Count",fNConts,-0.5,fNConts-0.5));
  BookHisto(new TH1F("ContSent",          "Cont Sent;Cont ID;Count",fNConts,-0.5,fNConts-0.5));
  BookHisto(new TH1F("ContDownscale",     "Cont Downscale;Mask ID;Count",fNConts,-0.5,fNConts-0.5));
  BookHisto(new TH1F("ContEffDownscale",  "Cont Effective Downscale;Cont ID;Count",fNConts,-0.5,fNConts-0.5));
  BookHisto(new TH1F("ContObserved",      "Cont Observed;Cont ID;Count",fNConts,-0.5,fNConts-0.5));
  BookHisto(new TH1F("ContDAQEfficiency", "Cont DAQ Efficiency;Cont ID;Count",fNConts,-0.5,fNConts-0.5));

  TString name="";
  name="L0TPGranularity";
  BookHisto(new TH2F(name, name+";Offline L0TP Granularity; Online L0TP Granularity", 8, -0.5, 7.5, 8, -0.5, 7.5));
  BookHisto(new TH1F("GoodBursts", "Number of good bursts", 2, -0.5, 1.5));

  name="MaskValidation";
  BookHistoArray(new TH2F(name, name+";Tool Flagged;L0 Flagged",2,-0.5,1.5,2,-0.5,1.5), fNMasks);
  name="ContValidation";
  BookHistoArray(new TH2F(name, name+";Tool Flagged;L0 Flagged",2,-0.5,1.5,2,-0.5,1.5), fNMasks);
}

void L0TriggerValidation::DefineMCSimple(){}

void L0TriggerValidation::StartOfRunUser(){}

void L0TriggerValidation::StartOfBurstUser(){}

void L0TriggerValidation::ProcessSpecialTriggerUser(int , unsigned int){}

void L0TriggerValidation::ProcessEOBEvent(){
  fMaskNames.clear();
  fDownScale.clear();

  FillHisto("GoodBursts", 0);
  fBadBurst = false;
  if(!GetL0Data()) fBadBurst = true ;
  if(!GetL0SpecialTrigger()) fBadBurst = true ;
  if(fBadBurst){
    std::cout << std::endl << user_normal() << "This is a bad burst." << std::endl;
    return;
  }
  FillHisto("GoodBursts", 1);

  // set up primitive handler tool
  fL0PrimHandle->SetData(GetL0Data(), GetEventHeader()->GetRunID());
  fL0PrimHandle->GetL0Masks(GetL0SpecialTrigger(), fMaskNames, fDownScale);
  std::cout << user_normal() << "L0TP granularity offline " << fL0PrimHandle->GetL0TPGranularity(0) << std::endl;
  std::cout << user_normal() << "L0TP granularity online  " << GetL0SpecialTrigger()->GetFineTimeBits() << std::endl;
  FillHisto("L0TPGranularity",
	    fL0PrimHandle->GetL0TPGranularity(0),
	    GetL0SpecialTrigger()->GetFineTimeBits());

  Int_t ContDS     = GetL0SpecialTrigger()->GetControlTriggerDownscalingFactor();
  Int_t CGenerated = GetL0SpecialTrigger()->GetNControlTriggersGenerated();
  Int_t CSent      = GetL0SpecialTrigger()->GetNControlTriggersSent();
  Double_t CEffDS  = 0;
  if(CSent>0) CEffDS = double(CGenerated) / double(CSent);

  FillHisto("ContGenerated",    0, CGenerated);
  FillHisto("ContSent",         0, CSent);
  FillHisto("ContDownscale",    0, ContDS);
  FillHisto("ContEffDownscale", 0, CEffDS);
  
  std::vector<L0Mask> L0Masks = GetL0SpecialTrigger()->GetL0Masks();
  for(unsigned int i=0;i<L0Masks.size();i++){
    Int_t Generated = L0Masks.at(i).GetNTriggersGenerated();
    Int_t Sent      = L0Masks.at(i).GetNTriggersSent();
    Int_t Downscale = L0Masks.at(i).GetDownscalingFactor();
    Double_t EffDS  = 0;
    if(Sent>0) EffDS = double(Generated) / double(Sent);

    FillHisto("MaskGenerated", i,    Generated);
    FillHisto("MaskSent", i,         Sent);
    FillHisto("MaskDownscale", i,    Downscale);
    FillHisto("MaskEffDownscale", i, EffDS);
  }

}

void L0TriggerValidation::Process(int ){

  if(fBadBurst) return;

  Int_t Flags = GetL0Data()->GetTriggerFlags();
  Int_t level1Word = (GetEventHeader()->GetTriggerType()&0x00FF00)>>8;
  Bool_t L1AutoPass = ((level1Word&128) == 128) ;

  Bool_t Physics = GetL0Data()->GetDataType() & 0x1 ;
  if(Physics){
    // Check that primitives in the L0TP packet match the trigger flags
    Int_t RichTime = fL0PrimHandle->GetTriggerTime(kL0RICH);
    for(unsigned int i=0; i<fMaskNames.size(); ++i){
      Int_t Flag = (Flags>>i)&0x1;
      FillHisto("MaskObserved", i, Flag);
      if(L1AutoPass) FillHisto("MaskObservedAP", i, Flag);
      Int_t hasMask = fL0PrimHandle->CheckPrimitives(fMaskNames[i], RichTime);
      FillHistoArray("MaskValidation", i, hasMask, Flag);
    }
  }

  Bool_t Control = GetL0Data()->GetDataType() & 0x10;
  if(Control){
    // Check that physics flags are set properly in control events
    FillHisto("ContObserved", 0);
    Bool_t RICHCondition = fL0PrimHandle->CheckCondition(kL0RICH);
    if(RICHCondition){
      Int_t RichTime = fL0PrimHandle->GetTriggerTime(kL0RICH);
      for(unsigned int i=0; i<fMaskNames.size(); ++i){
	Int_t Flag = (Flags>>i)&0x1;
	Int_t hasMask = fL0PrimHandle->CheckPrimitives(fMaskNames[i], RichTime);
	FillHistoArray("ContValidation", i, hasMask, Flag);	
      } // loop over masks
    } // rich condition
  } // control events
  
}

void L0TriggerValidation::PostProcess(){
}

void L0TriggerValidation::EndOfBurstUser(){
}

void L0TriggerValidation::EndOfRunUser(){
}

void L0TriggerValidation::EndOfJobUser(){
  SaveAllPlots();
}

void L0TriggerValidation::DrawPlot(){
}

L0TriggerValidation::~L0TriggerValidation(){
}
