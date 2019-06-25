// ---------------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson) 2017-05-08
// ---------------------------------------------------------------

/// \class L0RateComputer
/// \Brief 
/// An analyzer that uses the primitive dumps to estimate the L0 trigger
/// rate. It takes an unbias sample of primitives by using the upper
/// timing sideband in collected physics/control events, builds a fake
/// L0TP packet, then uses the L0PrimitiveHandler to determine which 
/// masks would have fired.
/// \EndBrief

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "L0RateComputer.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

#include "TPrimitive.hh"

L0RateComputer::L0RateComputer(Core::BaseAnalysis *ba) : Analyzer(ba, "L0RateComputer")
{
  RequestL0Data();
  RequestBeamSpecialTrigger();
  RequestL0SpecialTrigger();

  fL0PrimHandle = L0PrimitiveHandler::GetInstance();
  fL0PrimHandle->NewInputFile("./L0PrimitiveBits.txt");
  fL0PrimHandle->PrintDetectorBits();
  fL0PrimHandle->PrintDetectorKey();

  fNMasks = 16;
  fArgonionCount = 0.0;
  fControlDS=0;
  
  AddPrimitiveReader("CHOD", false);
  SetL0MatchingWindowWidth("CHOD",70);

  AddPrimitiveReader("RICH", false);
  SetL0MatchingWindowWidth("RICH",70);

  AddPrimitiveReader("IRC", false);
  SetL0MatchingWindowWidth("IRC",70);

  AddPrimitiveReader("LAV", false);
  SetL0MatchingWindowWidth("LAV",70);

  AddPrimitiveReader("MUV3", false);
  SetL0MatchingWindowWidth("MUV3",70);

  AddPrimitiveReader("LKr", false);
  SetL0MatchingWindowWidth("LKr",70);
}

void L0RateComputer::InitOutput(){
}

void L0RateComputer::InitHist(){  
  BookHisto(new TH1F("MaskGenerated","Mask Generated;Mask ID;Count",fNMasks,-0.5,fNMasks-0.5));
  BookHisto(new TH1F("FakeGenerated","Fake Generated;Fake ID;Count",fNMasks,-0.5,fNMasks-0.5));

  BookHisto(new TH1F("MaskSent","Mask Sent;Mask ID;Count",fNMasks,-0.5,fNMasks-0.5));
  BookHisto(new TH1F("FakeSent","Fake Sent;Fake ID;Count",fNMasks,-0.5,fNMasks-0.5));
  
  BookHisto(new TH1F("PhysicsGenerated", "Physics trigger generated", 1, 0.5,1.5));
  BookHisto(new TH1F("PhysicsSent", "Physics trigger sent", 1, 0.5,1.5));

  BookHisto(new TH1F("ControlGenerated","Control Generated;Mask ID;Count",1,0.5,1.5));
  BookHisto(new TH1F("ControlSent","Control Sent;Mask ID;Count", 1, 0.5,1.5));
  BookHisto(new TH1F("FakeControlGenerated", "Control trigger generated", 1, 0.5,1.5));
  BookHisto(new TH1F("FakeControlSent", "Control trigger sent", 1, 0.5,1.5));
  
  BookHisto(new TH1F("nEvents", "nEvents", 1, 0.5, 1.5));
  BookHisto(new TH1F("nRICH", "nRICH", 1, 0.5, 1.5));

  BookHisto(new TH1F("L0Triggers", "L0Triggers", 1, 0.5, 1.5));  
  BookHisto(new TH1F("Argonion", "Argonion", 1, 0.5, 1.5));  
  BookHisto(new TH1F("T10Intensity", "T10Intensity", 1, 0.5, 1.5));  
}

void L0RateComputer::DefineMCSimple(){}

void L0RateComputer::StartOfRunUser(){}

void L0RateComputer::StartOfBurstUser(){}

void L0RateComputer::ProcessSpecialTriggerUser(int , unsigned int){}

void L0RateComputer::ProcessEOBEvent(){

  BeamSpecialTrigger* Beam = GetBeamSpecialTrigger();
  fArgonionCount=Beam->GetCountsARGONION()/100000000.;
  TargetInfo T10 = Beam->GetT10();
  FillHisto("Argonion",     1, fArgonionCount);
  FillHisto("T10Intensity", 1, T10.GetIntensity());


  fMaskNames.clear();
  fDownScales.clear();
  fL0PrimHandle->SetData(GetL0Data(), GetEventHeader()->GetRunID());
  fL0PrimHandle->GetL0Masks(GetL0SpecialTrigger(), fMaskNames, fDownScales);
  fMaskNames.resize(16,"");
  fDownScales.resize(16,0);

  std::vector< L0Mask > L0Masks;
  L0Masks = GetL0SpecialTrigger()->GetL0Masks();
  for(unsigned int i=0;i<L0Masks.size();i++){
    FillHisto("MaskGenerated", i, L0Masks.at(i).GetNTriggersGenerated() );
    FillHisto("MaskSent"     , i, L0Masks.at(i).GetNTriggersSent() );
  }

  fControlDS = GetL0SpecialTrigger()->GetControlTriggerDownscalingFactor();
  std::cout << user_normal() << "There are " << GetL0SpecialTrigger()->GetNControlTriggersGenerated()
	    << " control generated and " << GetL0SpecialTrigger()->GetNControlTriggersSent()
	    << " control triggers sent." << std::endl;
  FillHisto("ControlGenerated", 1, GetL0SpecialTrigger()->GetNControlTriggersGenerated());
  FillHisto("ControlSent",      1, GetL0SpecialTrigger()->GetNControlTriggersSent());

  std::cout << user_normal() << "control trigger downscaling : " << fControlDS << std::endl;

}

void L0RateComputer::Process(int ){

  Bool_t Type = GetL0Data()->GetDataType() & 0x11;
  if(!Type) return;

  // Count events
  FillHisto("nEvents", 1);

  // Create fake L0TPData
  L0TPData* MyL0TPData = CreateL0TPData();
  
  // set up L0PrimitiveHandler with fake L0TP data.
  fL0PrimHandle->SetData(MyL0TPData, GetEventHeader()->GetRunID());
  
  // require RICH primitive in trigger slot.
  Bool_t HasRICH = fL0PrimHandle->IsPrimitiveInSlot(kL0RICH, kL0TriggerSlot);
  if(HasRICH){
    FillHisto("nRICH",1);
  
    // Get time of RICH primitive in the trigger slot
    Int_t RichTime = fL0PrimHandle->GetTriggerTime(kL0RICH);
    
    Bool_t PhysicsGenerated = false ;
    Bool_t PhysicsSent      = false ;
    for(int i=0; i<fNMasks; ++i){
      Bool_t hasMask = fL0PrimHandle->CheckPrimitives(fMaskNames[i], RichTime);
      if(hasMask){
	FillHisto("FakeGenerated",i);
	PhysicsGenerated=true;
	Double_t downscale = fDownScales[i];
	if( gRandom->Rndm()<(1.0/downscale)){
	  FillHisto("FakeSent",i);
	  PhysicsSent=true;
	}
      }
    }    
    if(PhysicsGenerated) FillHisto("PhysicsGenerated", 1);
    if(PhysicsSent)      FillHisto("PhysicsSent", 1);
  }
  
  ////////////////////////////////////////
  //// Control trigger 
  ////////////////////////////////////////
  Bool_t HasCHOD = fL0PrimHandle->IsPrimitiveInSlot(kL0CHOD, kL0TriggerSlot);
  if(HasCHOD){
    FillHisto("FakeControlGenerated", 1);
    if( gRandom->Rndm()<(1.0/fControlDS)){
      FillHisto("FakeControlSent", 1);
    }
  }  
  
}

L0TPData* L0RateComputer::CreateL0TPData(){

  std::vector< std::vector< TPrimitive> > prims(7); // 7 vectors of primitives
  prims[0] = FindAllPrimitiveInMatchingWindow("CHOD");
  prims[1] = FindAllPrimitiveInMatchingWindow("RICH");
  prims[2] = FindAllPrimitiveInMatchingWindow("LAV");
  prims[3] = FindAllPrimitiveInMatchingWindow("MUV3");
  prims[4] = FindAllPrimitiveInMatchingWindow("IRC");
  std::vector< TPrimitive> talks;
  prims[5] = talks;
  prims[6] = FindAllPrimitiveInMatchingWindow("LKr");  

  Int_t size = 64;
  Int_t mid  = 512;
  Int_t low  = mid-size;
  Int_t upp  = mid+size;
  Int_t high = mid+size+size;

  std::vector<L0Primitive> input(7*3);
  for(unsigned int i=0; i<7; ++i){
    
    for(unsigned int j=0; j<prims[i].size(); ++j){
      Int_t TSdiff = prims[i][j].GetTimeStamp() - GetL0Data()->GetTimeStamp();
      Int_t time = prims[i][j].GetFineTime() + 256*TSdiff;
      
      if(time>=low && time <high){
	// This primitive will be used.
	
	L0Primitive pp;
	pp.SetPrimitiveID(prims[i][j].GetPrimitiveID());
	pp.SetFineTime(prims[i][j].GetFineTime());

	// kL0TriggerSlot, kL0PreviousSlot, kL0NextSlot
	if(time>=low && time<mid)       input[i+7]  = pp;
	else if(time>=mid && time<upp)  input[i]    = pp;
	else if(time>=upp&&time<high)   input[i+14] = pp;
	else std::cout << user_normal() << "something went wrong here " << std::endl;
      }
    }
  }

  L0TPData* MyL0TPData = new L0TPData();
  MyL0TPData->SetPrimitives(input);
  
  return MyL0TPData;
}



void L0RateComputer::PostProcess(){

}

void L0RateComputer::EndOfBurstUser(){
}

void L0RateComputer::EndOfRunUser(){

}

void L0RateComputer::EndOfJobUser(){

  
  SaveAllPlots();
}

void L0RateComputer::DrawPlot(){
}

L0RateComputer::~L0RateComputer(){
}
