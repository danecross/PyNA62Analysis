// ---------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2017-10-10
//
// ---------------------------------------------------------

#include "L0TriggerEmulator.hh"

L0TriggerEmulator::L0TriggerEmulator(Core::BaseAnalysis *ba) : VL0Emulator(ba, "Trigger"){
  fL0Detector=-1;
  AddParam("L0TPLAV",     &fL0TPLAV,     64*TdcCalib);
  AddParam("L0TPCalo",    &fL0TPCalo,    64*TdcCalib);
  AddParam("L0TPMUV3",    &fL0TPMUV3,    64*TdcCalib);
  AddParam("L0TPNewCHOD", &fL0TPNewCHOD, 64*TdcCalib);
}

void L0TriggerEmulator::InitHist(){
  if(!GetIsTree()) return ;
  // prepare the timestamp and L0Reference for the first event.
  
  // turn off the internal event time generation.
  ReconfigureAnalyzer("L0CHODEmulator",    "GenerateEventTime", false);
  ReconfigureAnalyzer("L0RICHEmulator",    "GenerateEventTime", false);
  ReconfigureAnalyzer("L0LAVEmulator",     "GenerateEventTime", false);
  ReconfigureAnalyzer("L0MUV3Emulator",    "GenerateEventTime", false);
  ReconfigureAnalyzer("L0NewCHODEmulator", "GenerateEventTime", false);
  ReconfigureAnalyzer("L0CaloEmulator",    "GenerateEventTime", false);

  // compute and send event time to all emulator modules.
  Configure();
}

void L0TriggerEmulator::Process(int ){
  if(!GetIsTree()) return ;
  
  //Build masks. 

  // copy RICH primitives into fEventClusters
  fEventClusters = 
    *(ClusVec*)GetOutput("L0RICHEmulator.EmulatedL0RICHPrimitives");      

  // get pointers to other primitives
  ClusVec* NewCHODPrims = (ClusVec*)GetOutput("L0NewCHODEmulator.EmulatedL0NewCHODPrimitives");      
  ClusVec* MUV3Prims    = (ClusVec*)GetOutput("L0MUV3Emulator.EmulatedL0MUV3Primitives");      
  ClusVec* LAVPrims     = (ClusVec*)GetOutput("L0LAVEmulator.EmulatedL0LAVPrimitives");      
  ClusVec* CaloPrims    = (ClusVec*)GetOutput("L0CaloEmulator.EmulatedL0CaloPrimitives");      
  ClusVec* CHODPrims    = (ClusVec*)GetOutput("L0CHODEmulator.EmulatedL0CHODPrimitives");      
  
  if(NewCHODPrims==nullptr) 
    std::cout << user() << "No pointer to emulated NewCHOD primitives" << std::endl;
  if(MUV3Prims==nullptr) 
    std::cout << user() << "No pointer to emulated MUV3 primitives" << std::endl;
  if(LAVPrims==nullptr) 
    std::cout << user() << "No pointer to emulated LAV primitives" << std::endl;
  if(CaloPrims==nullptr) 
    std::cout << user() << "No pointer to emulated Calo primitives" << std::endl;
  if(CHODPrims==nullptr) 
    std::cout << user() << "No pointer to emulated CHOD primitives" << std::endl;

  ClusVec::iterator RICHPrim = fEventClusters.begin(); 
  for(; RICHPrim != fEventClusters.end(); ++RICHPrim){

    // RICH primitive times.
    Double_t RICHTime = RICHPrim->GetAverageTime(); 

    // Merge NewCHOD primitives
    ClusVec::iterator NCPrim = NewCHODPrims->begin(); 
    for(; NCPrim != NewCHODPrims->end(); ++NCPrim){
      Double_t NCTime = NCPrim->GetAverageTime();
      if(fabs(RICHTime-NCTime)<fL0TPNewCHOD){
	RICHPrim->MergePrimitive(*NCPrim);
      }
    }
       
    // Merge MUV3 primitives
    ClusVec::iterator MUV3Prim = MUV3Prims->begin(); 
    for(; MUV3Prim != MUV3Prims->end(); ++MUV3Prim){
      Double_t MUV3Time = MUV3Prim->GetAverageTime();
      if(fabs(RICHTime-MUV3Time)<fL0TPMUV3){
	RICHPrim->MergePrimitive(*MUV3Prim);
      }
    }
    
    // Merge LAV primitives
    ClusVec::iterator LAVPrim = LAVPrims->begin(); 
    for(; LAVPrim != LAVPrims->end(); ++LAVPrim){
      Double_t LAVTime = LAVPrim->GetAverageTime();
      if(fabs(RICHTime-LAVTime)<fL0TPLAV){
	RICHPrim->MergePrimitive(*LAVPrim);
      }
    }

    // Merge Calo primitives
    ClusVec::iterator CaloPrim = CaloPrims->begin(); 
    for(; CaloPrim != CaloPrims->end(); ++CaloPrim){
      Double_t CaloTime = CaloPrim->GetAverageTime();
      if(fabs(RICHTime-CaloTime)<fL0TPCalo){
	RICHPrim->MergePrimitive(*CaloPrim);
      }
    }
    
  } // end loop on RICH primitives.
}

void L0TriggerEmulator::PostProcess(){
  if(!GetIsTree()) return ;
  // prepare the timestamp and L0Reference for the next event.
  fEventClusters.clear();
  Configure();
}

void L0TriggerEmulator::EndOfJobUser(){
  // does nothing
}

void L0TriggerEmulator::Configure(){

  EventTimes();
  
  ReconfigureAnalyzer("L0CHODEmulator",    "EventTimeStamp", fEventTimeStamp);
  ReconfigureAnalyzer("L0CHODEmulator",    "EventReference", fL0Reference);

  ReconfigureAnalyzer("L0RICHEmulator",    "EventTimeStamp", fEventTimeStamp);
  ReconfigureAnalyzer("L0RICHEmulator",    "EventReference", fL0Reference);

  ReconfigureAnalyzer("L0LAVEmulator",     "EventTimeStamp", fEventTimeStamp);
  ReconfigureAnalyzer("L0LAVEmulator",     "EventReference", fL0Reference);

  ReconfigureAnalyzer("L0MUV3Emulator",    "EventTimeStamp", fEventTimeStamp);
  ReconfigureAnalyzer("L0MUV3Emulator",    "EventReference", fL0Reference);

  ReconfigureAnalyzer("L0NewCHODEmulator", "EventTimeStamp", fEventTimeStamp);
  ReconfigureAnalyzer("L0NewCHODEmulator", "EventReference", fL0Reference);

  ReconfigureAnalyzer("L0CaloEmulator",    "EventTimeStamp", fEventTimeStamp);
  ReconfigureAnalyzer("L0CaloEmulator",    "EventReference", fL0Reference);
}

void L0TriggerEmulator::FillTimes(){
  // does nothing
}

void L0TriggerEmulator::Simple(){
  // does nothing
}

void L0TriggerEmulator::Detailed(){
  // does nothing
}

void L0TriggerEmulator::SetPrimitiveIDs(ClusVec::iterator /*clustit*/){
  // does nothing
}

void L0TriggerEmulator::GenerateAccidentals(){
  // does nothing
}



