// ---------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2017-10-10
//
// ---------------------------------------------------------
/// \class VL0Emulator
/// \Brief
/// Base class for L0 trigger emulators.
/// \EndBrief
/// \Detailed
/// Base class for L0 trigger emulators.
/// Defines most configurable parameters, the Process() function,
/// the shuffling and splitting functions, and the output data.
/// The L0TP overwriting is also implemented here.
///
/// The configurable parameters are:
/// "Active" -- whether the emulator will be run. 
/// "Simple" -- whether the emualtor will run the simple algorithm.
///    The simple algorithm builds just one cluster close to the
///    reference time of the event.
/// "Debug" -- detailed printout in some cases.
///
/// "L0Window" -- the 'matching' window of the L0 firmware, used by the
///    various clustering algorithms (NOW DEPRECATED, use ClusteringWindow!)
/// "RunPeriod" -- implements different emulator behaviour, given that 
///   the firmware versions were changed through the years. Particularly
///   important to remove the 'quadrant drop' of NewCHOD in 2017 onwards.
/// "FineTimeBit" -- The width of the RAM in the L0TP, used to determine
///   if two primitives would overwrite each other. Setting this parameter
///    greater than 8 stops the overwriting check from being performed.
///
/// "ClusteringWindow" -- NEW method. Passes the clustering window as an interger
///   number of TDC units. The ClusteringWindow value overrides the L0Window value
///   at the StartOfBurstUser.
///
/// "ShuffleAlgorithm" -- which hit sorting should be performed.
///    0 -> No sorting.
///    1 -> Random sorting.
///    2 -> Time ordered.
///    3 -> Time reversed.
///    4 -> Partially random, fraction determined by "ShuffleFraction"
/// "ShuffleSeed" -- seed used in the random shuffle algorithm,
///    which implements a Mersenne Twister.
/// "ShuffleFraction" -- determines fraction of hits that are randomised
///    when using "ShuffleAlgorithm" == 4
///
/// "GenerateAlgorithm" -- changes the usage of hits-vs-intensity histograms
///    when generating accidental hits.
///    0 -> Number of hits to generate drawn directly from the histogram,
///          so "GenerateGTKIntensity" must be from 0 to 1200 inclusive.
///    1 -> Number of hits drawn using multiple samples at a lower intensity,
///         designed to give better performance at high intensities where
///         the number of events in the histogram is small.
/// "GenerateSeed" -- seed used to generate accidental hits.
/// "GenerateGTKIntensity" -- number of accidental hits to be generated 
///    when running emulator on MC events, based on the beam intensity
///    as measured by the BeamIntensity tool, which uses GTK hits.
///
/// "GenerateEventTime" -- Determines whether a fake event time should 
///    be generated for MC events. Defaults to TRUE. Turned off by 
///    L0TriggerEmulator to define a single event time for all emulators.
/// "EventTimeStamp" -- used to give the emulator a certain event timestamp.
/// "EventReference" -- used to give the emulator a L0 reference time.
///
/// \author Chris Parkinson (chris.parkinson@cern.ch)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "VL0Emulator.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

VL0Emulator::VL0Emulator(Core::BaseAnalysis *ba, TString DetectorName) : Analyzer(ba, string("L0"+DetectorName+"Emulator"))
{
  fDetectorName=DetectorName;
  AddParam("Active", &fActive, true);
  AddParam("Simple", &fSimple, false);
  AddParam("Debug", &fDebug, false);

  // 2016 values by default. 
  AddParam("FineTimeBit", &fFineTimeBit, 1);
  AddParam("RunPeriod", &fRunPeriod, 0);

  // Clustering windows (L0Window is deprecated)
  AddParam("L0Window", &fL0Window, -1.0);
  AddParam("ClusteringWindow", &fClusteringWindow, -1);

  AddParam("ShuffleAlgorithm", &fShuffleAlgo, 1);
  AddParam("ShuffleSeed", &fShuffleSeed, -1);
  AddParam("ShuffleFraction", &fShuffleFrac, 1.00); // 100%

  AddParam("GenerateAlgorithm", &fGenAlgo, 0);
  AddParam("GenerateSeed", &fGenSeed, -1);
  AddParam("GenerateGTKIntensity", &fGTKIntensity, -99);

  AddParam("GenerateEventTime", &fGenEventTime, true);
  AddParam("EventTimeStamp", &fEventTimeStamp, 0.0);
  AddParam("EventReference", &fL0Reference, 0.0);

  RequestL0Data();			    
  fRanGen = new TRandom3();

  // random seed, overwritten later unless shuffle seed is zero.
  std::random_device r;
  fShuffler.seed(r());
  
  // accidentals are generated over +/- fEventSize (400ns)
  // multiply number of accidentals requested by 4/100
  // so that the requested number is N/10us
  // i.e. fGenerateAccidentals=10 -> 10/10us -> 1/1us -> 1MHz random hits
  fEventSize       = 200.0;
  fGenAccidentals = fGTKIntensity;
}

void VL0Emulator::InitOutput(){
  TString name = "EmulatedL0"+fDetectorName+"Primitives";
  RegisterOutput(name, &fEventClusters);
}

void VL0Emulator::InitHist(){
  if(!GetIsTree()) return ;
  
  // plot number of hits or candidates in the data
  BookHisto(new TH1F("nCandidates_"+fDetectorName,"",61,-0.5,60.5)); 

  // plot number of primitives produced by the emulator
  BookHisto(new TH1F("nPrimitives_"+fDetectorName,"",61,-0.5,60.5)); 

  // plot time of the hits w.r.t L0Reference
  BookHisto(new TH1F("hitTimes_"+fDetectorName,"",402,-100.5,100.5)); 

  // plot time of the hits w.r.t TimeStamp
  BookHisto(new TH1F("hitTimesNoRef_"+fDetectorName,"",402,-100.5,100.5)); 
  
  // plot time of the generated accidentals
  BookHisto(new TH1F("hitTimesAccidentals_"+fDetectorName,"",402,-100.5,100.5)); 
  
  // plot time of the primitives w.r.t L0Reference
  BookHisto(new TH1F("primitiveTime_"+fDetectorName,"",402,-100.5,100.5)); 
  
  // plot time difference between primitives
  BookHisto(new TH1F("deltaTime_"+fDetectorName,"",201,-100.5,100.5)); 
}

void VL0Emulator::StartOfBurstUser(){
  
  if(!GetIsTree()) return ;
  
  // Set fL0Window using (in order of preference):
  //  - the NEW ClusteringWindow command (integer)
  //  - the DEPRECATED L0Window command (float)
  //  - the DEFAULT value (128*TdcCalib)
  if(fClusteringWindow>=0) fL0Window = fClusteringWindow*TdcCalib;
  else if(fL0Window>=0.0)  {}
  else                     fL0Window = 128*TdcCalib;

  // Setting random generator seeds at start of each burst.
  if (fRanGen) {
    if (fGenSeed!=-1) {
      fRanGen->SetSeed(fGenSeed);
    } else {
      fRanGen->SetSeed(GetEventHeader()->GetBurstID());
    }
  }

  if(fShuffleSeed!=0) { // fShuffleSeed==0 is dealt with in the constructor
    if (fShuffleSeed!=-1) {
      fShuffler.seed(fShuffleSeed);
    } else {
      fShuffler.seed(GetEventHeader()->GetBurstID());
    }
  }
}

void VL0Emulator::ProcessSpecialTriggerUser(int, unsigned int) {}

void VL0Emulator::Process(Int_t) {
  if(!GetIsTree()) return ;

  if(!fActive) return; // remove unwanted trigger emulators
  
  if(GetWithMC()){
    if(fGenEventTime) EventTimes();
    //else{event time should come from L0TriggerEmulator}
  }
  else{  
    fEventTimeStamp = GetEventHeader()->GetTimeStamp()*ClockPeriod;
    fL0Reference    = int(GetL0Data()->GetReferenceFineTime())*TdcCalib;
  }
  
  // Convert hits to clusters (detector specific)
  FillTimes();

  // Set upper and lower clustering edges for all hits (clusters).
  SetAllEdges();

  // generate accidentals for MC events.
  if(!(fGTKIntensity<0)) GenerateAccidentals();
  
  // run simple or detailed trigger emulation
  if(fSimple) Simple();
  else        Detailed();

  // convert clusters to primitives
  BuildPrimitives();
  FillHisto("nPrimitives_"+fDetectorName, fEventClusters.size());
}

void VL0Emulator::EventTimes(){
  fEventTimeStamp  = 10240*ClockPeriod;
  fEventTimeStamp += int(fRanGen->Rndm()*256)*ClockPeriod;
  fL0Reference     = int(fRanGen->Rndm()*256)*TdcCalib;
}

void VL0Emulator::SetAllEdges(){
  HitVec::iterator hit = fTimes.begin();
  for( ; hit != fTimes.end(); ++hit) hit->SetEdges(fL0Window);
}

void VL0Emulator::ShuffleAlgo(HitVec& input){

  // algo=0 does not shuffle the hits.
  // algo=1 (default) shuffles according to mersenne twister
  //        given the seed fShuffleSeed (defualt 3457).
  // algo=2 gives time-ordered hits.
  // algo=3 gives time-reversed hits.
  if(fShuffleAlgo==0) return;
  else if(fShuffleAlgo==1) std::shuffle(input.begin(), input.end(), fShuffler);
  else{
    SortAlgo(input);
    if(fShuffleAlgo==3) std::reverse(input.begin(), input.end());
    if(fShuffleAlgo==4) PartialSort(input);
  }
}

HitMap VL0Emulator::SplitAlgo(HitVec& input, Int_t SplitSize){
  HitMap timesSlots;
  HitMap::iterator mapit;

  HitVec::iterator vecit = input.begin();

  for(; vecit != input.end(); ++vecit){
    UInt_t ifulltime  = ((vecit->GetTime())/ClockPeriod)/SplitSize ;
    
    mapit = timesSlots.find(ifulltime);
    if(mapit != timesSlots.end()){
      (mapit->second).push_back(*vecit);// push back time to existing vector.
    }
    else{
      HitVec temp(1, *vecit);  // add hit to a new vector
      timesSlots[ifulltime] = temp;
    }
  }
  return timesSlots;
}

HitMap VL0Emulator::SplitByPP(HitVec& input){
  HitMap timesSlots;
  HitMap::iterator mapit;

  HitVec::iterator vecit = input.begin();
  for(; vecit != input.end(); ++vecit){
    Int_t pp = vecit->GetPP();

    mapit = timesSlots.find(pp);
    
    if(mapit != timesSlots.end()){
      (mapit->second).push_back(*vecit);// push back time to existing vector.
    }
    else{
      HitVec temp(1, *vecit);  // add hit to a new vector
      timesSlots[pp] = temp;
    }
    
  }
  return timesSlots;
}

HitMap VL0Emulator::SplitSlots(HitVec& input){
  return SplitAlgo(input, 1);
}

HitMap VL0Emulator::Split100(HitVec& input){
  return SplitAlgo(input, 4);
}

HitMap VL0Emulator::SplitFrames(HitVec& input){
  return SplitAlgo(input, 256);
}

void VL0Emulator::SplitClear(HitMap::iterator it){
  if((it->second).size()>31){
    if(fDebug) std::cout << user_normal() << "[INFO] Split size : " << (it->second).size()
			 << " clearing the split!" << std::endl;
    (it->second).clear();
  }
}

void VL0Emulator::SplitLimit(HitMap::iterator it){
  while((it->second).size()>32){
    (it->second).pop_back();
  }
}

void VL0Emulator::InsertAlgo(ClusVec& clusters, ClusVec::iterator clustit, HitVec::iterator hitit){
  clusters.insert(clustit, *hitit);
}

void VL0Emulator::SortAlgo(ClusVec& clusters){
  std::sort(clusters.begin(), clusters.end(),
	    [](const EmulatedL0Primitive& a, const EmulatedL0Primitive& b)
	    {return (a.GetFirstTime()<b.GetFirstTime());} );
}

void VL0Emulator::PartialSort(ClusVec& clusters){
  // input clusters are sorted.

  std::vector<Int_t> indexes;
  ClusVec storage;

  // pick out random clusters and store their index
  for(unsigned i=0; i<clusters.size(); ++i){
    if(fRanGen->Rndm()<fShuffleFrac){
      storage.push_back(clusters[i]);
      indexes.push_back(i);
    } 
  }

  // shuffle the extracted clusters
  std::shuffle(storage.begin(), storage.end(), fShuffler);

  // insert the shuffled clusters back in the original positions
  for(unsigned i=0; i<indexes.size(); ++i){
    clusters[ indexes[i] ] = storage[i];
  }
}

void VL0Emulator::L0TPOverwriting(){

  int bs = 8-fFineTimeBit;
  if(bs<0||bs>8){
    std::cout << user_normal() << "[ERROR] "
	      << "Invalid FineTimeBit." << std::endl;
    return;
  }
  Long64_t lastSegment = -999;  
  
  for(unsigned int i=0; i<fEventClusters.size(); ++i){
    Double_t ft = fEventClusters[i].GetAverageTime();
    Long64_t segment = ft/TdcCalib; 
    segment = (segment>>bs);
    
    if(!(lastSegment<0)){ // cant check first primitive
      if(segment == lastSegment){
	int j= i-1;
	if(fDebug) std::cout << user_normal() << "[INFO] Primitive " << i
			     << " overwrites primitive " << j << "." << std::endl;
	fEventClusters.erase(fEventClusters.begin()+j); // delete previous cluster
	i--;
      }
    }
    lastSegment = segment;
  }
}

void VL0Emulator::BuildPrimitives(){
   
  ClusVec::iterator clustit;
  clustit = fEventClusters.begin(); 
  
  Bool_t first=true;
  Double_t cTimeLast=0;
  
  for(; clustit != fEventClusters.end(); ++clustit){    
    // remove event time from primitives (and refernece time for MC events)
    if(GetWithMC()) clustit->RemoveEventTime(fEventTimeStamp + fL0Reference);
    else            clustit->RemoveEventTime(fEventTimeStamp);
    Double_t cTime = clustit->GetAverageTime();
    if(GetWithMC()) FillHisto("primitiveTime_"+fDetectorName, cTime);
    else            FillHisto("primitiveTime_"+fDetectorName, cTime-fL0Reference);
    if(!first) FillHisto("deltaTime_"+fDetectorName, cTime-cTimeLast);
    first=false;
    cTimeLast = cTime;

    SetPrimitiveIDs(clustit); // detector specific primitive IDs
  }
}

void VL0Emulator::PostProcess(){
  fTimes.clear();
  fEventClusters.clear();
}

void VL0Emulator::EndOfBurstUser(){
}

void VL0Emulator::EndOfRunUser(){
}

void VL0Emulator::EndOfJobUser(){
  SaveAllPlots();
}

void VL0Emulator::DrawPlot(){
}

VL0Emulator::~VL0Emulator(){
  delete fRanGen;
  fRanGen=NULL;
}
