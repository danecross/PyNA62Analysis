// ---------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2017-10-10
//
// ---------------------------------------------------------

/// \class L0NewCHODEmulator
/// \Brief
/// Emulator of the NewCHOD L0 trigger-generating firmware. Inherits from L0MUV3Emulator.
/// Returns std::vector<EmulatedL0Primitive>
///
/// WARNING! To use this code the L0MUV3Emulator must also be included in the list of
/// analyzers!
///
/// \EndBrief
/// \Detailed
/// Emulator of the NewCHOD L0 trigger-generating firmware. Inherits from L0MUV3Emulator.
/// Returns std::vector<EmulatedL0Primitive>
/// See documentation of L0MUV3Emulator for more details.
///
/// Pileup (accidental) hits are generated using input histograms that can be made using
/// the 'BuildNewCHODPileupHisto' analyzer, with a default file provided in the CDB.
///  The generation is made in multiple 4ns windows to attempt to reproduce the
/// time-structure of hits in the NewCHOD, which is typically non-uniform due to anomalous
/// events with very large hit multiplicities.
///
/// WARNING! To use this code the L0MUV3Emulator must also be included in the list of
/// analyzers!
///
/// \author Chris Parkinson (chris.parkinson@cern.ch)
/// \EndDetailed

// add_preanalyzer L0MUV3Emulator

#include "ConfigSettings.hh"
#include "L0NewCHODEmulator.hh"
#include "BeamIntensityGenerator.hh"
#include "NA62ConditionsService.hh"
#include "TRecoNewCHODEvent.hh"

L0NewCHODEmulator::L0NewCHODEmulator(Core::BaseAnalysis *ba) : L0MUV3Emulator(ba, "NewCHOD"){
  fL0Detector = kL0NewCHOD;
  RequestTree("NewCHOD", new TRecoNewCHODEvent, "Reco");
  RequestL0Data(); // only for T0 evaluation

  TString fname = NA62ConditionsService::GetInstance()->GetFullPath("NewCHODPileupHitMap.root");
  ParseInputFile(fname);

  AddParam("InnerFraction",     &fFracInner, 0.221);

  // these are not used by NewCHOD emulator any more...
  AddParam("TightFraction",     &fFracTight, 0.916);
  AddParam("Quad0Fraction",     &fFracQuads[0], 0.257);
  AddParam("Quad1Fraction",     &fFracQuads[1], 0.510);
  AddParam("Quad2Fraction",     &fFracQuads[2], 0.752);
}

void L0NewCHODEmulator::ParseInputFile(TString fname){
  fFile = nullptr;
  fFile = new TFile(fname,"READ");
  if(!fFile){
    std::cout << user() << "Could not find FILE of intensity histogram." << std::endl;
  }
  fHist = nullptr;
  fFile->GetObject("BuildNewCHODPileupHisto/CumulativePDFCount",fHist);
  if(!fHist){
    std::cout << user() << "Could not find HIST of intensity histogram." << std::endl;
  }

  fQuadrantHist = nullptr;
  fFile->GetObject("BuildNewCHODPileupHisto/CumulativePDFQuadrant",fQuadrantHist);
  if(!fHist){
    std::cout << user() << "Could not find HIST of quadrant histogram." << std::endl;
  }
}

L0NewCHODEmulator::~L0NewCHODEmulator(){
  // delete TFile pointer from ParseInputFile?
}

void L0NewCHODEmulator::InitOutput(){

  //Event and split occupancies
  RegisterOutput("EventOccupancy", &fEventOccupancy);
  RegisterOutput("SplitOccupancy", &fSplitOccupancy);
  VL0Emulator::InitOutput();
}

void L0NewCHODEmulator::InitHist(){
  if(!GetIsTree()) return ;

  // plot quadrant ID of generated pileup hits
  BookHisto(new TH1F("QuadrantID_"+fDetectorName,"",4,-0.5,3.5));

  // plot tight/loose of generated pileup hits
  BookHisto(new TH1F("TightLoose_"+fDetectorName,"",2,-0.5,1.5));

  // plot the number of hits in a +/-2ns window
  BookHisto(new TH1F("nHits4ns_"+fDetectorName,"",101,-0.5,100.5));

  // plot the number of hits in a +/-4ns window
  BookHisto(new TH1F("nHits8ns_"+fDetectorName,"",101,-0.5,100.5));

  // plot the number of generated hits in the +/- 100ns event
  BookHisto(new TH1F("nHitsEvent_"+fDetectorName,"",101,-0.5,100.5));

  // plot the number of generated hits in the 100ns split
  BookHisto(new TH1F("nHitsSplit_"+fDetectorName,"",101,-0.5,100.5));

  VL0Emulator::InitHist();
}

void L0NewCHODEmulator::FillTimes(){

  TRecoNewCHODEvent* event = GetEvent<TRecoNewCHODEvent>("Reco");
  Int_t nHits = event->GetNHits();
  FillHisto("nCandidates_NewCHOD", nHits);

  // list of inner (spasimir) tiles
  int InnerList[18] = {101,106,201,206,301,302,303,306,307,308,309,401,402,403,406,407,408,409} ;

  fTimes.reserve(nHits);
  for(int i=0; i<nHits; ++i){
    TRecoNewCHODHit* hit  = static_cast<TRecoNewCHODHit*>(event->GetHit(i));
    Double_t time         = GetRawTime(hit);
    if(GetWithMC()) time += fL0Reference;
    Double_t fulltime     = time+fEventTimeStamp;
    Bool_t tight          = (hit->GetType() == kTightCandidate);
    Int_t quad            = hit->GetQuadrantID()-1;
    Bool_t inner          = false;
    for(int j=0; j<18; j++){
      if(InnerList[j] == hit->GetTileID()) inner=true;
    }
    FillHisto("hitTimes_NewCHOD", time-fL0Reference);
    FillHisto("hitTimesNoRef_NewCHOD", time);
    fTimes.push_back(EmulatedL0Primitive(fL0Detector, fulltime, tight, inner, quad));
  }
}

Double_t L0NewCHODEmulator::GetRawTime(TRecoNewCHODHit* hit){

  // Notes.
  // For MC events no T0 are available, so the code does nothing special

  // Note that currently the averaging will not give an integer value.
  // The proper behaviour (in comments below) should be used once the TriggerDriftT0 is available.
  
  if(GetWithMC()){
    // MC ONLY (there are no T0s)
    return hit->GetTime();
  }
  else{
    // DATA ONLY
    Double_t t1 = hit->GetTime1() + fT0Map[hit->GetChannel1()] + fEventTriggerDriftT0;
    t1 = std::round(t1/TdcCalib);
    if(hit->GetChannel2()==-1){ // only one channel
      return t1*TdcCalib;
    }
    else{
      Double_t t2 = hit->GetTime2() + fT0Map[hit->GetChannel2()] + fEventTriggerDriftT0;
      t2 = std::round(t2/TdcCalib);
      Double_t df = std::max(t1,t2) - std::min(t1,t2); // +ve integer
      Double_t av = std::min(t1,t2) + std::floor(df/2.0); // average, as implemented in the firmware
      return av*TdcCalib;
    }
  }
}

void L0NewCHODEmulator::SetPrimitiveIDs(ClusVec::iterator clustit){
  std::vector<Bool_t> quads;
  std::vector<Int_t> hits;
  clustit->GetHitInfo(quads, hits);

  Int_t TI = hits[1];
  Int_t LI = hits[2];
  Int_t TO = hits[3];
  Int_t LO = hits[4];

  Int_t NT = TO + TI;
  Int_t NL = LO + LI;
  Int_t N  = NT + NL;
  Int_t NO = TO + LO;

  Int_t NQ = 0;
  for(int i=0;i<4;++i){
    if(quads[i]) NQ++;
  }
  Bool_t QX = (quads[0] && quads[2]) || (quads[1] && quads[3]);
  Bool_t TB = (quads[0] || quads[3]) && (quads[1] || quads[2]);

  Bool_t Q02   = (quads[0] && quads[2]);
  Bool_t Q13   = (quads[1] && quads[3]);
  Bool_t Q4    = Q02 && Q13;
  Bool_t Q02EX = Q02 && !Q13;
  Bool_t Q13EX = Q13 && !Q02;

  Bool_t E1 = ( quads[0] && !quads[1] && !quads[2] && !quads[3]);
  Bool_t E2 = (!quads[0] && !quads[1] && !quads[2] &&  quads[3]);
  Bool_t E3 = (!quads[0] && !quads[1] &&  quads[2] && !quads[3]);
  Bool_t E4 = (!quads[0] &&  quads[1] && !quads[2] && !quads[3]);

  clustit->SetPrimID("NewCHOD", (N>0));

  clustit->SetPrimID("H1",   (N>0));
  clustit->SetPrimID("HT1",  (NT>0));
  clustit->SetPrimID("H2",   (N>1));
  clustit->SetPrimID("HT2",  (NT>1));
  clustit->SetPrimID("HO1",  (NO>0));
  clustit->SetPrimID("HTO1", (TO>0));

  clustit->SetPrimID("UTMC",  (N<5));
  clustit->SetPrimID("UTMCO", (NO<5));

  clustit->SetPrimID("Q1", (NQ>0));
  clustit->SetPrimID("Q2", (NQ>1));
  clustit->SetPrimID("QX",  QX);
  clustit->SetPrimID("QTB", TB);

  clustit->SetPrimID("QE1", E1);
  clustit->SetPrimID("QE2", E2);
  clustit->SetPrimID("QE3", E3);
  clustit->SetPrimID("QE4", E4);

  clustit->SetPrimID("Q02",   Q02);
  clustit->SetPrimID("Q13",   Q13);
  clustit->SetPrimID("Q4",    Q4);
  clustit->SetPrimID("Q02EX", Q02EX);
  clustit->SetPrimID("Q13EX", Q13EX);
}

void L0NewCHODEmulator::GenerateAccidentals(){

  fEventOccupancy=0;
  fSplitOccupancy=0;

  ClusVec early;
  ClusVec late ;

  Double_t FullEventSize = fEventSize*2.0;
  Double_t Step = 4.0;
  Int_t nSteps = FullEventSize/Step;

  if(!fHist){
    std::cout << user_normal() <<
      "Tried to generate accidentals but there's no intensity histogram. Aborting." << std::endl;
    return;
  }

  if(!fQuadrantHist){
    std::cout << user_normal() <<
      "Tried to generate accidentals but there's no quadrant histogram. Aborting." << std::endl;
    return;
  }

  ///////////////////////////////////////////////////////////////////
  //// Determine how many hits should be generated.
  //// This can be done either by directly reading the intensity histogram
  //// at the given intensity, or by reading a lower intensity (where there
  //// are more events in the source histogram) multiple times.
  //// NS = Number of samples
  //// GI = GTK Intensity to be used
  //// e.g. (GI=800 NS=1) is the same as (GI=400, NS=2)
  ///////////////////////////////////////////////////////////////////

  Int_t NS=1;
  Double_t GI=0.0;
  if(fGenAlgo==0){
    // do nothing, using fGTKIntensity directly
    // also the default case
    NS = 1;
    GI = fGTKIntensity;
  }
  if(fGenAlgo==1){
    GI = *(Double_t*)GetOutput("BeamIntensityGenerator.BeamIntensity");

    // only using intensity distribution below 800MHz
    // intensity is probed as multiple samples from a smaller intensity.
    if(GI>800){
      NS = 1 + int(GI/800.0);
      GI = GI/double(NS);
    }
  }
  Int_t xbin = fHist->GetXaxis()->FindFixBin(GI);

  // Ensure that the requested intensity actually exists in the histogram
  if(xbin==0 || xbin>fHist->GetXaxis()->GetNbins()){
    std::cout << user() << "Invalid GTK intensity of " << fGTKIntensity
	      << "MHz used with fGenAlgo=="<< fGenAlgo << ". Intensity must be "
	      << " between 0 and 1200MHz for this GenAlgo." << std::endl;
    return;
  }

  // loop through the number of steps (of 4ns) and generate events.
  Int_t lastntogen=0;
  for(int iStep=0; iStep<nSteps; ++iStep){
    Double_t start = (-fEventSize)+(iStep*Step);
    Double_t end   = start+Step;

    // how many hits to generate?
    Int_t ntogen=0;
    for(int s=0; s<NS; ++s){
      Double_t rr2 = fRanGen->Rndm();
      Int_t tempN=101;
      for(int q=0; q<101; ++q){
	if(rr2<fHist->GetBinContent(xbin,q+1)){
	  tempN = q;
	  break;
	}
      }
      ntogen += tempN;
    }

    // Fill monitoring histograms
    FillHisto("nHits4ns_"+fDetectorName, ntogen);
    if((iStep%2)==1){
      FillHisto("nHits8ns_"+fDetectorName, ntogen+lastntogen);
      lastntogen=0;
    }
    else lastntogen=ntogen;

    // what kind of hit to generate (quadrant, tight/loose)
    for(Int_t i=0; i<ntogen; ++i){
      Double_t time     = fRanGen->Uniform(start, end);
      Double_t fulltime = time + fEventTimeStamp;  // uniform time

      Double_t rr3= fRanGen->Rndm();
      Int_t quad  =99;
      Bool_t tight=true;
      for(int q=0; q<8; ++q){
	if(rr3<fQuadrantHist->GetBinContent(q+1)){
	  quad  = q%4;
	  tight = (q<4);
	  break;
	}
      }
      Bool_t   inner    = (fRanGen->Rndm()<fFracInner);

      if(quad==99){
	std::cout << user_normal() << " Could not generate a quadrant for this hit."
		  << " Using a random number." << std::endl;
	quad = fRanGen->Uniform(0,4);
      }

      // Should this hit appear before or after the MC event?
      if(iStep<(nSteps/2)){
	early.push_back(EmulatedL0Primitive(fL0Detector, fulltime, tight, inner, quad));
      }
      else{
	late.push_back(EmulatedL0Primitive(fL0Detector, fulltime, tight, inner, quad));
      }

      // Fill some monitoring histograms
      FillHisto("QuadrantID_"+fDetectorName, quad);
      FillHisto("TightLoose_"+fDetectorName, tight);
      FillHisto("hitTimes_"+fDetectorName, time);
      FillHisto("hitTimesNoRef_"+fDetectorName, time);
      FillHisto("hitTimesAccidentals_"+fDetectorName, time);
    }// end hit generation in each step
  }// end loop over steps

  // Fill fTimes vector with the extra hits that are before the event
  fTimes.insert(fTimes.begin(), early.begin(), early.end());

  // Fill fTimes vector with the extra hits that are after the event
  fTimes.insert(fTimes.end(), late.begin(), late.end());

  // Fill outputs for event and split occupancies
  SplitOccupancy();
}

void L0NewCHODEmulator::SplitOccupancy(){

  fEventOccupancy=0;
  fSplitOccupancy=0;

  Int_t TimeStamp  = fEventTimeStamp/ClockPeriod;
  Int_t ETimeSplit = TimeStamp/4;

  HitVec::iterator it = fTimes.begin();

  for( ; it != fTimes.end(); ++it){

    Double_t fulltime = it->GetTime();
    Int_t ifulltime   = fulltime/ClockPeriod;

    // std::cout << " TimeStamp " << TimeStamp << std::endl;
    // std::cout << " ETimeSplit " << ETimeSplit << std::endl;
    // std::cout << " fulltime " << fulltime << std::endl;
    // std::cout << " ifulltime " << ifulltime << std::endl;
    // std::cout << " iplittime " << (ifulltime/4) << std::endl;
    // std::cout << " Diff " << abs(ifulltime-TimeStamp) << std::endl;

    Int_t tsdiff = ifulltime-TimeStamp;
    if(tsdiff>-5 && tsdiff<4) fEventOccupancy++; // 8 readout slots: -4, -3, -2, -1, 0, 1, 2, 3

    ifulltime /= 4;
    if(ifulltime==ETimeSplit) fSplitOccupancy++;
  }

  // std::cout << " fEventOccupancy " << fEventOccupancy << std::endl;
  // std::cout << " fSplitOccupancy " << fSplitOccupancy << std::endl;
}
