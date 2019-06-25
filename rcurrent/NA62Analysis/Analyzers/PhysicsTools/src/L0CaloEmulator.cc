// ---------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2017-10-10
//
// ---------------------------------------------------------

#include "L0CaloEmulator.hh"
#include "VL0Emulator.hh"

#include "TRecoLKrEvent.hh"

L0CaloEmulator::L0CaloEmulator(Core::BaseAnalysis *ba) : VL0Emulator(ba, "Calo"){
  fL0Detector = kL0Calo;
  RequestTree("LKr", new TRecoLKrEvent, "Reco");
  AddParam("ClusterModuleSize", &fClusterModuleSize, 0); // not used
}

void L0CaloEmulator::FillTimes(){

  TRecoLKrEvent* event = GetEvent<TRecoLKrEvent>("Reco");
  Int_t nCand = event->GetNCandidates();
  FillHisto("nCandidates_Calo", nCand);

  fTimes.reserve(nCand);
  for(int i=0; i<nCand; ++i){
    TRecoLKrCandidate* cand = static_cast<TRecoLKrCandidate*>(event->GetCandidate(i));
    Double_t time         = cand->GetTime();
    if(GetWithMC()) time += fL0Reference;
    Double_t fulltime     = time+fEventTimeStamp;
    Double_t energy       = cand->GetClusterEnergy();
    FillHisto("hitTimes_Calo", time-fL0Reference);
    fTimes.push_back(EmulatedL0Primitive(kL0Calo, fulltime, energy));
  }
}

void L0CaloEmulator::Simple(){

  // make event cluster from hits consistent with L0 Reference Time
  HitVec::iterator hit = fTimes.begin();
  for(; hit != fTimes.end(); ++hit){
    Double_t relTime = hit->GetFirstTime() - fEventTimeStamp;
    // for L0Calo, sum over 3 windows of fL0Window
    if(fabs(relTime-fL0Reference)<(fL0Window*1.5)){
      if(fEventClusters.size())	fEventClusters[0].AddToCluster(hit);
      else                      fEventClusters.push_back(*hit);
    }
  }
}

void L0CaloEmulator::Detailed(){

  if(fDebug){
    std::cout << user_normal() << "[INFO] There is no detailed emulation of L0Calo." << std::endl;
    std::cout << user_normal() << "[INFO] Running Simple emulation instead." << std::endl;
  }
  Simple();
}

void L0CaloEmulator::SetPrimitiveIDs(ClusVec::iterator clustit){
  std::vector<Bool_t> quads;
  std::vector<Int_t> hits;
  clustit->GetHitInfo(quads, hits);

  Int_t         N = hits[0];
  Double_t energy = hits[5];

  clustit->SetPrimID("E1",  energy>1000.0);
  clustit->SetPrimID("E2",  energy>2000.0);
  clustit->SetPrimID("E5",  energy>5000.0);
  clustit->SetPrimID("E10", energy>10000.0);
  clustit->SetPrimID("E20", energy>20000.0);
  clustit->SetPrimID("E30", energy>30000.0);
  clustit->SetPrimID("E40", energy>40000.0);

  clustit->SetPrimID("C1", N==1 );
  clustit->SetPrimID("C2", N>1 );

  clustit->SetPrimID("C1E10", (N==1 && energy>10000.0) );
  clustit->SetPrimID("C2E10", (N>1  && energy>10000.0) );

  // axion triggers?
  clustit->SetPrimID("C2E1", (N>1  && energy>1000.0) );
  clustit->SetPrimID("C2E2", (N>1  && energy>2000.0) );

  // kaon mode triggers
  clustit->SetPrimID("LKr2", (energy>2000.0 && N>1) );
  clustit->SetPrimID("LKr20", energy>20000.0);
  clustit->SetPrimID("LKr30", (energy>30000.0 || (energy>5000.0 && N>1)) );

}

void L0CaloEmulator::GenerateAccidentals(){
  Int_t ngenerate = fRanGen->Poisson(fGenAccidentals);
  for(Int_t i=0; i<ngenerate; ++i){
    Double_t time     = fRanGen->Uniform(-fEventSize, fEventSize);
    Double_t fulltime = time + fEventTimeStamp;  // uniform time

    Double_t energy   = 0.0;
    if(fRanGen->Rndm()<0.5) energy = fRanGen->Uniform(300,600);
    else                    energy = fRanGen->Uniform(300,50000);

    fTimes.push_back(EmulatedL0Primitive(kL0Calo, fulltime, energy));
    FillHisto("hitTimes_Calo", time);
  }
}
