// ---------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2017-10-10
//
// ---------------------------------------------------------

#include "L0LAVEmulator.hh"
#include "VL0Emulator.hh"
#include "TRecoLAVEvent.hh"

L0LAVEmulator::L0LAVEmulator(Core::BaseAnalysis *ba) : VL0Emulator(ba, "LAV"){
  fL0Detector = kL0LAV;
  RequestTree("LAV", new TRecoLAVEvent, "Reco");

  AddParam("ClusterModuleSize", &fClusterModuleSize, 32);
}

void L0LAVEmulator::FillTimes(){

  TRecoLAVEvent* event = GetEvent<TRecoLAVEvent>("Reco");
  Int_t nHits = event->GetNHits();
  FillHisto("nCandidates_LAV", nHits);

  fTimes.reserve(nHits);
  for(int i=0; i<nHits; ++i){
    TRecoLAVHit* hit  = static_cast<TRecoLAVHit*>(event->GetHit(i));

    // Only LAV12
    Int_t StationID = hit->GetLAVID();
    if(StationID!=12) continue;

    // L0 edgemask requirement
    Bool_t LL = hit->GetEdgeMask() & 0x1;
    Bool_t LH = hit->GetEdgeMask() & 0x2;
    if(!(LL&&LH)) continue;

    // LAV: make online slewing correction
    Double_t LEL = hit->GetLeadingEdgeLow();
    Double_t LEH = hit->GetLeadingEdgeHigh();
    Double_t VH = 15.0;
    Double_t VL =  5.0;
    Double_t time = LEL - ((LEH-LEL)*VH)/(VH-VL);
    if(GetWithMC()) time += fL0Reference;
    Double_t fulltime = time+fEventTimeStamp;
    FillHisto("hitTimes_LAV", time-fL0Reference);
    fTimes.push_back(EmulatedL0Primitive(kL0LAV, fulltime));
  }
}

void L0LAVEmulator::Simple(){

  // make event cluster from hits consistent with L0 Reference Time
  HitVec::iterator hit = fTimes.begin();
  for(; hit != fTimes.end(); ++hit){
    Double_t relTime = hit->GetFirstTime() - fEventTimeStamp;
    if(fabs(relTime-fL0Reference)<fL0Window){
      if(fEventClusters.size())	fEventClusters[0].AddToCluster(hit);
      else                      fEventClusters.push_back(*hit);
    }
  }
}

void L0LAVEmulator::Detailed(){
  // LAV specific building of emulated L0 primitives

  HitMap Frames = SplitFrames(fTimes);
  HitMap::iterator frameit = Frames.begin();
  for(; frameit != Frames.end(); ++frameit){

    // run clustering algorithm on each frame
    ClusVec FrameClusters;

    // shuffle hits within each frame
    ShuffleAlgo(frameit->second);

    // get pointer to last cluster in the event
    ClusVec::reverse_iterator lastCluster = fEventClusters.rbegin();

    // run over all hits and insert into cluster vector
    // a) in the last cluster of the previous split
    // b) add to the current split
    HitVec::iterator hitit = (frameit->second).begin();
    for(; hitit != (frameit->second).end(); ++hitit){
      // check (a) last cluster of the previous frame
      Bool_t Taken=false;
      if(lastCluster != fEventClusters.rend()){
	Taken = lastCluster->Compare(hitit);
      }
      // check (b) add to current split
      if(!Taken) AddHitToSplit(FrameClusters, hitit);
    } // loop on hits

    // limit size of frame clusters to 32 units.
    // Unfortunately the limit is at 32 cells per FRAME,
    // but only (up to) 200ns of each frame is recorded in the event.
    // Given the information available we cannot know how many clusters are in the module
    // at the start of this event, so cannot emulate how this peice of firmware would behave.
    // This emulator assumes that the frame was empty of clusters until this event.
    if(FrameClusters.size()>unsigned(fClusterModuleSize)){
      if(fDebug) std::cout << user_normal() << "[WARNING] More than "
                           << fClusterModuleSize << " clusters in this frame!" << std::endl;
      while(FrameClusters.size()>unsigned(fClusterModuleSize)) FrameClusters.pop_back();
    }

    // sort remaining 32 clusters
    SortAlgo(FrameClusters);

    // insert frame clusters into frame clusters
    fEventClusters.insert(fEventClusters.end(),
              FrameClusters.begin(),
              FrameClusters.end());

  } // loop on frames

  // Merge primitives within 25ns
  LAVPrimitiveMerging(fEventClusters);
}

void L0LAVEmulator::AddHitToSplit(ClusVec& SplitClusters, HitVec::iterator hitit){
  SplitClusters.push_back(*hitit);
}

void L0LAVEmulator::SetPrimitiveIDs(ClusVec::iterator clustit){
  Int_t N=clustit->GetNHits();
  clustit->SetPrimID("LAV", (N>2));
}

void L0LAVEmulator::LAVPrimitiveMerging(ClusVec& clusters){

  Long64_t lastSegment = -999;

  for(unsigned int i=0; i<clusters.size(); ++i){
    Double_t ft = clusters[i].GetAverageTime();
    Long64_t segment = ft/ClockPeriod; // same as time stamp

    if(!(lastSegment<0)){
      if(segment == lastSegment){
    // There can be only one. Merge clusters. Pick the largest, sum the hits
    int j= i-1;
    if(fDebug) std::cout << user_normal() << "[INFO] Primitive " << i << " (segment " << segment
                 << ") merges with primitive " << j
                 << " (segment " << lastSegment << ")." << std::endl;
    clusters[i].SetNHits(clusters[i].GetNHits() + clusters[j].GetNHits());
    clusters.erase(clusters.begin()+j); // delete previous cluster
    i--;
      }
    }
    lastSegment = segment;
  }
}

void L0LAVEmulator::GenerateAccidentals(){
  Int_t ngenerate = fRanGen->Poisson(fGenAccidentals);
  for(Int_t i=0; i<ngenerate; ++i){
    Double_t time     = fRanGen->Uniform(-fEventSize, fEventSize);
    Double_t fulltime = time + fEventTimeStamp;  // uniform time
    fTimes.push_back(EmulatedL0Primitive(kL0LAV, fulltime));
    FillHisto("hitTimes_LAV", time);
  }
}
