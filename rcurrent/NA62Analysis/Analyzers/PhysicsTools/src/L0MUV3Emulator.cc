// ---------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2017-10-10
//
// ---------------------------------------------------------

/// \class L0MUV3Emulator
/// \Brief
/// Emulator of the MUV3 L0 trigger-generating firmware. Inherits from VL0Emulator.
/// Returns std::vector<EmulatedL0Primitive> with size 1
/// \EndBrief
/// \Detailed
/// Emulator of the MUV3 L0 trigger-generating firmware. Inherits from VL0Emulator.
/// Returns std::vector<EmulatedL0Primitive> with any size
///
/// The simple algorithm builds a cluster of candidates within "L0Window" of the "EventReference"
/// The cluster is converted to an EmulatedL0Primitive.
///
/// The detailed algorithm converts each MUV3 candidate in the event to a cluster.
/// The candidates are separated into frames, and each frame is separated into 64 100ns 'splits'
/// The candidates are clustered in each split via a "Distributor" cluster. As each candidate
/// is read, it either merges with the distributor, is added directly to a vector of clusters,
/// or replaces the distributor cluster. In the latter case, the distributor cluster is added
/// to the vector of clusters. Each time a cluster is added to the vector of clusters, it
/// compares itself against each existing cluster, based on the time of the clusters.
/// The incoming cluster can either merge with one of the existing clusters, will be added
/// to the end of the vector, or it will replace one of the existing clusters. In the latter
/// case, all the existing clusters will be moved one position along the vector.
/// The order of the clusters is such that the first element in the vector has the largest time.
/// The cluster order is reversed, to put it in chronological order, and added to the vector
/// of clusters which is eventually returned to the user.
/// If more than one cluster falls within the same L0TP RAM segment, the earlier cluster is deleted.
/// Remaining clusters are converted to EmulatedL0Primitive and are added to the output data.
///
/// An example usage (of the simple algorithm) is available in K3piSelection.
/// The detailed algorithm can be used as follows
/// \code
/// ClusVec MUV3EmulatedPrimitives =
///   *(std::vector<EmulatedL0Primitive>*)GetOutput("L0MUV3Emulator.EmulatedL0MUV3Primitives");
/// ClusVec::iterator MUV3Prim = MUV3EmulatedPrimitives.begin();
/// for(; MUV3Prim != MUV3EmulatedPrimitives.end(); ++MUV3Prim){
///   if(fabs(MUV3Prim->GetAverageTime()-(RichTime*TdcCalib))<10.0 && MUV3Prim->GetPrimID("MUV3")){
///     // MUV3 trigger is efficient
///   }
/// }
/// \endcode
///
/// Control plots are also produced showing: the number of hits in the event;
/// the number of emulated primitives in the event; the time distribution of the hits;
/// the time distribution of the primitives; and the time-difference between consecutive
/// primitives.
///
/// To generate accidentals for MC samples, the number of hits generated within a 200ns window
/// (i.e. +/- 100ns around the trigger time) is equal to the input parameter "GenerateGTKIntensity".
///
/// \author Chris Parkinson (chris.parkinson@cern.ch)
/// \EndDetailed

#include "L0MUV3Emulator.hh"
#include "TRecoMUV3Event.hh"
#include "EventHeader.hh"

L0MUV3Emulator::L0MUV3Emulator(Core::BaseAnalysis *ba) : L0MUV3Emulator(ba, "MUV3"){
  fL0Detector = kL0MUV3;
  RequestTree("MUV3", new TRecoMUV3Event, "Reco");
  AddParam("InnerFraction",     &fFracInner, 0.373);
  AddParam("TightFraction",     &fFracTight, 0.846);
  AddParam("Quad0Fraction",     &fFracQuads[0], 0.262);
  AddParam("Quad1Fraction",     &fFracQuads[1], 0.498);
  AddParam("Quad2Fraction",     &fFracQuads[2], 0.749);
}

L0MUV3Emulator::L0MUV3Emulator(Core::BaseAnalysis *ba, TString DetectorName) :
  VL0Emulator(ba, DetectorName), fAccidentals(nullptr) {
  AddParam("ClusterModuleSize", &fClusterModuleSize, 8);
}

L0MUV3Emulator::~L0MUV3Emulator(){
  // don't destroy anything
}

void L0MUV3Emulator::FillTimes(){

  TRecoMUV3Event* event = GetEvent<TRecoMUV3Event>("Reco");
  Int_t nCand = event->GetNCandidates();
  FillHisto("nCandidates_MUV3", nCand);

  fTimes.reserve(nCand);
  for(int i=0; i<nCand; ++i){
    TRecoMUV3Candidate* cand = static_cast<TRecoMUV3Candidate*>(event->GetCandidate(i));
    Double_t   time       = cand->GetTimeNoT0();
    if(GetWithMC()) time += fL0Reference;
    Double_t fulltime     = time+fEventTimeStamp;
    Bool_t tight          = cand->IsTight();
    Bool_t inner          = cand->IsInner();
    TVector3 pos          = cand->GetPosition();
    Int_t quad=-1;
    if(!inner){
      if(pos.X()>0){
	if(pos.Y()>0) quad=0;
	else          quad=1;
      }
      else{
	if(pos.Y()>0) quad=2;
	else          quad=3;
      }
    }
    FillHisto("hitTimes_MUV3", time-fL0Reference);
    fTimes.push_back(EmulatedL0Primitive(kL0MUV3, fulltime, tight, inner, quad));
  }
}

void L0MUV3Emulator::Simple(){

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


void L0MUV3Emulator::Detailed(){
  // MUV3 specific building of emulated L0 primitives

  ClusVec Dummy;

  HitMap Frames = SplitFrames(fTimes);
  HitMap::iterator frameit = Frames.begin();
  for(; frameit != Frames.end(); ++frameit){

    // new distributor for each frame.
    HitVec::iterator Distributor = Dummy.end();

    // separate frame into splits
    HitMap Splits = Split100(frameit->second);

    // run clustering algorithm on each split
    // starts on the split that fired the trigger
    Int_t DistributorSplitTime = (GetEventHeader()->GetTimeStamp()/4);
    DistributorSplitTime -= 2; // the split before the first one in the event
    DistributorSplitTime *= 4;
    // ** This might not work with hits generated using the internal hit generator!

    ClusVec FrameClusters;
    HitMap::iterator splitit = Splits.begin();
    for(; splitit != Splits.end(); ++splitit){

      // shuffle hits within each split
      ShuffleAlgo(splitit->second);

      // clear or limit number of hits in the split
      // SplitLimit assumes 32-deep split fifo in 2018 (fRunPeriod=2)
      if(fRunPeriod<2) SplitClear(splitit);
      else             SplitLimit(splitit);

      // The distributor split is always 'end' which means it's always different to the current split,
      // causing the underflow bug to appear.
      // Unless the split being looked at is the first split in the frame!
      // In this case there is no distributor cluster, so the bug cannot appear, the first hit immediately
      // becomes the distributor cluster and the underflow protection is there.
      // Note that the second split could also be okay if there was no hit in the first split, however,
      // in 4.5M splits only 1k had no hits -- meaning something like 0.02% percent.

      Int_t splitTime = (splitit->first)*4;
      Int_t frameTime = (splitTime/64)*64; // frame time is split time truncated to 64. Also (frameit->first)*256;

      // run over all hits and insert into cluster vector
      ClusVec SplitClusters;
      HitVec::iterator hitit = (splitit->second).begin();
      for(; hitit != (splitit->second).end(); ++hitit){

	// New bug.
	// If this split is the one after the distro cluster,
	// and the hit is not included in the distributor cluster, so becomes
	// the new distributor cluster, then make the 'underflow' version of
	// the distributor calculation (bug=true).

	Bool_t underflow = true;
	underflow &= (fRunPeriod<3); // no bug in run period 3
	underflow &= (splitTime == (DistributorSplitTime+4)); // no bug when a split is skipped.
	underflow &= (splitTime != frameTime); // no bug in first split in the frame

	Bool_t Taken=false;

	if(Distributor!=Dummy.end()){
	  //compare hits to the distributor.
	  Taken = Distributor->Compare(hitit);
	  if(Taken) continue; // hit added to distributor
	  // Note that split time is not updated when hit is added to distributor

	  // hit not added to distributor.
	  // is it earlier or later?
	  // C.F. line 386 of sl_cluster_distributor. Note that in the firmare a hit from the next
	  // split only checks the upper edge of the distributor cluster, while above both edges are checked.
	  // There is no effect, though,
	  // because a cluster affected by the distributor bug must be very far from these hits

	  // since it is at the start of the previous split.
	  if(hitit->GetTime()<Distributor->GetFirstTime()){
	    // hit is earlier! Add it to the split clusters.

	    // Drop quadrants in 2016!
	    if(fRunPeriod==0){
	      if(fDebug) std::cout << user_normal() << "[INFO] Clearing quadrant info!" << std::endl;
	      hitit->DropQuadrants();
	    }

	    // add hit to split clusters.
	    AddHitToSplit(SplitClusters, hitit);
	  }
	  else{
	    // hit is later. Hit becomes the new distributor!
	    // Add the distributor to the split clusters.

	    // Underflow bug! Recompute cluster edges.
	    if(underflow) hitit->SetEdges(fL0Window, true, splitTime*ClockPeriod);

	    // Adding distributor to split clusters.
	    // Last cluster from split N will be added at the start of split N+1
	    // This should have no impact on the emulator results.
	    AddHitToSplit(SplitClusters, Distributor);
	    Distributor = hitit;
	    DistributorSplitTime = splitTime;
	  }

	}
	else{
	  // Underflow bug! Recompute cluster edges.
	  if(underflow) hitit->SetEdges(fL0Window, true, splitTime*ClockPeriod);

	  // Distributor does not exist. Hit is first hit in frame (or first hit in the event!!)
	  Distributor = hitit; // set distributor as first hit.
	  DistributorSplitTime = splitTime;
	}

      } // loop on hits

      // resize split to contain max number of clusters
      if(SplitClusters.size()>unsigned(fClusterModuleSize)){
	if(fDebug) std::cout << user_normal() << "[WARNING] More than "
			     << fClusterModuleSize << " clusters in this split!" << std::endl;
	while(SplitClusters.size()>unsigned(fClusterModuleSize)) SplitClusters.pop_back();
      }

      // reverse clusters in split to read in chronological order
      std::reverse(SplitClusters.begin(), SplitClusters.end());

      // insert split clusters into frame clusters
      FrameClusters.insert(FrameClusters.end(),
			   SplitClusters.begin(),
			   SplitClusters.end());
    } // loop on splits

    // end of frame. add the distributor to the frame clusters.
    if(Distributor!=Dummy.end()) FrameClusters.push_back(*Distributor);

    // insert frame clusters into frame clusters
    fEventClusters.insert(fEventClusters.end(),
			  FrameClusters.begin(),
			  FrameClusters.end());

  } // loop on frames
}

void L0MUV3Emulator::AddHitToSplit(ClusVec& SplitClusters, HitVec::iterator hitit){

  // MUV3 firmware inserts new clusters
  // in order, sorting them on-the-fly.
  // The cluster with the largest time
  // is stored in element 0 and is read
  // out last.

  // loop over existing clusters in the split
  // Three options for adding a hit:
  // (a) merge hit with existing clusters
  // (b) insert new cluster into the split
  // (c) add new cluster at the end of the split
  //     (also to add the first hit to the split)

  ClusVec::iterator clustit = SplitClusters.begin();
  for(; clustit != SplitClusters.end(); ++clustit){

    // (a) merge into i'th cluster
    if( clustit->Compare(hitit) ) return;

    // (b) insert new cluster into the split
    if(hitit->GetTime()>=clustit->GetFirstTime()+fL0Window){ // C.F. line 296 of sl_cluster_sorter_cell
      // Adds hitit in the position BEFORE clustit!
      // ... to achieve reverse-time-ordered clusters,  as they are in the real firmware
      InsertAlgo(SplitClusters, clustit, hitit);
      return;
    }
  }
  // (c) add new cluster to the end of the split
  SplitClusters.push_back(*hitit);
}

void L0MUV3Emulator::SetPrimitiveIDs(ClusVec::iterator clustit){
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

  Bool_t E1 = ( quads[0] && !quads[1] && !quads[2] && !quads[3]);
  Bool_t E2 = (!quads[0] && !quads[1] && !quads[2] &&  quads[3]);
  Bool_t E3 = (!quads[0] && !quads[1] &&  quads[2] && !quads[3]);
  Bool_t E4 = (!quads[0] &&  quads[1] && !quads[2] && !quads[3]);

  clustit->SetPrimID("MUV",  (N>0));
  clustit->SetPrimID("MUV3", (N>0));

  clustit->SetPrimID("M2", (N>1));
  clustit->SetPrimID("MO2", (NO>1));
  clustit->SetPrimID("M1", (N>0));
  clustit->SetPrimID("MO1", (NO>0));

  clustit->SetPrimID("MM2",  (NT==1 && NL>0));
  clustit->SetPrimID("MT2",  (NT>1));
  clustit->SetPrimID("ML2",  (NL>1));

  clustit->SetPrimID("MMO2", (TO==1 && LO>0));
  clustit->SetPrimID("MTO2", (TO>1));
  clustit->SetPrimID("MLO2", (LO>1));

  clustit->SetPrimID("MTO1", (TO>0));
  clustit->SetPrimID("MLO1", (LO>0));
  clustit->SetPrimID("MT1",  (NT>0));
  clustit->SetPrimID("ML1",  (NL>0));

  clustit->SetPrimID("MQE1", E1);
  clustit->SetPrimID("MQE2", E2);
  clustit->SetPrimID("MQE3", E3);
  clustit->SetPrimID("MQE4", E4);

  clustit->SetPrimID("MQ2",  (NQ>1));
  clustit->SetPrimID("MQX",   QX);
  clustit->SetPrimID("MOQ2", (NQ>1));
  clustit->SetPrimID("MOQX",  QX);
  clustit->SetPrimID("MTB",   TB);
}

void L0MUV3Emulator::GenerateAccidentals(){
  Int_t ngenerate = fRanGen->Poisson(fGenAccidentals);
  for(Int_t i=0; i<ngenerate; ++i){
    Double_t time     = fRanGen->Uniform(-fEventSize, fEventSize);
    Double_t fulltime = time + fEventTimeStamp;  // uniform time
    Bool_t   tight    = (fRanGen->Rndm()<fFracTight);  // 95% tight
    Bool_t   inner    = (fRanGen->Rndm()<fFracInner);  // 10% inner
    Double_t rrr      =  fRanGen->Rndm(); // for quadrants
    Int_t quad=-1; // no quadrant for inner hits
    if(!inner){
      quad=3;
      for(int q=0; q<3; ++q){
	if(rrr<fFracQuads[q]){
	  quad=q;
	  break;
	}
      }
    }
    fTimes.push_back(EmulatedL0Primitive(fL0Detector, fulltime, tight, inner, quad));
    FillHisto("hitTimes_"+fDetectorName, time);
  }
}
