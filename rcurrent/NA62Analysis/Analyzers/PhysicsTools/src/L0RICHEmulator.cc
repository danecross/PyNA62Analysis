// ---------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2017-10-10
//
// ---------------------------------------------------------

/// \class L0RICHEmulator
/// \Brief
/// Emulator of the RICH L0 trigger-generating firmware. Inherits from VL0Emulator.
/// Returns std::vector<EmulatedL0Primitive>
/// \EndBrief
/// \Detailed
/// Emulator of the RICH L0 trigger-generating firmware. Inherits from VL0Emulator.
/// Returns std::vector<EmulatedL0Primitive>
///
/// The simple algorithm clusters hits within "L0Window" of the "EventReference"
/// A multiplicity cut is imposed based on "SLThreshold"
/// Remaining clusters are converted to EmulatedL0Primitive and are added to the output data.
///
/// The detailed algorithm converts each RICH hit in the event to a cluster.
/// The hits are separated into four containers, each corresponding to one PP of the Tel62.
/// The hits in each PP are separated into 25ns slots.
/// Hits in each PP are then clustered separately, according to "L0Window", reading each slot in time order.
/// Clusters that contain less than "PPThreshold" hits are deleted.
/// The cluster time is set to the average time, and are sorted
/// The clusters from each PP are clustered together.
/// Clusters that contain less than "SLThreshold" hits are deleted.
/// If more than one cluster falls within the same L0TP RAM segment, the earlier cluster is deleted.
/// Remaining clusters are converted to EmulatedL0Primitive and are added to the output data.
///
/// An example usage is available in K3piSelection:
/// \code
/// FillHisto("trigger/Emulated_L0_RICH_Den", BurstID); // efficiency denominator
/// ClusVec RICHEmulatedPrimitives =
///   *(std::vector<EmulatedL0Primitive>*)GetOutput("L0RICHEmulator.EmulatedL0RICHPrimitives");
/// ClusVec::iterator RICHPrim = RICHEmulatedPrimitives.begin();
/// for(; RICHPrim != RICHEmulatedPrimitives.end(); ++RICHPrim){
///   if(fabs(RICHPrim->GetAverageTime()-(RichTime*TdcCalib))<10.0 && RICHPrim->GetPrimID("RICH")){
///     FillHisto("trigger/Emulated_L0_RICH_Num", BurstID); // efficiency numerator
///     break; // avoid double-counting
///   }
/// }
/// \endcode
///
/// Control plots are also produced showing: the number of hits in the event;
/// the number of emulated primitives in the event; the time distribution of the hits;
/// the time distribution of the primitives; and the time-difference between consecutive
/// primitives.
///
/// \author Chris Parkinson (chris.parkinson@cern.ch)
/// \EndDetailed

#include "L0RICHEmulator.hh"
#include "TRecoRICHEvent.hh"

L0RICHEmulator::L0RICHEmulator(Core::BaseAnalysis *ba) : L0RICHEmulator(ba, "RICH"){
  fL0Detector = kL0RICH;
  RequestTree("RICH", new TRecoRICHEvent, "Reco");
  AddParam("PP0Fraction", &fFracPP[0], 0.271);
  AddParam("PP1Fraction", &fFracPP[1], 0.483);
  AddParam("PP2Fraction", &fFracPP[2], 0.751);
}

L0RICHEmulator::L0RICHEmulator(Core::BaseAnalysis *ba, TString DetectorName) : VL0Emulator(ba, DetectorName){
  AddParam("PPThreshold", &fPPThreshold, 2);
  AddParam("SLThreshold", &fSLThreshold, 2);
  AddParam("ClusterModuleSize", &fClusterModuleSize, 4);
}

void L0RICHEmulator::FillTimes(){

  TRecoRICHEvent* event = GetEvent<TRecoRICHEvent>("Reco");
  Int_t nHits = event->GetNHits();
  FillHisto("nCandidates_RICH", nHits);

  fTimes.reserve(nHits);
  for(int i=0; i<nHits; ++i){
    TRecoRICHHit* hit = static_cast<TRecoRICHHit*>(event->GetHit(i));
    if(!hit->GetOrSuperCellID()) continue; // skip PM hits
    Double_t time         = hit->GetTime();
    if(GetWithMC()) time += fL0Reference;
    Double_t fulltime     = time+fEventTimeStamp;
    Int_t pp = 2*hit->GetDiskID()+hit->GetUpDownDiskID();
    FillHisto("hitTimes_RICH", time-fL0Reference);
    fTimes.push_back(EmulatedL0Primitive(fL0Detector, fulltime, pp));
  }
}

void L0RICHEmulator::Simple(){

  // make event cluster from hits consistent with L0 Reference Time
  HitVec::iterator hit = fTimes.begin();
  for(; hit != fTimes.end(); ++hit){
    Double_t relTime = hit->GetFirstTime() - fEventTimeStamp;
    if(fabs(relTime-fL0Reference)<fL0Window){
      if(fEventClusters.size())	fEventClusters[0].AddToCluster(hit);
      else                      fEventClusters.push_back(*hit);
    }
  }
  // apply mult cut after SL stage
  MultiplicityCut(fEventClusters, fSLThreshold);
}

void L0RICHEmulator::Detailed(){
  // RICH specific building of emulated L0 primitives

  // separate hits by their PP.
  HitMap splitPP = SplitByPP(fTimes);

  // run cluster algo on each PP separately
  ClusVec Combined;
  for(int i=0; i<4; ++i){
    // output of each separate PP is added to the 'combined' list.
    ClusterAlgo(splitPP[i], Combined);
  }

  // apply mult cuts after PP stage
  MultiplicityCut(Combined, fPPThreshold);

  // Set cluster times as their average time
  SetTimesToAverage(Combined);

  // Sort the 'combined' vector in time
  SortAlgo(Combined);

  // run cluster algo on the combination.
  ClusterAlgo(Combined, fEventClusters);

  // apply mult cut after SL stage
  MultiplicityCut(fEventClusters, fSLThreshold);

  // account for overwriting of primitives in the L0TP
  L0TPOverwriting();
}

void L0RICHEmulator::ClusterAlgo(HitVec& hits, ClusVec& result){

  // local vector of clusters
  ClusVec Clusters;

  // separate frame into slots
  HitMap Splits = SplitSlots(hits);

  // run clustering algorithm on each split (slot)
  HitMap::iterator splitit = Splits.begin();
  for(; splitit != Splits.end(); ++splitit){

    // get pointer to last cluster in the event
    ClusVec::reverse_iterator lastCluster = Clusters.rbegin();

    // run over all hits and insert into cluster vector
    // a) in the last cluster of the previous split
    // b) add to the current split.
    ClusVec SplitClusters;
    HitVec::iterator hitit = (splitit->second).begin();
    for(; hitit != (splitit->second).end(); ++hitit){

      // check (a) last cluster of the previous split
      Bool_t Taken=false;
      if(lastCluster != Clusters.rend()){
  	Taken = lastCluster->Compare(hitit); // what to do here?
      }

      if(!Taken) AddHitToSplit(SplitClusters, hitit);
    } // loop on hits

    if(SplitClusters.size()>unsigned(fClusterModuleSize)){
      if(fDebug) std::cout << user_normal() << "[WARNING] More than "
			   << fClusterModuleSize << " clusters in this slot!" << std::endl;
      while(SplitClusters.size()>unsigned(fClusterModuleSize)) SplitClusters.pop_back();
    }

    // clusters in the split are sorted on readout.
    SortAlgo(SplitClusters);

    // insert split clusters into event clusters
    Clusters.insert(Clusters.end(),
		    SplitClusters.begin(),
		    SplitClusters.end());

  } // loop on splits

  // add produced clusters to result list (PP: Combined, SL: fEventClusters)
  result.insert(result.end(), Clusters.begin(), Clusters.end());
}

void L0RICHEmulator::AddHitToSplit(ClusVec& SplitClusters, HitVec::iterator hitit){
  // RICH firmware just adds new clusters at the end of the split.
  // They are sorted upon readout.

  // loop over existing clusters in the split
  // Two options for adding a hit:
  // (a) merge hit with existing clusters
  // (b) add new cluster at the end of the split
  //     (also to add the first hit to the split)

  ClusVec::iterator clustit = SplitClusters.begin();
  for(; clustit != SplitClusters.end(); ++clustit){
    // (a) merge into i'th cluster
    if( clustit->Compare(hitit) ) return;
  }
  // (b) add new cluster to the end of the split
  SplitClusters.push_back(*hitit);
}

void L0RICHEmulator::SetPrimitiveIDs(ClusVec::iterator clustit){
  // only one primitive ID (multiplicity cuts already applied)
  clustit->SetPrimID(fDetectorName, true);
}

void L0RICHEmulator::GenerateAccidentals(){
  Int_t ngenerate = fRanGen->Poisson(fGenAccidentals);
  for(Int_t i=0; i<ngenerate; ++i){
    Double_t time     = fRanGen->Uniform(-fEventSize, fEventSize);
    Double_t fulltime = time + fEventTimeStamp;  // uniform time
    Double_t rrr      = fRanGen->Rndm();
    Int_t pp=3;
    for(int q=0; q<3; ++q){
      if(rrr<fFracPP[q]){
	pp=q;
	break;
      }
    }
    fTimes.push_back(EmulatedL0Primitive(fL0Detector, fulltime, pp));
    FillHisto("hitTimes_"+fDetectorName, time);
  }
}

void L0RICHEmulator::MultiplicityCut(ClusVec& clusters, Int_t Threshold){
  for(unsigned int i=0; i<clusters.size(); ++i){
    if(clusters[i].GetNHits() < Threshold){
      clusters.erase(clusters.begin()+i); // delete this cluster
      i--;
    }
  }
}

void L0RICHEmulator::SetTimesToAverage(ClusVec& clusters){
  ClusVec::iterator cluster = clusters.begin() ;
  for(; cluster!=clusters.end(); ++cluster){
    cluster->SetTimeToAverageTime();
  }
}
