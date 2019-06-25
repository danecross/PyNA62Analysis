/// \class FilterOneTrackTwoClusters
/// \Brief
/// Filter for K+->pi+pi0 and K+->pi+gg decays
/// \EndBrief
/// \Detailed
/// \author Maria Brigida Brunetti (Maria.Brigida.Brunetti@cern.ch)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include "FilterOneTrackTwoClusters.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "GeometricAcceptance.hh"
#include "TriggerConditions.hh"
#include "DownstreamTrack.hh"
#include "EnergyCluster.hh"
#include "LAVMatching.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

FilterOneTrackTwoClusters::FilterOneTrackTwoClusters(Core::BaseAnalysis *ba) : Analyzer(ba, "FilterOneTrackTwoClusters")
{
  RequestAllMCTrees();
  RequestAllRecoTrees();
  fTrigger_nonMuon = TriggerConditions::GetInstance()->GetL0TriggerID("RICH-Q1-nMUV");
}

void FilterOneTrackTwoClusters::InitOutput(){
}

void FilterOneTrackTwoClusters::InitHist(){
}

void FilterOneTrackTwoClusters::DefineMCSimple(){
}

void FilterOneTrackTwoClusters::StartOfRunUser(){
}

void FilterOneTrackTwoClusters::StartOfBurstUser(){
}

void FilterOneTrackTwoClusters::Process(Int_t){

       L0TPData* L0Data = GetL0Data();
       Int_t runNumber = GetRunID();
       Bool_t nonMuonOn = TriggerConditions::GetInstance()->L0TriggerOn(runNumber, L0Data, fTrigger_nonMuon);
       Bool_t isControlTrigger = TriggerConditions::GetInstance()->IsControlTrigger(L0Data);

       //Ask control trigger or non-muon trigger
       if (!isControlTrigger && !nonMuonOn) return;
       //Ask at least 1 track
       std::vector<DownstreamTrack> Tracks = *GetOutput<std::vector<DownstreamTrack>>("DownstreamTrackBuilder.Output");
       if (!Tracks.size()) return;

       Bool_t atLeastOneTrackInFV=false;
       Bool_t atLeastOneTrackCDA=false;
       Bool_t atLeastOneTrackIn4Chambers=false;
       Bool_t atLeastOneTrackPosCharge=false;
       Bool_t atLeastOneTrackChi2=false;
       //Bool_t atLeastOneTrackNotMatchedToLAV=false;
       vector<Int_t> goodTracksIDs;
       UInt_t iTrack=0;

       TRecoLAVEvent* lavEvent = GetEvent<TRecoLAVEvent>();
       LAVMatching* lavMatching= *GetOutput<LAVMatching*>("PhotonVetoHandler.LAVMatching");
       Double_t refTime(-999);

       //Loop over all tracks and store the ID of "good" tracks
       while (iTrack<Tracks.size()){
         iTrack++;
         if (Tracks[iTrack-1].GetNominalBeamAxisVertex().Z()<100000.)   continue;
         if (Tracks[iTrack-1].GetNominalBeamAxisVertex().Z()>180000.)   continue;
         atLeastOneTrackInFV = true;
         if (Tracks[iTrack-1].GetNominalBeamAxisCDA()>100.)             continue;
         atLeastOneTrackCDA = true;
         if (Tracks[iTrack-1].GetNChambers()!=4)                        continue;
         atLeastOneTrackIn4Chambers = true;
         if (Tracks[iTrack-1].GetCharge()!=1)                           continue;
         atLeastOneTrackPosCharge = true;
         if (Tracks[iTrack-1].GetChi2()>40.0)                           continue;
         atLeastOneTrackChi2 = true;
         goodTracksIDs.push_back(iTrack-1);
       }

      //AT least 1 "good" track
      if (!atLeastOneTrackInFV) return;
      if (!atLeastOneTrackCDA) return;
      if (!atLeastOneTrackIn4Chambers) return;
      if (!atLeastOneTrackPosCharge) return;
      if (!atLeastOneTrackChi2) return;

      //Ask at least 2 clusters in LKr
      std::vector<EnergyCluster> Clusters =  *GetOutput<std::vector<EnergyCluster>>("EnergyClusterBuilder.Output");
      if(Clusters.size()<2) return;

      //At least 2 clusters not associated to any track
      TVector3 trackToClusterDistance(0,0,0);
      Double_t ClusterX(0), ClusterY(0), TrackX(0), TrackY(0);
      UInt_t iCluster(0);
      Int_t nClustersNotMatchedWithTrack(0);
      Bool_t clusterMatchedWithTrack;
      vector<Int_t> nonMatchedClustersIDs;
      while(iCluster<Clusters.size()){
        TRecoLKrCandidate* LKrCand = Clusters[iCluster].GetLKrCandidate();
        ClusterX = LKrCand->GetClusterX();
        ClusterY = LKrCand->GetClusterY();

        clusterMatchedWithTrack = false;
        for (UInt_t iTr=0; iTr<Tracks.size(); iTr++){
          TrackX = Tracks[iTr].xAtAfterMagnet(GeometricAcceptance::GetInstance()->GetZLKr());
          TrackY = Tracks[iTr].yAtAfterMagnet(GeometricAcceptance::GetInstance()->GetZLKr());
          trackToClusterDistance.SetXYZ(ClusterX-TrackX,ClusterY-TrackY,0);
          if (trackToClusterDistance.Mag()<50.) {
             clusterMatchedWithTrack=true;
            break;
          }
        }

        if(!clusterMatchedWithTrack) {
          nClustersNotMatchedWithTrack++;
          nonMatchedClustersIDs.push_back(iCluster);
        }
        iCluster++;
      }
      if (nClustersNotMatchedWithTrack<2) return;

      //Discard event if there is NO combination of 1 good track and 2 good clusters with total momentum between 60 GeV and 80 GeV, consistent in time, with no LAV matches
      Bool_t goodOneTrackTwoClustersCombination = false;
      Double_t cluster1En(0), cluster2En(0), cluster1X(0), cluster2X(0), cluster1Y(0), cluster2Y(0), cluster1Time(0), cluster2Time(0);
      TVector3 goodTrackVertex(0,0,0), goodTrackMomentum(0,0,0), cluster1Momentum(0,0,0), cluster2Momentum(0,0,0), Ptot(0,0,0);
      UInt_t iGoodTrack(0), iNonMatchedCluster1(0), iNonMatchedCluster2(0);
      Int_t goodTrackID(0), nonMatchedClusterID1(0), nonMatchedClusterID2(0);
      Double_t trackBestTime(-999), timeDiffTrackCluster1(0), timeDiffTrackCluster2(0), timeDiffClusters(0);

      //Loop over all good tracks
      Bool_t lavMatched(false);
      while (goodOneTrackTwoClustersCombination==false && iGoodTrack<goodTracksIDs.size()) {
        goodTrackID = goodTracksIDs.at(iGoodTrack);
        goodTrackVertex = Tracks[goodTrackID].GetNominalBeamAxisVertex();
        goodTrackMomentum = Tracks[goodTrackID].GetMomentumBeforeMagnet();
        //First loop on clusters to select first cluster in pair
        while (goodOneTrackTwoClustersCombination==false && iNonMatchedCluster1<nonMatchedClustersIDs.size()){
          nonMatchedClusterID1 = nonMatchedClustersIDs.at(iNonMatchedCluster1);
          TRecoLKrCandidate* LKrCand1 = Clusters[nonMatchedClusterID1].GetLKrCandidate();
          cluster1X = LKrCand1->GetClusterX();
          cluster1Y = LKrCand1->GetClusterY();
	  cluster1Time = LKrCand1->GetTime();
          cluster1En = LKrCand1->GetClusterEnergy();
          cluster1Momentum.SetX(cluster1X-goodTrackVertex.X());
          cluster1Momentum.SetY(cluster1Y-goodTrackVertex.Y());
          cluster1Momentum.SetZ(GeometricAcceptance::GetInstance()->GetZLKr()-goodTrackVertex.Z());
          cluster1Momentum.SetMag(cluster1En);
          iNonMatchedCluster2=iNonMatchedCluster1+1;
          //Second loop on clusters to select second cluster in pair
          while (goodOneTrackTwoClustersCombination==false && iNonMatchedCluster2>iNonMatchedCluster1 && iNonMatchedCluster2<nonMatchedClustersIDs.size()) {
            nonMatchedClusterID2 = nonMatchedClustersIDs.at(iNonMatchedCluster2);
            TRecoLKrCandidate* LKrCand2 = Clusters[nonMatchedClusterID2].GetLKrCandidate();
            cluster2X = LKrCand2->GetClusterX();
            cluster2Y = LKrCand2->GetClusterY();
	    cluster2Time = LKrCand2->GetTime();
            cluster2En = LKrCand2->GetClusterEnergy();
            cluster2Momentum.SetX(cluster2X-goodTrackVertex.X());
            cluster2Momentum.SetY(cluster2Y-goodTrackVertex.Y());
            cluster2Momentum.SetZ(GeometricAcceptance::GetInstance()->GetZLKr()-goodTrackVertex.Z());
            cluster2Momentum.SetMag(cluster2En);
            Ptot = cluster1Momentum+cluster2Momentum+goodTrackMomentum;
	    if(Tracks[goodTrackID].CHODAssociationExists()) trackBestTime = Tracks[goodTrackID].GetCHODTime();
	    else if (Tracks[goodTrackID].NewCHODAssociationExists()) trackBestTime = Tracks[goodTrackID].GetNewCHODTime();
	    if(Tracks[goodTrackID].CHODAssociationExists() || Tracks[goodTrackID].NewCHODAssociationExists()){
	      timeDiffTrackCluster1 = TMath::Abs(trackBestTime-cluster1Time);
	      timeDiffTrackCluster2 = TMath::Abs(trackBestTime-cluster2Time);
	    }
	    timeDiffClusters = TMath::Abs(LKrCand1->GetTime() - LKrCand2->GetTime());
            if(trackBestTime>-900) refTime = (trackBestTime+cluster1Time+cluster2Time)/3;
            else refTime = (cluster1Time+cluster2Time)/2;
            lavMatching->SetReferenceTime(refTime);
            lavMatched = lavMatching->LAVHasTimeMatching(lavEvent);

            if (Ptot.Mag()>60000 && Ptot.Mag()<80000 && timeDiffTrackCluster1<10 && timeDiffTrackCluster2<10 && timeDiffClusters<10 && !lavMatched) {
              goodOneTrackTwoClustersCombination=true;
              break;
            }
            iNonMatchedCluster2++;
          }
          iNonMatchedCluster1++;
          iNonMatchedCluster2=0;
        }
        iGoodTrack++;
        iNonMatchedCluster1=0;
      }
      if(!goodOneTrackTwoClustersCombination) return;
      FilterAccept();
}

void FilterOneTrackTwoClusters::PostProcess(){
}

void FilterOneTrackTwoClusters::EndOfBurstUser(){
}

void FilterOneTrackTwoClusters::EndOfRunUser(){
}

void FilterOneTrackTwoClusters::DrawPlot(){
}
