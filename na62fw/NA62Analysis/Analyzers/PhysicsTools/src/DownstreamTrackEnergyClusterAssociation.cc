// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-12-14
//
// ---------------------------------------------------------

/// \class DownstreamTrackEnergyClusterAssociation
/// \Brief
/// Associate DownstreamTracks to EnergyClusters
/// \EndBrief
/// \Detailed
/// The output of EnergyClusterBuilder is updated with the information of associated DownstreamTracks to each EnergyCluster.
/// Analogously, the output of DownstreamTrackBuilder is updated with the information of associated EnergyClusters to each DownstreamTrack.
/// \author Karim Massri (karim.massri@cern.ch)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "DownstreamTrackEnergyClusterAssociation.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "DownstreamTrack.hh"
#include "EnergyCluster.hh"

using namespace NA62Analysis;
using namespace NA62Constants;

DownstreamTrackEnergyClusterAssociation::DownstreamTrackEnergyClusterAssociation(Core::BaseAnalysis *ba) : Analyzer(ba, "DownstreamTrackEnergyClusterAssociation"){}

void DownstreamTrackEnergyClusterAssociation::InitOutput(){}

void DownstreamTrackEnergyClusterAssociation::InitHist(){}

void DownstreamTrackEnergyClusterAssociation::DefineMCSimple(){}

void DownstreamTrackEnergyClusterAssociation::StartOfRunUser(){}

void DownstreamTrackEnergyClusterAssociation::StartOfBurstUser(){}

void DownstreamTrackEnergyClusterAssociation::Process(Int_t){

  std::vector<DownstreamTrack> DownstreamTracks =
    *(std::vector<DownstreamTrack>*)GetOutput("DownstreamTrackBuilder.Output");
  std::vector<EnergyCluster> EnergyClusters =
    *(std::vector<EnergyCluster>*)GetOutput("EnergyClusterBuilder.Output");

  // Add tracks info to EnergyClusters
  for(UInt_t iTrack=0;iTrack<DownstreamTracks.size();iTrack++){
    if(DownstreamTracks[iTrack].GetLKrAssociationOutput().GetBestAssociationRecord()) {
      UInt_t ClusterID = DownstreamTracks[iTrack].GetLKrAssociationOutput().GetBestAssociationRecord()->GetClusterID();
      EnergyClusters[ClusterID].AddTrackForWhichThisClusterIsBestMatch(&(*(DownstreamTracks.begin()+iTrack)));
    }
  }

  //Add clusters info to DownstreamTracks
  for(UInt_t iCluster=0;iCluster<EnergyClusters.size();iCluster++){
    if(EnergyClusters[iCluster].GetSpectrometerAssociation().GetBestAssociationRecord()) {
      UInt_t TrackID = EnergyClusters[iCluster].GetSpectrometerAssociation().GetBestAssociationRecord()->GetTrackID();
      DownstreamTracks[TrackID].AddClusterForWhichThisTrackIsBestMatch(&(*(EnergyClusters.begin()+iCluster)));
    }
  }
}

void DownstreamTrackEnergyClusterAssociation::PostProcess(){

}

void DownstreamTrackEnergyClusterAssociation::EndOfBurstUser(){
}

void DownstreamTrackEnergyClusterAssociation::EndOfRunUser(){

}

void DownstreamTrackEnergyClusterAssociation::EndOfJobUser(){
}

void DownstreamTrackEnergyClusterAssociation::DrawPlot(){
}

DownstreamTrackEnergyClusterAssociation::~DownstreamTrackEnergyClusterAssociation(){
}
