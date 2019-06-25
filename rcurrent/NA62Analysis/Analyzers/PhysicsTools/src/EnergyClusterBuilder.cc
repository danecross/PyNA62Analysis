// ---------------------------------------------------------
// History:
//
// Updated by Zuzana Kucerova (zuzana.kucerova@cern.ch) 2019-05-15
//  - updated electromagnetic cuts, added isolation
// Created by Karim Massri (karim.massri@cern.ch) 2016-11-24
//
// ---------------------------------------------------------

/// \class EnergyClusterBuilder
/// \Brief
/// Build a EnergyCluster container for each LKr cluster
/// \EndBrief
/// \Detailed
/// For each cluster, a EnergyCluster container is filled. The container includes the information
/// about the association of the cluster with candidates in other subdetectors (Spectrometer),
/// including the pointers to the relevant candidates in subdetectors.
/// A vector of EnergyCluster containers output by the algorithm
/// can be accessed in an analyzer in the following way.
/// \code
/// std::vector<EnergyCluster> Clusters =
///   *(std::vector<EnergyCluster>*)GetOutput("EnergyClusterBuilder.Output");
/// \endcode
/// \author Karim Massri (karim.massri@cern.ch)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include "EnergyClusterBuilder.hh"
#include "GeometricAcceptance.hh"
#include "Event.hh"
#include "Persistency.hh"

#include "LKrSpectrometerAssociationOutput.hh"

using namespace NA62Analysis;
using namespace NA62Constants;

EnergyClusterBuilder::EnergyClusterBuilder(Core::BaseAnalysis *ba) : Analyzer(ba, "EnergyClusterBuilder") {
  RequestTree("LKr",          new TRecoLKrEvent,     "Reco");

  // parameters of 2D electromagnetic cut: (fCutK1*E + fCutQ1) < N(cells) < (fCutK2*E + fCutQ2)
  AddParam("CutK1", "double", &fCutK1, 0.001);
  AddParam("CutQ1", "double", &fCutQ1, 0.);
  AddParam("CutK2", "double", &fCutK2, 0.0015);
  AddParam("CutQ2", "double", &fCutQ2, 10.);
  AddParam("CutSR1", "double", &fCutSR1, 0.); 
  AddParam("CutSR2", "double", &fCutSR2, 10000.); 

  // parameters of isolation
  AddParam("CutMinDist", "double", &fCutMinDist, 150.);
  AddParam("CutMaxTimeDiff", "double", &fCutMaxTimeDiff, 7.);

  fLKrEvent = nullptr;
}

EnergyClusterBuilder::~EnergyClusterBuilder() {
}

void EnergyClusterBuilder::InitHist() {
    BookHisto("hNCellsVsClusterEnergy",
              new TH2F("NCellsVsClusterEnergy",
                       "NCells vs Cluster Energy; E_{cluster} [MeV]; N cells",
                       1000, 0., 100000., 150, 0, 150));
    BookHisto("hNCellsVsClusterEnergy_EM",
              new TH2F("NCellsVsClusterEnergy_EM",
                       "N Cells vs Cluster Energy for EM clusters; E_{cluster} [MeV]; N cells",
                       1000, 0., 100000., 150, 0, 150));
    BookHisto("hCellRatioVsSeedRatio", 
	      new TH2F("hCellRatioVsSeedRatio", "(N Cells)/(Cluster Energy) vs (Seed Energy)/(Cluster Energy); (Seed Energy)/(Cluster Energy); (N Cells)/(Cluster Energy)",
		       100, 0., 2., 1000, 0., 10.));
}

void EnergyClusterBuilder::InitOutput() {
  RegisterOutput("Output", &fContainer);
}

void EnergyClusterBuilder::StartOfRunUser() {
}

void EnergyClusterBuilder::Process(Int_t) {
  SetOutputState("Output", kOValid);
  fContainer.clear();

  if (!GetIsTree()) return;

  fLKrEvent  = GetEvent<TRecoLKrEvent>();

  std::vector<LKrSpectrometerAssociationOutput> LKrSpec =
    *GetOutput<std::vector<LKrSpectrometerAssociationOutput>>("LKrSpectrometerAssociation.Output");

  for (Int_t iCluster=0; iCluster<fLKrEvent->GetNCandidates(); iCluster++) {
    TRecoLKrCandidate* LKrCand = static_cast<TRecoLKrCandidate*>(fLKrEvent->GetCandidate(iCluster));

    EnergyCluster Cluster(iCluster,LKrCand);
    Cluster.SetSpectrometerAssociation(LKrSpec[iCluster]);
    Cluster.SetIsElectromagnetic(IsClusterElectromagnetic(LKrCand));
    Cluster.SetIsIsolated(IsClusterIsolated(LKrCand, iCluster));
    Cluster.SetIsInLKrAcceptance(IsInLKrAcceptance(LKrCand));
    fContainer.push_back(Cluster);
  }
}

void EnergyClusterBuilder::EndOfJobUser() {
  SaveAllPlots();
}

Bool_t EnergyClusterBuilder::IsClusterElectromagnetic(TRecoLKrCandidate* LKrCand) {
  double E = LKrCand->GetClusterEnergy();
  int NC = LKrCand->GetNCells();
  FillHisto("hNCellsVsClusterEnergy", E, NC);
  double seed = LKrCand->GetClusterSeedEnergy();
  double seedratio = seed/E;
  FillHisto("hCellRatioVsSeedRatio", seedratio, (NC-2.21)/(0.93*E/1000.));
  if ((NC > (fCutK1*E + fCutQ1)) && (NC < (fCutK2*E + fCutQ2)) && (seedratio > fCutSR1) && (seedratio < fCutSR2)) {
    FillHisto("hNCellsVsClusterEnergy_EM", E, NC);
    return true;
  } else {
    return false;
  }
}

Bool_t EnergyClusterBuilder::IsClusterIsolated(TRecoLKrCandidate* LKrCand, int ID) {
  TVector2 posCluster(LKrCand->GetClusterX(), LKrCand->GetClusterY());
  bool isIsol = true;
  for(int i=0; i<fLKrEvent->GetNCandidates(); i++){
    if(i==ID) continue;
    TRecoLKrCandidate *TestC = static_cast<TRecoLKrCandidate*>(fLKrEvent->GetCandidate(i));
    if(fabs(LKrCand->GetTime() - TestC->GetTime())>fCutMaxTimeDiff) continue;
    TVector2 posTestCluster(TestC->GetClusterX(), TestC->GetClusterY());
    if((posCluster - posTestCluster).Mod()<fCutMinDist){
      isIsol = false;
      break;
    }
  }

  return isIsol;
}

Bool_t EnergyClusterBuilder::IsInLKrAcceptance(TRecoLKrCandidate* LKrCand){
  Bool_t is = false;
  is = GeometricAcceptance::GetInstance()->InAcceptance(LKrCand->GetClusterX(), LKrCand->GetClusterY(), kLKr);
  return is;
}
