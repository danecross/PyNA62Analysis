// ---------------------------------------------------------------
//
// History:
//
// Updated by Zuzana Kucerova 2018-03-23
// - GTK and KTAG candidate quality cuts
// Updated by Michal Koval (michal.koval@cern.ch) 2018-02-12
// Updated by Zuzana Kucerova (zuzana.kucerova@cern.ch) 2017-10-17
// - Introduce Pi0Selection without using STRAW spectrometer
// Created by Karim Massri (karim.massri@cern.ch) 2016-11-15
//
// ---------------------------------------------------------------

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "Pi0Selection.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "GeometricAcceptance.hh"
#include "EnergyCluster.hh"
#include "BlueTubeTracker.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class Pi0Selection
/// \Brief
/// Pi0->gg selection in LKr.
/// \EndBrief
/// \Detailed
/// Pi0->gg selection in LKr.
///
/// The analyzer looks for pairs of electromagnetic clusters in the LKr forming
/// a neutral vertex inside the fiducial decay region. The neutral vertex can
/// be reconstructed with or without the use of GTK information, the option is
/// controlled by the parameter fUseGTK (=false by default).
///
/// The analyzer has two outputs:
/// EventSelected = true if at least one good pi0 candidate was found
/// SelectedPi0 = vector of Pi0SelectionOutput (struct defined in Pi0Selection.hh)
///
/// An example of requesting the output with exactly one pi0 selected (see more in K2piSelection):
/// \code
///   std::vector<Pi0SelectionOutput> pi0Selected =
///    *(std::vector<Pi0SelectionOutput>*)GetOutput("Pi0Selection.SelectedPi0");
///   if (pi0Selected.size() != 1) return;
///   Pi0SelectionOutput pi0 = pi0Selected.at(0);
/// \endcode
/// \EndDetailed

TF1 Pi0Selection::fVertexZFunction("fVertexZ", &Pi0Selection::VertexZFunctionFormula, 0., 1000000., 14);

Pi0Selection::Pi0Selection(Core::BaseAnalysis *ba) : Analyzer(ba, "Pi0Selection") {

  RequestTree(new TRecoLKrEvent);
  RequestTree(new TRecoCedarEvent);
  RequestTree(new TRecoGigaTrackerEvent);

  AddParam("UseGTK"                      , "bool"  , &fUseGTK                      , false);
  AddParam("CheckIsolation"              , "bool"  , &fCheckIsolation              , false);
  AddParam("CheckSpectrometerAssociation", "bool"  , &fCheckSpectrometerAssociation, true);
  AddParam("CheckLKrAcceptance"          , "bool"  , &fCheckLKrAcceptance          , false);
  AddParam("CutMaxTimeDiffClusterTrigger", "double", &fCutMaxTimeDiffClusterTrigger, 20.);
  AddParam("CutMinDistToDeadCell"        , "double", &fCutMinDistToDeadCell        , 20.);
  AddParam("CutMinClusterEnergy"         , "double", &fCutMinClusterEnergy         , 1000.);
  AddParam("CutMaxClusterTimeDiff"       , "double", &fCutMaxClusterTimeDiff       , 5.);
  AddParam("CutMinClusterDist"           , "double", &fCutMinClusterDist           , 200.);
  AddParam("CutMaxTotalEnergy"           , "double", &fCutMaxTotalEnergy           , 75000.);
  AddParam("CutMinTotalEnergy"           , "double", &fCutMinTotalEnergy           , 2000.);
  AddParam("CutMinNSectorsKTAG"          , "int"   , &fCutMinNSectorsKTAG          , 5);
  AddParam("CutTimeDiffGTKKTAG"          , "double", &fCutTimeDiffGTKKTAG          , 0.5);
  AddParam("CutMaxChi2GTKCandidate"      , "double", &fCutMaxChi2GTKCandidate      , 50.);
  AddParam("CutMaxVertexZ"               , "double", &fCutMaxVertexZ               , 180000.);
  AddParam("CutMinVertexZ"               , "double", &fCutMinVertexZ               , 105000.);

  fReadingData = kTRUE;
  fTriggerConditions = TriggerConditions::GetInstance();
  fLKrStartPos = GeometricAcceptance::GetInstance()->GetZLKr();
  fEventSelected = false;
}

void Pi0Selection::InitOutput() {
  RegisterOutput("EventSelected", &fEventSelected);
  RegisterOutput("ElectromagneticClusters", &fEMClusters);
  RegisterOutput("SelectedPi0", &fSelectedPi0); // output: vector of Pi0SelectionOutput
}

void Pi0Selection::InitHist() {
  fReadingData = GetIsTree();

  if (fReadingData) {
    BookHisto("hNClusters", new TH1I("NClusters", "Number of LKr clusters; N(clusters); Entries", 20, -0.5, 19.5));
    BookHisto("hTimeDiffClusterTrigger", new TH1F("TimeDiffClusterTrigger", "#DeltaT (LKr cluster - trigger); #DeltaT [ns]; Entries/0.5ns", 200, -50., 50.));
    BookHisto("hNGoodEMClusters", new TH1I("NGoodClusters", "N Good Electromagnetic Clusters; N", 10, -0.5, 9.5));
    BookHisto("hGammaDeltaT", new TH1F("GammaDeltaT", "Gamma2-Gamma1 time difference [ns]; #Delta T [ns]; Entries/0.5ns", 200, -50., 50.));
    BookHisto("hGammaDistance", new TH1F("GammaDistance", "Cluster Distance; #DeltaD (clusters) [mm]; Entries/1cm", 200, 0., 2000.));
    BookHisto("hTotalLKrClusterEnergy", new TH1F("TotalLKrClusterEnergy", "Total LKr Cluster Energy; E_{total} [MeV]; Entries/1GeV", 80, 0., 80000.));
    BookHisto("hGamma1Energy", new TH1F("Gamma1Energy", "Gamma1 energy [MeV]; E_{#gamma1} [MeV]; Entries/250MeV", 320, 0, 80000));
    BookHisto("hGamma2Energy", new TH1F("Gamma2Energy", "Gamma2 energy [MeV]; E_{#gamma2} [MeV]; Entries/250MeV", 320, 0, 80000));
    BookHisto("hNElmagPairsOfClusters", new TH1I("NElmagPairsOfClusters","N Elmag Pairs Of Clusters; N_{elmag}", 10, -0.5, 9.5));
    BookHisto("hTimeDiffGTKCedar", new TH1F("TimeDiffGTKCedar", "#DeltaT(GTK - Cedar); #Delta T [ns]; Entries/0.1ns", 400, -20., 20.));
    BookHisto("hKaonCandidate_NOM_GTK", new TH1I("KaonCandidate_NOM_GTK", "Kaon candidate: nominal or GTK;NOM(0) or GTK(1);# #pi^{0}", 2, 0, 2));
    BookHisto("hNeutralVertexZ", new TH1F("NeutralVertexZ", "Z position of neutral vertex; Z [mm]; Entries/1m", 250, 0., 250000.));
    BookHisto("hPi0Position", new TH1F("Pi0Position", "Position (vertex) of Pi0 production; Z [mm]; Entries/1m", 250, 0., 250000.));
    BookHisto("hNSelectedPi0s",new TH1I("NSelectedPi0s", "Number of selected neutral pions in the event; N(#pi^{0}); Entries", 10, -0.5, 9.5));
  } else {
    cout << user_normal() << "Reading my own output" << endl;
  }
}

void Pi0Selection::StartOfRunUser(){
  /// \MemberDescr
  /// This method is called at the beginning of the processing (corresponding to a start of run in the normal NA62 data taking)\n
  /// Do here your start of run processing if any
  /// \EndMemberDescr
}

void Pi0Selection::StartOfBurstUser(){
  /// \MemberDescr
  /// This method is called when a new file is opened in the ROOT TChain (corresponding to a start/end of burst in the normal NA62 data taking) + at the beginning of the first file\n
  /// Do here your start/end of burst processing if any
  /// \EndMemberDescr
}

void Pi0Selection::Process(Int_t eventID) {
  if (!fReadingData) return; // no action if reading its own output in --histo mode

  // Read LKr cluster builder output
  auto Clusters = *(vector<EnergyCluster>*)GetOutput("EnergyClusterBuilder.Output");

  // Prepare/clear output
  fEventSelected = false;
  SetOutputState("EventSelected", kOValid);
  fEMClusters.clear();
  SetOutputState("ElectromagneticClusters", kOValid);
  fSelectedPi0.clear();
  SetOutputState("SelectedPi0", kOValid);

  // Prepare trigger variables
  Bool_t physicsTrigger = fTriggerConditions->IsPhysicsTrigger(GetL0Data());
  Bool_t controlTrigger = fTriggerConditions->IsControlTrigger(GetL0Data());
  Double_t fineTime = GetEventHeader()->GetFineTime();
  if (!GetWithMC()) { // data
    if (controlTrigger) { // control
      fineTime = GetL0Data()->GetPrimitive(kL0TriggerSlot, kL0CHOD).GetFineTime();
    } else if (physicsTrigger) { // physics
      fineTime = GetL0Data()->GetPrimitive(kL0TriggerSlot, kL0RICH).GetFineTime();
    } else {
      cout<< user() << "Data event " << eventID << " is not Physics nor Control" << endl;
    }
  }
  Double_t triggerTime = fineTime*TdcCalib;

  // Filter only good electromagnetic clusters
  FillHisto("hNClusters", Clusters.size());
  vector<EnergyCluster> goodClusters;
  for (const auto &clus : Clusters) {
    FillHisto("hTimeDiffClusterTrigger", clus.GetTime() - triggerTime);
    if (fabs(clus.GetTime() - triggerTime) > fCutMaxTimeDiffClusterTrigger) continue;
    if (clus.GetClusterDDeadCell() < fCutMinDistToDeadCell) continue;
    if (!clus.GetIsElectromagnetic()) continue;
    if (fCheckLKrAcceptance && !clus.GetIsInLKrAcceptance()) continue;
    if (clus.GetClusterEnergy() < fCutMinClusterEnergy) continue;
    goodClusters.push_back(clus);
  }
  FillHisto("hNGoodEMClusters", goodClusters.size());
  fEMClusters = goodClusters;
  
  // Select good pairs of LKr clusters
  vector<pair<EnergyCluster, EnergyCluster>> clusterPairs;
  for (UInt_t i = 0; i < goodClusters.size(); i++) {
    EnergyCluster &c1 = goodClusters.at(i);
    if (fCheckSpectrometerAssociation && c1.SpectrometerAssociationExists()) continue;
    if (fCheckIsolation && !c1.GetIsIsolated()) continue;
    for(UInt_t j = i+1; j < goodClusters.size(); j++) {
      EnergyCluster &c2 = goodClusters.at(j);
      if (fCheckSpectrometerAssociation && c2.SpectrometerAssociationExists()) continue;
      if (fCheckIsolation && !c2.GetIsIsolated()) continue;
      // required to be close in time
      FillHisto("hGammaDeltaT", c1.GetTime() - c2.GetTime());
      if (fabs(c1.GetTime() - c2.GetTime()) > fCutMaxClusterTimeDiff) continue;
      // required to be separated in position
      Double_t rr = sqrt(pow(c1.GetClusterX() - c2.GetClusterX(), 2) +
			 pow(c1.GetClusterY() - c2.GetClusterY(), 2));
      FillHisto("hGammaDistance", rr);
      if (rr < fCutMinClusterDist) continue;
      // total energy cut
      Double_t totalE = c1.GetClusterEnergy() + c2.GetClusterEnergy();
      FillHisto("hTotalLKrClusterEnergy", totalE);
      if ((totalE > fCutMaxTotalEnergy) || (totalE < fCutMinTotalEnergy)) continue;
      // sort pair by energy
      if (c1.GetClusterEnergy() < c2.GetClusterEnergy()) {
	clusterPairs.push_back(make_pair(c1, c2));
      } else {
	clusterPairs.push_back(make_pair(c2, c1));
      }
      FillHisto("hGamma1Energy", clusterPairs.back().first.GetClusterEnergy());
      FillHisto("hGamma2Energy", clusterPairs.back().second.GetClusterEnergy());
    }
  }
  FillHisto("hNElmagPairsOfClusters", clusterPairs.size());

  for (const auto &empair : clusterPairs) {
    Double_t timePi0 = 0.5*(empair.first.GetTime() + empair.second.GetTime());
    Int_t gtkID = -1;
    if (fUseGTK) {
      Int_t ktagID = FindBestKTAG(timePi0);
      gtkID = FindBestGTK(ktagID, fCutTimeDiffGTKKTAG);
    }
    FillHisto("hKaonCandidate_NOM_GTK", (gtkID<0 ? 0 : 1));
    TVector3 neutralVertex = Pi0Vertex(empair.first, empair.second, gtkID);
    FillHisto("hNeutralVertexZ", neutralVertex.Z());
    if (fCutMinVertexZ > neutralVertex.Z() || neutralVertex.Z() > fCutMaxVertexZ) continue;
    // Compute photon momenta from the cluster Energy and neutral vertex position
    TLorentzVector gamma1 = PhotonMomentum(empair.first, neutralVertex);
    TLorentzVector gamma2 = PhotonMomentum(empair.second, neutralVertex);
    // Pi0 selection was successful, set the output variables
    Pi0SelectionOutput pi0Out;
    pi0Out.fMomentum = gamma1 + gamma2;
    pi0Out.fPosition = neutralVertex;
    pi0Out.fTime = timePi0;
    pi0Out.fGTKID = gtkID;
    pi0Out.fGammaMomenta = make_pair(gamma1, gamma2);
    pi0Out.fClustersID = make_pair(empair.first.GetClusterID(), empair.second.GetClusterID());
    fSelectedPi0.push_back(pi0Out);
  }

  for (const auto &pi0Out : fSelectedPi0) {
    FillHisto("hPi0Position", pi0Out.fPosition.Z()); // [mm]
  }
  FillHisto("hNSelectedPi0s", fSelectedPi0.size());
  fEventSelected = fSelectedPi0.size() > 0;
}

void Pi0Selection::PostProcess() {
  /// \MemberDescr
  /// This function is called after an event has been processed by all analyzers.
  /// Invalidate all registered outputs.
  /// \EndMemberDescr
  SetOutputState("EventSelected", kOInvalid);
  SetOutputState("SelectedPi0", kOInvalid);
}

void Pi0Selection::EndOfBurstUser(){
  /// \MemberDescr
  /// This method is called when a new file is opened in the ROOT TChain (corresponding to a start/end of burst in the normal NA62 data taking) + at the end of the last file\n
  /// Do here your start/end of burst processing if any.
  /// Be careful: this is called after the event/file has changed.
  /// \EndMemberDescr
}

void Pi0Selection::EndOfRunUser(){
  /// \MemberDescr
  /// This method is called at the end of the processing (corresponding to a end of run in the normal NA62 data taking)\n
  /// Do here your end of run processing if any\n
  /// \EndMemberDescr

}

void Pi0Selection::EndOfJobUser(){
  if (fReadingData) { // Data mode: save output
    SaveAllPlots();
    return;
  }
}

void Pi0Selection::DrawPlot(){

}

Pi0Selection::~Pi0Selection(){
  /// \MemberDescr
  /// Destructor of the Analyzer. If you allocated any memory for class
  /// members, delete them here.
  /// \EndMemberDescr
}

Int_t Pi0Selection::FindBestKTAG(Double_t referenceTime) {
  Int_t ktagID = -1;
  auto CedarEvent = GetEvent<TRecoCedarEvent>();
  Int_t nc = CedarEvent->GetNCandidates();
  Double_t minTimeDiff = 9999999.;
  for(Int_t i=0; i<nc; i++){
    TRecoCedarCandidate *CedarCand = static_cast<TRecoCedarCandidate*>(CedarEvent->GetCandidate(i));
    if(CedarCand->GetNSectors()<fCutMinNSectorsKTAG) continue;
    Double_t cedarTime = CedarCand->GetTime();
    Double_t timeDiff = cedarTime - referenceTime;
    if(fabs(timeDiff)<minTimeDiff){
      minTimeDiff = fabs(timeDiff);
      ktagID = i;
    }
  }
  return ktagID;
}

Int_t Pi0Selection::FindBestGTK(Int_t ktagID, Double_t cut) {
  Int_t gtkID = -1;
  if (ktagID < 0) return gtkID;
  auto CedarEvent = GetEvent<TRecoCedarEvent>();
  TRecoCedarCandidate *CedarCand = static_cast<TRecoCedarCandidate*>(CedarEvent->GetCandidate(ktagID));
  Double_t cedarTime = CedarCand->GetTime();
  Double_t minTimeDiff = cut;
  auto GTKEvent = GetEvent<TRecoGigaTrackerEvent>();
  for (Int_t i = 0; i < GTKEvent->GetNCandidates(); i++) {
    TRecoGigaTrackerCandidate *GTKCand = static_cast<TRecoGigaTrackerCandidate*>(GTKEvent->GetCandidate(i));
    if(GTKCand->GetChi2()>fCutMaxChi2GTKCandidate) continue;
    Double_t timeGTK = GTKCand->GetTime();
    Double_t timeDiff = timeGTK - cedarTime;
    FillHisto("hTimeDiffGTKCedar", timeDiff);
    if (fabs(timeDiff) < cut) {
      if (fabs(timeDiff) < minTimeDiff) {
        minTimeDiff = fabs(timeDiff);
        gtkID = i;
      }
    }
  }
  return gtkID;
}

TVector3 Pi0Selection::Pi0Vertex(const EnergyCluster& c1, const EnergyCluster& c2, Int_t gtkID) {
  TVector3 vertex;
  if (gtkID == -1) {
    Double_t rr = sqrt(pow(c1.GetClusterX() - c2.GetClusterX(), 2) + pow(c1.GetClusterY() - c2.GetClusterY(), 2));
    Double_t d = sqrt((c1.GetClusterEnergy())*(c2.GetClusterEnergy()))*rr/MPI0;
    Double_t LKrvtxZ = fLKrStartPos - d;
    vertex.SetXYZ((LKrvtxZ - 102000.)*TMath::Tan(0.0012), 0, LKrvtxZ);
  } else {
    auto GTKEvent = GetEvent<TRecoGigaTrackerEvent>();
    TRecoGigaTrackerCandidate *GTKCand = static_cast<TRecoGigaTrackerCandidate*>(GTKEvent->GetCandidate(gtkID));
    TVector3 posGTK = GTKCand->GetPosition(2);
    TVector3 momGTK = GTKCand->GetMomentum();
    Double_t par[14] = {c1.GetClusterEnergy(), c1.GetClusterX(), c1.GetClusterY(), fLKrStartPos,
			c2.GetClusterEnergy(), c2.GetClusterX(), c2.GetClusterY(), fLKrStartPos,
			momGTK.X(), momGTK.Y(), momGTK.Z(), posGTK.X(), posGTK.Y(), posGTK.Z()};
    fVertexZFunction.SetParameters(par);
    Double_t z = fVertexZFunction.GetX(0, 102400., 180000.); // solve: f(zvtx) = 0
    if (z < 180000. && z > 102400.) {
      BlueTubeTracker::GetInstance()->SetCharge(1);
      BlueTubeTracker::GetInstance()->SetInitialPosition(GTKCand->GetPosition(2));
      BlueTubeTracker::GetInstance()->SetInitialMomentum(GTKCand->GetMomentum());
      BlueTubeTracker::GetInstance()->SetZFinal(z);
      BlueTubeTracker::GetInstance()->TrackParticle();
      vertex = BlueTubeTracker::GetInstance()->GetFinalPosition();
    }
  }
  return vertex;
}

Double_t Pi0Selection::VertexZFunctionFormula(Double_t *var, Double_t *par) {
  Double_t z = var[0];
  TVector3 pos1;
  pos1.SetXYZ(par[1], par[2], par[3]);
  TVector3 pos2;
  pos2.SetXYZ(par[5], par[6], par[7]);
  Double_t E1 = par[0];
  Double_t E2 = par[4];
  TVector3 posGTK;
  posGTK.SetXYZ(par[11], par[12], par[13]);
  TVector3 momGTK;
  momGTK.SetXYZ(par[8], par[9], par[10]);
  Double_t slopeX = par[8]/par[10];
  Double_t slopeY = par[9]/par[10];
  TVector3 posV;
  if (z < 183311. && z > 102400.){
    BlueTubeTracker::GetInstance()->SetCharge(1);
    BlueTubeTracker::GetInstance()->SetInitialPosition(posGTK);
    BlueTubeTracker::GetInstance()->SetInitialMomentum(momGTK);
    BlueTubeTracker::GetInstance()->SetZFinal(z);
    BlueTubeTracker::GetInstance()->TrackParticle();
    posV = BlueTubeTracker::GetInstance()->GetFinalPosition();
  } else {
    posV.SetXYZ(posGTK.X() + slopeX*(z-posGTK.Z()), posGTK.Y() + slopeY*(z-posGTK.Z()), z);
  }
  return MPI0*MPI0 - 2*E1*E2*(1-((pos1-posV)*(pos2-posV))/((pos1-posV).Mag()*(pos2-posV).Mag()));
}

TLorentzVector Pi0Selection::PhotonMomentum(const EnergyCluster& c1, const TVector3& vertex) {
  Double_t clx = c1.GetClusterX();
  Double_t cly = c1.GetClusterY();
  Double_t clE = c1.GetClusterEnergy();
  TVector3 ClusterPointer(clx-vertex.X(), cly-vertex.Y(), GeometricAcceptance::GetInstance()->GetZLKr()-vertex.Z());
  TLorentzVector gamma;
  gamma.SetVectM(ClusterPointer.Unit()*clE, 0);
  return gamma;
}

Double_t Pi0Selection::ComputeDiPhotonMass(const EnergyCluster& c1, const EnergyCluster& c2, const TVector3 vertex)
{
  TLorentzVector gamma1 = PhotonMomentum(c1, vertex);
  TLorentzVector gamma2 = PhotonMomentum(c2, vertex);
  return (gamma1+gamma2).M();
}
