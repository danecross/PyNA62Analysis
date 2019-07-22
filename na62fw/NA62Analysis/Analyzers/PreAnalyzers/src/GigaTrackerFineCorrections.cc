// ---------------------------------------------------------------
//
// History:
//
// Created by Alina Kleimenova (alina.kleimenova@cern.ch) 16/10/2018
//
// ---------------------------------------------------------------

/// \class GigaTrackerFineCorrections
/// \Brief
/// Apply momentum and rotation corrections to the reconstructed GTK tracks
/// \EndBrief
/// \Detailed
/// GTK fine alignment correction to track directions (data only):
/// d(x') = A+Bx; d(y') = C+Dy.
/// \EndDetailed

#include "GigaTrackerFineCorrections.hh"
#include "NA62ConditionsService.hh"
#include "TRecoGigaTrackerEvent.hh"
#include "Stream.hh"

using namespace std;

GigaTrackerFineCorrections::GigaTrackerFineCorrections(Core::BaseAnalysis *ba) : Analyzer(ba, "GigaTrackerFineCorrections")
{
  RequestTree("GigaTracker", new TRecoGigaTrackerEvent, "Reco");
  fGTKMomentumScale = 1.0;
  fA=0.;
  fB=0.;
  fC=0.;
  fD=0.;
}
void GigaTrackerFineCorrections::InitOutput() {}

void GigaTrackerFineCorrections::StartOfRunUser(){
  fGTKMomentumScale = 1.0;
  fA=0.;
  fB=0.;
  fC=0.;
  fD=0.;

  if (GetStreamInfo()->GetRecoInfo().GetRevision() == "v1.0.0"){
    std::cout << user_normal() <<
      "In the revision v1.0.0 track corrections were already applied. Skipping" << std::endl;
    return;
  }

  if (GetWithMC()) {
    fGTKMomentumScale = (GetStreamInfo()->GetMCInfo().GetRevision() =="v0.11.0") ? 1.00049 : 1.00081;
    return;
  }

  TString Line;
  TString RotationFileName = "GigaTracker-Rotation.dat";
  if(NA62ConditionsService::GetInstance()->Open(RotationFileName)!=kSuccess) return;
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(RotationFileName))) {
    if (Line.BeginsWith("#")) continue;
    TObjArray *l = Line.Tokenize(" ");
    Int_t Run = ((TObjString*)(l->At(0)))->GetString().Atoi();
    if (Run!=GetRunID()) { // wrong run number
      delete l;
      continue;
    }
    fA = ((TObjString*)(l->At(1)))->GetString().Atof();
    fB = ((TObjString*)(l->At(2)))->GetString().Atof();
    fC = ((TObjString*)(l->At(3)))->GetString().Atof();
    fD = ((TObjString*)(l->At(4)))->GetString().Atof();
    delete l;
  }
  NA62ConditionsService::GetInstance()->Close(RotationFileName);
  Line.Clear();

  TString MomentumFileName = "GigaTracker-MomentumScale.dat";
  if(NA62ConditionsService::GetInstance()->Open(MomentumFileName)!=kSuccess) return;
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(MomentumFileName))) {
    if (Line.BeginsWith("#")) continue;
    TObjArray *l = Line.Tokenize(" ");
    Int_t    Run   = ((TObjString*)(l->At(0)))->GetString().Atoi();
    Double_t Scale = ((TObjString*)(l->At(1)))->GetString().Atof();
    delete l;
    if (Run!=GetRunID()) continue; // wrong run number
    fGTKMomentumScale = Scale;
  }
  NA62ConditionsService::GetInstance()->Close(MomentumFileName);
}

void GigaTrackerFineCorrections::Process(Int_t) {
  if (GetStreamInfo()->GetRecoInfo().GetRevision() == "v1.0.0") return;
  TRecoGigaTrackerEvent* GTKEvent = GetEvent<TRecoGigaTrackerEvent>();
  if (!GTKEvent) return;
  if (!GTKEvent->GetNCandidates()) return;
  if (!GetWithMC()){
    for (Int_t i=0; i<GTKEvent->GetNCandidates(); i++) {
      TRecoGigaTrackerCandidate *Gcand = static_cast<TRecoGigaTrackerCandidate*>(GTKEvent->GetCandidate(i));
      Double_t d_dxdz = fA * 1e-6 + fB * 1e-6 * Gcand->GetPosition(2).X();
      Double_t d_dydz = fC * 1e-6 + fD * 1e-6 * Gcand->GetPosition(2).Y();
      Double_t dPx    = Gcand->GetMomentum().Z() * d_dxdz;
      Double_t dPy    = Gcand->GetMomentum().Z() * d_dydz;
      TVector3 Corr   = TVector3(dPx, dPy, 0.0);
      TVector3 Mom    = Gcand->GetMomentum() + Corr;
      Gcand->SetMomentum(Mom);
    }
  }

  if (fabs(fGTKMomentumScale-1.0)>1e-4) {
    for (Int_t i=0; i<GTKEvent->GetNCandidates(); i++) {
      TRecoGigaTrackerCandidate *Gcand = static_cast<TRecoGigaTrackerCandidate*>(GTKEvent->GetCandidate(i));
      TVector3 Mom = (1.0/fGTKMomentumScale) * Gcand->GetMomentum();
      Gcand->SetMomentum(Mom);
    }
  }
}
