// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2017-12-11
//
// ---------------------------------------------------------------

/// \class GigaTrackerAlignment
/// \Brief
/// Computation of GTK station (x,y) offsets, momentum scale and rotation corrections
/// \EndBrief
/// \Detailed
/// GTK track positions in the three planes are compared with those extrapolated from downstream
/// K3pi decays, taking into account the effect of the "blue tube" magnetic field in the vacuum tank.
/// This is a two-step analyzer: it should first run on reconstructred data,
/// and then on its own output to obtain the final results.
/// When the correct (x,y) offsets computed earlier are in place,
/// this analyser can be used to evaluate the fine GTK corrections, which include
/// the momentum scale and rotation corrections.
/// The rotation correction is a correction to GTK track slopes using the formulae dx'=A+Bx, dy'=C+Dy.
/// By default, this analyzer computes the fine corrections. To compute the (x,y) offsets,
/// one should provide (at step 1, i.e. when running over the data) a parameter value as follows:
/// \code
/// ./MyExec -l <list> -p "GigaTrackerAlignment:FineCorrections=0"
/// \endcode
/// As this analyzer is based on K3piSelection, it also runs K3piSelection which leads,
/// in particular, to beam parameter evaluation.<br><br>
/// This analyser rebuilds the GTK event using GigaTrackerRecoAlgorithm,
/// however the GTK event is cloned first and the original event is not modified.
/// An attempt to measure the GTK pixel efficiency is also made (probably to be optimized).
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include <TStyle.h>
#include "GigaTrackerAlignment.hh"
#include "GeometricAcceptance.hh"
#include "EventChecker.hh"
#include "TF1.h"
#include "TLine.h"
#include "TLegend.h"
#include "BlueTubeTracker.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

GigaTrackerAlignment::GigaTrackerAlignment(Core::BaseAnalysis *ba) :
  Analyzer(ba, "GigaTrackerAlignment"),
  fHdx1(nullptr), fHdy1(nullptr), fHdx2(nullptr), fHdy2(nullptr), fHdx3(nullptr), fHdy3(nullptr),
  fHdPx(nullptr), fHdPy(nullptr), fHPrat(nullptr), 
  fFdx1(nullptr), fFdy1(nullptr), fFdx2(nullptr), fFdy2(nullptr), fFdx3(nullptr), fFdy3(nullptr),
  fFdPx(nullptr), fFdPy(nullptr), fFPrat(nullptr),
  fHdxdz_vs_x(nullptr), fHdydz_vs_y(nullptr), fHStationEfficiency(nullptr),
  fGdxdz_vs_x(nullptr), fGdydz_vs_y(nullptr), fFdxdz_vs_x(nullptr), fFdydz_vs_y(nullptr) {

  fGTKReco = new GigaTrackerRecoAlgorithm(ba, this, "GTKAlgoAlignment");
  RequestBeamData();
  RequestTree("GigaTracker", new TRecoGigaTrackerEvent);
  fReadingData = kTRUE;
  fZGTK1   = GeometricAcceptance::GetInstance()->GetZGTK1();
  fZGTK2   = GeometricAcceptance::GetInstance()->GetZGTK2();
  fZTrim5  = GeometricAcceptance::GetInstance()->GetZTrim5();
  fZGTK3   = GeometricAcceptance::GetInstance()->GetZGTK3();
  fTrim5PT = 90.0; // [MeV/c]

  // False = station (x,y) alignment.
  // True (default) = fine corrections, i.e. momentum scale and rotation (dx'=A+Bx; dy'=C+Dy).
  AddParam("FineCorrections", &fFineCorrections, true);
}

GigaTrackerAlignment::~GigaTrackerAlignment() {
  if (fGTKReco) delete fGTKReco;
}

void GigaTrackerAlignment::InitHist() {
  fReadingData = GetIsTree(); // false in --histo mode, true otherwise

  if (fReadingData) {
    cout << user_normal() << "Reading reconstructed data for GTK " <<
      (fFineCorrections ? "fine" : "(x,y)") << " alignment" << endl;

    if (!fFineCorrections) fGTKReco->SetRedoXYCorr(2); // use raw hit positions
    else                   fGTKReco->SetRedoXYCorr(1); // re-do (x,y) corrections
    fGTKReco->SetFineCorr(false);
    fGTKReco->SetRedoTimeCorr(false);
    fGTKReco->SetFineTimeCorr(false);
    fGTKReco->SetTimeWindowWrtReference(5.0); // [ns]

    BookHisto("hK3piMomentum", new TH1F
	      ("K3piMomentum", "K3pi momentum;Momentum [GeV/c]", 100, 70, 80));
    BookHisto("hK3pidxdz", new TH1F
	      ("K3pidxdz", "K3pi dx/dz", 100, -0.0025, 0.0025));
    BookHisto("hK3pidydz", new TH1F
	      ("K3pidydz", "K3pi dy/dz", 100, -0.0025, 0.0025));
    BookHisto("hK3piGTK1x", new TH1F
              ("K3piGTK1x", "K3pi x at GTK1 plane;x [mm]", 100, -30, 30));
    BookHisto("hK3piGTK1y", new TH1F
              ("K3piGTK1y", "K3pi y at GTK1 plane;y [mm]", 100, -15, 15));
    BookHisto("hK3piGTK2x", new TH1F
              ("K3piGTK2x", "K3pi x at GTK2 plane;x [mm]", 100, -30, 30));
    BookHisto("hK3piGTK2y", new TH1F
              ("K3piGTK2y", "K3pi y at GTK2 plane;y [mm]", 100, -15, 15));
    BookHisto("hK3piGTK3x", new TH1F
	      ("K3piGTK3x", "K3pi x at GTK3 plane;x [mm]", 100, -30, 30));
    BookHisto("hK3piGTK3y", new TH1F
              ("K3piGTK3y", "K3pi y at GTK3 plane;y [mm]", 100, -15, 15));

    BookHisto("hNGTKCandidates", new TH1F("NGTKCandidates", "Number of GTK candidates", 20, -0.5, 19.5));
    BookHisto("hGTKCandidateChi2", new TH1F("GTKCandidateChi2", "GTK candidate #chi^{2}", 100, 0, 100));
    BookHisto("hDeltaTime", new TH1F("DeltaTime", "GTK #minus Vertex(CHOD) time;Time difference [ns]", 160, -4, 4));

    BookHisto("hTrackGTK1x", new TH1F
              ("TrackGTK1x", "Track x at GTK1 plane;x [mm]", 100, -30, 30));
    BookHisto("hTrackGTK1y", new TH1F
              ("TrackGTK1y", "Track y at GTK1 plane;y [mm]", 100, -15, 15));
    BookHisto("hTrackGTK2x", new TH1F
              ("TrackGTK2x", "Track x at GTK2 plane;x [mm]", 100, -30, 30));
    BookHisto("hTrackGTK2y", new TH1F
              ("TrackGTK2y", "Track y at GTK2 plane;y [mm]", 100, -15, 15));
    BookHisto("hTrackGTK3x", new TH1F
              ("TrackGTK3x", "Track x at GTK3 plane;x [mm]", 100, -30, 30));
    BookHisto("hTrackGTK3y", new TH1F
              ("TrackGTK3y", "Track y at GTK3 plane;y [mm]", 100, -15, 15));

    for (Int_t i=0; i<5; i++) {
      TString name = Form("Pull_%d", i);
      BookHisto(name, new TH1F(name, name, 100, -5, 5));
    }
    for (Int_t i=0; i<5; i++) {
      TString name = Form("Pull_%d_Best", i);
      BookHisto(name, new TH1F(name, name, 100, -5, 5));
    }
    BookHisto("Chi2_GTK_K3pi", new TH1F
	      ("Chi2_GTK_K3pi", "Chi2_GTK_K3pi", 100, 0, 50));
    BookHisto("Chi2_GTK_K3pi_Best", new TH1F
              ("Chi2_GTK_K3pi_Best", "Chi2_GTK_K3pi_Best", 100, 0, 50));

    // To distinguish (x,y) from fine corrections at step1
    if (!fFineCorrections) BookHisto("hxy", new TH1F("xy", "Dummy", 1, 0, 1));

    // Station (x,y) misalignment monitor histograms
    BookHisto("hdx1", new TH1F("dx1", "#Deltax (GTK1);[mm]", 100, -5, 5));
    BookHisto("hdy1", new TH1F("dy1", "#Deltay (GTK1);[mm]", 100, -5, 5));
    BookHisto("hdx2", new TH1F("dx2", "#Deltax (GTK2);[mm]", 100, -5, 5));
    BookHisto("hdy2", new TH1F("dy2", "#Deltay (GTK2);[mm]", 100, -5, 5));
    BookHisto("hdx3", new TH1F("dx3", "#Deltax (GTK3);[mm]", 100, -5, 5));
    BookHisto("hdy3", new TH1F("dy3", "#Deltay (GTK3);[mm]", 100, -5, 5));

    // GTK rotation/bending monitor histograms
    // NB: the (x,y) bin size is multiple of GTK pixel size (0.3mm)
    BookHisto("hdxdz_vs_x3",  new TH2F
	      ("hdxdz_vs_x3", "#Delta(dx/dz) vs x_{GTK3};x3 [mm];(dx/dz)_{3#pi}#minus(dx/dz)_{GTK}",
	       100, -30, 30, 100, -1e-4, 1e-4));
    BookHisto("hdydz_vs_x3",  new TH2F
              ("hdydz_vs_x3", "#Delta(dy/dz) vs x_{GTK3};x3 [mm];(dy/dz)_{3#pi}#minus(dy/dz)_{GTK}",
               100, -30, 30, 100, -1e-4, 1e-4));
    BookHisto("hdxdz_vs_y3",  new TH2F
              ("hdxdz_vs_y3", "#Delta(dx/dz) vs y_{GTK3};y3 [mm];(dx/dz)_{3#pi}#minus(dx/dz)_{GTK}",
               100, -15, 15, 100, -1e-4, 1e-4));
    BookHisto("hdydz_vs_y3",  new TH2F
	      ("hdydz_vs_y3", "#Delta(dy/dz) vs y_{GTK3};y3 [mm];(dy/dz)_{3#pi}#minus(dy/dz)_{GTK}",
	       100, -15, 15, 100, -1e-4, 1e-4));

    // Momentum scale monitor
    BookHisto("hPrat", new TH1F("Prat", "P_{GTK}/P_{3#pi}", 100, 0.98, 1.02));

    // Efficiency monitor
    BookHisto("hHitTime", new TH1F("HitTime", "HitTime", 1200, -60, 60));
    BookHisto("hHitTrackDistance1", new TH1F("HitTrackDistance1", "HitTrackDistance1", 100, 0, 50));
    BookHisto("hHitTrackDistance2", new TH1F("HitTrackDistance2", "HitTrackDistance2", 100, 0, 50));
    BookHisto("hHitTrackDistance3", new TH1F("HitTrackDistance3", "HitTrackDistance3", 100, 0, 50));
    Double_t IntStep = 1000./45./3.; // about 7.4 MHz
    BookHisto("hStationEfficiency", new TH2F
	      ("StationEfficiency", "StationEfficiency", 20, 0, 200*IntStep, 4, -1.5, 2.5));
    BookHisto("hStationEfficiencyA", new TH2F
	      ("StationEfficiencyA", "StationEfficiencyA", 20, 0, 200*IntStep, 4, -1.5, 2.5));

    // Other moniroting histograms
    BookHisto("hdPx",    new TH1F("dPx", "#DeltaP_{x} (GTK#minusK_{3#pi});[MeV/c]", 100, -20, 20));
    BookHisto("hdPy",    new TH1F("dPy", "#DeltaP_{y} (GTK#minusK_{3#pi});[MeV/c]", 100, -20, 20));
    BookHisto("hdPx_MC", new TH1F("dPx_MC", "#DeltaP_{x} (GTK#minusTrue);[MeV/c]", 100, -20, 20));
    BookHisto("hdPy_MC", new TH1F("dPy_MC", "#DeltaP_{y} (GTK#minusTrue);[MeV/c]", 100, -20, 20));
  }
  else {
    fFineCorrections = !((Bool_t)RequestHistogram(fAnalyzerName, "xy", true)); // (x,y) or fine corrections?
    cout << user_normal() << "Reading my own output for " <<
      ((!fFineCorrections) ? "(x,y)" : "fine") << " alignment" << endl;

    // Station (x,y) offsets
    fHdx1 = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "dx1", true));
    fHdy1 = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "dy1", true));
    fHdx2 = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "dx2", true));
    fHdy2 = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "dy2", true));
    fHdx3 = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "dx3", true));
    fHdy3 = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "dy3", true));
    // Fine alignment: (dx/dz vs x) and (dy/dz vs y)
    fHdxdz_vs_x = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "hdxdz_vs_x3", true));
    fHdydz_vs_y = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "hdydz_vs_y3", true));
    // Momentum scale
    fHPrat = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "Prat", true));
    // General monitoring, not used fo evaluate the corrections
    fHdPx = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "dPx", true));
    fHdPy = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "dPy", true));
    // Efficiency
    fHStationEfficiency = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "StationEfficiencyA", true));
  }
}

void GigaTrackerAlignment::Process(Int_t) {

  if (!fReadingData) return; // no action if reading own output in --histo mode

  // Discard GTK bad events
  if (EventChecker::BadQualityMask(GetEventHeader(), kGigaTracker)) return;

  // Require a K3pi event reconstructed by K3piSelection
  Bool_t K3piSelected = *GetOutput<Bool_t>("K3piSelection.EventSelected");
  if (!K3piSelected) return;
  TVector3 Pos = *GetOutput<TVector3>("K3piSelection.VertexPosition");
  TVector3 Mom = *GetOutput<TVector3>("K3piSelection.TotalMomentum");
  Double_t VertexTime = *GetOutput<Double_t>("K3piSelection.K3piTime"); // track mean CHOD time

  //////////////////////////////////////////////////////////////////////
  // Kaon positions at GTK stations evaluated from the K3pi decay.
  // The vertical displacement of GTK2 is not taken into account,
  // because it is corrected for in TRecoGigaTrackerCandidate structure.

  // Track the "downstream kaon" upstream to GTK3 in the blue field
  BlueTubeTracker::GetInstance()->SetCharge(1);
  BlueTubeTracker::GetInstance()->SetInitialPosition(Pos);
  BlueTubeTracker::GetInstance()->SetInitialMomentum(Mom);
  BlueTubeTracker::GetInstance()->SetZFinal(fZGTK3);
  BlueTubeTracker::GetInstance()->TrackParticle();
  TVector3 PosGTK3 = BlueTubeTracker::GetInstance()->GetFinalPosition();
  TVector3 MomGTK3 = BlueTubeTracker::GetInstance()->GetFinalMomentum();

  fK3piMomentum  = MomGTK3.Mag();
  fK3pidxdz      = MomGTK3.x()/MomGTK3.z();
  fK3pidydz      = MomGTK3.y()/MomGTK3.z();
  fK3piPos[2][0] = PosGTK3.x();
  fK3piPos[2][1] = PosGTK3.y();

  // Extrapolate upstream to GTK1 and GTK2, accounting for TRIM5 effect
  fK3piPos[0][0] = fK3piPos[2][0] + fK3pidxdz*(fZGTK1-fZGTK3) + fTrim5PT/fK3piMomentum*(fZTrim5-fZGTK1);
  fK3piPos[0][1] = fK3piPos[2][1] + fK3pidydz*(fZGTK1-fZGTK3);
  fK3piPos[1][0] = fK3piPos[2][0] + fK3pidxdz*(fZGTK2-fZGTK3) + fTrim5PT/fK3piMomentum*(fZTrim5-fZGTK2);
  fK3piPos[1][1] = fK3piPos[2][1] + fK3pidydz*(fZGTK2-fZGTK3);

  FillHisto("hK3piMomentum", fK3piMomentum*1e-3); // [MeV/c] --> [GeV/c]
  FillHisto("hK3pidxdz",     fK3pidxdz);
  FillHisto("hK3pidydz",     fK3pidydz);
  FillHisto("hK3piGTK1x",    fK3piPos[0][0]);
  FillHisto("hK3piGTK1y",    fK3piPos[0][1]);
  FillHisto("hK3piGTK2x",    fK3piPos[1][0]);
  FillHisto("hK3piGTK2y",    fK3piPos[1][1]);
  FillHisto("hK3piGTK3x",    fK3piPos[2][0]);
  FillHisto("hK3piGTK3y",    fK3piPos[2][1]);

  // Rerun GTK reconstruction on a cloned GTK event
  TRecoGigaTrackerEvent* GTKEventOriginal = GetEvent<TRecoGigaTrackerEvent>();
  TRecoGigaTrackerEvent* GTKEvent = new TRecoGigaTrackerEvent(*GTKEventOriginal);
  fGTKReco->Process(GTKEvent, VertexTime);

  // Determine the GTK track best matching the K3pi kaon
  Int_t iBestGTKTrack = -999;
  Double_t chi2min = 999;
  FillHisto("hNGTKCandidates", GTKEvent->GetNCandidates());
  for (Int_t iGTK=0; iGTK<GTKEvent->GetNCandidates(); iGTK++) { // loop over GTK tracks
    TRecoGigaTrackerCandidate *Gcand = static_cast<TRecoGigaTrackerCandidate*>(GTKEvent->GetCandidate(iGTK));
    FillHisto("hGTKCandidateChi2", Gcand->GetChi2());
    //if (Gcand->GetChi2()>50.0) continue; // no cut on chi2: it is large if the GTK is misaligned
    Double_t DeltaTime = Gcand->GetTime()-VertexTime;
    FillHisto("hDeltaTime", DeltaTime);
    if (fabs(DeltaTime)>0.6) continue; // [ns]
    Double_t chi2 = Chi2_GTK_K3pi(Gcand);
    for (Int_t i=0; i<5; i++) FillHisto(Form("Pull_%d", i), fPull[i]);
    FillHisto("Chi2_GTK_K3pi", chi2);
    if (chi2<chi2min) {
      iBestGTKTrack = iGTK;
      chi2 = chi2min;
    }
  }
  if (iBestGTKTrack<0) {
    delete GTKEvent;
    return;
  }

  // The best GTK candidate: check its chi2
  TRecoGigaTrackerCandidate *Gcand =
    static_cast<TRecoGigaTrackerCandidate*>(GTKEvent->GetCandidate(iBestGTKTrack));
  Double_t chi2 = Chi2_GTK_K3pi(Gcand);
  for (Int_t i=0; i<5; i++) FillHisto(Form("Pull_%d_Best", i), fPull[i]);
  FillHisto("Chi2_GTK_K3pi_Best", chi2); // No cut on chi2, it can be large if GTK is misaligned

  // For the best GTK candidate selected, check the GTK-Straw alignment
  FillHisto("hTrackGTK1x", Gcand->GetPosition(0).X());
  FillHisto("hTrackGTK1y", Gcand->GetPosition(0).Y());
  FillHisto("hTrackGTK2x", Gcand->GetPosition(1).X());
  FillHisto("hTrackGTK2y", Gcand->GetPosition(1).Y());
  FillHisto("hTrackGTK3x", Gcand->GetPosition(2).X());
  FillHisto("hTrackGTK3y", Gcand->GetPosition(2).Y());

  // Histograms for coarse (x,y) alignment
  FillHisto("hdx1", Gcand->GetPosition(0).X()-fK3piPos[0][0]);
  FillHisto("hdy1", Gcand->GetPosition(0).Y()-fK3piPos[0][1]);
  FillHisto("hdx2", Gcand->GetPosition(1).X()-fK3piPos[1][0]);
  FillHisto("hdy2", Gcand->GetPosition(1).Y()-fK3piPos[1][1]);
  FillHisto("hdx3", Gcand->GetPosition(2).X()-fK3piPos[2][0]);
  FillHisto("hdy3", Gcand->GetPosition(2).Y()-fK3piPos[2][1]);

  // Histograms for fine alignment (rotation, bending)
  Double_t dxdz_gtk = Gcand->GetMomentum().X()/Gcand->GetMomentum().Z();
  Double_t dydz_gtk = Gcand->GetMomentum().Y()/Gcand->GetMomentum().Z();
  FillHisto("hdxdz_vs_x3", Gcand->GetPosition(2).X(), fK3pidxdz-dxdz_gtk);
  FillHisto("hdydz_vs_x3", Gcand->GetPosition(2).X(), fK3pidydz-dydz_gtk);
  FillHisto("hdxdz_vs_y3", Gcand->GetPosition(2).Y(), fK3pidxdz-dxdz_gtk);
  FillHisto("hdydz_vs_y3", Gcand->GetPosition(2).Y(), fK3pidydz-dydz_gtk);

  // Histogram for momentum scale correction
  FillHisto("hPrat", Gcand->GetMomentum().Mag()/fK3piMomentum);

  // General monitoring
  FillHisto("hdPx", Gcand->GetMomentum().x()-MomGTK3.x());
  FillHisto("hdPy", Gcand->GetMomentum().y()-MomGTK3.y());

  // GTK plane efficiency measurement:
  // check for an in-time hit within a certain distance from expected kaon position
  // NB: station active areas are 60.8 x 27.0 mm^2.
  Double_t Dist[3]  = {999.0, 999.0, 999.0};
  Double_t DistCut[3] = {8.0, 7.5, 7.0};

  Int_t Nhits = GTKEvent->GetNHits();
  for (Int_t i=0; i<Nhits; i++) {
    TRecoGigaTrackerHit* hit = static_cast<TRecoGigaTrackerHit*>(GTKEvent->GetHit(i));
    Double_t dt = hit->GetTime()-VertexTime;
    FillHisto("hHitTime", dt);
    if (fabs(dt)<1.0) {
      Int_t station = hit->GetStationNo();
      Double_t x = hit->GetPosition().X();
      Double_t y = hit->GetPosition().Y();
      Double_t r = sqrt(pow(x-fK3piPos[station][0], 2) + pow(y-fK3piPos[station][1], 2));
      FillHisto(Form("hHitTrackDistance%d", station+1), r);
      if (r<Dist[station]) Dist[station] = r;
    }
  }

  // Instantaneous beam intensity [MHz], still to be defined for MC
  Double_t Intensity = (!GetWithMC()) ? GetBeamData()->GetInstantaneousIntensity() : 0.0;

  // GTK efficiency for all K3pi events
  FillHisto("hStationEfficiency", Intensity, -1.0); // normalization
  for (Int_t i=0; i<3; i++)
    if (Dist[i]<DistCut[i]) FillHisto("hStationEfficiency", Intensity, i);

  // GTK for K3pi extrapolated into acceptance of all stations
  if (fabs(fK3piPos[0][0])<30.4-DistCut[0] && fabs(fK3piPos[0][1])<13.5-DistCut[0] &&
      fabs(fK3piPos[1][0])<30.4-DistCut[1] && fabs(fK3piPos[1][1])<13.5-DistCut[1] &&
      fabs(fK3piPos[2][0])<30.4-DistCut[2] && fabs(fK3piPos[2][1])<13.5-DistCut[2]) {
    FillHisto("hStationEfficiencyA", Intensity, -1.0); // normalization
    for (Int_t i=0; i<3; i++)
      if (Dist[i]<DistCut[i]) FillHisto("hStationEfficiencyA", Intensity, i);
  }

  // For MC, check (GTK reconstructed-true) kaon momentum
  if (GetWithMC()) {
    Event *evt = GetMCEvent();
    if (evt->GetNKineParts()) {
      TVector3 GtkPos = evt->GetKinePart(0)->GetPosGigaTrackerExit();
      if (GtkPos.Z()>100000.0) { // has the kaon reached the checkpoint?
	TLorentzVector TrueMom = evt->GetKinePart(0)->GetMomGigaTrackerExit();
	FillHisto("hdPx_MC", Gcand->GetMomentum().x()-TrueMom.X());
	FillHisto("hdPy_MC", Gcand->GetMomentum().y()-TrueMom.Y());
      }
    }
  }
  delete GTKEvent;
}

void GigaTrackerAlignment::EndOfJobUser() {
  if (fReadingData) { // Data mode: save output
    SaveAllPlots();
    return;
  }

  /////////////////////////////////////
  // Histo mode: analyze the histograms

  if (!fHdx1) { // Histo mode required but no histograms found
    cout << user_normal() << "Asked to read my own output but cannot found it" << endl;
    return;
  }
  if (fHdx1->Integral()<1000) {
    cout << user_normal() << "Error: low statistics, no results produced" << endl;
    return;
  }

  // Station (x,y) alignment
  if (!fFineCorrections) {
    fFdx1 = new TF1("fFdx1", "gaus", fHdx1->GetMean()-2.5, fHdx1->GetMean()+2.5);
    fFdy1 = new TF1("fFdy1", "gaus", fHdy1->GetMean()-2.5, fHdy1->GetMean()+2.5);
    fFdx2 = new TF1("fFdx2", "gaus", fHdx2->GetMean()-2.5, fHdx2->GetMean()+2.5);
    fFdy2 = new TF1("fFdy2", "gaus", fHdy2->GetMean()-2.5, fHdy2->GetMean()+2.5);
    fFdx3 = new TF1("fFdx3", "gaus", fHdx3->GetMean()-2.5, fHdx3->GetMean()+2.5);
    fFdy3 = new TF1("fFdy3", "gaus", fHdy3->GetMean()-2.5, fHdy3->GetMean()+2.5);
    fHdx1->Fit(fFdx1, "R0Q");
    fHdy1->Fit(fFdy1, "R0Q");
    fHdx2->Fit(fFdx2, "R0Q");
    fHdy2->Fit(fFdy2, "R0Q");
    fHdx3->Fit(fFdx3, "R0Q");
    fHdy3->Fit(fFdy3, "R0Q");

    ofstream outfile(Form("GigaTracker-XYCorrections.run%06d_%04d-run%06d_%04d.dat",
			  GetRunID(), 0, GetRunID(), 9999));
    outfile << "# GTK station (x,y) offsets for run " << GetRunID() << endl;
    outfile << "# Format: StationID X[mm] Y[mm]" << endl;
    outfile << Form("0 %6.3f %6.3f", fFdx1->GetParameter(1), fFdy1->GetParameter(1)) << endl;
    outfile << Form("1 %6.3f %6.3f", fFdx2->GetParameter(1), fFdy2->GetParameter(1)) << endl;
    outfile << Form("2 %6.3f %6.3f", fFdx3->GetParameter(1), fFdy3->GetParameter(1)) << endl;
    outfile.close();

    cout << user_normal() << Form("Run %d offset_x1 %6.3f +- %5.3f\n",
				  GetRunID(), fFdx1->GetParameter(1), fFdx1->GetParError(1));
    cout << user_normal() << Form("Run %d offset_y1 %6.3f +- %5.3f\n",
				  GetRunID(), fFdy1->GetParameter(1), fFdy1->GetParError(1));
    cout << user_normal() << Form("Run %d offset_x2 %6.3f +- %5.3f\n",
				  GetRunID(), fFdx2->GetParameter(1), fFdx2->GetParError(1));
    cout << user_normal() << Form("Run %d offset_y2 %6.3f +- %5.3f\n",
				  GetRunID(), fFdy2->GetParameter(1), fFdy2->GetParError(1));
    cout << user_normal() << Form("Run %d offset_x3 %6.3f +- %5.3f\n",
				  GetRunID(), fFdx3->GetParameter(1), fFdx3->GetParError(1));
    cout << user_normal() << Form("Run %d offset_y3 %6.3f +- %5.3f\n",
				  GetRunID(), fFdy3->GetParameter(1), fFdy3->GetParError(1));
  }

  if (fFineCorrections) {
    // Fine corrections: momentum scale
    fFPrat = new TF1("fFPrat", "gaus", fHPrat->GetMean()-0.005, fHPrat->GetMean()+0.005);
    fHPrat->Fit(fFPrat, "R0Q");

    // General monitoring
    fFdPx  = new TF1("fFdPx",  "gaus", fHdPx->GetMean()-3.0, fHdPx->GetMean()+3.0);
    fFdPy  = new TF1("fFdPy",  "gaus", fHdPy->GetMean()-3.0, fHdPy->GetMean()+3.0);
    fHdPx->Fit(fFdPx, "R0Q");
    fHdPy->Fit(fFdPy, "R0Q");

    // Print the momentum scale results
    ofstream outfile1(Form("GigaTracker-MomentumScale.run%06d_%04d-run%06d_%04d.dat",
			   GetRunID(), 0, GetRunID(), 9999));
    outfile1 << "# GTK momentum scale corrections for run " << GetRunID() << endl;
    outfile1 << "# Format: RunNumber MomentumScale" << endl;
    outfile1 << Form("%06d %7.5f", GetRunID(), fFPrat->GetParameter(1)) << endl;
    outfile1.close();
    cout << user_normal() << Form("Run %d dPx[MeV] %6.3f +- %5.3f\n",
		 GetRunID(), fFdPx->GetParameter(1), fFdPx->GetParError(1));
    cout << user_normal() << Form("Run %d dPy[MeV] %6.3f +- %5.3f\n",
		 GetRunID(), fFdPy->GetParameter(1), fFdPy->GetParError(1));
    cout << user_normal() << Form("Run %d MomScale %7.5f +- %7.5f\n",
		 GetRunID(), fFPrat->GetParameter(1), fFPrat->GetParError(1));

    // Fine corrections: rotation
    fGdxdz_vs_x = nullptr;
    fFdxdz_vs_x = nullptr;
    fGdydz_vs_y = nullptr;
    fFdydz_vs_y = nullptr;
    Double_t A=0.0, B=0.0, C=0.0, D=0.0;

    Bool_t RotationConstantsComputed = true;
    if (fHdxdz_vs_x->Integral()<10000) {
      cout << user_normal() << "Warning: low statistics, rotation constants not computed" << endl;
      RotationConstantsComputed = false;
    }
    else { // compute the rotation constants
      TF1 *fg = new TF1("fg", "gaus", -0.5e-4, +0.5e+4);
      Double_t x[100], y[100], dx[100], dy[100];

      for (Int_t i=0; i<fHdxdz_vs_x->GetNbinsX(); i++) {
	x[i]  = fHdxdz_vs_x->GetXaxis()->GetBinCenter(i+1);
	dx[i] = y[i] = dy[i] = 0.0;
	TH1D *slice = fHdxdz_vs_x->ProjectionY("slice", i+1, i+1);
	if (slice->Integral()<100) {
	  delete slice;
	  continue;
	}
	slice->Fit(fg, "R0Q");
	y[i]  = fg->GetParameter(1);
	dy[i] = fg->GetParError(1);
	delete slice;
      }
      fGdxdz_vs_x = new TGraphErrors(100, x, y, dx, dy);
      fFdxdz_vs_x = new TF1("Fdxdz_vs_x", "pol1", -30, 30);
      fGdxdz_vs_x->Fit(fFdxdz_vs_x, "0QR");

      for (Int_t i=0; i<fHdydz_vs_y->GetNbinsX(); i++) {
	x[i]  = fHdydz_vs_y->GetXaxis()->GetBinCenter(i+1);
	dx[i] = y[i] = dy[i] = 0.0;
	TH1D *slice = fHdydz_vs_y->ProjectionY("slice", i+1, i+1);
	if (slice->Integral()<100) {
	  delete slice;
	  continue;
	}
	slice->Fit(fg, "R0Q");
	y[i]  = fg->GetParameter(1);
	dy[i] = fg->GetParError(1);
	delete slice;
      }
      delete fg;
      fGdydz_vs_y = new TGraphErrors(100, x, y, dx, dy);
      fFdydz_vs_y = new TF1("Fdydz_vs_y", "pol1", -15, 15);
      fGdydz_vs_y->Fit(fFdydz_vs_y, "0QR");

      A = 1e6 * fFdxdz_vs_x->GetParameter(0);
      B = 1e6 * fFdxdz_vs_x->GetParameter(1);
      C = 1e6 * fFdydz_vs_y->GetParameter(0);
      D = 1e6 * fFdydz_vs_y->GetParameter(1);
    }

    // Print the rotation constants
    ofstream outfile2(Form("GigaTracker-Rotation.run%06d_%04d-run%06d_%04d.dat",
			   GetRunID(), 0, GetRunID(), 9999));
    outfile2 << "# GTK rotation constants for run " << GetRunID() << endl;
    outfile2 << "# Momenta are corrected as follows: dx'=A+Bx, dy'=C+Dy" << endl;
    outfile2 << "# Format: Run A B C D [x10^6]" << endl;
    if (!RotationConstantsComputed)
      outfile2 << "# Warning: evaluation failed due to low statistics" << endl;
    outfile2 << Form("%06d %7.4f %7.4f %7.4f %7.4f", GetRunID(), A, B, C, D) << endl;
    outfile2.close();

    cout << user_normal() << Form
      ("Run %d dxdz_vs_x_par[x10^6] %7.4f %7.4f %7.4f %7.4f\n", GetRunID(), A, B, C, D);
  }

  // Station efficiencies
  for (Int_t IntBin=0; IntBin<fHStationEfficiency->GetNbinsX(); IntBin++) {
    fInt[IntBin] = fHStationEfficiency->GetXaxis()->GetBinCenter(IntBin+1);
    fZero[IntBin] = 0.0;
    Double_t N = fHStationEfficiency->GetBinContent(fHStationEfficiency->GetBin(IntBin+1, 1));
    for (Int_t i=0; i<3; i++) {
      Double_t n = fHStationEfficiency->GetBinContent(fHStationEfficiency->GetBin(IntBin+1, i+2));
      if (N>0) {
	fEff[i][IntBin]  = n/N;
	fdEff[i][IntBin] = sqrt(fEff[i][IntBin]*(1.0-fEff[i][IntBin])/N);
      }
      else {
	fEff[i][IntBin] = fdEff[i][IntBin] = 0.0;
      }
    }
  }

  // Produce the PDF report
  BuildPDFReport();

  if (fFdx1) delete fFdx1;
  if (fFdy1) delete fFdy1;
  if (fFdx2) delete fFdx2;
  if (fFdy2) delete fFdy2;
  if (fFdx3) delete fFdx3;
  if (fFdy3) delete fFdy3;
  if (fFdPx) delete fFdPx;
  if (fFdPy) delete fFdPy;
  if (fFPrat) delete fFPrat;
  if (fGdxdz_vs_x) delete fGdxdz_vs_x;
  if (fFdxdz_vs_x) delete fFdxdz_vs_x;
  if (fGdydz_vs_y) delete fGdydz_vs_y;
  if (fFdydz_vs_y) delete fFdydz_vs_y;
}

void GigaTrackerAlignment::BuildPDFReport() {
  TString OutputPDFFileName = fFineCorrections ?
    (fAnalyzerName + "-step2.pdf") : (fAnalyzerName + "-step1.pdf");
  gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed
  gStyle->SetOptStat(11);
  gStyle->SetOptFit(0);

  TLine *l = new TLine();
  l->SetLineWidth(2);
  l->SetLineColor(kGreen+2);

  fHdx1->SetLineColor(kBlue);
  fHdx1->SetFillColor(kYellow);
  fHdx2->SetLineColor(kBlue);
  fHdx2->SetFillColor(kYellow);
  fHdx3->SetLineColor(kBlue);
  fHdx3->SetFillColor(kYellow);
  fHdy1->SetLineColor(kBlue);
  fHdy1->SetFillColor(kYellow);
  fHdy2->SetLineColor(kBlue);
  fHdy2->SetFillColor(kYellow);
  fHdy3->SetLineColor(kBlue);
  fHdy3->SetFillColor(kYellow);
  fHdPx->SetLineColor(kBlue);
  fHdPx->SetFillColor(kYellow);
  fHdPy->SetLineColor(kBlue);
  fHdPy->SetFillColor(kYellow);
  fHPrat->SetLineColor(kBlue);
  fHPrat->SetFillColor(kYellow);

  TCanvas *Canvas = new TCanvas("GSAlignmentCanvas");
  Canvas->Print(Form(OutputPDFFileName + "["), "pdf"); // open output file

  // Station offsets
  if (!fFineCorrections) {
    Canvas->Divide(3,2);
    for (Int_t i=1; i<=6; i++) {
      Canvas->GetPad(i)->SetLeftMargin(0.04);
      Canvas->GetPad(i)->SetRightMargin(0.01);
      Canvas->GetPad(i)->SetTopMargin(0.06);
      Canvas->GetPad(i)->SetBottomMargin(0.10);
    }

    fHdx1->SetMinimum(0.0);
    fHdx2->SetMinimum(0.0);
    fHdx3->SetMinimum(0.0);
    fHdy1->SetMinimum(0.0);
    fHdy2->SetMinimum(0.0);
    fHdy3->SetMinimum(0.0);

    Canvas->cd(1); fHdx1->Draw(); fFdx1->Draw("same");
    l->DrawLine(fFdx1->GetParameter(1), 0.0, fFdx1->GetParameter(1), 0.4*fFdx1->GetParameter(0));
    Canvas->cd(2); fHdx2->Draw(); fFdx2->Draw("same");
    l->DrawLine(fFdx2->GetParameter(1), 0.0, fFdx2->GetParameter(1), 0.4*fFdx2->GetParameter(0));
    Canvas->cd(3); fHdx3->Draw(); fFdx3->Draw("same");
    l->DrawLine(fFdx3->GetParameter(1), 0.0, fFdx3->GetParameter(1), 0.4*fFdx3->GetParameter(0));
    Canvas->cd(4); fHdy1->Draw(); fFdy1->Draw("same");
    l->DrawLine(fFdy1->GetParameter(1), 0.0, fFdy1->GetParameter(1), 0.4*fFdy1->GetParameter(0));
    Canvas->cd(5); fHdy2->Draw(); fFdy2->Draw("same");
    l->DrawLine(fFdy2->GetParameter(1), 0.0, fFdy2->GetParameter(1), 0.4*fFdy2->GetParameter(0));
    Canvas->cd(6); fHdy3->Draw(); fFdy3->Draw("same");
    l->DrawLine(fFdy3->GetParameter(1), 0.0, fFdy3->GetParameter(1), 0.4*fFdy3->GetParameter(0));
    Canvas->Print(OutputPDFFileName, "pdf");
  }

  else {
    // Fine alignment page 1: general monitoring
    Canvas->Clear();
    Canvas->Divide(3,1);
    for (Int_t i=1; i<=3; i++) {
      Canvas->GetPad(i)->SetLeftMargin(0.04);
      Canvas->GetPad(i)->SetRightMargin(0.01);
      Canvas->GetPad(i)->SetTopMargin(0.15);
      Canvas->GetPad(i)->SetBottomMargin(0.15);
    }

    Canvas->cd(1); fHdPx->Draw(); fFdPx->Draw("same");
    l->DrawLine(fFdPx->GetParameter(1), 0.0, fFdPx->GetParameter(1), 0.4*fFdPx->GetParameter(0));
    Canvas->cd(2); fHdPy->Draw(); fFdPy->Draw("same");
    l->DrawLine(fFdPy->GetParameter(1), 0.0, fFdPy->GetParameter(1), 0.4*fFdPy->GetParameter(0));
    Canvas->cd(3); fHPrat->Draw(); fFPrat->Draw("same");
    l->DrawLine(fFPrat->GetParameter(1), 0.0, fFPrat->GetParameter(1), 0.4*fFPrat->GetParameter(0));
    Canvas->Print(OutputPDFFileName, "pdf");

    // Fine alignment page 2: (dx/dz vs x) and (dy/dz vs y)
    if (fGdxdz_vs_x) { // fine alignment is requested and successful
      Canvas->Clear();
      Canvas->Divide(2,1);
      for (Int_t i=1; i<=2; i++) {
	Canvas->GetPad(i)->SetLeftMargin(0.10);
	Canvas->GetPad(i)->SetRightMargin(0.03);
	Canvas->GetPad(i)->SetTopMargin(0.15);
	Canvas->GetPad(i)->SetBottomMargin(0.15);
      }
      Canvas->cd(1);
      fHdxdz_vs_x->Draw("colz"); fGdxdz_vs_x->Draw("e same"); fFdxdz_vs_x->Draw("same");
      Canvas->cd(2);
      fHdydz_vs_y->Draw("colz"); fGdydz_vs_y->Draw("e same"); fFdydz_vs_y->Draw("same");
      Canvas->Print(OutputPDFFileName, "pdf");
    }
  }

  // Page with tation efficiencies
  gStyle->SetOptStat(0);

  TGraphErrors *gEff1 = new TGraphErrors(12, fInt, &fEff[0][0], fZero, &fdEff[0][0]);
  TGraphErrors *gEff2 = new TGraphErrors(12, fInt, &fEff[1][0], fZero, &fdEff[1][0]);
  TGraphErrors *gEff3 = new TGraphErrors(12, fInt, &fEff[2][0], fZero, &fdEff[2][0]);
  gEff1->SetMarkerColor(kRed);
  gEff1->SetMarkerStyle(kFullCircle);
  gEff1->SetLineColor(kRed);
  gEff1->SetLineWidth(2);
  gEff2->SetMarkerColor(kBlue);
  gEff2->SetMarkerStyle(kFullCircle);
  gEff2->SetLineColor(kBlue);
  gEff2->SetLineWidth(2);
  gEff3->SetMarkerColor(kGreen+2);
  gEff3->SetMarkerStyle(kFullCircle);
  gEff3->SetLineColor(kGreen+2);
  gEff3->SetLineWidth(2);

  Canvas->Clear();
  TH2F *h = new TH2F("h", "GTK efficiency vs rate;Instanteous beam rate [MHz];Efficiency",
		     1, 0, 1000, 1, 0.95, 1);
  h->Draw();
  gEff1->Draw("same pe");
  gEff2->Draw("same pe");
  gEff3->Draw("same pe");

  // draw legend
  TLegend *leg = new TLegend(0.75,0.78,0.95,0.97);
  leg->SetFillColor(0);
  leg->AddEntry(gEff1, "GTK1", "pl");
  leg->AddEntry(gEff2, "GTK2", "pl");
  leg->AddEntry(gEff3, "GTK3", "pl");
  leg->Draw();

  Canvas->Print(OutputPDFFileName, "pdf");
  Canvas->Print(Form(OutputPDFFileName + "]"), "pdf"); // close file

  if (Canvas) delete Canvas;
  if (gEff1)  delete gEff1;
  if (gEff2)  delete gEff2;
  if (gEff3)  delete gEff3;
  if (h)      delete h;
  if (leg)    delete leg;
  gErrorIgnoreLevel = -1; // restore the default
}

Double_t GigaTrackerAlignment::Chi2_GTK_K3pi(TRecoGigaTrackerCandidate *cand) {
  Double_t rms[5] = {300.0, 3e-5, 3e-5, 2.0, 2.0}; // Resolutions [MeV/c, rad, rad, mm, mm]

  fPull[0] = cand->GetMomentum().Mag() - fK3piMomentum;
  fPull[1] = cand->GetMomentum().X()/cand->GetMomentum().Z() - fK3pidxdz;
  fPull[2] = cand->GetMomentum().Y()/cand->GetMomentum().Z() - fK3pidydz;
  fPull[3] = cand->GetPosition(2).X() - fK3piPos[2][0];
  fPull[4] = cand->GetPosition(2).Y() - fK3piPos[2][1];
  for (Int_t i=0; i<5; i++) fPull[i] /= rms[i];

  Double_t chi2 = 0.0;
  for (Int_t i=0; i<5; i++) chi2 += (fPull[i]*fPull[i]);
  return chi2;
}
