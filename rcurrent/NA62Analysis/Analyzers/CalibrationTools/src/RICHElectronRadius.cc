// ---------------------------------------------------------------
//
// History:
//
// Created by Viacheslav Duk (Viacheslav.Duk@cern.ch) 16.09.2016
// Revised by Viacheslav Duk (Viacheslav.Duk@cern.ch) 15.03.2018
//
// ---------------------------------------------------------------  

#include <stdlib.h>
#include <iostream>
#include "RICHElectronRadius.hh"
#include "Event.hh"
#include "Persistency.hh"
#include <TF1.h>

#include "BeamParameters.hh"
#include "DownstreamTrack.hh"
#include "SpectrometerCHODAssociationOutput.hh"
#include "LAVMatching.hh"
#include "SAVMatching.hh"
#include "RICHParameters.hh"
#include "RICHAcceptance.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class RICHElectronRadius
/// \Brief
/// Calculation of the the electron ring radius; this radius can be used to calculate the refractive index of neon.
/// \EndBrief
/// \Detailed
/// Electron selection is based on Ke3Selection analyzer enhanced with E/p cut to identify electrons.
/// The most reliable ring radius is obtained by the iterative single ring fit performed in the RICH reconstruction.
/// The value of this radius is printed out. The radius is extracted from a RICH PMTimeCandidate 
/// via the method TRecoRICHCandidate::GetRingRadiusSingleRing().
/// Results of other ring fits (both standalone and track seeded) are stored in histograms.
/// The analyzer can be run in two modes: 1) read the reconstructed data;
/// 2) read its own output (using the --histo command line option) to compute the electron ring radius.
/// \author Viacheslav Duk (Viacheslav.Duk@cern.ch)
/// \EndDetailed

RICHElectronRadius::RICHElectronRadius(Core::BaseAnalysis *ba) : Analyzer(ba, "RICHElectronRadius") {
  RequestTree("RICH",new TRecoRICHEvent);
  RequestTree("Cedar",new TRecoCedarEvent);
  fReadingData = kTRUE;
}

RICHElectronRadius::~RICHElectronRadius() {}

void RICHElectronRadius::InitHist() {

  fReadingData = GetIsTree();

  if (fReadingData) {
    cout << user_normal() << "Reading reconstructed data" << endl;
    BookHisto("hTrackAtRICHMirrors",
              new TH2F("hTrackAtRICHMirrors", "Track projection on the mirror plane;x [mm];y [mm]",
                       2200, -1100., 1100., 2200, -1100., 1100.));

    // main histogram (used at the second step)
    BookHisto("hRadius_IterativeSingleRing",
              new TH1F("hRadius_IterativeSingleRing",
		       "Electron ring radius, iterative single ring fit;P [GeV/c];Ring radius [mm]", 160, 160., 240.));

    // Iterative single ring fit, standalone (PMTimeCandidate)
    BookHisto("hNHitsVsP_IterativeSingleRing", 
	      new TH2F("hNHitsVsP_IterativeSingleRing", 
		       "NHits vs P, standalone iterative single ring fit;P [GeV/c];Number of hits", 66, 4.5, 70.5, 40, -0.5, 39.5));
    BookHisto("hRadiusVsP_IterativeSingleRing", 
	      new TH2F("hRadiusVsP_IterativeSingleRing", 
		       "R vs P, standalone iterative single ring fit;P [GeV/c];Ring radius [mm]", 66, 4.5, 70.5, 160, 160., 240.));
    BookHisto("hRingCentre_IterativeSingleRing", 
	      new TH2F("hRingCentre_IterativeSingleRing", 
		       "Ring centre, standalone iterative single ring fit;x [mm];y [mm]", 600, -500., 100., 600, -300., 300.));
    BookHisto("hNHitsVsRadius_IterativeSingleRing",
              new TH2F("hNHitsVsRadius_IterativeSingleRing", 
		       "NHits vs R, standalone iterative single ring fit;R [mm];Number of hits", 160, 160., 240., 40, -0.5, 39.5));
    BookHisto("hPullVsP_IterativeSingleRing",
              new TH2F("hPullVsP_IterativeSingleRing", 
		       "Pull vs P, standalone iterative single ring fit;P [GeV/c];Pull [mm]", 66, 4.5, 70.5, 500, -50., 50.));
    BookHisto("hDRingCentre_IterativeSingleRing",
              new TH2F("hDRingCentre_IterativeSingleRing", 
		       "Ring centre resolution, standalone iterative single ring fit;Dx [mm];Dy [mm]", 40, -20., 20., 40, -20., 20.));

    // Iterative single ring fit, standalone (PMTimeCandidate), single mirror selection
    BookHisto("hNHitsVsP_SingleMirror_IterativeSingleRing",
	      new TH2F("hNHitsVsP_SingleMirror_IterativeSingleRing", 
		       "NHits vs P, standalone iterative single ring fit (Single Mirror);P [GeV/c];Number of hits", 66, 4.5, 70.5, 40, -0.5, 39.5));
    BookHisto("hRadiusVsP_SingleMirror_IterativeSingleRing",
              new TH2F("hRadiusVsP_SingleMirror_IterativeSingleRing", 
		       "R vs P, standalone iterative single ring fit (Single Mirror);P [GeV/c];Ring radius [mm]", 66, 4.5, 70.5, 160, 160., 240.));
    BookHisto("hRingCentre_SingleMirror_IterativeSingleRing",
              new TH2F("hRingCentre_SingleMirror_IterativeSingleRing", 
		       "Ring centre, standalone iterative single ring fit (Single Mirror);x [mm];y [mm]", 600, -500., 100., 600, -300., 300.));
    BookHisto("hNHitsVsRadius_SingleMirror_IterativeSingleRing",
              new TH2F("hNHitsVsRadius_SingleMirror_IterativeSingleRing", 
		       "NHits vs R, standalone iterative single ring fit;R [mm];Number of hits", 160, 160., 240., 40, -0.5, 39.5));
    BookHisto("hPullVsP_SingleMirror_IterativeSingleRing",
              new TH2F("hPullVsP_SingleMirror_IterativeSingleRing", 
		       "Pull vs P, standalone iterative single ring fit (Single Mirror);P [GeV/c];Pull [mm]", 66, 4.5, 70.5, 500, -50., 50.));
    BookHisto("hDRingCentre_SingleMirror_IterativeSingleRing",
              new TH2F("hDRingCentre_SingleMirror_IterativeSingleRing", 
		       "Ring centre resolution, standalone iterative single ring fit (Single Mirror);Dx [mm];Dy [mm]", 40, -20., 20., 40, -20., 20.));

    // Single ring fit, standalone (PMTimeCandidate)
    BookHisto("hNHitsVsP_SingleRing",
              new TH2F("hNHitsVsP_SingleRing", 
		       "NHits vs P, standalone single ring fit;P [GeV/c];Number of hits", 66, 4.5, 70.5, 40, -0.5, 39.5));
    BookHisto("hRadiusVsP_SingleRing",
              new TH2F("hRadiusVsP_SingleRing", 
		       "R vs P, standalone single ring fit;P [GeV/c];Ring radius [mm]", 66, 4.5, 70.5, 160, 160., 240.));
    BookHisto("hRingCentre_SingleRing",
              new TH2F("hRingCentre_SingleRing", 
		       "Ring centre, standalone single ring fit;x [mm];y [mm]", 600, -500., 100., 600, -300., 300.));
    BookHisto("hNHitsVsRadius_SingleRing",
              new TH2F("hNHitsVsRadius_SingleRing", 
		       "NHits vs R, standalone single ring fit;R [mm];Number of hits", 160, 160., 240., 40, -0.5, 39.5));
    BookHisto("hPullVsP_SingleRing",
              new TH2F("hPullVsP_SingleRing", 
		       "Pull vs P, standalone single ring fit;P [GeV/c];Pull [mm]", 66, 4.5, 70.5, 500, -50., 50.));

    // Single ring fit, track seeded (SpectrometerRICHAssociationTrkSeeded)
    BookHisto("hNHitsVsP_SingleRingTrkSeeded",
              new TH2F("hNHitsVsP_SingleRingTrkSeeded", 
		       "NHits vs P, track seeded single ring fit;P [GeV/c];Number of hits", 66, 4.5, 70.5, 40, -0.5, 39.5));
    BookHisto("hRadiusVsP_SingleRingTrkSeeded",
              new TH2F("hRadiusVsP_SingleRingTrkSeeded", 
		       "R vs P, track seeded single ring fit;P [GeV/c];Ring radius [mm]", 66, 4.5, 70.5, 160, 160., 240.));
    BookHisto("hRingCentre_SingleRingTrkSeeded",
              new TH2F("hRingCentre_SingleRingTrkSeeded", 
		       "Ring centre, track seeded single ring fit;x [mm];y [mm]", 600, -500., 100., 600, -300., 300.));
    BookHisto("hNHitsVsRadius_SingleRingTrkSeeded",
              new TH2F("hNHitsVsRadius_SingleRingTrkSeeded", 
		       "NHits vs R, track seeded single ring fit;R [mm];Number of hits", 160, 160., 240., 40, -0.5, 39.5));
    BookHisto("hPullVsP_SingleRingTrkSeeded",
              new TH2F("hPullVsP_SingleRingTrkSeeded", 
		       "Pull vs P, track seeded single ring fit;P [GeV/c];Pull [mm]", 66, 4.5, 70.5, 500, -50., 50.));

    // Multi-ring fit, standalone (RingCandidate)
    BookHisto("hNHitsVsP_MultiRing",
              new TH2F("hNHitsVsP_MultiRing", 
		       "NHits vs P, standalone multi-ring fit;P [GeV/c];Number of hits", 66, 4.5, 70.5, 40, -0.5, 39.5));
    BookHisto("hRadiusVsP_MultiRing",
              new TH2F("hRadiusVsP_MultiRing", 
		       "R vs P, standalone multi-ring fit;P [GeV/c];Ring radius [mm]", 66, 4.5, 70.5, 160, 160., 240.));
    BookHisto("hRingCentre_MultiRing",
              new TH2F("hRingCentre_MultiRing", 
		       "Ring centre, standalone multi-ring fit;x [mm];y [mm]", 600, -500., 100., 600, -300., 300.));
    BookHisto("hNHitsVsRadius_MultiRing",
              new TH2F("hNHitsVsRadius_MultiRing", 
		       "NHits vs R, standalone multi-ring fit;R [mm];Number of hits", 160, 160., 240., 40, -0.5, 39.5));
    BookHisto("hPullVsP_MultiRing",
              new TH2F("hPullVsP_MultiRing", 
		       "Pull vs P, standalone multi-ring fit;P [GeV/c];Pull [mm]", 66, 4.5, 70.5, 500, -50., 50.));

    // Multi-ring fit, track seeded (SpectrometerRICHAssociation)
    BookHisto("hNHitsVsP_Likelihood",
              new TH2F("hNHitsVsP_Likelihood", 
		       "NHits vs P, track seeded multi-ring fit;P [GeV/c];Number of hits", 66, 4.5, 70.5, 40, -0.5, 39.5));
    BookHisto("hRadiusVsP_Likelihood",
              new TH2F("hRadiusVsP_Likelihood", 
		       "R vs P, track seeded multi-ring fit;P [GeV/c];Ring radius [mm]", 66, 4.5, 70.5, 160, 160., 240.));
    BookHisto("hRingCentre_Likelihood",
              new TH2F("hRingCentre_Likelihood", 
		       "Ring centre, track seeded multi-ring fit;x [mm];y [mm]", 600, -500., 100., 600, -300., 300.));
    BookHisto("hNHitsVsRadius_Likelihood",
              new TH2F("hNHitsVsRadius_Likelihood", 
		       "NHits vs R, track seeded multi-ring fit;R [mm];Number of hits", 160, 160., 240., 40, -0.5, 39.5));
    BookHisto("hPullVsP_Likelihood",
              new TH2F("hPullVsP_Likelihood", 
		       "Pull vs P, track seeded multi-ring fit;P [GeV/c];Pull [mm]", 66, 4.5, 70.5, 500, -50., 50.));

  }
  else {
    cout << user_normal() << "Reading my own output" << endl;
    fHElectronRadius = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "hRadius_IterativeSingleRing", true));
  }
}

void RICHElectronRadius::Process(Int_t) {

  if (!fReadingData) return; // no action if reading its own output in --histo mode

  TRecoCedarEvent*  CEDARevent  = GetEvent<TRecoCedarEvent>();
  TRecoRICHEvent*   RICHevent   = GetEvent<TRecoRICHEvent>();

  std::vector<DownstreamTrack> Tracks =
    *GetOutput<std::vector<DownstreamTrack>>("DownstreamTrackBuilder.Output");

  Int_t  RunNumber = GetRunID();
  time_t BurstTime = GetEventHeader()->GetBurstTime();
  Double_t PredictedRingRadius = 189.6; 
  if (!GetWithMC()) { // data: read parameters from the DB
    PredictedRingRadius = RICHParameters::GetInstance()->GetElectronRingRadius(RunNumber, BurstTime);
  }

  //////////////////////////////////////////////
  // Ke3 selection
  /////////////////////////////////////////////

  // check standard Ke3 selection
  Bool_t Ke3Selected = *(Bool_t*)GetOutput("Ke3Selection.EventSelected");
  if (!Ke3Selected) return;

  // at least one good candidate in Cedar
  Int_t NCedarCandidates = CEDARevent->GetNCandidates();
  Int_t NGoodCedarCandidates = 0;
  for (Int_t i=0; i<NCedarCandidates; i++) {
    TRecoCedarCandidate* CedarCandidate = static_cast<TRecoCedarCandidate*>(CEDARevent->GetCandidate(i));
    if (CedarCandidate->GetNSectors()>4) NGoodCedarCandidates++;
  }
  if (!NGoodCedarCandidates) return;

  // find good track ID (the same definition as in Ke3Selection)
  Int_t GoodTrackID = -1;
  for(UInt_t iTrack=0; iTrack<Tracks.size();iTrack++){

    TRecoSpectrometerCandidate* Scand = Tracks[iTrack].GetSpectrometerCandidate();
    Int_t    Q            = Tracks[iTrack].GetCharge();
    Double_t Ptrack       = Tracks[iTrack].GetMomentum(); // spectrometer calibration included
    Double_t Ptrackbefore = Tracks[iTrack].GetMomentumBeforeFit();
    Double_t Chi2track    = Tracks[iTrack].GetChi2();
    Double_t cda          = Tracks[iTrack].GetBeamAxisCDA();
    Double_t Zvtx         = Tracks[iTrack].GetBeamAxisVertex().Z();

    if (Q!=1) continue;
    if (Chi2track>20.0) continue;
    if (Scand->GetNChambers()!=4) continue;
    if (fabs(Ptrack-Ptrackbefore)>20000.0) continue; // 20 GeV 
    if (Ptrack<5000 || Ptrack>70000) continue;
    if (Zvtx<110000 || Zvtx>180000) continue;
    if (cda>25) continue;

    GoodTrackID = iTrack;
  }
  // safety check
  if (GoodTrackID<0) return;

  // define a good track candidate
  TRecoSpectrometerCandidate* GoodTrackCandidate = Tracks[GoodTrackID].GetSpectrometerCandidate();

  // track momentum in GeV
  Double_t TrackMomentumInGeV = Tracks[GoodTrackID].GetMomentum()*0.001;

  // define track time as a CHOD time
  if (!Tracks[GoodTrackID].CHODAssociationExists()) return;
  Double_t TrackTime = Tracks[GoodTrackID].GetCHODTime();

  // track-Cedar time cut: |dt| < 1 ns
  Double_t DtTrackCedar   =  999.999;
  for (Int_t i=0; i<NCedarCandidates; i++) {
    TRecoCedarCandidate* CedarCandidate = static_cast<TRecoCedarCandidate*>(CEDARevent->GetCandidate(i));
    if (CedarCandidate->GetNSectors()<5) continue;
    Double_t Dt = TrackTime - CedarCandidate->GetTime();
    if (fabs(Dt) < fabs(DtTrackCedar)) {
      DtTrackCedar = Dt;
    }
  }
  if (fabs(DtTrackCedar)>1.0) return;

  // K2pi background rejection (from 2-body kinematics)
  TVector3 KaonThreeMomentum = BeamParameters::GetInstance()->GetBeamThreeMomentum();
  TLorentzVector KaonMomentum;
  KaonMomentum.SetVectM(KaonThreeMomentum, MKCH);
  TLorentzVector PionMomentum;
  PionMomentum.SetVectM(GoodTrackCandidate->GetThreeMomentumBeforeMagnet(), MPI);
  Double_t Mmiss2Pi = (KaonMomentum-PionMomentum).M2();

  if (Mmiss2Pi*1E-06>0. && Mmiss2Pi*1E-06<0.04) return;


  //////////////////////////////////////////////////////
  // PID cuts
  /////////////////////////////////////////////////////

  // Cuts on LKr cluster energy and E/p
  if (Tracks[GoodTrackID].GetLKrEnergy()<1000.) return;
  if (Tracks[GoodTrackID].GetLKrEoP()<0.96 || Tracks[GoodTrackID].GetLKrEoP()>1.03) return;


  /////////////////////////////////////////////////////////////////
  // RICH selection
  /////////////////////////////////////////////////////////////////

  // select events with a good time candidate in the RICH
  if (RICHevent->GetNPMTimeCandidates()!=1) return;

  TRecoRICHCandidate* RICHCand = static_cast<TRecoRICHCandidate*>(RICHevent->GetPMTimeCandidate(0));

  // new cuts on the acceptance
  Double_t SingleRingFitRadius = RICHCand->GetRingRadiusSingleRing();
  if (!RICHAcceptance::GetInstance()->GetRICHMirrorPlaneAcceptance(GoodTrackCandidate, SingleRingFitRadius, 5.)) return; // 5 mm margin
  if (!RICHAcceptance::GetInstance()->GetRICHBeamPipeAcceptance(GoodTrackCandidate,    SingleRingFitRadius))     return;
  if (!RICHAcceptance::GetInstance()->GetRICHPMPlaneAcceptance(GoodTrackCandidate,     SingleRingFitRadius, 0.)) return; // 0 mm margin

  // at least 4 hits in the iterative single ring fit
  if (RICHCand->GetNHitsSingleRing()<4) return;

  // cut on the track slope difference (measured by the Spectrometer and by the RICH) 
  Double_t SlopeX_RICH = RICHCand->GetRingCenterSingleRing().X()/17000.;
  Double_t SlopeY_RICH = RICHCand->GetRingCenterSingleRing().Y()/17000.;
  if(fabs(SlopeX_RICH - GoodTrackCandidate->GetSlopeXAfterMagnet()) > 0.0008) return;
  if(fabs(SlopeY_RICH - GoodTrackCandidate->GetSlopeYAfterMagnet()) > 0.0008) return;

  // track projection on the mirror plane
  Double_t fZRICHMirror = GeometricAcceptance::GetInstance()->GetZRICHMirror();
  Double_t x    = GoodTrackCandidate->xAt(fZRICHMirror);
  Double_t y    = GoodTrackCandidate->yAt(fZRICHMirror);
  TVector2 TrackAtMirrors(x, y);
  FillHisto("hTrackAtRICHMirrors", x, y);

  // single mirror flag (hexagonal mirrors only)
  Bool_t SingleMirrorFlag = false;
  for (Int_t i=1; i<23; i++) {
    TVector2 MirrorCentre(RICHParameters::GetInstance()->GetMirrorCentreX(i), RICHParameters::GetInstance()->GetMirrorCentreY(i));
    TVector2 Dr = TrackAtMirrors-MirrorCentre;
    if (Dr.Mod()<(300.-SingleRingFitRadius)) { // 300 mm is a maximal radius of a circle inside a hexagonal mirror
      SingleMirrorFlag = true;
    }
  }
  // difference between the real and the predicted ring centre
  TVector2 DRingCentre = RICHCand->GetRingCenterSingleRing() - Tracks[GoodTrackID].GetRICHRingPredictedCentrePosition();


  // Iterative single ring fit, standalone (PMTimeCandidate)
  FillHisto("hRadius_IterativeSingleRing", SingleRingFitRadius);

  FillHisto("hNHitsVsP_IterativeSingleRing",      TrackMomentumInGeV, RICHCand->GetNHitsSingleRing());
  FillHisto("hRadiusVsP_IterativeSingleRing",     TrackMomentumInGeV, SingleRingFitRadius);
  FillHisto("hRingCentre_IterativeSingleRing",    RICHCand->GetRingCenterSingleRing().X(), RICHCand->GetRingCenterSingleRing().Y());
  FillHisto("hNHitsVsRadius_IterativeSingleRing", SingleRingFitRadius, RICHCand->GetNHitsSingleRing());
  FillHisto("hPullVsP_IterativeSingleRing",       TrackMomentumInGeV, (SingleRingFitRadius-PredictedRingRadius)*sqrt(RICHCand->GetNHitsSingleRing()-3));
  FillHisto("hDRingCentre_IterativeSingleRing",   DRingCentre.X(), DRingCentre.Y());

  if (SingleMirrorFlag) {
    FillHisto("hNHitsVsP_SingleMirror_IterativeSingleRing",      TrackMomentumInGeV, RICHCand->GetNHitsSingleRing());
    FillHisto("hRadiusVsP_SingleMirror_IterativeSingleRing",     TrackMomentumInGeV, SingleRingFitRadius);
    FillHisto("hRingCentre_SingleMirror_IterativeSingleRing",    RICHCand->GetRingCenterSingleRing().X(), RICHCand->GetRingCenterSingleRing().Y());
    FillHisto("hNHitsVsRadius_SingleMirror_IterativeSingleRing", SingleRingFitRadius, RICHCand->GetNHitsSingleRing());
    FillHisto("hPullVsP_SingleMirror_IterativeSingleRing",       TrackMomentumInGeV, (SingleRingFitRadius-PredictedRingRadius)*sqrt(RICHCand->GetNHitsSingleRing()-3));
    FillHisto("hDRingCentre_SingleMirror_IterativeSingleRing",   DRingCentre.X(), DRingCentre.Y());

  }

  // Single ring fit, standalone (PMTimeCandidate)
  FillHisto("hNHitsVsP_SingleRing",      TrackMomentumInGeV, RICHCand->GetNHits());
  FillHisto("hRadiusVsP_SingleRing",     TrackMomentumInGeV, RICHCand->GetRingRadius());
  FillHisto("hRingCentre_SingleRing",    RICHCand->GetRingCenter().X(), RICHCand->GetRingCenter().Y());
  FillHisto("hNHitsVsRadius_SingleRing", RICHCand->GetRingRadius(), RICHCand->GetNHits());
  FillHisto("hPullVsP_SingleRing",       TrackMomentumInGeV, (RICHCand->GetRingRadius()-PredictedRingRadius)*sqrt(RICHCand->GetNHits()-3));

  // Single ring fit, track seeded (SpectrometerRICHAssociationTrkSeeded)
  FillHisto("hNHitsVsP_SingleRingTrkSeeded",      TrackMomentumInGeV, Tracks[GoodTrackID].GetRICHSingleRingTrkSeededNHits());
  FillHisto("hRadiusVsP_SingleRingTrkSeeded",     TrackMomentumInGeV, Tracks[GoodTrackID].GetRICHSingleRingTrkSeededRadius());
  FillHisto("hRingCentre_SingleRingTrkSeeded",    Tracks[GoodTrackID].GetRICHSingleRingTrkSeededCentrePosition().X(),
            Tracks[GoodTrackID].GetRICHSingleRingTrkSeededCentrePosition().Y());
  FillHisto("hNHitsVsRadius_SingleRingTrkSeeded", Tracks[GoodTrackID].GetRICHSingleRingTrkSeededRadius(), Tracks[GoodTrackID].GetRICHSingleRingTrkSeededNHits());
  FillHisto("hPullVsP_SingleRingTrkSeeded",       TrackMomentumInGeV,
            (Tracks[GoodTrackID].GetRICHSingleRingTrkSeededRadius()-PredictedRingRadius)*sqrt(Tracks[GoodTrackID].GetRICHSingleRingTrkSeededNHits()-3));

  // Multi-ring fit, standalone (RingCandidate)
  if (RICHevent->GetNRingCandidates()==1) {
    TRecoRICHCandidate* RICHRingCand = static_cast<TRecoRICHCandidate*>(RICHevent->GetRingCandidate(0));
    FillHisto("hNHitsVsP_MultiRing",      TrackMomentumInGeV, RICHRingCand->GetNHits());
    FillHisto("hRadiusVsP_MultiRing",     TrackMomentumInGeV, RICHRingCand->GetRingRadius());
    FillHisto("hRingCentre_MultiRing",    RICHRingCand->GetRingCenter().X(), RICHRingCand->GetRingCenter().Y());
    FillHisto("hNHitsVsRadius_MultiRing", RICHRingCand->GetRingRadius(), RICHRingCand->GetNHits());
    FillHisto("hPullVsP_MultiRing",       TrackMomentumInGeV, (RICHRingCand->GetRingRadius()-PredictedRingRadius)*sqrt(RICHRingCand->GetNHits()-3));
  }

  // Multi-ring fit, track seeded (SpectrometerRICHAssociation)
  Int_t MostLikely = Tracks[GoodTrackID].GetRICHMostLikelyHypothesis();
  FillHisto("hNHitsVsP_Likelihood",      TrackMomentumInGeV, Tracks[GoodTrackID].GetRICHRingNHits(MostLikely));
  FillHisto("hRadiusVsP_Likelihood",     TrackMomentumInGeV, Tracks[GoodTrackID].GetRICHRingRadius());
  FillHisto("hRingCentre_Likelihood",    Tracks[GoodTrackID].GetRICHRingCentrePosition().X(), Tracks[GoodTrackID].GetRICHRingCentrePosition().Y());
  FillHisto("hNHitsVsRadius_Likelihood", Tracks[GoodTrackID].GetRICHRingRadius(), Tracks[GoodTrackID].GetRICHRingNHits(MostLikely));
  FillHisto("hPullVsP_Likelihood",       TrackMomentumInGeV, 
	    (Tracks[GoodTrackID].GetRICHRingRadius()-PredictedRingRadius)*sqrt(Tracks[GoodTrackID].GetRICHRingNHits(MostLikely)-3));
  
}

void RICHElectronRadius::EndOfJobUser() {

  // Data mode: save output
  if (fReadingData) {
    SaveAllPlots();
    return;
  }
  if (!fHElectronRadius) { // Histo mode required but no histograms found
    cout << user_normal() << "Asked to read my own output but cannot found it" << endl;
    return;
  }

  // Histo mode: calculate the electron ring radius
  Double_t fVElectronRadius = 0.0;
  Double_t fVElectronRadiusError = 0.0;
  if (fHElectronRadius->Integral()>600) { // need some statistics

    Int_t MaximumBin = fHElectronRadius->GetMaximumBin();            // Find maximum bin
    Double_t FitCenter = fHElectronRadius->GetBinCenter(MaximumBin); // Location of the most populated bin
    Double_t rgMin  = FitCenter - 3.0;                               // define a range +/- 2sigma from peak
    Double_t rgMax  = FitCenter + 3.0;

    TF1* FitFunction = new TF1("f1", "gaus", rgMin, rgMax);
    fHElectronRadius->Fit(FitFunction, "R0Q");
    fVElectronRadius = FitFunction->GetParameter(1);
    fVElectronRadiusError = FitFunction->GetParError(1);

    // build pdf report
    TString OutputPDFFileName = fAnalyzerName + ".pdf";
    gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed
    gStyle->SetOptStat(11);
    TCanvas *Canvas0 = new TCanvas("RICHElectronRadiusCanvas0");
    Canvas0->Print(Form(OutputPDFFileName + "["), "pdf"); // open file
    fHElectronRadius->DrawCopy();
    FitFunction->Draw("same");
    Canvas0->Print(OutputPDFFileName, "pdf");
    Canvas0->Print(Form(OutputPDFFileName + "]"), "pdf"); // close file
    gErrorIgnoreLevel = -1; // restore the default

    delete FitFunction;
  }

  cout << user_normal() << "Statistics (N_ev); RICH electron ring radius and its error [mm]: " <<
    fHElectronRadius->Integral() << " " << fVElectronRadius << " " << fVElectronRadiusError << endl;
}
