// ---------------------------------------------------------------
//
// History:
//
// Association to records added  (karim.massri@cern.ch) 2016-12-07
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-09-15
//
// ---------------------------------------------------------------

/// \class SpectrometerLKrAssociation
/// \Brief
/// Geometrical and time association of LKr candidates to Spectrometer candidates
/// \EndBrief
/// \Detailed
/// For spectrometer each track, an in-time LKr candidate (cluster) geometrically associated
/// to the track is found, and the track E/p is computed.
/// For track timing, the CHOD, NewCHOD or spectrometer "leading time" are used
/// (in this order of precedence, whichever is defined).
/// If several in-time LKr clusters are geometrically associated,
/// then the spacially closest of them is considered.
/// ClusterID=-1 means no associated cluster is found. An example of use:
/// \code
/// std::vector<SpectrometerLKrAssociationOutput> SpecLKR =
///   *(std::vector<SpectrometerLKrAssociationOutput>*)GetOutput("SpectrometerLKrAssociation.Output");
/// for (UInt_t i=0; i<SpecLKR.size(); i++) {
///   SpectrometerLKrAssociationOutput sl = SpecLKR[i];
///   sl.Print();
/// }
/// \endcode
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include "SpectrometerLKrAssociation.hh"
#include "SpectrometerCHODAssociation.hh"
#include "SpectrometerNewCHODAssociation.hh"
#include "LKrClusterCorrectionFunctions.hh"
#include "Event.hh"
#include "Persistency.hh"

using namespace NA62Analysis;
using namespace NA62Constants;

SpectrometerLKrAssociation::SpectrometerLKrAssociation(Core::BaseAnalysis *ba) :
  Analyzer(ba, "SpectrometerLKrAssociation") {
  if (!GetIsTree()) return;

  AddParam("MaxTrackClusterDistance",  &fMaxTrackClusterDistance,  50.0); // [mm]
  AddParam("MaxTrackClusterDeltaTime", &fMaxTrackClusterDeltaTime, 10.0); // [ns]
  fZLKr = GeometricAcceptance::GetInstance()->GetZLKr();
  RequestTree("LKr",          new TRecoLKrEvent,          "Reco");
  RequestTree("Spectrometer", new TRecoSpectrometerEvent, "Reco");

  fMonitoringHistograms = false;
}

void SpectrometerLKrAssociation::InitHist() {
  if (!GetIsTree()) return;

  if (fMonitoringHistograms) {
    BookHisto("hNtracksNclusters", new
	      TH2F("hNtracksNclusters", "N(clusters) vs N(Tracks);Number of tracks;Number of LKr clusters",
		   8, -0.5, 7.5, 8, -0.5, 7.5));
    BookHisto("hXYcluster", new
	      TH2F("hXYcluster", "(x,y) of LKr clusters;x [mm];y [mm]",
		   60, -1200, 1200, 60, -1200, 1200));
  }
  BookHisto("hDistanceTrackCluster", new
	    TH1F("hDistanceTrackCluster", "Track-cluster distance;Distance [mm]",
		 100, 0.0, fMaxTrackClusterDistance));
  BookHisto("hdXTrackCluster", new
	    TH1F("hdXTrackCluster", "Track-cluster dX;dX [mm]",
		 100, -fMaxTrackClusterDistance, fMaxTrackClusterDistance));
  BookHisto("hdYTrackCluster", new
	    TH1F("hdYTrackCluster", "Track-cluster dY;dY [mm]",
		 100, -fMaxTrackClusterDistance, fMaxTrackClusterDistance));
  BookHisto("hClusterTrackDeltaTimeVsEnergy", new
            TH2F("hClusterTrackDeltaTimeVsEnergy","Cluster-track time difference vs energy;[GeV];[ns]",
                 50, 0, 50, 50, -50.0, 50.0));
  BookHisto("hClusterTrackDeltaTime", new
            TH1F("hClusterTrackDeltaTime","Cluster-track time difference;[ns]",
                 100, -50.0, 50.0));
  BookHisto("hClusterTrackDeltaTime1GeV", new
            TH1F("hClusterTrackDeltaTime1GeV","Cluster-track time difference (E>1GeV);[ns]",
                 100, -50.0, 50.0));
  if (fMonitoringHistograms) {
    BookHisto("hEnergyNCells", new
	      TH2F("hEnergyNCells", "Number of cells vs energy;Energy [GeV];Number of cells",
		   60, 0, 60, 50, 0.5, 100.5));
    BookHisto("hEnergyPerCell", new
	      TH1F("hEnergyPerCell", "Energy / number of cells;Mean energy/cell [GeV]",
		   150, 0, 1.5));
  }
  BookHisto("hEoP", new TH1F
	    ("hEoP", "Energy-to-momentum ratio (E/p);E/p", 125, 0.0, 1.25));
  BookHisto("hEoPTotal", new TH1F
	    ("hEoPTotal", "E/p summed over matching clusters;E/p", 125, 0.0, 1.25));
}

void SpectrometerLKrAssociation::InitOutput() {
  if (!GetIsTree()) return;
  RegisterOutput("Output", &fContainer);
}

void SpectrometerLKrAssociation::Process(Int_t) {
  if (!GetIsTree()) return;

  SetOutputState("Output", kOValid);
  fContainer.clear();

  std::vector<SpectrometerCHODAssociationOutput> SpecCHOD =
    *GetOutput<std::vector<SpectrometerCHODAssociationOutput>>("SpectrometerCHODAssociation.Output");
  std::vector<SpectrometerNewCHODAssociationOutput> SpecNewCHOD =
    *GetOutput<std::vector<SpectrometerNewCHODAssociationOutput>>("SpectrometerNewCHODAssociation.Output");

  TRecoLKrEvent* LKrEvent = GetEvent<TRecoLKrEvent>();
  TRecoSpectrometerEvent* SpectrometerEvent = GetEvent<TRecoSpectrometerEvent>();

  // Monitoring: switched off by default to save memory
  if (fMonitoringHistograms) {
    FillHisto("hNtracksNclusters", SpectrometerEvent->GetNCandidates(), LKrEvent->GetNCandidates());
    for (Int_t i=0; i<LKrEvent->GetNCandidates(); i++) {
      TRecoLKrCandidate* LKrCand = static_cast<TRecoLKrCandidate*>(LKrEvent->GetCandidate(i));
      FillHisto("hXYcluster", LKrCand->GetClusterX(), LKrCand->GetClusterY());
    }
  }

  for (Int_t iTr=0; iTr<SpectrometerEvent->GetNCandidates(); iTr++) {
    SpectrometerLKrAssociationOutput Asso;
    TRecoSpectrometerCandidate* SCand = static_cast<TRecoSpectrometerCandidate*>(SpectrometerEvent->GetCandidate(iTr));
    Double_t xt = SCand->xAtAfterMagnet(fZLKr);
    Double_t yt = SCand->yAtAfterMagnet(fZLKr);

    // Track time: use CHOD, NewCHOD or spectrometer "leading time" (in this order of precedence)
    Double_t TrackTime = SCand->GetLeadingTime(); // 1.2ns resolution, as opposed to 5ns for GetTime()
    if (SpecCHOD[iTr].isAssociated())
      TrackTime = SpecCHOD[iTr].GetBestAssociationRecord()->GetCHODCandidate()->GetTime();
    else if (SpecNewCHOD[iTr].isAssociated())
      TrackTime = SpecNewCHOD[iTr].GetBestAssociationRecord()->GetRecoHitTime();

    Int_t    ClosestClusterRecID = -1;
    Double_t RMin                = 99999.9; // distance to the closest cluster
    Double_t dx                  = 99999.9;
    Double_t dy                  = 99999.9;
    Double_t TotalEnergy         = 0.0;
    for (Int_t iClus=0; iClus<LKrEvent->GetNCandidates(); iClus++) {
      TRecoLKrCandidate* LKrCand = static_cast<TRecoLKrCandidate*>(LKrEvent->GetCandidate(iClus));
      Double_t xc = LKrCand->GetClusterX();
      Double_t yc = LKrCand->GetClusterY();
      Double_t R  = sqrt((xt-xc)*(xt-xc)+(yt-yc)*(yt-yc)); // track-cluster distance

      // Total energy: use a larger search radius to account for LKr misalignment, and no timing cuts
      if (R<fMaxTrackClusterDistance+5.0) TotalEnergy += LKrCand->GetClusterEnergy();

      // Use the standard search radius and check track/cluster timing for all other purposes
      if (R<fMaxTrackClusterDistance) {
	FillHisto("hClusterTrackDeltaTimeVsEnergy",
                  1e-3*LKrCand->GetClusterEnergy(), LKrCand->GetTime()-TrackTime);
        FillHisto("hClusterTrackDeltaTime", LKrCand->GetTime()-TrackTime);
	if (LKrCand->GetClusterEnergy()>1000.)
	  FillHisto("hClusterTrackDeltaTime1GeV", LKrCand->GetTime()-TrackTime);

        if (fabs(LKrCand->GetTime()-TrackTime)<fMaxTrackClusterDeltaTime) {
	  if (R<RMin) {
	    RMin = R;
	    dx   = xt-xc;
	    dy   = yt-yc;
	    ClosestClusterRecID = Asso.GetNAssociationRecords();
	  }
	  SpectrometerLKrAssociationRecord Rec(iClus, LKrCand);
	  Rec.SetEoP(LKrCand->GetClusterEnergy()/SCand->GetMomentum());
	  Rec.SetTrackClusterDistance(R);
	  Asso.AddAssociationRecord(Rec);
	}
      }
    }
    Asso.SetBestAssociationRecordID(ClosestClusterRecID);
    Asso.SetTotalEnergy(TotalEnergy);
    Asso.SetTotalEoP(TotalEnergy/SCand->GetMomentum());
    fContainer.push_back(Asso);

    if (Asso.isAssociated()) {
      FillHisto("hDistanceTrackCluster", RMin);
      FillHisto("hdXTrackCluster", dx);
      FillHisto("hdYTrackCluster", dy);
      FillHisto("hEoP", Asso.GetBestAssociationRecord()->GetEoP());
      FillHisto("hEoPTotal", Asso.GetTotalEoP());
      if (fMonitoringHistograms) {
	Double_t Energy = Asso.GetBestAssociationRecord()->GetLKrCandidate()->GetClusterEnergy();
	Int_t    Ncells = Asso.GetBestAssociationRecord()->GetLKrCandidate()->GetNCells();
	FillHisto("hEnergyNCells",  1e-3*Energy, Ncells);
	FillHisto("hEnergyPerCell", 1e-3*Energy/(1.0*Ncells));
      }
    }
  }
}

void SpectrometerLKrAssociation::EndOfJobUser() {
  if (!GetIsTree()) return;
  SaveAllPlots();
}
