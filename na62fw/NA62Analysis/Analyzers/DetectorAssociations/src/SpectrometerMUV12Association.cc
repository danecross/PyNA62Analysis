// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-10-28
//
// ---------------------------------------------------------------

/// \class SpectrometerMUV12Association
/// \Brief
/// Geometrical association of MUV1 and MUV2 candidates to Spectrometer candidates
/// \EndBrief
/// \Detailed
/// For each track, find the MUV1 and MUV2 candidates geometrically associated
/// to the track. ClusterID=-1 means no associated cluster is found. An example of use:
/// \code
/// std::vector<SpectrometerMUV1AssociationOutput> SpecMUV12 =
///   *(std::vector<SpectrometerMUV12AssociationOutput>*)GetOutput("SpectrometerMUV12Association.Output");
/// for (UInt_t i=0; i<SpecMUV12.size(); i++) {
///   SpectrometerMUV1AssociationOutput sm = SpecMUV12[i];
///   sm.Print();
/// }
/// \endcode
/// The results of this association are not propagated to DownstreamTrack;
/// the output of the more elaborate SpectrometerCalorimetersAssociation tool is used.
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include "SpectrometerMUV12Association.hh"
#include "Event.hh"
#include "Persistency.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

SpectrometerMUV12Association::SpectrometerMUV12Association(Core::BaseAnalysis *ba) : Analyzer(ba, "SpectrometerMUV12Association") {
  if (!GetIsTree()) return;

  ///////////////////////////////////////////////////////////////////////////////
  // Minimum track-cluster distances for matching in MUV1 and MUV2 are set below.
  // The scintillator strip widths are 60 mm (54 mm for 4 inner strips) in MUV1,
  // and 119 mm in MUV2. The matching radii and procedure need optimization.

  AddParam("MaxTrackClusterDistanceMUV1", &fMaxTrackClusterDistance1, 100.0); // [mm]
  AddParam("MaxTrackClusterDistanceMUV2", &fMaxTrackClusterDistance2, 100.0); // [mm]

  fZMUV1 = GeometricAcceptance::GetInstance()->GetZMUV1();
  fZMUV2 = GeometricAcceptance::GetInstance()->GetZMUV2();
  RequestTree("Spectrometer", new TRecoSpectrometerEvent, "Reco");
  RequestTree("MUV1",         new TRecoMUV1Event,         "Reco");
  RequestTree("MUV2",         new TRecoMUV2Event,         "Reco");
}

void SpectrometerMUV12Association::InitHist() {
  if (!GetIsTree()) return;

  /*
  BookHisto("hMUV1ClusterPosition", new
	    TH2F("hMUV1ClusterPosition", "MUV1 cluster position;X [mm];Y [mm]",
		 60, -1500, 1500, 60, -1500, 1500));
  BookHisto("hMUV2ClusterPosition", new
	    TH2F("hMUV2ClusterPosition", "MUV2 cluster position;X [mm];Y [mm]",
		 60, -1500, 1500, 60, -1500, 1500));
  */
  BookHisto("hMUV1DistanceTrackCluster", new
	    TH1F("hMUV1DistanceTrackCluster", "MUV1 track-cluster distance;Distance [mm]",
		 100, 0, 10.0*fMaxTrackClusterDistance1));
  BookHisto("hMUV1dXTrackCluster", new
	    TH1F("hMUV1dXTrackCluster", "MUV1 track-cluster dX;dX [mm]",
		 100, -10.0*fMaxTrackClusterDistance1, 10.0*fMaxTrackClusterDistance1));
  BookHisto("hMUV1dYTrackCluster", new
	    TH1F("hMUV1dYTrackCluster", "MUV1 track-cluster dY;dY [mm]",
		 100, -10.0*fMaxTrackClusterDistance1, 10.0*fMaxTrackClusterDistance1));
  BookHisto("hMUV2DistanceTrackCluster", new
	    TH1F("hMUV2DistanceTrackCluster", "MUV2 track-cluster distance;Distance [mm]",
		 100, 0, 10.0*fMaxTrackClusterDistance2));
  BookHisto("hMUV2dXTrackCluster", new
	    TH1F("hMUV2dXTrackCluster", "MUV2 track-cluster dX;dX [mm]",
		 100, -10.0*fMaxTrackClusterDistance2, 10.0*fMaxTrackClusterDistance2));
  BookHisto("hMUV2dYTrackCluster", new
	    TH1F("hMUV2dYTrackCluster", "MUV2 track-cluster dY;dY [mm]",
		 100, -10.0*fMaxTrackClusterDistance2, 10.0*fMaxTrackClusterDistance2));
}

void SpectrometerMUV12Association::InitOutput() {
  if (!GetIsTree()) return;
  RegisterOutput("Output", &fContainer);
}

void SpectrometerMUV12Association::Process(Int_t) {
  if (!GetIsTree()) return;

  SetOutputState("Output", kOValid);
  fContainer.clear();

  TRecoSpectrometerEvent* STRAWevent = GetEvent<TRecoSpectrometerEvent>();
  TRecoMUV1Event* MUV1event = GetEvent<TRecoMUV1Event>();
  TRecoMUV1Event* MUV2event = GetEvent<TRecoMUV1Event>();

  // Monitoring: switched off by default to save memory
  /*
  for (Int_t iClus=0; iClus<MUV1event->GetNCandidates(); iClus++) {
    TRecoMUV1Candidate* Mcand = (TRecoMUV1Candidate*)MUV1event->GetCandidate(iClus);
    FillHisto("hMUV1ClusterPosition", Mcand->GetPosition().X(), Mcand->GetPosition().Y());
  }
  for (Int_t iClus=0; iClus<MUV2event->GetNCandidates(); iClus++) {
    TRecoMUV2Candidate* Mcand = (TRecoMUV2Candidate*)MUV2event->GetCandidate(iClus);
    FillHisto("hMUV2ClusterPosition", Mcand->GetPosition().X(), Mcand->GetPosition().Y());
  }
  */

  for (Int_t iTrack=0; iTrack<STRAWevent->GetNCandidates(); iTrack++) {
    TRecoSpectrometerCandidate* Scand = static_cast<TRecoSpectrometerCandidate*>(STRAWevent->GetCandidate(iTrack));
    Double_t xt1     = Scand->xAtAfterMagnet(fZMUV1);
    Double_t yt1     = Scand->yAtAfterMagnet(fZMUV1);
    Double_t xt2     = Scand->xAtAfterMagnet(fZMUV2);
    Double_t yt2     = Scand->yAtAfterMagnet(fZMUV2);

    Double_t Energy1 = 0.0;     // energy of the closest MUV1 cluster
    Double_t Rmin1   = 99999.9; // distance to the closest MUV1 cluster
    Double_t xbest1  = 99999.9;
    Double_t ybest1  = 99999.9;
    Double_t dx1     = 99999.9;
    Double_t dy1     = 99999.9;
    Double_t Time1   = 99999.9;

    Double_t Energy2 = 0.0;     // energy of the closest MUV2 cluster
    Double_t Rmin2   = 99999.9; // distance to the closest MUV2 cluster
    Double_t xbest2  = 99999.9;
    Double_t ybest2  = 99999.9;
    Double_t dx2     = 99999.9;
    Double_t dy2     = 99999.9;
    Double_t Time2   = 99999.9;

    Int_t ClosestClusterIndex1 = -1;
    for (Int_t iClus=0; iClus<MUV1event->GetNCandidates(); iClus++) {
      TRecoMUV1Candidate* Mcand = static_cast<TRecoMUV1Candidate*>(MUV1event->GetCandidate(iClus));
      Double_t xc = Mcand->GetPosition().X();
      Double_t yc = Mcand->GetPosition().Y();
      Double_t R  = sqrt((xt1-xc)*(xt1-xc)+(yt1-yc)*(yt1-yc));
      if (R<Rmin1) {
	Rmin1   = R;
	xbest1  = xc;
	ybest1  = yc;
	dx1     = xt1-xc;
	dy1     = yt1-yc;
	Energy1 = Mcand->GetEnergy();
	Time1   = Mcand->GetTime();
	if (R<fMaxTrackClusterDistance1) ClosestClusterIndex1 = iClus;
      }
    }

    Int_t ClosestClusterIndex2 = -1;
    for (Int_t iClus=0; iClus<MUV2event->GetNCandidates(); iClus++) {
      TRecoMUV2Candidate* Mcand = static_cast<TRecoMUV2Candidate*>(MUV2event->GetCandidate(iClus));
      Double_t xc = Mcand->GetPosition().X();
      Double_t yc = Mcand->GetPosition().Y();
      Double_t R  = sqrt((xt2-xc)*(xt2-xc)+(yt2-yc)*(yt2-yc));
      if (R<Rmin2) {
	Rmin2   = R;
	xbest2  = xc;
	ybest2  = yc;
	dx2     = xt2-xc;
	dy2     = yt2-yc;
	Energy2 = Mcand->GetEnergy();
	Time2   = Mcand->GetTime();
	if (R<fMaxTrackClusterDistance2) ClosestClusterIndex2 = iClus;
      }
    }

    FillHisto("hMUV1DistanceTrackCluster", Rmin1);
    FillHisto("hMUV1dXTrackCluster", dx1);
    FillHisto("hMUV1dYTrackCluster", dy1);
    FillHisto("hMUV2DistanceTrackCluster", Rmin2);
    FillHisto("hMUV2dXTrackCluster", dx2);
    FillHisto("hMUV2dYTrackCluster", dy2);

    SpectrometerMUV12AssociationOutput Asso(iTrack);
    if (ClosestClusterIndex1>=0) {
      Asso.SetMUV1ClusterID(ClosestClusterIndex1);
      Asso.SetMUV1ClusterPosition(TVector3(xbest1, ybest1, fZMUV1));
      Asso.SetTrackMUV1ClusterDistance(Rmin1);
      Asso.SetMUV1ClusterEnergy(Energy1);
      Asso.SetMUV1ClusterTime(Time1);
    }
    if (ClosestClusterIndex2>=0) {
      Asso.SetMUV2ClusterID(ClosestClusterIndex2);
      Asso.SetMUV2ClusterPosition(TVector3(xbest2, ybest2, fZMUV2));
      Asso.SetTrackMUV2ClusterDistance(Rmin2);
      Asso.SetMUV2ClusterEnergy(Energy2);
      Asso.SetMUV2ClusterTime(Time2);
    }
    fContainer.push_back(Asso);
  }
}

void SpectrometerMUV12Association::EndOfJobUser() {
  if (!GetIsTree()) return;
  SaveAllPlots();
}
