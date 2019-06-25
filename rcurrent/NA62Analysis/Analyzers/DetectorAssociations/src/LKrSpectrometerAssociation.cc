// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-11-24
//
// ---------------------------------------------------------

/// \class LKrSpectrometerAssociation
/// \Brief
/// Geometrical association of Spectrometer candidates to LKr candidates
/// \EndBrief
/// \Detailed
/// For each cluster, find the Spectrometer candidate (track) geometrically associated
/// to the cluster, and compute E/p. TrackID=-1 means no associated track is found. An example of use:
/// \code
/// std::vector<LKrSpectrometerAssociationOutput> SpecLKR =
///   *(std::vector<LKrSpectrometerAssociationOutput>*)GetOutput("LKrSpectrometerAssociation.Output");
/// for (UInt_t i=0; i<SpecLKR.size(); i++) {
///   LKrSpectrometerAssociationOutput sl = SpecLKR[i];
///   sl.Print();
/// }
/// \endcode
/// \author Karim Massri (karim.massri@cern.ch)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include "LKrSpectrometerAssociation.hh"
#include "SpectrometerCHODAssociation.hh"
#include "SpectrometerNewCHODAssociation.hh"
#include "Event.hh"
#include "Persistency.hh"

using namespace NA62Analysis;
using namespace NA62Constants;

LKrSpectrometerAssociation::LKrSpectrometerAssociation(Core::BaseAnalysis *ba) :
  Analyzer(ba, "LKrSpectrometerAssociation") {
  if (!GetIsTree()) return;

  AddParam("MaxClusterTrackDistance",  &fMaxClusterTrackDistance,  50.0); // [mm]
  AddParam("MaxClusterTrackDeltaTime", &fMaxClusterTrackDeltaTime,  5.0); // [ns]
  RequestTree("LKr",          new TRecoLKrEvent,          "Reco");
  RequestTree("Spectrometer", new TRecoSpectrometerEvent, "Reco");
}

void LKrSpectrometerAssociation::InitHist() {
  if (!GetIsTree()) return;
  BookHisto("hDistanceClusterTrack", new
	    TH1F("hDistanceClusterTrack", "Track-cluster distance;Distance [mm]",
		 100, 0, fMaxClusterTrackDistance));
  BookHisto("hdXClusterTrack", new
	    TH1F("hdXClusterTrack", "Track-cluster dX;dX [mm]",
		 100, -fMaxClusterTrackDistance, fMaxClusterTrackDistance));
  BookHisto("hdYClusterTrack", new
	    TH1F("hdYClusterTrack", "Track-cluster dY;dY [mm]",
		 100, -fMaxClusterTrackDistance, fMaxClusterTrackDistance));
  BookHisto("hEoP", new TH1F("hEoP", "Energy-to-momentum ratio; E/p", 125, 0.0, 1.25));
}

void LKrSpectrometerAssociation::InitOutput() {
  if (!GetIsTree()) return;
  RegisterOutput("Output", &fContainer);
}

void LKrSpectrometerAssociation::Process(Int_t) {
  if (!GetIsTree()) return;

  SetOutputState("Output", kOValid);
  fContainer.clear();

  std::vector<SpectrometerCHODAssociationOutput> SpecCHOD =
    *(std::vector<SpectrometerCHODAssociationOutput>*)GetOutput("SpectrometerCHODAssociation.Output");
  std::vector<SpectrometerNewCHODAssociationOutput> SpecNewCHOD =
    *(std::vector<SpectrometerNewCHODAssociationOutput>*)GetOutput("SpectrometerNewCHODAssociation.Output");

  TRecoLKrEvent* LKrEvent = GetEvent<TRecoLKrEvent>();
  TRecoSpectrometerEvent* SpectrometerEvent = GetEvent<TRecoSpectrometerEvent>();

  for (Int_t iCluster=0; iCluster<LKrEvent->GetNCandidates(); iCluster++) {
    LKrSpectrometerAssociationOutput Asso;
    TRecoLKrCandidate* LKrCand = static_cast<TRecoLKrCandidate*>(LKrEvent->GetCandidate(iCluster));
    Double_t xCls              = LKrCand->GetClusterX();
    Double_t yCls              = LKrCand->GetClusterY();
    Int_t    ClosestTrackRecID = -1;
    Double_t RMin              = 99999.9; // distance to the closest track
    Double_t dx                = 99999.9;
    Double_t dy                = 99999.9;
    for (Int_t iTrack=0; iTrack<SpectrometerEvent->GetNCandidates(); iTrack++) {
      TRecoSpectrometerCandidate* SpecCand = static_cast<TRecoSpectrometerCandidate*>(SpectrometerEvent->GetCandidate(iTrack));
      Double_t xTrk = SpecCand->xAtAfterMagnet(GeometricAcceptance::GetInstance()->GetZLKr());
      Double_t yTrk = SpecCand->yAtAfterMagnet(GeometricAcceptance::GetInstance()->GetZLKr());
      Double_t R  = sqrt(pow(xCls-xTrk,2.)+pow(yCls-yTrk,2.));
      if (R<fMaxClusterTrackDistance) {
        Double_t TrackTime = SpecCand->GetTime(); //use CHOD, NewCHOD or the track time (in this order)
        if(SpecCHOD[iTrack].isAssociated()) TrackTime = SpecCHOD[iTrack].GetBestAssociationRecord()->GetCHODCandidate()->GetTime();
        else if(SpecNewCHOD[iTrack].isAssociated()) TrackTime = SpecNewCHOD[iTrack].GetBestAssociationRecord()->GetRecoHitTime();
        if(fabs(LKrCand->GetTime()-TrackTime)>fMaxClusterTrackDeltaTime) continue; //out of time
        if (R<RMin) {
          RMin     = R;
          dx       = xCls-xTrk;
          dy       = yCls-yTrk;
          ClosestTrackRecID = Asso.GetNAssociationRecords();
        }
        LKrSpectrometerAssociationRecord Rec(iTrack,SpecCand);
        Rec.SetEoP(LKrCand->GetClusterEnergy()/SpecCand->GetMomentum());
        Rec.SetClusterTrackDistance(R);
        Asso.AddAssociationRecord(Rec);
      }
    }
    Asso.SetBestAssociationRecordID(ClosestTrackRecID);
    fContainer.push_back(Asso);

    if(Asso.isAssociated()){
      FillHisto("hDistanceClusterTrack", RMin);
      FillHisto("hdXClusterTrack", dx);
      FillHisto("hdYClusterTrack", dy);
      FillHisto("hEoP", Asso.GetBestAssociationRecord()->GetEoP());
    }
  }
}

void LKrSpectrometerAssociation::EndOfJobUser() {
  if (!GetIsTree()) return;
  SaveAllPlots();
}
