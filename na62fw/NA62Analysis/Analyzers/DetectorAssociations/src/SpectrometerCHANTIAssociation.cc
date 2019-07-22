// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2018-01-28
//
// ---------------------------------------------------------

/// \class SpectrometerCHANTIAssociation
/// \Brief
/// Geometrical association of CHANTI candidates to Spectrometer candidates
/// \EndBrief
/// \Detailed
/// For each track, find the CHANTI candidate geometrically associated to the track. 
/// CandidateID=-1 means no associated candidate is found. An example of use:
/// \code
/// std::vector<SpectrometerCHANTIAssociationOutput> SpecCHANTI =
///   *(std::vector<SpectrometerCHANTIAssociationOutput>*)GetOutput("SpectrometerCHANTIAssociation.Output");
/// for (UInt_t i=0; i<SpecCHANTI.size(); i++) {
///   SpectrometerCHANTIAssociationOutput sl = SpecCHANTI[i];
///   sl.Print();
/// }
/// \endcode
/// \author Karim Massri (karim.massri@cern.ch)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include "SpectrometerCHANTIAssociation.hh"
#include "Event.hh"
#include "Persistency.hh"

using namespace NA62Analysis;
using namespace NA62Constants;

SpectrometerCHANTIAssociation::SpectrometerCHANTIAssociation(Core::BaseAnalysis *ba) :
  Analyzer(ba, "SpectrometerCHANTIAssociation") {
  if (!GetIsTree()) return;

  AddParam("MaxTrackCandidateDistance", &fMaxTrackCandidateDistance, 20.0); // [mm]
  RequestTree("Spectrometer", new TRecoSpectrometerEvent, "Reco");
  RequestTree("CHANTI",       new TRecoCHANTIEvent, "Reco");

  fZCHANTI   = GeometricAcceptance::GetInstance()->GetZGTK3()+28.; //First CHANTI Plane: 28 mm downstream GTK3
  fSize      = 150.;  //mm
  fSizeHoleX =  47.5; //mm
  fSizeHoleY =  32.5; //mm
}

void SpectrometerCHANTIAssociation::InitHist() {
  if (!GetIsTree()) return;
  BookHisto("hDistanceTrackCandidate", new
	    TH1F("hDistanceTrackCandidate", "Track-Candidate distance;Distance [mm]", 100, 0, 200.));
  BookHisto("hdXTrackCandidate", new
	    TH1F("hdXTrackCandidate", "Track-Candidate dX;dX [mm]", 100, -200., 200.));
  BookHisto("hdYTrackCandidate", new
	    TH1F("hdYTrackCandidate", "Track-Candidate dY;dY [mm]", 100, -200., 200.));
  BookHisto("hDistanceTrackCandidateAcc", new
	    TH1F("hDistanceTrackCandidateAcc", "Track-Candidate distance;Distance [mm]", 100, 0, 200.));
  BookHisto("hdXTrackCandidateAcc", new
	    TH1F("hdXTrackCandidateAcc", "Track-Candidate dX;dX [mm]", 100, -200., 200.));
  BookHisto("hdYTrackCandidateAcc", new
	    TH1F("hdYTrackCandidateAcc", "Track-Candidate dY;dY [mm]", 100, -200., 200.));
}

void SpectrometerCHANTIAssociation::InitOutput() {
  if (!GetIsTree()) return;
  RegisterOutput("Output", &fContainer);
}

void SpectrometerCHANTIAssociation::Process(Int_t) {
  if (!GetIsTree()) return;

  SetOutputState("Output", kOValid);
  fContainer.clear();

  TRecoSpectrometerEvent* SpectrometerEvent = GetEvent<TRecoSpectrometerEvent>();
  TRecoCHANTIEvent* CHANTIEvent = GetEvent<TRecoCHANTIEvent>();

  for (Int_t iTrack=0; iTrack<SpectrometerEvent->GetNCandidates(); iTrack++) {
    SpectrometerCHANTIAssociationOutput Asso;
    TRecoSpectrometerCandidate* SpectrometerCand = static_cast<TRecoSpectrometerCandidate*>(SpectrometerEvent->GetCandidate(iTrack));
    Double_t xTrk = SpectrometerCand->xAtBeforeMagnet(fZCHANTI);
    Double_t yTrk = SpectrometerCand->yAtBeforeMagnet(fZCHANTI);
    Int_t    ClosestCandidateRecID = -1;
    Double_t RMin = 99999.9; // distance to the closest candidate
    Double_t dx   = 99999.9;
    Double_t dy   = 99999.9;

    Bool_t InnerAcceptance = !(fabs(xTrk)<fSizeHoleX && fabs(yTrk)<fSizeHoleY);
    Bool_t OuterAcceptance = (fabs(xTrk)<fSize && fabs(yTrk)<fSize);
    Bool_t InAcceptance    = InnerAcceptance && OuterAcceptance;

    Double_t DistanceToOuterEdge = 0.0;
    if (InAcceptance) {
      DistanceToOuterEdge = TMath::Min(fSize-fabs(xTrk), fSize-fabs(yTrk));
    }
    else {
      if      (fabs(xTrk)>fSize && fabs(yTrk)<fSize) DistanceToOuterEdge = fSize-fabs(xTrk);
      else if (fabs(yTrk)>fSize && fabs(xTrk)<fSize) DistanceToOuterEdge = fSize-fabs(yTrk);
      else    DistanceToOuterEdge = sqrt(pow(xTrk-fSize,2.)+pow(yTrk-fSize,2.));
    }
    Asso.SetInAcceptance(InAcceptance);
    Asso.SetDistanceToEdge(DistanceToOuterEdge);

    for (Int_t iCHANTICand=0; iCHANTICand<CHANTIEvent->GetNCandidates(); iCHANTICand++) {
      TRecoCHANTICandidate* CHANTICand = static_cast<TRecoCHANTICandidate*>(CHANTIEvent->GetCandidate(iCHANTICand));
      Double_t xCHANTI = CHANTICand->GetXPos();
      Double_t yCHANTI = CHANTICand->GetYPos();
      Double_t R  = sqrt(pow(xTrk-xCHANTI,2.)+pow(yTrk-yCHANTI,2.));
      if (R<RMin) {
        RMin     = R;
        dx       = xTrk-xCHANTI;
        dy       = yTrk-yCHANTI;
        if (R<fMaxTrackCandidateDistance) ClosestCandidateRecID = Asso.GetNAssociationRecords();
      }
      if (R<fMaxTrackCandidateDistance) {
        SpectrometerCHANTIAssociationRecord Rec(iCHANTICand,CHANTICand);
        Rec.SetTrackCandidateDistance(R);
        Asso.AddAssociationRecord(Rec);
      }
    }
    Asso.SetBestAssociationRecordID(ClosestCandidateRecID);
    fContainer.push_back(Asso);

    FillHisto("hDistanceTrackCandidate", RMin);
    FillHisto("hdXTrackCandidate", dx);
    FillHisto("hdYTrackCandidate", dy);
    if(InAcceptance){
      FillHisto("hDistanceTrackCandidateAcc", RMin);
      FillHisto("hdXTrackCandidateAcc", dx);
      FillHisto("hdYTrackCandidateAcc", dy);
    }
  }
}

void SpectrometerCHANTIAssociation::EndOfJobUser() {
  if (!GetIsTree()) return;
  SaveAllPlots();
}
