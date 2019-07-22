// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-07-13
// Search radius function modified by Lubos Bician (lbician@cern.ch) 2016-12-08
//
// ---------------------------------------------------------------

/// \class SpectrometerNewCHODAssociation
/// \Brief
/// Geometrical association of NewCHOD RecoHits to Spectrometer candidates
/// \EndBrief
/// \Detailed
/// Builds an array of SpectrometerNewCHODAssociationOutput objects.
/// The number of these objects is equal to the number of Spectrometer tracks.
/// Each of them contains the track ID and basic track information, as well as
/// a collection of SpectrometerNewCHODAssociationRecords.
/// Each SpectrometerNewCHODAssociationRecord contains the ID and basic information about a
/// NewCHOD RecoHit associated (i.e. found to be geometrically compatible) with the track.
/// The geometrical compatibility is determined by extrapolating the track to the NewCHOD
/// front plane, and checking if the NewCHOD RecoHit tile overlaps with a "search circle"
/// around the track impact point. The "search radius" is inversely proportional to
/// the track momentum to account for multiple scattering.
/// The "best" association is the one with the smallest distance from the track extrapolation
/// to the corresponding tile (this distance is zero if the extrapolated track crosses
/// a tile). The NewCHOD time of the track is computed as the one of the "best"
/// association; a single association is chosen at random in case of multiple "best"
/// associations (for example, in case of multiple hits in a tile, or extrapolated track
/// crossing two overlapping tiles).
/// No SpectrometerNewCHODAssociationRecords in a SpectrometerNewCHODAssociationOutput structure
/// means that no NewCHOD candidates are associated with the track,
/// otherwise a NewCHOD candidate is accociated.
/// Example of use:
/// \code
///  std::vector<SpectrometerNewCHODAssociationOutput> SpecNewCHOD =
///    *(std::vector<SpectrometerNewCHODAssociationOutput>*)GetOutput("SpectrometerNewCHODAssociation.Output");
///  for (UInt_t itrack=0; itrack<SpecNewCHOD.size(); itrack++) {
///    SpectrometerNewCHODAssociation Match = SpecNewCHOD[itrack];
///    Match.Print();
///    for (UInt_t i=0; i<Match.GetNAssociationRecords(); i++) {
///      SpectrometerNewCHODAssociationRecord Record = Match.GetAssociationRecord(i);
///      Record.Print();
///    }
///  }
/// \endcode
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \todo Define the NewCHOD time better in case of multiple "best" associations?
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include "SpectrometerNewCHODAssociation.hh"
#include "Event.hh"
#include "Persistency.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

SpectrometerNewCHODAssociation::SpectrometerNewCHODAssociation(Core::BaseAnalysis *ba) :
  Analyzer(ba, "SpectrometerNewCHODAssociation") {
  if (!GetIsTree()) return;

  RequestTree("NewCHOD",      new TRecoNewCHODEvent,      "Reco");
  RequestTree("Spectrometer", new TRecoSpectrometerEvent, "Reco");

  fNewCHODGeometry = new NewCHODGeometry();

  fScaleFactor = 6.0; // defines the track-NewCHOD RecoHit association distance
  fZNewCHOD    = GeometricAcceptance::GetInstance()->GetZNewCHOD();
  fRmax        = GeometricAcceptance::GetInstance()->GetNewCHODRmax();
  fRmin        = GeometricAcceptance::GetInstance()->GetNewCHODRmin();
}

SpectrometerNewCHODAssociation::~SpectrometerNewCHODAssociation() {
  if (!GetIsTree()) return;
  delete fNewCHODGeometry;
}

void SpectrometerNewCHODAssociation::InitHist() {
  if (!GetIsTree()) return;
  BookHisto("hDistanceTrackNewCHODRecoHit", new TH1F
	    ("hDistanceTrackNewCHODRecoHit", "Track - NewCHOD RecoHit distance; Distance [mm]",
	     100, 0, 1000));
}

void SpectrometerNewCHODAssociation::InitOutput() {
  if (!GetIsTree()) return;
  RegisterOutput("Output", &fContainer);
}

void SpectrometerNewCHODAssociation::Process(Int_t) {
  if (!GetIsTree()) return;

  SetOutputState("Output", kOValid);
  fContainer.clear();

  TRecoNewCHODEvent* NewCHODevent = GetEvent<TRecoNewCHODEvent>();
  TRecoSpectrometerEvent* STRAWevent = GetEvent<TRecoSpectrometerEvent>();

  for (Int_t iTrack=0; iTrack<STRAWevent->GetNCandidates(); iTrack++) {
    TRecoSpectrometerCandidate* Scand = static_cast<TRecoSpectrometerCandidate*>(STRAWevent->GetCandidate(iTrack));
    Double_t xt   = Scand->xAtAfterMagnet(fZNewCHOD);
    Double_t yt   = Scand->yAtAfterMagnet(fZNewCHOD);
    Double_t Time = Scand->GetTime();

    ////////////////////////////////////////////////////////////////////
    // Estimated effect of scattering and STRAW resolution:
    // sigma(x,y) = (0.07 + 47300/p) mm.
    // This curve has been calculated using large Kmu2 MC sample.
    // Assume 10 GeV/c momentum for data with the MNP33 magnet off.

    Double_t Momentum      = (Scand->GetMomentum()>0) ? Scand->GetMomentum() : 10000.0;
    Double_t SearchRadius  = 47.37; //mm
    if(Momentum>1000.) SearchRadius  = 0.07 + 47300.0/Momentum;
    SearchRadius *= fScaleFactor;
    Bool_t InnerAcceptance = (sqrt(xt*xt+yt*yt)>fRmin);
    Bool_t OuterAcceptance = (sqrt(xt*xt+yt*yt)<fRmax);
    Bool_t InAcceptance    = InnerAcceptance && OuterAcceptance;

    SpectrometerNewCHODAssociationOutput Asso(iTrack);
    Asso.SetTrackTime(Time);
    Asso.SetTrackXY(xt, yt);
    Asso.SetSearchRadius(SearchRadius);
    Asso.SetTrackMomentum(Scand->GetMomentum());
    Asso.SetInAcceptance(InAcceptance);

    Int_t    BestAssociationRecordID = -1;
    Double_t MinTrackTileDistance = 999999;
    for (Int_t iHit=0; iHit<NewCHODevent->GetNHits(); iHit++) {

      TRecoNewCHODHit* Hit = static_cast<TRecoNewCHODHit*>(NewCHODevent->GetHit(iHit));
      Int_t    iTile = Hit->GetTileID();
      Double_t x     = Hit->GetX();
      Double_t y     = Hit->GetY();

      ///////////////////////////////////////////////////
      // Find corners of the tile; check for direct match

      Double_t x1 = fNewCHODGeometry->GetTileXmin(iTile);
      Double_t x2 = fNewCHODGeometry->GetTileXmax(iTile);
      Double_t y1 = fNewCHODGeometry->GetTileYmin(iTile);
      Double_t y2 = fNewCHODGeometry->GetTileYmax(iTile);

      Double_t TrackTileDistance = 0.0;
      Bool_t   DirectMatch       = (xt>x1 && xt<x2 && yt>y1 && yt<y2);

      if (!DirectMatch) { // extrapolated track misses the tile: compute the distance
	if      (yt>y1 && yt<y2) TrackTileDistance = TMath::Min(fabs(xt-x1), fabs(xt-x2));
	else if (xt>x1 && xt<x2) TrackTileDistance = TMath::Min(fabs(yt-y1), fabs(yt-y2));
	else if (xt>x2 && yt>y2) TrackTileDistance = sqrt((xt-x2)*(xt-x2)+(yt-y2)*(yt-y2));
	else if (xt>x2 && yt<y1) TrackTileDistance = sqrt((xt-x2)*(xt-x2)+(yt-y1)*(yt-y1));
	else if (xt<x1 && yt<y1) TrackTileDistance = sqrt((xt-x1)*(xt-x1)+(yt-y1)*(yt-y1));
	else if (xt<x1 && yt>y2) TrackTileDistance = sqrt((xt-x1)*(xt-x1)+(yt-y2)*(yt-y2));
      }

      if (TrackTileDistance<SearchRadius) { // an association record is formed
	// This is the best record so far: the first record in the list is considered in case of several hits in the same tile
	if (TrackTileDistance<MinTrackTileDistance) {
	  MinTrackTileDistance = TrackTileDistance;
	  BestAssociationRecordID = Asso.GetNAssociationRecords(); // this is the ID of the record to be added
	}
        Double_t TrackHitDistance = sqrt((xt-x)*(xt-x)+(yt-y)*(yt-y));
	SpectrometerNewCHODAssociationRecord
	  Record(iHit, iTile, Hit->GetType(), Hit->GetTime(),
		 TrackTileDistance, TrackHitDistance, x, y, DirectMatch);
	FillHisto("hDistanceTrackNewCHODRecoHit", TrackHitDistance);
	Asso.AddAssociationRecord(Record);
      }
    }
    Asso.SetBestAssociationRecordID(BestAssociationRecordID);
    fContainer.push_back(Asso);
  }
}

void SpectrometerNewCHODAssociation::EndOfJobUser() {
  if (!GetIsTree()) return;
  SaveAllPlots();
}
