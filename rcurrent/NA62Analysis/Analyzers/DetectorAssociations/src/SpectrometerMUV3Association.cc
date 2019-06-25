// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-07-09
//
// ---------------------------------------------------------------

/// \class SpectrometerMUV3Association
/// \Brief
/// Geometrical association of MUV3 candidates to Spectrometer tracks
/// \EndBrief
/// \Detailed
/// Builds an array of SpectrometerMUV3AssociationOutput objects.
/// The number of these objects is equal to the number of Spectrometer tracks.
/// Each of them contains the track ID and basic track information, as well as
/// a collection of SpectrometerMUV3AssociationRecords.
/// Each SpectrometerMUV3AssociationRecord contains the ID and basic information about a
/// MUV3 candidate associated (i.e. found to be geometrically compatible) with the track.
/// The geometrical compatibility is determined by extrapolating the track to the MUV3
/// front plane, and checking if the MUV3 candidate tile overlaps with a "search circle"
/// around the track impact point. The "search radius" is inversely proportional to
/// the track momentum.
/// Timing compatibility is not checked. The DownstreamTrack container provides
/// a set of methods allowing the user to check conveniently the track/MUV3 time consistency.
/// Absence of SpectrometerMUV3AssociationRecords in a SpectrometerMUV3AssociationOutput structure
/// means that no MUV3 candidates are associated with the track,
/// otherwise a MUV3 candidate is accociated.
/// Example of use:
/// \code
///  std::vector<SpectrometerMUV3AssociationOutput> SpecMUV3 =
///    *(std::vector<SpectrometerMUV3AssociationOutput>*)GetOutput("SpectrometerMUV3Association.Output");
///  for (UInt_t itrack=0; itrack<SpecMUV3.size(); itrack++) {
///    SpectrometerMUV3Association Match = SpecMUV3[itrack];
///    Match.Print();
///    for (UInt_t i=0; i<Match.GetNAssociationRecords(); i++) {
///      SpectrometerMUV3AssociationRecord Record = Match.GetAssociationRecord(i);
///      Record.Print();
///    }
///  }
/// \endcode
/// A possibility of emulating addiitonal MUV3 inefficiency for MC events is provided
/// via the InefficiencyForMC parameter, set to zero by default.
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include "SpectrometerMUV3Association.hh"
#include "Event.hh"
#include "Persistency.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

SpectrometerMUV3Association::SpectrometerMUV3Association(Core::BaseAnalysis *ba) :
  Analyzer(ba, "SpectrometerMUV3Association"), fRandom(nullptr) {
  AddParam("InefficiencyForMC", &fInefficiencyforMC, 0.0);
  if (!GetIsTree()) return;

  RequestTree("MUV3",         new TRecoMUV3Event,         "Reco");
  RequestTree("Spectrometer", new TRecoSpectrometerEvent, "Reco");

  fScaleFactor = 4.0;
  fZMUV3       = GeometricAcceptance::GetInstance()->GetZMUV3();
  fSize        = GeometricAcceptance::GetInstance()->GetMUV3Rmax();
  fRmin        = GeometricAcceptance::GetInstance()->GetMUV3Rmin();

  fRandom = new TRandom2();
}

SpectrometerMUV3Association::~SpectrometerMUV3Association() {
  if (fRandom) delete fRandom;
}

void SpectrometerMUV3Association::StartOfBurstUser() {
  if (!GetIsTree()) return;
  if (!GetEventHeader()) return;
  if (!GetWithMC()) return;
  fRandom->SetSeed(GetEventHeader()->GetBurstID()); // to ensure reproducibility
}

void SpectrometerMUV3Association::InitHist() {
  if (!GetIsTree()) return;

  if (GetWithMC()) {
    if (fInefficiencyforMC<0.0) fInefficiencyforMC = 0.0;
    if (fInefficiencyforMC>1.0) fInefficiencyforMC = 1.0;
    cout << user_normal() << "MUV3 inefficiency emulated for MC: " << fInefficiencyforMC << endl;
  }

  BookHisto("hDistanceTrackMUV3Candidate", new TH1F
	    ("hDistanceTrackMUV3Candidate", "Track-MUV3 candidate distance; Distance [mm]",
	     100, 0, 1000));
}

void SpectrometerMUV3Association::InitOutput() {
  if (!GetIsTree()) return;
  RegisterOutput("Output", &fContainer);
}

void SpectrometerMUV3Association::Process(Int_t) {
  if (!GetIsTree()) return;
  SetOutputState("Output", kOValid);
  fContainer.clear();

  TRecoMUV3Event* MUV3event = GetEvent<TRecoMUV3Event>();
  TRecoSpectrometerEvent* STRAWevent = GetEvent<TRecoSpectrometerEvent>();

  // Emulate MUV3 inefficiency for MC by rejecting some candidates (in a reproducible way)
  if (GetWithMC() && fInefficiencyforMC>1e-10) {
    for (Int_t i=0; i<MUV3event->GetNCandidates(); i++) {
      if (fRandom->Rndm()<fInefficiencyforMC) MUV3event->RemoveCandidate(i--);
    }
  }

  for (Int_t iTrack=0; iTrack<STRAWevent->GetNCandidates(); iTrack++) {
    TRecoSpectrometerCandidate* Scand = static_cast<TRecoSpectrometerCandidate*>(STRAWevent->GetCandidate(iTrack));
    Double_t xt   = Scand->xAtAfterMagnet(fZMUV3);
    Double_t yt   = Scand->yAtAfterMagnet(fZMUV3);
    Double_t Time = Scand->GetTime();

    ///////////////////////////////////////////////////////////////
    // Estimated effect of scattering: sigma(x,y)=530 mm*GeV / p.
    // Assume 10 GeV/c momentum for data with the MNP33 magnet off.

    Double_t Momentum      = (Scand->GetMomentum()>0) ? Scand->GetMomentum() : 10000.0;
    Double_t SearchRadius  = fScaleFactor * 530.0; // [mm]
    if (Momentum>1000.) SearchRadius = fScaleFactor * 530000.0 / Momentum;
    Bool_t InnerAcceptance = (sqrt(xt*xt+yt*yt)>fRmin);
    Bool_t OuterAcceptance = (fabs(xt)<fSize && fabs(yt)<fSize);
    Bool_t InAcceptance    = InnerAcceptance && OuterAcceptance;

    Double_t DistanceToOuterEdge = 0.0;
    if (InAcceptance) {
      DistanceToOuterEdge = TMath::Min(fSize-fabs(xt), fSize-fabs(yt));
    }
    else {
      if      (fabs(xt)>fSize && fabs(yt)<fSize) DistanceToOuterEdge = fSize-fabs(xt);
      else if (fabs(yt)>fSize && fabs(xt)<fSize) DistanceToOuterEdge = fSize-fabs(yt);
      else    DistanceToOuterEdge = sqrt((xt-fSize)*(xt-fSize)+(yt-fSize)*(yt-fSize));
    }

    SpectrometerMUV3AssociationOutput Asso(iTrack);
    Asso.SetTrackTime(Time);
    Asso.SetTrackXY(xt, yt);
    Asso.SetSearchRadius(SearchRadius);
    Asso.SetTrackMomentum(Scand->GetMomentum());
    Asso.SetInAcceptance(InAcceptance);
    Asso.SetDistanceToEdge(DistanceToOuterEdge);

    for (Int_t iMu=0; iMu<MUV3event->GetNCandidates(); iMu++) {
      TRecoMUV3Candidate* Mcand = static_cast<TRecoMUV3Candidate*>(MUV3event->GetCandidate(iMu));
      Int_t    iTile = Mcand->GetTileID();
      Double_t xm    = Mcand->GetX();
      Double_t ym    = Mcand->GetY();

      ///////////////////////////////////////////////////
      // Find corners of the tile; check for direct match

      Double_t TileSize = (iTile<144) ? fSize/6.0 : fSize/9.0;
      Double_t xm1 = xm - 0.5*TileSize;
      Double_t xm2 = xm + 0.5*TileSize;
      Double_t ym1 = ym - 0.5*TileSize;
      Double_t ym2 = ym + 0.5*TileSize;

      Double_t TrackTileDistance = 0.0;
      Bool_t   DirectMatch       = (xt>=xm1 && xt<=xm2 && yt>=ym1 && yt<=ym2);

      if (!DirectMatch) { // extrapolated track misses the tile: compute the distance
	if      (yt>=ym1 && yt<=ym2) TrackTileDistance = TMath::Min(fabs(xt-xm1), fabs(xt-xm2));
	else if (xt>=xm1 && xt<=xm2) TrackTileDistance = TMath::Min(fabs(yt-ym1), fabs(yt-ym2));
	else if (xt>=xm2 && yt>=ym2) TrackTileDistance = sqrt((xt-xm2)*(xt-xm2)+(yt-ym2)*(yt-ym2));
	else if (xt>=xm2 && yt<=ym1) TrackTileDistance = sqrt((xt-xm2)*(xt-xm2)+(yt-ym1)*(yt-ym1));
	else if (xt<=xm1 && yt<=ym1) TrackTileDistance = sqrt((xt-xm1)*(xt-xm1)+(yt-ym1)*(yt-ym1));
	else if (xt<=xm1 && yt>=ym2) TrackTileDistance = sqrt((xt-xm1)*(xt-xm1)+(yt-ym2)*(yt-ym2));
      }

      if (TrackTileDistance<SearchRadius) {
        Double_t TrackCandidateDistance = sqrt((xt-xm)*(xt-xm)+(yt-ym)*(yt-ym));
	SpectrometerMUV3AssociationRecord
	  Record(iMu, iTile, Mcand->GetType(), Mcand->GetTime(),
		 TrackTileDistance, TrackCandidateDistance, xm, ym, DirectMatch);
	FillHisto("hDistanceTrackMUV3Candidate", TrackCandidateDistance);
	Asso.AddAssociationRecord(Record);
      }
    }
    fContainer.push_back(Asso);
  }
}

void SpectrometerMUV3Association::EndOfJobUser() {
  if (!GetIsTree()) return;
  SaveAllPlots();
}
