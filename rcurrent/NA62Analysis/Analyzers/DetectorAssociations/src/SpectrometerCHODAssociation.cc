// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-07-09
//
// Adopted for the CHOD by Viacheslav Duk (Viacheslav.Duk@cern.ch) 26.02.2016
//
// ---------------------------------------------------------------

/// \class SpectrometerCHODAssociation
/// \Brief
/// Geometrical association of CHOD candidates to Spectrometer candidates
/// \EndBrief
/// \Detailed
/// Builds an array of SpectrometerCHODAssociationOutput objects.
/// The number of these objects is equal to the number of Spectrometer tracks.
/// Each of them contains the track ID and basic track information, as well as
/// a collection of SpectrometerCHODAssociationRecords.
/// Each SpectrometerCHODAssociationRecord contains the ID and basic information about a
/// CHOD candidate associated (i.e. found to be geometrically compatible) with the track.
/// The geometrical compatibility is determined by extrapolating the track to the corresponding
/// CHOD plane (V-plane for x-coordinate and H-plane for y-coordinate),
/// and checking if the CHOD candidate (i.e. slab intersection) overlaps with a "search circle"
/// around the track impact point. The "search radius" is 20 mm 
/// (the default value is chosen from the analysis of the minimum bias data).
/// No SpectrometerCHODAssociationRecord objects in a SpectrometerCHODAssociationOutput structure means that
/// no CHOD candidates are associated with a track, otherwise a CHOD candidate is accociated.
/// It is possible that more than one CHOD candidate is associated with the track 
/// (i.e. there are several SpectrometerCHODAssociationRecord objects).
/// When the hit number is large (Nhits-2*Ntracks>30, this case is called "shower-like events"),
/// no association is performed.
///  Example of use:
/// \code
///  std::vector<SpectrometerCHODAssociationOutput> SpectrometerCHOD =
///    *(std::vector<SpectrometerCHODAssociationOutput>*)GetOutput("SpectrometerCHODAssociation.Output");
///  for (UInt_t itrack=0; itrack<SpectrometerCHOD.size(); itrack++) {
///    SpectrometerCHODAssociationOutput sca = SpectrometerCHOD[itrack];
///    sca.Print();
///    for (UInt_t iCHODCandidate=0; iCHODCandidate<sca.GetNAssociationRecords(); iCHODCandidate++) {
///      SpectrometerCHODAssociationRecord Record = sca.GetAssociationRecord(iCHODCandidate);
///      Record.Print();
///    }
///  }
/// \endcode
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk), adopted for CHOD by Viacheslav Duk (Viacheslav.Duk@cern.ch)
/// \EndDetailed

#include "SpectrometerCHODAssociation.hh"
#include "Persistency.hh"
#include "NA62ConditionsService.hh"
#include "GeometricAcceptance.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

SpectrometerCHODAssociation::SpectrometerCHODAssociation(Core::BaseAnalysis *ba) :
  Analyzer(ba, "SpectrometerCHODAssociation") {
  if (!GetIsTree()) return;

  RequestTree("CHOD",         new TRecoCHODEvent,         "Reco");
  RequestTree("Spectrometer", new TRecoSpectrometerEvent, "Reco");

  // calculate slab widths
  for (Int_t i=0; i<64; i++) {
    fSlabWidth[i] = 65.0;
    if ((i>=11 && i<=20) || (i>=43 &&i<=52)) fSlabWidth[i] = 99.;
  }
  for (Int_t i=64; i<128; i++) {
    fSlabWidth[i] = 99.0;
    if ((i>=69 &&i<=90) || (i>=101 &&i<=122)) fSlabWidth[i] = 65.;
  }

  fAlignmentFileName = "CHOD-Alignment.dat";
  fOffsetX = 0.;
  fOffsetY = 0.;
}

void SpectrometerCHODAssociation::InitHist() {
  if (!GetIsTree()) return;
  BookHisto("hDistanceTrackCHODCandidate", new TH1F
	    ("hDistanceTrackCHODCandidate", "Track - CHOD candidate distance; Distance [mm]",
	     50, 0., 100.));
  BookHisto("hTrackCHODCoordinateVsID_65", new TH2F
	    ("hTrackCHODCoordinateVsID_65", "(Track - CHOD) distance vs ID, 65mm slabs; slab ID", 
	     128, -0.5, 127.5, 300, -150., 150.));
  BookHisto("hTrackCHODCoordinateVsID_99", new TH2F
	    ("hTrackCHODCoordinateVsID_99", "(Track - CHOD) distance vs ID, 99mm slabs; slab ID", 
	     128, -0.5, 127.5, 300, -150., 150.));
}

void SpectrometerCHODAssociation::InitOutput() {
  if (!GetIsTree()) return;
  RegisterOutput("Output", &fOutput);
}

void SpectrometerCHODAssociation::StartOfRunUser() {
  if (!GetIsTree()) return;
  // load spatial alignment offsets for the data
  if (!GetWithMC()) LoadOffsets();
  cout << user_normal() << "CHOD (x,y) alignment offsets [mm]: " << fOffsetX << " " << fOffsetY << endl;
}

void SpectrometerCHODAssociation::LoadOffsets() {
  NA62ConditionsService::GetInstance()->Open(fAlignmentFileName);
  TString Line;
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fAlignmentFileName))) {
    if (Line.BeginsWith("#")) continue;
    TObjArray *l = Line.Tokenize(" ");
    fOffsetX = ((TObjString*)(l->At(0)))->GetString().Atof(); // X offset for V-plane
    fOffsetY = ((TObjString*)(l->At(3)))->GetString().Atof(); // Y offset for H-plane
    delete l;
  }
  NA62ConditionsService::GetInstance()->Close(fAlignmentFileName);
}

void SpectrometerCHODAssociation::Process(Int_t) {
  if (!GetIsTree()) return;
  SetOutputState("Output", kOValid);
  fOutput.clear();

  TRecoCHODEvent* CHODevent = GetEvent<TRecoCHODEvent>();
  TRecoSpectrometerEvent* STRAWevent = GetEvent<TRecoSpectrometerEvent>();

  Double_t Z_CHODVPlane = GeometricAcceptance::GetInstance()->GetZCHODVPlane();
  Double_t Z_CHODHPlane = GeometricAcceptance::GetInstance()->GetZCHODHPlane();

  // Shower-like event flag: association not performed for such events
  Bool_t ShowerFlag = (CHODevent->GetNHits()-2*STRAWevent->GetNCandidates() > 30);

  for (Int_t iTrack=0; iTrack<STRAWevent->GetNCandidates(); iTrack++) {
    TRecoSpectrometerCandidate* Scand = static_cast<TRecoSpectrometerCandidate*>(STRAWevent->GetCandidate(iTrack));
    Double_t xt = Scand->xAtAfterMagnet(Z_CHODVPlane);
    Double_t yt = Scand->yAtAfterMagnet(Z_CHODHPlane);
    Double_t xt_with_offset = xt - fOffsetX;
    Double_t yt_with_offset = yt - fOffsetY;
    Double_t TrackTime = Scand->GetTime();

    // The search radius is chosen from the minimum bias data
    Double_t SearchRadius    = 20.0;
    Double_t CHODInnerRadius = GeometricAcceptance::GetInstance()->GetCHODRmin();
    Double_t CHODOuterRadius = GeometricAcceptance::GetInstance()->GetCHODRmax();
    Bool_t InnerAcceptance   = (sqrt(xt_with_offset*xt_with_offset+yt_with_offset*yt_with_offset)>CHODInnerRadius);
    Bool_t OuterAcceptance   = (sqrt(xt_with_offset*xt_with_offset+yt_with_offset*yt_with_offset)<CHODOuterRadius);
    Bool_t InAcceptance      = InnerAcceptance && OuterAcceptance;
    Double_t DistanceToOuterEdge = InAcceptance ?
      CHODOuterRadius - sqrt(xt_with_offset*xt_with_offset+yt_with_offset*yt_with_offset) : 0.0;

    SpectrometerCHODAssociationOutput Asso(iTrack);
    Asso.SetTrackTime(TrackTime);
    Asso.SetTrackXY(xt_with_offset, yt_with_offset);
    Asso.SetSearchRadius(SearchRadius);
    Asso.SetTrackMomentum(Scand->GetMomentum());
    Asso.SetInAcceptance(InAcceptance);
    Asso.SetDistanceToEdge(DistanceToOuterEdge);
    Asso.SetShowerFlag(ShowerFlag);

    Int_t ClosestCHODRecID = -1;
    Double_t RMin = 99999.9; // distance to the closest CHOD candidate

    if (!ShowerFlag) { // perform association only for non-shower-like events
      for (Int_t iC=0; iC<CHODevent->GetNCandidates(); iC++) {
	TRecoCHODCandidate* CHODCandidate = static_cast<TRecoCHODCandidate*>(CHODevent->GetCandidate(iC));
	Double_t xm = CHODCandidate->GetHitPosition().X();
	Double_t ym = CHODCandidate->GetHitPosition().Y();

	///////////////////////////////////////////////////
	// Find corners of the tile; check for direct match

	Int_t *hit_indexes = CHODCandidate->GetHitsIndexes();
	Int_t iHit1 = hit_indexes[0]; // get first hit
	TRecoCHODHit* RecoHit1 = static_cast<TRecoCHODHit*>(CHODevent->GetHit(iHit1));
	Int_t iHit2 = hit_indexes[1]; // get second hit
	TRecoCHODHit* RecoHit2 = static_cast<TRecoCHODHit*>(CHODevent->GetHit(iHit2));

	Int_t ID_VHit = RecoHit1->GetChannelID();
	Int_t ID_HHit = RecoHit2->GetChannelID();
	Double_t x_width = fSlabWidth[ID_VHit];
	Double_t y_width = fSlabWidth[ID_HHit];
	Double_t xm1 = xm - 0.5*x_width;
	Double_t xm2 = xm + 0.5*x_width;
	Double_t ym1 = ym - 0.5*y_width;
	Double_t ym2 = ym + 0.5*y_width;

	// histogram for checking the search radius
	if (CHODevent->GetNCandidates()==1) {
	  if (x_width==65) FillHisto("hTrackCHODCoordinateVsID_65", ID_VHit, xt-xm);
	  if (x_width==99) FillHisto("hTrackCHODCoordinateVsID_99", ID_VHit, xt-xm);
	  if (y_width==65) FillHisto("hTrackCHODCoordinateVsID_65", ID_HHit, yt-ym);
	  if (y_width==99) FillHisto("hTrackCHODCoordinateVsID_99", ID_HHit, yt-ym);
	}

	Double_t TrackSlabEdgeDistance = 0.0;
	Bool_t   DirectMatch = (xt_with_offset>xm1 && xt_with_offset<xm2 && yt_with_offset>ym1 && yt_with_offset<ym2);

	if (!DirectMatch) { // extrapolated track misses the tile: compute the distance
	  if      (yt_with_offset>ym1 && yt_with_offset<ym2)
	    TrackSlabEdgeDistance = TMath::Min(fabs(xt_with_offset-xm1), fabs(xt_with_offset-xm2));
	  else if (xt_with_offset>xm1 && xt_with_offset<xm2)
	    TrackSlabEdgeDistance = TMath::Min(fabs(yt_with_offset-ym1), fabs(yt_with_offset-ym2));
	  else if (xt_with_offset>xm2 && yt_with_offset>ym2)
	    TrackSlabEdgeDistance = sqrt((xt_with_offset-xm2)*(xt_with_offset-xm2)+(yt_with_offset-ym2)*(yt_with_offset-ym2));
	  else if (xt_with_offset>xm2 && yt_with_offset<ym1)
	    TrackSlabEdgeDistance = sqrt((xt_with_offset-xm2)*(xt_with_offset-xm2)+(yt_with_offset-ym1)*(yt_with_offset-ym1));
	  else if (xt_with_offset<xm1 && yt_with_offset<ym1)
	    TrackSlabEdgeDistance = sqrt((xt_with_offset-xm1)*(xt_with_offset-xm1)+(yt_with_offset-ym1)*(yt_with_offset-ym1));
	  else if (xt_with_offset<xm1 && yt_with_offset>ym2)
	    TrackSlabEdgeDistance = sqrt((xt_with_offset-xm1)*(xt_with_offset-xm1)+(yt_with_offset-ym2)*(yt_with_offset-ym2));
	}

	if (TrackSlabEdgeDistance<SearchRadius) {
	  Double_t TrackCHODCandidateDistance =
            sqrt((xt_with_offset-xm)*(xt_with_offset-xm)+(yt_with_offset-ym)*(yt_with_offset-ym));
          if (TrackCHODCandidateDistance<RMin) {
            RMin = TrackCHODCandidateDistance;
            ClosestCHODRecID = Asso.GetNAssociationRecords();
          }
	  SpectrometerCHODAssociationRecord Record
	    (iC, CHODCandidate, ID_VHit, ID_HHit, CHODCandidate->GetTime(),
	     TrackSlabEdgeDistance, TrackCHODCandidateDistance, xm, ym, DirectMatch);
	  FillHisto("hDistanceTrackCHODCandidate", TrackCHODCandidateDistance);
	  Asso.AddAssociationRecord(Record);
	}
        Asso.SetBestAssociationRecordID(ClosestCHODRecID);
      }
    }
    fOutput.push_back(Asso);
  } // end of loop over Spectrometer candidates
}

void SpectrometerCHODAssociation::EndOfJobUser() {
  if (!GetIsTree()) return;
  SaveAllPlots();
}
