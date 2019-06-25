// ---------------------------------------------------------------
//
// History:
// 
// Adapted from the SpectrometerRICHAssociationSingleRing.cc
// by Francesco Brizioli (francesco.brizioli@cern.ch)
//
// ---------------------------------------------------------------

/// \class SpectrometerRICHAssociationTrackCentredRing
/// \Brief
/// Time association of "single ring track centred" RICH candidates to Spectrometer candidates
/// \EndBrief
/// \Detailed
/// The output is a SpectrometerRICHAssociation structure.
/// For each track candidate in the STRAW Spectrometer the best association with a RICH ring is given.
/// The RICH ring are reconstructed by the RICHSingleRingTrkCentredFit method.
/// Below is a basic example of use.
/// \code
///  std::vector<SpectrometerRICHAssociationOutputTrackCentredRing> SpecRICHTrackCentredRing =
///    *(std::vector<SpectrometerRICHAssociationOutputTrackCentredRing>*)GetOutput("SpectrometerRICHAssociationTrackCentredRing.Output");
///  for (UInt_t itrack=0; itrack<SpecRICH.size(); itrack++) {
///    SpectrometerRICHAssociationOutputTrackCentredRing sr = SpecRICHTrackCentredRing[itrack];
///    Int_t RingID = sr.GetRingID(); // RingID = -1 if association not done
///    if (RingID>-1) { // association has been done
///      Double_t Radius = sr.GetRingRadius();
///      // ...
///    }
///  }
/// \endcode
/// The output is also available from the Downstramtrack class, for example: DownstreamTrack::GetRICHSingleRingTrkCentredRadius()
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include "SpectrometerRICHAssociationTrackCentredRing.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "NA62ConditionsService.hh"
#include "RICHParameters.hh"
#include "RICHSingleRingTrkCentredFit.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

SpectrometerRICHAssociationTrackCentredRing::SpectrometerRICHAssociationTrackCentredRing(Core::BaseAnalysis *ba) :
  Analyzer(ba, "SpectrometerRICHAssociationTrackCentredRing"), fRefIndex(0.), fPoisson(nullptr), fSingleRingRadius(0.), fSingleRingChi2(0.), fSingleRingNHits(0), fElectronRingRadius(0.), fElectronRingNhits(0.){

  fContainer.clear();

  if (!GetIsTree()) return;

  RequestTree("RICH",         new TRecoRICHEvent,         "Reco");
  RequestTree("Spectrometer", new TRecoSpectrometerEvent, "Reco");

  fPoisson = new TF1("PoissonFunction", "TMath::Poisson(x, [0])", 0, 100);
  fRICHMirrorZPos = GeometricAcceptance::GetInstance()->GetZRICHMirror();
  fFocalLength    = 17020.0;

  fSingleRing = RICHSingleRingTrkCentredFit::GetInstance();
}

SpectrometerRICHAssociationTrackCentredRing::~SpectrometerRICHAssociationTrackCentredRing() {
  if (fPoisson) delete fPoisson;
}

void SpectrometerRICHAssociationTrackCentredRing::InitOutput() {
  if (!GetIsTree()) return;
  RegisterOutput("Output", &fContainer);
}

void SpectrometerRICHAssociationTrackCentredRing::InitHist() {

  NA62ConditionsService::GetInstance()->SetCurrentRunID(GetRunID());
  for (Int_t i=0; i<20; i++) fMirrorNumber[i] = RICHParameters::GetInstance()->GetMirrorNumber(i);
  for (Int_t i=0; i<25; i++) fMirrorAlignment[i][0] = fMirrorAlignment[i][1] = 0.0;

  // Initialize mirror positions
  for (Int_t i=0; i<20; i++) { // sequential numbering used in this class
    fMirrorPosition[i][0] = RICHParameters::GetInstance()->GetMirrorCentreX(fMirrorNumber[i]);
    fMirrorPosition[i][1] = RICHParameters::GetInstance()->GetMirrorCentreY(fMirrorNumber[i]);
  }
  for (Int_t i=0; i<25; i++) { // "standard" non-sequential numbering
    fMirrorSide[i] = -999;
    if (RICHParameters::GetInstance()->MirrorExists(i)) {
      fMirrorSide[i] = RICHParameters::GetInstance()->JuraMirror(i) ? 0 : 1;
    }
  }

  // Semihex mirrors: non-standard centres (defined as averages of other mirrors).
  // The centres are evaluated for full mirrors, not half-mirrors, so x is close to 0.

  fMirrorPosition[18][0]=fMirrorPosition[1][0]; // x of Mirror 17
  fMirrorPosition[18][1]=(fMirrorPosition[7][1]+fMirrorPosition[8][1])/2.;
  fMirrorPosition[19][0]=fMirrorPosition[16][0]; // x of Mirror 22
  fMirrorPosition[19][1]=(fMirrorPosition[9][1]+fMirrorPosition[10][1])/2.;

  //////////////////////////////////

  if (!GetIsTree()) return;
 
  BookHisto(new TH1D("hNHits","hNHits", 100, -0.5,99.5));
  BookHisto(new TH1D("hNPMTimeCand","hNPMTimeCand", 20, -0.5,19.5));
  BookHisto(new TH1D("hNRICHCandidateGoodFit","hNRICHCandidateGoodFit", 20, -0.5,19.5));
  BookHisto(new TH1D("hRingID","hRingID", 20, -2.5,17.5));
  BookHisto(new TH1D("hSingleRing_Radius","hSingleRing_Radius", 130, 75.,205.));
  BookHisto(new TH1D("hSingleRing_nHits","hSingleRing_nHits", 50, -0.5,49.5));
  BookHisto(new TH1D("hSingleRing_RICHMass","hSingleRing_RICHMass", 200, -100.,300.));

}

void SpectrometerRICHAssociationTrackCentredRing::StartOfRunUser() {
  if (!GetIsTree()) return;
}

void SpectrometerRICHAssociationTrackCentredRing::StartOfBurstUser() {
  if (!GetIsTree()) return;
  ////////////////////////////////////////////////////////////
  // Update the parameters of the electron ring for each burst  
  Int_t  RunNumber = GetRunID();
  time_t BurstTime = GetEventHeader()->GetBurstTime();
  // Defaults currently used for MC
  fElectronRingRadius = 190.0;
  fElectronRingNhits = 14.0;
  if (!GetWithMC()) { // data: read parameters from the DB
    fElectronRingRadius = RICHParameters::GetInstance()->GetElectronRingRadius(RunNumber, BurstTime);
    fElectronRingNhits =  RICHParameters::GetInstance()->GetElectronRingNHits (RunNumber, BurstTime);
  }
  //------ 2017 -----
  // fElectronRingRadius = 190.565;
  // fElectronRingNhits = 14.0;
  //-----------------
  fRefIndex = 1.0/(cos(atan(fElectronRingRadius/fFocalLength)));
  // Printout with -v2 or higher verbosity
  cout << user() <<
    Form("Run=%d, BurTime=%ld, Radius=%6.2fmm, RefIndex=%5.2fppm, Nhits=%5.2f",
	 RunNumber, BurstTime, fElectronRingRadius, (fRefIndex-1.0)*1.0e6, fElectronRingNhits) << endl;
}

void SpectrometerRICHAssociationTrackCentredRing::EndOfJobUser() {
  SaveAllPlots();
}

void SpectrometerRICHAssociationTrackCentredRing::Process(Int_t) {
  SetOutputState("Output", kOValid);
  fContainer.clear();

  if (!GetIsTree()) return;

  TRecoSpectrometerEvent* STRAWEvent = static_cast<TRecoSpectrometerEvent*>(GetEvent("Spectrometer"));
  TRecoRICHEvent*         RICHEvent  = static_cast<TRecoRICHEvent*>(GetEvent("RICH"));
  Int_t nRichHits = RICHEvent->GetNHits();
  Int_t nGoodRichHits = 0;
  for (Int_t i=0; i<nRichHits; i++) {
    TRecoRICHHit* hit = static_cast<TRecoRICHHit*>(RICHEvent->GetHit(i));
    if (hit->GetOrSuperCellID()==0){
      nGoodRichHits++;
    }
  }

  std::vector<SpectrometerCHODAssociationOutput> SpecCHOD =
   *GetOutput<std::vector<SpectrometerCHODAssociationOutput>>("SpectrometerCHODAssociation.Output");

  FillHisto("hNPMTimeCand", RICHEvent->GetNPMTimeCandidates());
  FillHisto("hNHits", nGoodRichHits);

  for (Int_t iTrack=0; iTrack<STRAWEvent->GetNCandidates(); iTrack++) {
    TRecoSpectrometerCandidate* Scand = static_cast<TRecoSpectrometerCandidate*>(STRAWEvent->GetCandidate(iTrack));
    Double_t TrackTime = Scand->GetTime();
    if (SpecCHOD[iTrack].GetNAssociationRecords()>0) {
      TrackTime = SpecCHOD[iTrack].GetBestAssociationRecord()->GetCHODCandidateTime();
    }
    SpectrometerRICHAssociationOutputTrackCentredRing Asso(iTrack, TrackTime, Scand->GetMomentum());
    // Match track to rings produced by the RICHSingleRingTrkCentredFit 
    // Find the closest ring to the track (in time)

    TVector2 RingPosition = TVector2((fFocalLength * Scand->GetSlopeXAfterMagnet()),(fFocalLength * Scand->GetSlopeYAfterMagnet()));
    Asso.SetRingPosition(RingPosition);
    Asso.SetRingRadius  (-999999.9);
    Asso.SetRingChi2    (-999999.9);
    Asso.SetRingTime    (-999999.9);
    Asso.SetMass        (-999999.9);
    Asso.SetNHits       (-99999999);
    Int_t  SingleRingID = TrackSingleRingMatching(TrackTime,RICHEvent,Scand); // matching function; SingleRingID = -1 if association not done 
    FillHisto("hRingID",SingleRingID);
    Asso.SetRingID(SingleRingID);

    if (SingleRingID>-1) { // If a ring has been associated to the track, fill in the data fields.
      Double_t RICHMass = -9999.9;
      Double_t RICHMass2 = -9999.9;
      Double_t cosTheta = (fSingleRingRadius/fFocalLength);
      Double_t n2 = fRefIndex*fRefIndex;
      Double_t ptrack = Scand->GetMomentum();
      RICHMass2 = ptrack*ptrack*((n2/(1+cosTheta*cosTheta)) -1);
      if(RICHMass2>0) {
      	RICHMass = sqrt(RICHMass2);
      }
      else {
	RICHMass = -1.0*sqrt(-1.0*RICHMass2);
      }
      Asso.SetRingRadius  (fSingleRingRadius);
      Asso.SetRingChi2    (fSingleRingChi2);
      Asso.SetRingTime    (fSingleRingTime);
      Asso.SetMass(RICHMass);
      Asso.SetNHits(fSingleRingNHits);
      FillHisto("hSingleRing_RICHMass",RICHMass);
      FillHisto("hSingleRing_Radius",fSingleRingRadius);
      FillHisto("hSingleRing_nHits",fSingleRingNHits);
    }
    fContainer.push_back(Asso);
  }
}

Int_t SpectrometerRICHAssociationTrackCentredRing::TrackSingleRingMatching(Double_t TrackTime, TRecoRICHEvent* RICHEvent, TRecoSpectrometerCandidate *thisTrack) {
  TVector3 posTrackAtMirror =  TVector3(thisTrack->xAt(fRICHMirrorZPos), thisTrack->yAt(fRICHMirrorZPos),fRICHMirrorZPos);
  Int_t mirrorid = MirrorSurface(posTrackAtMirror.X(),posTrackAtMirror.Y());

  // Associate tracks with the closest track centred ring in time
  Double_t mintime = -1.0;
  Int_t AssoRingID = -1;
  if (mirrorid==99) return AssoRingID;

  Int_t NHitsTrackCentred ;
  Double_t RadiusTrackCentred, Chi2TrackCentred, TimeTrackCentred ;

  Int_t NRICHCandidateGoodFit = 0 ;
  for (Int_t jring=0; jring<RICHEvent->GetNPMTimeCandidates(); jring++) {
    TRecoRICHCandidate *ring = static_cast<TRecoRICHCandidate*>(RICHEvent->GetPMTimeCandidate(jring));
    Bool_t goodFit = fSingleRing->Chi2Fit(RICHEvent,ring,thisTrack, NHitsTrackCentred, RadiusTrackCentred, Chi2TrackCentred, TimeTrackCentred ); // Ring fit function is called
    //    cout<<"jring = "<<jring<<" ; goodFit = "<<goodFit<<endl; // debugging
    if (!goodFit) continue;
    NRICHCandidateGoodFit ++ ;
    Double_t dtime = fabs(TimeTrackCentred-TrackTime);
    if (dtime<mintime || NRICHCandidateGoodFit==1) {
      mintime = dtime;
      fSingleRingRadius = RadiusTrackCentred ; // updated by track-centred fit function
      fSingleRingChi2 = Chi2TrackCentred;  // updated by track-centred fit function
      fSingleRingNHits = NHitsTrackCentred;  // updated by track-centred fit function
      fSingleRingTime = TimeTrackCentred; // updated by track-centred fit function
      AssoRingID = jring; // index of the associated ring
    }  
  }
  FillHisto("hNRICHCandidateGoodFit",NRICHCandidateGoodFit) ;
  return AssoRingID; // -1 if association not done
}

Int_t SpectrometerRICHAssociationTrackCentredRing::MirrorSurface(Double_t xin, Double_t yin) { // mirror ID from track information
  Double_t dmin = 1.0e12;
  Int_t mirrorID = 99;
  for (Int_t im=0; im<20; im++) {
    Double_t d =
      pow(fMirrorPosition[im][0]-xin, 2) +
      pow(fMirrorPosition[im][1]-yin, 2);
    if (d<dmin) {
      dmin = d;
      mirrorID = fMirrorNumber[im];
    }
  } // end of loop on mirrors
  return mirrorID;
}
