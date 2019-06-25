// ---------------------------------------------------------------
// History:
//
// Created by Viacheslav Duk (Viacheslav.Duk@cern.ch) 15.06.2018
// ---------------------------------------------------------------

/// \class FastMCHandler
/// \Brief
/// Simple simulation of NewCHOD and MUV3 response for fast MC
/// \EndBrief
/// \Detailed
/// In the one-step fast simulation all detectors downstream the Spectrometer
/// are disabled. The most crucial detectors for analyses are NewCHOD
/// (track matching) and MUV3 (PID). NewCHOD hits are injected for true muon tracks
/// and for pions from K+ decay (if a muon/pion was produced before NewCHOD).
/// MUV3 hits are injected for for true muon tracks (if a muon was produced before LKr).
/// Hits are assigned to tiles from linear track extrapolation, corrected for multiple scattering.
/// The hit times are set to zero for the moment.
/// A good Cedar candidate is also added.
/// \author Viacheslav Duk (Viacheslav.Duk@cern.ch)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include "FastMCHandler.hh"
#include "Event.hh"
#include "Persistency.hh"

#include "GeometricAcceptance.hh"
#include "NewCHODGeometry.hh"
#include "TCedarDigi.hh"
#include "TNewCHODDigi.hh"
#include "TMUV3Digi.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

FastMCHandler::FastMCHandler(Core::BaseAnalysis *ba) : Analyzer(ba, "FastMCHandler") {
  RequestTree("Cedar",   new TRecoCedarEvent);
  RequestTree("NewCHOD", new TRecoNewCHODEvent);
  RequestTree("MUV3",    new TRecoMUV3Event);

  fNewCHODGeometry = new NewCHODGeometry();
  fMUV3Geometry    = new MUV3Geometry();

  fZNewCHOD = GeometricAcceptance::GetInstance()->GetZNewCHOD();
  fZLKr     = GeometricAcceptance::GetInstance()->GetZLKr();
  fZMUV3    = GeometricAcceptance::GetInstance()->GetZMUV3();

  fGenerateMUV3HitsFromPions = true;
  fGenerateMuonMultipleScattering = true;
  fMultScatteringParameter = 530000.0; // [MeV*mm]

  fRandom = new TRandom2();
}

FastMCHandler::~FastMCHandler() {
  if (fMUV3Geometry)    delete fMUV3Geometry;
  if (fNewCHODGeometry) delete fNewCHODGeometry;
  if (fRandom)          delete fRandom;
}

void FastMCHandler::InitOutput() {
  RegisterOutput("IsFastSimulatedSample", &fFastSimulation);
}

void FastMCHandler::StartOfBurstUser() {
  fFastSimulation = false;
  if (!GetIsTree()) return;
  if (GetWithMC()) fFastSimulation = GetStreamInfo()->GetMCInfo().GetFastSimulationMode();
  fRandom->SetSeed(GetEventHeader()->GetBurstID()); // to ensure reproducibility
}

void FastMCHandler::Process(Int_t) {
  if (!fFastSimulation) return;
  if (!GetIsTree()) return;

  TRecoCedarEvent   *CedarEvent   = GetEvent<TRecoCedarEvent>();
  TRecoNewCHODEvent *NewCHODEvent = GetEvent<TRecoNewCHODEvent>();
  TRecoMUV3Event    *MUV3Event    = GetEvent<TRecoMUV3Event>();

  // Add Cedar hits
  TCedarDigi* CedarDigi = new TCedarDigi();
  Int_t NHitsToBeAdded = fRandom->Poisson(18);
  Int_t NHitsInSector[8] = {0, 0, 0, 0, 0, 0, 0, 0};
  for (Int_t iHit=0; iHit<NHitsToBeAdded; iHit++) {
    TRecoCedarHit *hit = static_cast<TRecoCedarHit*>(CedarEvent->AddHit(CedarDigi));
    Int_t iSector   = (Int_t)fRandom->Integer(8);
    Int_t ChannelID = 100*(iSector+1)+55; // dummy channel ID
    Int_t ROChannelID = ChannelID; // dummy channel ID
    hit->SetChannelID(ChannelID);
    hit->SetROChannelID(ROChannelID);
    hit->DecodeChannelID();
    hit->SetTime(0.);
    NHitsInSector[iSector]++;
  }
  delete CedarDigi;

  // Add a Cedar candidate
  TRecoCedarCandidate *CedarCandidate = static_cast<TRecoCedarCandidate*>(CedarEvent->AddCandidate());
  for (Int_t iHit=0; iHit<NHitsToBeAdded; iHit++) CedarCandidate->AddHit(iHit);
  Int_t NSectors = 0;
  for (Int_t iSector=0; iSector<8; iSector++) {
    if (NHitsInSector[iSector]>0) NSectors++;
  }
  CedarCandidate->SetNSectors(NSectors);

  ////////////////////////////////////////////
  // Add NewCHOD hits and MUV3 hits/candidates

  Event *evt = GetMCEvent();
  for (Int_t ipart=0; ipart<evt->GetNKineParts(); ipart++) {
    KinePart *Particle = static_cast<KinePart*>(evt->GetKineParts()->At(ipart));

    Double_t ProdZ = Particle->GetProdPos().Z();
    if (ProdZ>fZLKr) continue; // consider only particles produced upstream of the LKr
    Int_t code = Particle->GetPDGcode();
    if (Particle->GetEndPos().Z()<fZMUV3 && abs(code)==13) continue; // backward extrapolation does not work for these ones

    if (abs(code)==13) { // muon (+13: mu- ; -13: mu+)

      // Extrapolating backwards from the endpoint: no checkpoints after MNP33 exit (EG June 2018)
      TVector2 RealTrackPositionAtMUV3(Particle->xAtBackwards(fZMUV3), Particle->yAtBackwards(fZMUV3));

      // Add multiple scattering according to the most simple model (EG June 2018)
      if (fGenerateMuonMultipleScattering) {
	TVector2 Scattering
	  (fRandom->Gaus(0.0, fMultScatteringParameter/Particle->GetInitialMomentum().Mag()),
	   fRandom->Gaus(0.0, fMultScatteringParameter/Particle->GetInitialMomentum().Mag()));
	RealTrackPositionAtMUV3 += Scattering;
      }
      Int_t TrackMUV3TileID = fMUV3Geometry->GetTileID(RealTrackPositionAtMUV3);

      Int_t TrackNewCHODTileID = 999;
      if (ProdZ<fZNewCHOD) {
	TVector2 RealTrackPositionAtNewCHOD(Particle->xAtBackwards(fZNewCHOD), Particle->yAtBackwards(fZNewCHOD));
	TrackNewCHODTileID = fNewCHODGeometry->GetTileID(RealTrackPositionAtNewCHOD);
      }

      if (TrackMUV3TileID!=200) { // track reaches a MUV3 tile

	// MUV3 hits/candidates added only if there are no existing candidates in this tile (EG June 2018)
	Bool_t TileBusy = false;
	for (Int_t iCand=0; iCand<MUV3Event->GetNCandidates(); iCand++) {
	  TRecoMUV3Candidate *cand = static_cast<TRecoMUV3Candidate*>(MUV3Event->GetCandidate(iCand));
	  if (TrackMUV3TileID == cand->GetTileID()) TileBusy = true;
	}

	if (!TileBusy) {

	  // add MUV3 hits
	  TMUV3Digi* MUV3Digi = new TMUV3Digi();
	  TRecoMUV3Hit *hit1 = static_cast<TRecoMUV3Hit*>(MUV3Event->AddHit(MUV3Digi));
	  hit1->SetChannelID(TrackMUV3TileID);
	  hit1->DecodeChannelID();
	  TVector3 Position(fMUV3Geometry->GetTileCentreX(TrackMUV3TileID),
			    fMUV3Geometry->GetTileCentreY(TrackMUV3TileID),
			    fZMUV3);
	  hit1->SetPosition(Position);
	  hit1->SetTime(0.);
	  TRecoMUV3Hit *hit2 = static_cast<TRecoMUV3Hit*>(MUV3Event->AddHit(MUV3Digi));
	  hit2->SetChannelID(TrackMUV3TileID+200);
	  hit2->DecodeChannelID();
	  hit2->SetPosition(Position);
	  hit2->SetTime(0.);
	  delete MUV3Digi;

	  // add a MUV3 candidate
	  TRecoMUV3Candidate *Candidate = static_cast<TRecoMUV3Candidate*>(MUV3Event->AddCandidate());
	  Candidate->SetType(kTightCandidate);
	  Candidate->AddHit(MUV3Event->GetNHits()-1); // associate a hit to candidate 
	  Candidate->AddHit(MUV3Event->GetNHits()); // associate a hit to candidate 
	  Candidate->SetTileID(TrackMUV3TileID);
	  Candidate->SetChannel1(TrackMUV3TileID);
	  Candidate->SetChannel2(TrackMUV3TileID+200);
	  Candidate->SetX(fMUV3Geometry->GetTileCentreX(TrackMUV3TileID));
	  Candidate->SetY(fMUV3Geometry->GetTileCentreY(TrackMUV3TileID));
	  Candidate->SetTime1(0.);
	  Candidate->SetTime2(0.);
	  Candidate->SetTime(0.);
	}
      }

      if (TrackNewCHODTileID!=999) {

	// add a NewCHOD hit
	TNewCHODDigi* NewCHODDigi = new TNewCHODDigi();
    TRecoNewCHODHit *NewCHODHit1 = static_cast<TRecoNewCHODHit*>(NewCHODEvent->AddHit(NewCHODDigi));
	NewCHODHit1->SetChannelID(TrackNewCHODTileID);
	NewCHODHit1->DecodeChannelID();
	NewCHODHit1->SetChannel1(TrackNewCHODTileID);
	NewCHODHit1->SetChannel2(TrackNewCHODTileID+50);
	NewCHODHit1->SetTime1(0.);
	NewCHODHit1->SetTime2(0.);
	NewCHODHit1->SetTime(0.);
	NewCHODHit1->SetX(fNewCHODGeometry->GetTileCentreX(TrackNewCHODTileID));
	NewCHODHit1->SetY(fNewCHODGeometry->GetTileCentreY(TrackNewCHODTileID));
	NewCHODHit1->SetType(kTightCandidate);
        delete NewCHODDigi;
      }
    } // end of the muon part

    if (abs(code)==211 && Particle->GetParentIndex()==0) { // pi+ and pi- from K+ decay

      // track extrapolation to the MUV3 plane
      Double_t fZMNP33Exit = 203764.;
      TLorentzVector ThisMomentum = Particle->GetMomMNP33Exit();
      if (Particle->GetProdPos().Z()>fZMNP33Exit) ThisMomentum = Particle->GetInitial4Momentum();
      Double_t ThisR;
      Bool_t Accepted = false;
      Double_t Maximum = 1300.; // maximum of tile-track distance functions
      while(!Accepted) {
        ThisR = fRandom->Rndm()*1400.;
	if (Maximum*fRandom->Rndm() < GenerateDistanceToTile(ThisR, ThisMomentum.P())) Accepted = true;
      }
      Double_t ThisPhi = 2.0*fRandom->Rndm()*TMath::Pi();
      Double_t ThisX = ThisR*TMath::Cos(ThisPhi);
      Double_t ThisY = ThisR*TMath::Sin(ThisPhi);

      TVector2 RealTrackPositionAtMUV3;
      RealTrackPositionAtMUV3.Set(Particle->xAtBackwards(fZMUV3) + ThisX, Particle->yAtBackwards(fZMUV3) + ThisY);
      Int_t TrackMUV3TileID = fMUV3Geometry->GetTileID(RealTrackPositionAtMUV3);

      Int_t TrackNewCHODTileID = 999;
      if (ProdZ<fZNewCHOD) {
	TVector2 RealTrackPositionAtNewCHOD(Particle->xAtBackwards(fZNewCHOD), Particle->yAtBackwards(fZNewCHOD));
        TrackNewCHODTileID = fNewCHODGeometry->GetTileID(RealTrackPositionAtNewCHOD);
      }

      // add a NewCHOD hit
      if (TrackNewCHODTileID!=999) {
	TNewCHODDigi* NewCHODDigi = new TNewCHODDigi();
	TRecoNewCHODHit *NewCHODHit1 = static_cast<TRecoNewCHODHit*>(NewCHODEvent->AddHit(NewCHODDigi));
	NewCHODHit1->SetChannelID(TrackNewCHODTileID);
	NewCHODHit1->DecodeChannelID();
        NewCHODHit1->SetChannel1(TrackNewCHODTileID);
        NewCHODHit1->SetChannel2(TrackNewCHODTileID+50);
	NewCHODHit1->SetTime1(0.);
        NewCHODHit1->SetTime2(0.);
        NewCHODHit1->SetTime(0.);
        NewCHODHit1->SetX(fNewCHODGeometry->GetTileCentreX(TrackNewCHODTileID));
        NewCHODHit1->SetY(fNewCHODGeometry->GetTileCentreY(TrackNewCHODTileID));
        NewCHODHit1->SetType(kTightCandidate);
	delete NewCHODDigi;
      }

      Bool_t InelasticFlag  = false;
      // 0.00996 is the probability to have an extra MUV3 candidate from a pion, calculated from pnn MC
      if (fRandom->Rndm()<0.00996) InelasticFlag = true; 
      if (TrackMUV3TileID!=200 && Particle->GetEndPos().Z()>fZLKr && InelasticFlag) { // track reaches a MUV3 tile; pion reaches LKr

	Bool_t TileBusy = false;
	for (Int_t iCand=0; iCand<MUV3Event->GetNCandidates(); iCand++) {
	  TRecoMUV3Candidate *cand = static_cast<TRecoMUV3Candidate*>(MUV3Event->GetCandidate(iCand));
	  if (TrackMUV3TileID == cand->GetTileID()) TileBusy = true;
	}

	if (!TileBusy && fGenerateMUV3HitsFromPions) {
	  // add MUV3 hits
	  TMUV3Digi* MUV3Digi = new TMUV3Digi();
	  TRecoMUV3Hit *hit1 = static_cast<TRecoMUV3Hit*>(MUV3Event->AddHit(MUV3Digi));
	  hit1->SetChannelID(TrackMUV3TileID);
	  hit1->DecodeChannelID();
	  TVector3 Position(fMUV3Geometry->GetTileCentreX(TrackMUV3TileID),
			    fMUV3Geometry->GetTileCentreY(TrackMUV3TileID),
			    fZMUV3);
	  hit1->SetPosition(Position);
	  hit1->SetTime(0.);
	  TRecoMUV3Hit *hit2 = static_cast<TRecoMUV3Hit*>(MUV3Event->AddHit(MUV3Digi));
	  hit2->SetChannelID(TrackMUV3TileID+200);
	  hit2->DecodeChannelID();
	  hit2->SetPosition(Position);
	  hit2->SetTime(0.);
	  delete MUV3Digi;

	  // add a MUV3 candidate
	  TRecoMUV3Candidate *Candidate = static_cast<TRecoMUV3Candidate*>(MUV3Event->AddCandidate());
	  Candidate->SetType(kTightCandidate);
	  Candidate->AddHit(MUV3Event->GetNHits()-1); // associate a hit to candidate
	  Candidate->AddHit(MUV3Event->GetNHits()); // associate a hit to candidate
	  Candidate->SetTileID(TrackMUV3TileID);
	  Candidate->SetChannel1(TrackMUV3TileID);
	  Candidate->SetChannel2(TrackMUV3TileID+200);
	  Candidate->SetX(fMUV3Geometry->GetTileCentreX(TrackMUV3TileID));
	  Candidate->SetY(fMUV3Geometry->GetTileCentreY(TrackMUV3TileID));
	  Candidate->SetTime1(0.);
	  Candidate->SetTime2(0.);
	  Candidate->SetTime(0.);
	}
      } // end of MUV3 candidate generation from pions
    } // end of the pion part
  }
}

void FastMCHandler::PostProcess() {
  if (!fFastSimulation) return;
  if (!GetIsTree()) return;

  // For fast simulaton, clean up NewCHOD and MUV3 events manually
  TRecoNewCHODEvent *NewCHODEvent = GetEvent<TRecoNewCHODEvent>();
  TRecoMUV3Event    *MUV3Event    = GetEvent<TRecoMUV3Event>();
  NewCHODEvent->Clear();
  MUV3Event->Clear();
}

Double_t FastMCHandler::GenerateDistanceToTile(Double_t x, Double_t p) {
  // track-tile distance is parametrized by a polynomial in 4 momentum bins
  // the parametrization is calculated on pnn MC
  Double_t p0[] = {1.84240e+00,   4.44633e+00,  7.86402e+00,  1.25444e+01};
  Double_t p1[] = {-6.06584e-03, -1.38644e-02, -2.56062e-02, -4.31414e-02};
  Double_t p2[] = {7.77312e-06,   1.65862e-05,  3.19792e-05,  5.62940e-05};
  Double_t p3[] = {-4.49807e-09, -8.91258e-09, -1.79143e-08, -3.25911e-08};
  Double_t p4[] = {9.78920e-13,   1.80085e-12,  3.76671e-12,  7.01617e-12};

  Int_t iRange = 0;
  if (p>20000. && p<=30000.) iRange = 1;
  if (p>30000. && p<=40000.) iRange = 2;
  if (p>40000.)              iRange = 3;

  return p0[iRange]*pow(x,1) + p1[iRange]*pow(x,2) + p2[iRange]*pow(x,3) + p3[iRange]*pow(x,4) + p4[iRange]*pow(x,5);
}
