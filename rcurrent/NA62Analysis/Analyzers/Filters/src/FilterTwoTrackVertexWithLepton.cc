// ---------------------------------------------------------------
// History:
//
// Created by Viacheslav Duk (Viacheslav.Duk@cern.ch) 10.12.2016
//
// ---------------------------------------------------------------

#include <stdlib.h>
#include <iostream>
#include "FilterTwoTrackVertexWithLepton.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "DownstreamTrack.hh"
#include "GeometricAcceptance.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class FilterTwoTrackVertexWithLepton
/// \Brief
/// Selects events with at least 3 tracks and at least one pair of tracks
/// with a good vertex and at least one lepton
/// \EndBrief
/// \Detailed
/// This filter is useful for the studies of long-lived particles decaying to a pair of charged particles.
/// The filter is used in a standard way, the simplest way being
/// \code
/// ./MyApplication -i <input_file> -o <output_file> --filter
/// \endcode 
/// \author Viacheslav Duk (Viacheslav.Duk@cern.ch)
/// \EndDetailed

FilterTwoTrackVertexWithLepton::FilterTwoTrackVertexWithLepton(Core::BaseAnalysis *ba) :
  Analyzer(ba, "FilterTwoTrackVertexWithLepton") {
  RequestAllMCTrees();
  RequestAllRecoTrees();
  fCDAcomp = new TwoLinesCDA();
}

FilterTwoTrackVertexWithLepton::~FilterTwoTrackVertexWithLepton() {
  delete fCDAcomp;
}

void FilterTwoTrackVertexWithLepton::InitHist() {

  BookHisto("hTwoTrackVertexBefore",     new TH1F("TwoTrackVertexBefore", "Z-coordinate of the two-track vertex before the filter", 100, 0., 200000.));
  BookHisto("hTwoTrackMomentumBefore",   new TH1F("TwoTrackMomentumBefore", "Two-track momentum before the filter", 100, 0., 100.));

  BookHisto("hFilterFlag",               new TH1F("FilterFlag", "Filter flag", 2, -0.5, 1.5));

  BookHisto("hNTracks",                  new TH1F("NTracks", "N good tracks after the filter", 10, -0.5, 9.5));
  BookHisto("hNChambers",                new TH1F("NChambers", "N chambers after the filter", 5, -0.5, 4.5));
  BookHisto("hCDA",                      new TH1F("CDA", "CDA after the filter", 100, 0., 100.));
  BookHisto("hTwoTrackVertex",           new TH1F("TwoTrackVertex", "Z-coordinate of the two-track vertex after the filter", 100, 0., 200000.));
  BookHisto("hTwoTrackMomentum",         new TH1F("TwoTrackMomentum", "Two-track momentum after the filter", 100, 0., 100.));
  BookHisto("hDtNewCHOD",                new TH1F("DtNewCHOD", "dt NewCHOD after the filter", 200, -10., 10.));
  BookHisto("hDtTrigger",                new TH1F("DtTrigger", "(Ttrack - Ttrigger) after the filter", 200, -10., 10.));
}

void FilterTwoTrackVertexWithLepton::Process(Int_t) {

  Bool_t FilterFlag = false, LeptonFlag = false;
  Double_t EoPCut = 0.9;

  std::vector<DownstreamTrack> Tracks =
    *(std::vector<DownstreamTrack>*) GetOutput("DownstreamTrackBuilder.Output");

  Int_t NGoodTracks = 0;
  for(UInt_t i=0; i<Tracks.size(); i++){
    if (IsGoodTrack(Tracks[i])){
      NGoodTracks++;
    }
  }

  if (NGoodTracks>=3) {
    for (UInt_t i = 0; i < Tracks.size(); i++) {
      if (!IsGoodTrack(Tracks[i])) continue;
      
      for (UInt_t j = i+1; j < Tracks.size(); j++) {
        if (!IsGoodTrack(Tracks[j])) continue;
	
	// calculate CDA and vertex for two tracks
	fCDAcomp->SetLine1Point1(Tracks[i].GetPositionBeforeMagnet());
	fCDAcomp->SetLine2Point1(Tracks[j].GetPositionBeforeMagnet());
	fCDAcomp->SetDir1(Tracks[i].GetMomentumBeforeMagnet());
	fCDAcomp->SetDir2(Tracks[j].GetMomentumBeforeMagnet());
	fCDAcomp->ComputeVertexCDA();
	// calculate the total momentum of a track pair (MomentumSum)
	TVector3 MomentumSum = Tracks[i].GetMomentumBeforeMagnet() + Tracks[j].GetMomentumBeforeMagnet();
	// check if there is a lepton (muon ID: MUV3 assocation; electron ID: EoP)
	LeptonFlag = false;
	if (Tracks[i].MUV3AssociationExists() || Tracks[j].MUV3AssociationExists() ||
	    Tracks[i].GetLKrTotalEoP()>EoPCut || Tracks[j].GetLKrTotalEoP()>EoPCut)
	  LeptonFlag = true;
	// fill histograms illustrating cuts
	FillHisto("hTwoTrackVertexBefore", fCDAcomp->GetVertex().Z());
	FillHisto("hTwoTrackMomentumBefore", MomentumSum.Mag()*0.001);

	// cuts on CDA, z_vertex, MomentumSum and LeptonFlag
	if (fCDAcomp->GetCDA() < 25. &&
	    fCDAcomp->GetVertex().Z() > 103000. && fCDAcomp->GetVertex().Z() < 180000. && 
	    MomentumSum.Mag() < 70000. &&
	    LeptonFlag) {
	  // set a positive filter flag 
	  FilterFlag = true;

	  // fill histograms
	  FillHisto("hCDA", fCDAcomp->GetCDA());
	  FillHisto("hTwoTrackVertex", fCDAcomp->GetVertex().Z());
	  FillHisto("hTwoTrackMomentum", MomentumSum.Mag()*0.001);
	  FillHisto("hDtNewCHOD", Tracks[i].GetNewCHODTime()-Tracks[j].GetNewCHODTime());
	  FillHisto("hDtTrigger", Tracks[i].GetNewCHODTime()-GetL0Data()->GetReferenceFineTime()*TdcCalib);
	  FillHisto("hDtTrigger", Tracks[j].GetNewCHODTime()-GetL0Data()->GetReferenceFineTime()*TdcCalib);
	  FillHisto("hNChambers", Tracks[i].GetNChambers());
	  FillHisto("hNChambers", Tracks[j].GetNChambers());
	}
      }
    }
  }

  FillHisto("hFilterFlag", FilterFlag);
  if (FilterFlag) {
    FillHisto("hNTracks", NGoodTracks);
  }
  if (!FilterFlag) return;

  FilterAccept();
}

void FilterTwoTrackVertexWithLepton::EndOfJobUser() {
  SaveAllPlots();
}

Bool_t FilterTwoTrackVertexWithLepton::IsGoodTrack(DownstreamTrack Track)
{
  Bool_t IsGood = true;
  Int_t ChambersInAcceptance;
  // check geometric acceptance in STRAW
  ChambersInAcceptance = 0;
  for (Int_t iChamber=0; iChamber<4; iChamber++) {
    ChambersInAcceptance += GeometricAcceptance::GetInstance()->InAcceptance(&Track, kSpectrometer, iChamber);
  }
  if (ChambersInAcceptance!=4) IsGood = false;

  // quality cuts
  if (Track.GetChi2()>20. || fabs(Track.GetMomentumBeforeFit()-Track.GetMomentum())>20000.) IsGood = false;
  // momentum cut (p >= 5 GeV)
  if (Track.GetMomentum() < 5000.) IsGood = false;
  // in NewCHOD acceptance
  if (!GeometricAcceptance::GetInstance()->InAcceptance(&Track, kNewCHOD)) IsGood = false;
  // in LKr acceptance 
  if (!GeometricAcceptance::GetInstance()->InAcceptance(&Track, kLKr)) IsGood = false;
  // in MUV3 acceptance
  if (!GeometricAcceptance::GetInstance()->InAcceptance(&Track, kMUV3)) IsGood = false;
  // cut on the track time 
  if (fabs(Track.GetTrackTime()-GetL0Data()->GetReferenceFineTime()*TdcCalib)>8.) IsGood = false;

  return IsGood;
}
