// ---------------------------------------------------------------
// History:
//
// Created by Viacheslav Duk (Viacheslav.Duk@cern.ch) 24.02.2018
//
// ---------------------------------------------------------------

#include <stdlib.h>
#include <iostream>
#include "FilterDimuonTwoTrackVertex.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "DownstreamTrack.hh"
#include "GeometricAcceptance.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class FilterDimuonTwoTrackVertex
/// \Brief
/// Filter events with at least 3 good tracks
/// and at least one mu+mu- pair forming a good vertex
/// \EndBrief
/// \Detailed
/// This filter is useful for the studies of long-lived particles decaying to mu+mu-.
/// It can be used to run on the events filtered with FilterTwoTrackVertexWithLepton.
/// The filter is run in a standard way, the simplest way being
/// \code
/// ./MyApplication -i <input_file> -o <output_file> --filter
/// \endcode 
/// \author Viacheslav Duk (Viacheslav.Duk@cern.ch)
/// \EndDetailed

FilterDimuonTwoTrackVertex::FilterDimuonTwoTrackVertex(Core::BaseAnalysis *ba) :
  Analyzer(ba, "FilterDimuonTwoTrackVertex") {
  RequestAllMCTrees();
  RequestAllRecoTrees();
  fCDAcomp = new TwoLinesCDA();
}

FilterDimuonTwoTrackVertex::~FilterDimuonTwoTrackVertex() {
  delete fCDAcomp;
}

void FilterDimuonTwoTrackVertex::InitHist() {

  BookHisto("hRestrictedTwoTrackVertexBefore",   new TH1F("RestrictedTwoTrackVertexBefore", "Z-coordinate of the two-track vertex before the filter", 100, 0., 200000.));
  BookHisto("hRestrictedTwoTrackMomentumBefore", new TH1F("RestrictedTwoTrackMomentumBefore", "Two-track momentum before the filter", 100, 0., 100.));
  BookHisto("hNTracks",                          new TH1F("NTracks", "N good tracks after the filter", 10, -0.5, 9.5));
  BookHisto("hCDA",                              new TH1F("CDA", "CDA after the filter", 100, 0., 100.));
  BookHisto("hRestrictedTwoTrackVertex",         new TH1F("RestrictedTwoTrackVertex", "Z-coordinate of the two-track vertex after the filter", 100, 0., 200000.));
  BookHisto("hRestrictedTwoTrackMomentum",       new TH1F("RestrictedTwoTrackMomentum", "Two-track momentum after the filter", 100, 0., 100.));
  BookHisto("hDtNewCHOD",                        new TH1F("DtNewCHOD", "dt NewCHOD after the filter", 200, -10., 10.));
  BookHisto("hDtTrigger",                        new TH1F("DtTrigger", "(Ttrack - Ttrigger) after the filter", 200, -10., 10.));

  BookHisto("hFilterFlag",                       new TH1F("FilterFlag", "Filter flag", 2, -0.5, 1.5));
}

void FilterDimuonTwoTrackVertex::Process(Int_t) {

  TVector3 MomentumSum;
  Bool_t   FilterFlag = false;
  Bool_t   LeptonFlag = false;

  std::vector<DownstreamTrack> Tracks =
    *(std::vector<DownstreamTrack>*) GetOutput("DownstreamTrackBuilder.Output");

  Int_t NGoodTracks = 0;
  for (UInt_t i=0; i<Tracks.size(); i++) {
    if (IsGoodTrack(Tracks[i])) NGoodTracks++;
  }

  if (NGoodTracks>=3) {
    for (UInt_t i=0; i < Tracks.size(); i++) {
      if (!IsGoodTrack(Tracks[i])) continue;

      for (UInt_t j=i+1; j < Tracks.size(); j++) {
	if (!IsGoodTrack(Tracks[j])) continue;

	// calculate CDA and vertex for two tracks
	fCDAcomp->SetLine1Point1(Tracks[i].GetPositionBeforeMagnet());
	fCDAcomp->SetLine2Point1(Tracks[j].GetPositionBeforeMagnet());
	fCDAcomp->SetDir1(Tracks[i].GetMomentumBeforeMagnet());
	fCDAcomp->SetDir2(Tracks[j].GetMomentumBeforeMagnet());
	fCDAcomp->ComputeVertexCDA();
	// calculate the total momentum of a track pair (MomentumSum)
	MomentumSum = Tracks[i].GetMomentumBeforeMagnet() + Tracks[j].GetMomentumBeforeMagnet();
	// select mu+mu- verteces (muon ID: MUV3 assocation)
	LeptonFlag = false;
	if (Tracks[i].MUV3AssociationExists() && Tracks[j].MUV3AssociationExists() &&
	    Tracks[i].GetCharge()*Tracks[j].GetCharge()<0)
	  LeptonFlag = true;

	// fill histograms illustrating cuts
	FillHisto("hRestrictedTwoTrackVertexBefore", fCDAcomp->GetVertex().Z());
	FillHisto("hRestrictedTwoTrackMomentumBefore", MomentumSum.Mag()*0.001);

	// cuts on CDA, z_vertex, MomentumSum and LeptonFlag
	if (fCDAcomp->GetCDA() < 25. &&
	    fCDAcomp->GetVertex().Z() > 103000. && fCDAcomp->GetVertex().Z() < 180000. && 
	    MomentumSum.Mag() < 70000. &&
	    fabs(Tracks[i].GetNewCHODTime()-Tracks[j].GetNewCHODTime()) < 6. &&
	    LeptonFlag) {
	  // set a positive filter flag 
	  FilterFlag = true;

	  FillHisto("hRestrictedTwoTrackVertex", fCDAcomp->GetVertex().Z());
	  FillHisto("hRestrictedTwoTrackMomentum", MomentumSum.Mag()*0.001);
	  FillHisto("hCDA", fCDAcomp->GetCDA());
	  FillHisto("hDtNewCHOD", Tracks[i].GetNewCHODTime()-Tracks[j].GetNewCHODTime());
	  FillHisto("hDtTrigger", Tracks[i].GetNewCHODTime()-GetL0Data()->GetReferenceFineTime()*TdcCalib);
	  FillHisto("hDtTrigger", Tracks[j].GetNewCHODTime()-GetL0Data()->GetReferenceFineTime()*TdcCalib);
	}
      }
    }
  }

  FillHisto("hFilterFlag", FilterFlag);
  if (FilterFlag) FillHisto("hNTracks", NGoodTracks);
  if (!FilterFlag) return;

  FilterAccept();
}

void FilterDimuonTwoTrackVertex::EndOfJobUser() {
  SaveAllPlots();
}

Bool_t FilterDimuonTwoTrackVertex::IsGoodTrack(DownstreamTrack Track) {
  Bool_t IsGood = true;
  Int_t ChambersInAcceptance;
  // check geometric acceptance in STRAW 
  ChambersInAcceptance = 0;
  for (Int_t iChamber=0; iChamber<4; iChamber++) {
    ChambersInAcceptance += GeometricAcceptance::GetInstance()->InAcceptance(&Track, kSpectrometer, iChamber);
  }
  if (ChambersInAcceptance!=4) IsGood = false;
  // 3 or 4 chambers
  if (Track.GetNChambers()<3) IsGood = false;
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
  // association with NewCHOD
  if (!Track.NewCHODAssociationExists()) IsGood = false;
  // time cut
  if (fabs(Track.GetNewCHODTime()-GetL0Data()->GetReferenceFineTime()*TdcCalib)>5.) IsGood = false;

  return IsGood;
}
