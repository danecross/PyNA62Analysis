/// \class FilterHNLVertex
/// \Brief
/// Filter events for pi-mu, pi-e, e-e and mu-mu exotics triggers
/// \EndBrief

#include <stdlib.h>
#include <iostream>
#include "FilterHNLVertex.hh"
#include "DownstreamTrack.hh"
#include "TriggerConditions.hh"
#include "GeometricAcceptance.hh"
#include "BeamParameters.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

FilterHNLVertex::FilterHNLVertex(Core::BaseAnalysis *ba) :
  Analyzer(ba, "FilterHNLVertex") {

  RequestAllMCTrees();
  RequestAllRecoTrees();
  RequestL0Data();
  RequestL1Data();

  fCDAcomp = new TwoLinesCDA();
  fDistcomp = new PointLineDistance();
  fControlTriggerCounter = 0;
  AddParam("ControlTriggerDS", &fControlTriggerDS, 50);

  fStream = {"RICH-Q2-MO1", "RICH-Q2-M1", "RICH-Q2-MO1-LKr10", "RICH-Q2-M1-LKr20", "RICH-Q2-MO2-nLKr20", "RICH-Q2-MO2", "RICH-Q2-M2", "RICH-QX-LKr20", "RICH-LKr20", "RICH-Q2-nMUV-LKr20", "RICH-Q2-MO1-LKr20", "RICH-Q2-MO2-nLKr30", "RICH-QX-MO2"};

  for (UInt_t i = 0; i < fStream.size(); i++) {
    fID.push_back(TriggerConditions::GetInstance()->GetL0TriggerID(fStream[i]));
  }
}

FilterHNLVertex::~FilterHNLVertex() {
  if (fCDAcomp) delete fCDAcomp;
  if (fDistcomp) delete fDistcomp;
}

void FilterHNLVertex::Process(Int_t) {

  ///////////////////////////////////////////////////////
  // Note that the trigger selection does not work for MC

  L0TPData *L0TPData = GetL0Data();
  Bool_t L0ControlTrigger = TriggerConditions::GetInstance()->IsControlTrigger(L0TPData);

  // Write downscaled control triggers

  if (L0ControlTrigger) {
    fControlTriggerCounter++;
    if ((fControlTriggerCounter%fControlTriggerDS) == 0) {
      FilterAccept();
      return;
    }
  }

  // Check for the physics triggers of interest at L0

  Int_t  RunNumber = GetRunID();
  Bool_t L0OK = kFALSE;
  Bool_t L1OK = kFALSE;

  for (UInt_t i = 0; i < fID.size(); i++) {
    L0OK |= TriggerConditions::GetInstance()->L0TriggerOn(RunNumber, L0TPData, fID[i]);
  }

  if (!L0OK) return;

  // Check for notKTAG or KTAG don't care at L1

  for (UInt_t i = 0; i < fID.size(); i++) {
    std::string L1Algo = (std::string)TriggerConditions::GetInstance()->GetL1TriggerConditionName(RunNumber, fID[i]);
    std::size_t foundKTAG = L1Algo.find("KTAG");
    std::size_t foundnotKTAG = L1Algo.find("notKTAG");
    if (L1Algo != "none") {
      if (foundKTAG == std::string::npos || foundnotKTAG != std::string::npos) {
	L1OK = kTRUE;
      }
    }
  }

  if (!L1OK) return;

  std::vector<DownstreamTrack> Tracks =
    *(std::vector<DownstreamTrack>*) GetOutput("DownstreamTrackBuilder.Output");

  if (Tracks.size() < 2) return;

  for (UInt_t i = 0; i < Tracks.size(); i++) {
    if (GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[i], kSpectrometer, 0) &&
	GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[i], kSpectrometer, 1) &&
	GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[i], kSpectrometer, 2) &&
	GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[i], kSpectrometer, 3) &&
	GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[i], kCHOD)            &&
	GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[i], kMUV3)            &&
	GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[i], kLKr)) {

      for (UInt_t j = i+1; j < Tracks.size(); j++) {
	if (GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[j], kSpectrometer, 0) &&
	    GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[j], kSpectrometer, 1) &&
	    GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[j], kSpectrometer, 2) &&
	    GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[j], kSpectrometer, 3) &&
	    GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[j], kCHOD)            &&
	    GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[j], kMUV3)            &&
	    GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[j], kLKr)) {
	  fCDAcomp->SetLine1PointDir(Tracks[i].GetPositionBeforeMagnet(), Tracks[i].GetMomentumBeforeMagnet());
	  fCDAcomp->SetLine2PointDir(Tracks[j].GetPositionBeforeMagnet(), Tracks[j].GetMomentumBeforeMagnet());
	  fCDAcomp->ComputeVertexCDA();

	  if (fCDAcomp->GetCDA() < 50.) {
	    fDistcomp->SetLinePoint1(0., 0., 101800.);
	    fDistcomp->SetLineDir(BeamParameters::GetInstance()->GetNominalBeamThreeMomentum());
	    fDistcomp->SetPoint(fCDAcomp->GetVertex());
	    fDistcomp->ComputeDistance();
	    if (fDistcomp->GetDistance() > 100.) {
	      FilterAccept();
	      return;
	    }
	  }
	}
      }
    }
  }
}
