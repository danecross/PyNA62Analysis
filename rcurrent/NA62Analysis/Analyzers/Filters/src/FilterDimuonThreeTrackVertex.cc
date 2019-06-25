// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2017-03-02
//
// ---------------------------------------------------------------

/// \class FilterDimuonThreeTrackVertex
/// \Brief
/// Filter events with at least one reconstructed three-track vertex
/// suitable for further analysis of K decays to muon pairs
/// \EndBrief
/// \Detailed
/// Events are sent to the filter output if the event is accepted by a particular set of
/// trigger conditions, and contains at least one three-track vertex that satisfies
/// a number of loose requirements. See the code for the exact algorithm.
/// The filter is used in the usual way:
/// \code
/// ./MyApplication -i <input_file> -o <output_file> --filter
/// \endcode
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "FilterDimuonThreeTrackVertex.hh"
#include "SpectrometerTrackVertex.hh"
#include "GeometricAcceptance.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

FilterDimuonThreeTrackVertex::FilterDimuonThreeTrackVertex(Core::BaseAnalysis *ba) : Analyzer(ba, "FilterDimuonThreeTrackVertex") {

  fTrigCond       = TriggerConditions::GetInstance();
  fTrigMultiTrack = fTrigCond->GetL0TriggerID("RICH-QX");
  fTrigDimuon1    = fTrigCond->GetL0TriggerID("RICH-QX-MO2");
  fTrigDimuon2    = fTrigCond->GetL0TriggerID("RICH-QX-M2");

  RequestAllMCTrees();
  RequestAllRecoTrees();
}

void FilterDimuonThreeTrackVertex::StartOfRunUser() {
  // Account for the non-standard di-muon trigger during the early part of sample 17D
  fTrigDimuon1 = fTrigCond->GetL0TriggerID("RICH-QX-MO2");
  if (GetRunID()>=7615 && GetRunID()<=7649) fTrigDimuon1 = fTrigCond->GetL0TriggerID("RICH-Q2-MO2");
}

void FilterDimuonThreeTrackVertex::Process(Int_t) {

  Bool_t ControlTrigger = fTrigCond->IsControlTrigger(GetL0Data());
  Bool_t TriggerMultiTrackOn = fTrigCond->L0TriggerOn(GetRunID(), GetL0Data(), fTrigMultiTrack);
  Bool_t TriggerDimuonOn =
    fTrigCond->L0TriggerOn(GetRunID(), GetL0Data(), fTrigDimuon1) ||
    fTrigCond->L0TriggerOn(GetRunID(), GetL0Data(), fTrigDimuon2);
  Bool_t TriggerEnabled = // any interesting triggers at all?
      fTrigCond->L0TriggerEnabled(GetRunID(), fTrigDimuon1) ||
      fTrigCond->L0TriggerEnabled(GetRunID(), fTrigDimuon2);
  if (!TriggerEnabled) return; // no relevant physics triggers employed in this run
  if (!ControlTrigger && !TriggerMultiTrackOn && !TriggerDimuonOn) return;

  std::vector<SpectrometerTrackVertex> Vertices =
    *(std::vector<SpectrometerTrackVertex>*)GetOutput("SpectrometerVertexBuilder.Output");
  if (!Vertices.size()) return;

  std::vector<DownstreamTrack> Tracks =
    *(std::vector<DownstreamTrack>*)GetOutput("DownstreamTrackBuilder.Output");

  Int_t NGoodThreeTrackVtx = 0;
  for (UInt_t iVtx=0; iVtx<Vertices.size(); iVtx++) {
    if (Vertices[iVtx].GetNTracks()!=3)            continue;
    if (Vertices[iVtx].GetChi2()>40.0)             continue;
    if (Vertices[iVtx].GetPosition().z()<102000.0) continue;
    if (Vertices[iVtx].GetTotalMomentum()>90000.0) continue;

    Bool_t GoodTracks = true;
    Int_t MUV3associated = 0;
    for (Int_t j=0; j<3; j++) {
      TRecoSpectrometerCandidate* Scand = Vertices[iVtx].GetSpectrometerCandidate(j);
      for (Int_t ich=0; ich<4; ich++) {
	if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, ich))
	  GoodTracks = false;
      }
      Int_t iTrack = Vertices[iVtx].GetTrackIndex(j);
      if (Tracks[iTrack].MUV3AssociationExists()) MUV3associated++;
    } // end of loop on the tracks
    if (!GoodTracks) continue;

    // Ask for at least 2 tracks with MUV3 associations
    // for di-muon triggers which are not control or multi-track
    if (MUV3associated<2 && !ControlTrigger && !TriggerMultiTrackOn) continue;
    NGoodThreeTrackVtx++;
  }

  if (!NGoodThreeTrackVtx) return;
  FilterAccept();
}
