// ---------------------------------------------------------------
//
// History:
//
// Created by A. Shaikhiev (shaykhiev@inr.ru) 2017-03-06
//
// ---------------------------------------------------------------

/// \class FilterPositronThreeTrackVertex
/// \Brief
/// Filter events with a three-track vertex with a positron identified in RICH or LKr
/// \EndBrief
/// \Detailed
/// Only control, multi-track and multi-track electron triggers are filtered.
/// No output is produced if the multi-track electron trigger is disabled.
/// The basic conditions (on vertex chi2, position, momentum and geometric acceptance) are
/// identical to those in RestrictedThreeTrackVertex. For multi-track electron triggers which are not control
/// and not multi-track triggers, a positively charged track is required
/// with a higher likelihood of being a positron
/// than a pion in the RICH or an associated LKr cluster with E/p>0.8.
/// For control and multi-track triggers, no positron identification is required.
/// The filter is used in the usual way:
/// \code
/// ./MyApplication -i <input_file> -o <output_file> --filter
/// \endcode
/// \author A. Shaikhiev (shaykhiev@inr.ru)
/// \EndDetailed

#include "FilterPositronThreeTrackVertex.hh"
#include "SpectrometerTrackVertex.hh"
#include "GeometricAcceptance.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

FilterPositronThreeTrackVertex::FilterPositronThreeTrackVertex(Core::BaseAnalysis *ba) :
  Analyzer(ba, "FilterPositronThreeTrackVertex") {
  fTrigCond       = TriggerConditions::GetInstance();
  fTrigMultiTrack = fTrigCond->GetL0TriggerID("RICH-QX");
  fTrigElectron   = fTrigCond->GetL0TriggerID("RICH-QX-LKr20");
  RequestAllMCTrees();
  RequestAllRecoTrees();
}

void FilterPositronThreeTrackVertex::Process(Int_t) {

  Bool_t ControlTrigger      = fTrigCond->IsControlTrigger(GetL0Data());
  Bool_t TriggerMultiTrackOn = fTrigCond->L0TriggerOn(GetRunID(), GetL0Data(), fTrigMultiTrack);
  Bool_t TriggerPositronOn   = fTrigCond->L0TriggerOn(GetRunID(), GetL0Data(), fTrigElectron);
  Bool_t TriggerEnabled      = fTrigCond->L0TriggerEnabled(GetRunID(), fTrigElectron);
  if (!TriggerEnabled) return; // no relevant physics triggers employed in this run
  if (!ControlTrigger && !TriggerMultiTrackOn && !TriggerPositronOn) return;

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

    Bool_t AllTracksAreGood = true;
    Int_t PositronCandidatesInRICH = 0, PositronCandidatesInLKr = 0;
    for (Int_t j=0; j<3; j++) {
      TRecoSpectrometerCandidate* Scand = Vertices[iVtx].GetSpectrometerCandidate(j);
      for (Int_t ich=0; ich<4; ich++) {
	if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, ich))
	  AllTracksAreGood = false;
      }
      Int_t iTrack = Vertices[iVtx].GetTrackIndex(j);
      if (Tracks[iTrack].GetCharge() != 1) continue;
      if (Tracks[iTrack].GetRICHLikelihoodElectron() > Tracks[iTrack].GetRICHLikelihoodPion())
	PositronCandidatesInRICH++;
      if (Tracks[iTrack].GetLKrTotalEoP()>0.8) PositronCandidatesInLKr++;
    } // end of loop on the tracks
    if (!AllTracksAreGood) continue;

    // For vertices with all trackes classified as good:
    // check for a positron candidate in RICH or LKr
    // for di-electron triggers which are not control or multi-track triggers.
    // For control and multi-track trigggers, no check for positrons is made.
    if (!PositronCandidatesInRICH && !PositronCandidatesInLKr && !ControlTrigger && !TriggerMultiTrackOn) continue;
    NGoodThreeTrackVtx++;
  }

  if (!NGoodThreeTrackVtx) return;
  FilterAccept();
}
