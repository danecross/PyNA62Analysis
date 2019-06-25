// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2017-03-05
//
// ---------------------------------------------------------------

/// \class FilterMuonElectronThreeTrackVertex
/// \Brief
/// Filter events with at least one reconstructed three-track vertex
/// suitable for further analysis of K decays to muon-electron pairs
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

#include "FilterMuonElectronThreeTrackVertex.hh"
#include "SpectrometerTrackVertex.hh"
#include "GeometricAcceptance.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

FilterMuonElectronThreeTrackVertex::FilterMuonElectronThreeTrackVertex(Core::BaseAnalysis *ba) :
  Analyzer(ba, "FilterMuonElectronThreeTrackVertex") {

  // Below is the list of L0 trigger conditions considered by this filter.
  // The RICH-LKr20 trigger, enabled in a few 2016 runs with typically large DS, is discarded.

  fTrigCond       = TriggerConditions::GetInstance();
  fTrigMultiTrack = fTrigCond->GetL0TriggerID("RICH-QX");
  fTrigElectron   = fTrigCond->GetL0TriggerID("RICH-QX-LKr20");     // 2016-2018 data
  fTrigMuon1      = fTrigCond->GetL0TriggerID("RICH-QX-M1");        // early 2016 data
  fTrigMuon2      = fTrigCond->GetL0TriggerID("RICH-QX-MO1");       // late 2016 data
  fTrigMuon3      = fTrigCond->GetL0TriggerID("RICH-QX-MO1-LKr10"); // 2017+2018 data

  RequestAllMCTrees();
  RequestAllRecoTrees();
}

void FilterMuonElectronThreeTrackVertex::Process(Int_t) {

  Bool_t ControlTrigger        = fTrigCond->IsControlTrigger(GetL0Data());
  Bool_t TriggerMultiTrackOn   = fTrigCond->L0TriggerOn(GetRunID(), GetL0Data(), fTrigMultiTrack);
  Bool_t TriggerMuonElectronOn =
    fTrigCond->L0TriggerOn(GetRunID(), GetL0Data(), fTrigElectron) ||
    fTrigCond->L0TriggerOn(GetRunID(), GetL0Data(), fTrigMuon1) ||
    fTrigCond->L0TriggerOn(GetRunID(), GetL0Data(), fTrigMuon2) ||
    fTrigCond->L0TriggerOn(GetRunID(), GetL0Data(), fTrigMuon3);
  Bool_t TriggerEnabled = // any interesting triggers at all?
    fTrigCond->L0TriggerEnabled(GetRunID(), fTrigElectron) ||
    fTrigCond->L0TriggerEnabled(GetRunID(), fTrigMuon1) ||
    fTrigCond->L0TriggerEnabled(GetRunID(), fTrigMuon2) ||
    fTrigCond->L0TriggerEnabled(GetRunID(), fTrigMuon3);
  if (!TriggerEnabled) return; // no relevant physics triggers employed in this run
  if (!ControlTrigger && !TriggerMultiTrackOn && !TriggerMuonElectronOn) return;

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
    Int_t MUV3associated = 0, ElectronCandidates = 0;
    for (Int_t j=0; j<3; j++) {
      TRecoSpectrometerCandidate* Scand = Vertices[iVtx].GetSpectrometerCandidate(j);
      for (Int_t ich=0; ich<4; ich++) {
	if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, ich))
	  GoodTracks = false;
      }
      Int_t iTrack = Vertices[iVtx].GetTrackIndex(j);
      if (Tracks[iTrack].MUV3AssociationExists()) MUV3associated++;
      if (Tracks[iTrack].GetLKrTotalEoP()>0.8) ElectronCandidates++;
    } // end of loop on the tracks
    if (!GoodTracks) continue;

    // Ask for a muon (mu+-) candidate and an electron (e+-) candidate
    // for the muon-electron triggers which are not control or multi-track
    if ((!MUV3associated || !ElectronCandidates) && !ControlTrigger && !TriggerMultiTrackOn) continue;
    NGoodThreeTrackVtx++;
  }

  if (!NGoodThreeTrackVtx) return;
  FilterAccept();
}
