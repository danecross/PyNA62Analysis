// ---------------------------------------------------------------
//
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2016-12-05
//
// ---------------------------------------------------------------

/// \class FilterRestrictedThreeTrackVertex
/// \Brief
/// Filter events with at least one reconstructed three-track vertex,
/// imposing loose constraints on the tracks and the vertex
/// \EndBrief
/// \Detailed
/// Events are sent to the filter output if at least one three-track vertex that satisfies
/// a number of loose requirements is found in the event. The filter is used in the usual way:
/// \code
/// ./MyApplication -i <input_file> -o <output_file> --filter
/// \endcode
/// \author Chris Parkinson (chris.parkinson@cern.ch)
/// \EndDetailed

#include "FilterRestrictedThreeTrackVertex.hh"
#include "SpectrometerTrackVertex.hh"
#include "GeometricAcceptance.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

FilterRestrictedThreeTrackVertex::FilterRestrictedThreeTrackVertex(Core::BaseAnalysis *ba) : Analyzer(ba, "FilterRestrictedThreeTrackVertex") {
  RequestAllMCTrees();
  RequestAllRecoTrees();
}

void FilterRestrictedThreeTrackVertex::Process(Int_t) {

  std::vector<SpectrometerTrackVertex> Vertices =
    *(std::vector<SpectrometerTrackVertex>*)GetOutput("SpectrometerVertexBuilder.Output");
  if (!Vertices.size()) return;
  
  Int_t NGoodThreeTrackVtx = 0;
  for (UInt_t i=0; i<Vertices.size(); i++) {
    if (Vertices[i].GetNTracks()!=3)            continue;
    if (Vertices[i].GetChi2()>40.0)             continue;
    if (Vertices[i].GetPosition().z()<102000.0) continue;
    if (Vertices[i].GetTotalMomentum()>90000.0) continue;

    Bool_t GoodTracks = true;
    for (Int_t j=0; j<3; j++) {
      TRecoSpectrometerCandidate* Scand = Vertices[i].GetSpectrometerCandidate(j);
      for (Int_t ich=0; ich<4; ich++) {
	if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, ich))
	  GoodTracks = false;
      }
    } // end of loop on the tracks
    if (!GoodTracks) continue;
    NGoodThreeTrackVtx++;
  }

  if (NGoodThreeTrackVtx) FilterAccept();
}
