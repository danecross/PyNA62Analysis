// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-03-27
//
// ---------------------------------------------------------------

/// \class FilterThreeTrackVertex
/// \Brief
/// Filter events with at least one reconstructed three-track vertex
/// \EndBrief
/// \Detailed
/// Events are sent to the filter output if at least one three-track vertex with chi2<100
/// is found in the event. The filter is used in the usual way:
/// \code
/// ./MyApplication -i <input_file> -o <output_file> --filter
/// \endcode
/// The maximum chi2 value of the vertex can be specified as a command line argument, for example:
/// \code
/// ./MyApplication -i <input_file> -o <output_file> -p "SpectrometerVertexBuilder:MaxChi2=200" --filter
/// \endcode
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include "FilterThreeTrackVertex.hh"
#include "Event.hh"
#include "Persistency.hh"

#include "SpectrometerTrackVertex.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

FilterThreeTrackVertex::FilterThreeTrackVertex(Core::BaseAnalysis *ba) : Analyzer(ba, "FilterThreeTrackVertex") {
  RequestAllMCTrees();
  RequestAllRecoTrees();
}

void FilterThreeTrackVertex::Process(Int_t) {

  std::vector<SpectrometerTrackVertex> Vertices =
    *(std::vector<SpectrometerTrackVertex>*)GetOutput("SpectrometerVertexBuilder.Output");
  if (!Vertices.size()) return;

  Int_t NThreeTrackVtx = 0;
  for (UInt_t i=0; i<Vertices.size(); i++) {
    if (Vertices[i].GetNTracks()==3) NThreeTrackVtx++;
  }

  if (!NThreeTrackVtx) return;
  FilterAccept();
}
