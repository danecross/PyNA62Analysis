// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-07-18
//
// ---------------------------------------------------------------

/// \class FilterEOPBase
/// \Brief
/// Base class for track event filtering: at least N tracks with E/p>x.
/// \EndBrief
/// \Detailed
/// This is a base filter class, and it should not be called.
/// The filtering is based in the total E/p summed over all the clusters in the search radius.
/// Filters in Analyzers/PhysicsTools that inherit from it should be used.
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include "FilterEOPBase.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "DownstreamTrack.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

FilterEOPBase::FilterEOPBase(Core::BaseAnalysis *ba, std::string Name) : Analyzer(ba, Name) {
  RequestAllMCTrees();
  RequestAllRecoTrees();
}

void FilterEOPBase::InitHist() {
  // Enlarge the track-cluster association distance (standard = 50mm)
  // to account for the spectrometer-LKr alignment correction not normally applied when filtering
  ReconfigureAnalyzer("SpectrometerLKrAssociation", "MaxTrackClusterDistance", 55.0); // [mm]
}

void FilterEOPBase::InitOutput() {
  RegisterOutput("EventSelected", &fEventSelected);
}

void FilterEOPBase::Process(Int_t) {

  // Initialize the outputs
  SetOutputState("EventSelected", kOValid);
  fEventSelected = false;

  std::vector<DownstreamTrack> Tracks =
    *(std::vector<DownstreamTrack>*)GetOutput("DownstreamTrackBuilder.Output");
  if (Tracks.size() < (UInt_t)fMinNTracks) return;

  Int_t NGoodTracks = 0;
  for (UInt_t i=0; i<Tracks.size(); i++) {
    // E/p summed over all clusters in the search area, with manual cluster energy corrections
    Double_t eop     = Tracks[i].GetLKrTotalEoP();
    Double_t Zvertex = Tracks[i].GetNominalBeamAxisVertex().z(); // Vertex formed by track and beam axis

    // Avoid loss of "anomalous" tracks when using default cuts
    if (eop>99.0)    eop = 99.0;
    if (Zvertex<0.0) Zvertex = 0.0;
    if (eop>fMinEOP && eop<fMaxEOP && Zvertex>fMinZvertex) NGoodTracks++;
  }
  if (NGoodTracks<fMinNTracks) return;

  fEventSelected = true;
  FilterAccept();
}
