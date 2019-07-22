// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-12-10
//
// ---------------------------------------------------------------

/// \class FilterRestrictedPositron
/// \Brief
/// Filter events with at least one reconstructed positron track
/// \EndBrief
/// \Detailed
/// Minimal selection (including qE/p>0.8) is applied as can be seen in the code.
/// The filter is used as follows:
/// \code
/// ./MyApplication -i <input_file> -o <output_file> --filter
/// \endcode
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include "FilterRestrictedPositron.hh"
#include "DownstreamTrack.hh"
#include "GeometricAcceptance.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

FilterRestrictedPositron::FilterRestrictedPositron(Core::BaseAnalysis *ba) :
  Analyzer(ba, "FilterRestrictedPositron") {
  RequestAllMCTrees();
  RequestAllRecoTrees();
}

void FilterRestrictedPositron::Process(Int_t) {

  std::vector<DownstreamTrack> Tracks =
    *(std::vector<DownstreamTrack>*)GetOutput("DownstreamTrackBuilder.Output");
  if (!Tracks.size()) return;

  Int_t NGoodTracks = 0;
  for (UInt_t i=0; i<Tracks.size(); i++) {
    if (Tracks[i].GetCharge()!=1)                         continue;
    if (Tracks[i].GetMomentum()>70000.)                   continue;
    if (Tracks[i].GetLKrTotalEoP()<0.8)                   continue;
    if (Tracks[i].GetNominalBeamAxisVertex().z()< 90000.) continue;
    if (Tracks[i].GetNominalBeamAxisVertex().z()>180000.) continue;
    if (Tracks[i].GetChi2()>40.0)                         continue;
    Bool_t InAcceptance = true;
    for (Int_t ich=0; ich<4; ich++) {
      if (!GeometricAcceptance::GetInstance()->InAcceptance(Tracks[i].GetSpectrometerCandidate(), kSpectrometer, ich)) {
        InAcceptance = false;
      }
    }
    if (!InAcceptance) continue;
    NGoodTracks++;
  }

  if (!NGoodTracks) return;
  FilterAccept();
}
