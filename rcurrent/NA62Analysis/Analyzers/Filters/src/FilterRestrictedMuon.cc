// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2017-10-17
//
// ---------------------------------------------------------------

/// \class FilterRestrictedMuon
/// \Brief
/// Filter Kmu2-like events triggered with the control trigger
/// \EndBrief
/// \Detailed
/// Example of use:
/// \code
/// ./MyApplication -i <input_file> -o <output_file> --filter
/// \endcode
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include "TriggerConditions.hh"
#include "DownstreamTrack.hh"
#include "FilterRestrictedMuon.hh"
#include "GeometricAcceptance.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

FilterRestrictedMuon::FilterRestrictedMuon(Core::BaseAnalysis *ba) :
  Analyzer(ba, "FilterRestrictedMuon") {
  RequestAllMCTrees();
  RequestAllRecoTrees();
}

void FilterRestrictedMuon::Process(Int_t) {
  Bool_t ControlTrigger = TriggerConditions::GetInstance()->IsControlTrigger(GetL0Data());
  if (!ControlTrigger) return;

  std::vector<DownstreamTrack> Tracks =
    *(std::vector<DownstreamTrack>*)GetOutput("DownstreamTrackBuilder.Output");
  if (!Tracks.size()) return;

  Int_t NGoodTracks = 0;
  for (UInt_t i=0; i<Tracks.size(); i++) {
    if (!GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[i], kSpectrometer, 0)) continue;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[i], kSpectrometer, 1)) continue;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[i], kSpectrometer, 2)) continue;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[i], kSpectrometer, 3)) continue;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[i], kLKr)) continue;
    if (Tracks[i].GetNChambers()!=4)                      continue;
    if (Tracks[i].GetCharge()!=1)                         continue;
    if (Tracks[i].GetMomentum()>70000.)                   continue;
    if (Tracks[i].GetLKrEoP()>0.25)                       continue;
    if (!Tracks[i].MUV3AssociationExists())               continue;
    if (Tracks[i].GetNominalBeamAxisVertex().z()<105000.) continue;
    if (Tracks[i].GetNominalBeamAxisVertex().z()>180000.) continue;
    if (Tracks[i].GetNominalBeamAxisCDA()>60.0)           continue;
    if (Tracks[i].GetChi2()>40.0)                         continue;
    NGoodTracks++;
  }

  if (!NGoodTracks) return;
  FilterAccept();
}
