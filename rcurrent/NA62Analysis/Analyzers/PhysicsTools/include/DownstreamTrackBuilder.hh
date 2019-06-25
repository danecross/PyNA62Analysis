// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-02-29
//
// ---------------------------------------------------------------

#ifndef DOWNSTREAMTRACKBUILDER_HH
#define DOWNSTREAMTRACKBUILDER_HH

#include "Analyzer.hh"
#include "DownstreamTrack.hh"
#include "TwoLinesCDA.hh"

class TRecoSpectrometerEvent;

class DownstreamTrackBuilder : public NA62Analysis::Analyzer {

public:
  explicit DownstreamTrackBuilder(NA62Analysis::Core::BaseAnalysis *ba);
  ~DownstreamTrackBuilder();
  void InitHist() {}
  void InitOutput();
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser() {}
  void EndOfBurstUser() {}
  void StartOfRunUser();
  void EndOfRunUser() {}
  void EndOfJobUser() {}
  void PostProcess() {}
  void DrawPlot() {}

protected:
  std::vector<DownstreamTrack> fContainer;

private:
  DownstreamTrack fTrack;
  Double_t fZTrim5;  ///< z coordinate of middle of Trim5 (=101.8m): used to define a point on the nominal beam axis
  Double_t fZSTRAW1; ///< z coordinate of STRAW1 front plane (=183.5m): used for Track/KinePart matching
  TwoLinesCDA *fCDAcomp1, *fCDAcomp2;

  Bool_t   IsFakeTrack(TRecoSpectrometerEvent*, Int_t);
  void     MatchKinePart();
};
#endif
