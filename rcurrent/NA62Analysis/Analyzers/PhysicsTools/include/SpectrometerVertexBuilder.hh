// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-08-25
//
// ---------------------------------------------------------------

#ifndef SPECTROMETERVERTEXBUILDER_HH
#define SPECTROMETERVERTEXBUILDER_HH

#include "Analyzer.hh"
#include "VertexLSF.hh"
#include "SpectrometerTrackVertex.hh"

class SpectrometerVertexBuilder : public NA62Analysis::Analyzer {

public:
  explicit SpectrometerVertexBuilder(NA62Analysis::Core::BaseAnalysis *ba);
  ~SpectrometerVertexBuilder();
  void InitHist();
  void InitOutput();
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser() {}
  void EndOfBurstUser() {}
  void StartOfRunUser() {}
  void EndOfRunUser() {}
  void EndOfJobUser();
  void PostProcess() {}
  void DrawPlot() {}

private:
  Bool_t fBuild2TrackVertices; ///< Build 2-track vertices?
  Bool_t fBuild3TrackVertices; ///< Build 3-track vertices?
  Bool_t fBuild4TrackVertices; ///< Build 4-track vertices?
  Bool_t fBuild5TrackVertices; ///< Build 5-track vertices?

  Bool_t    fBlueFieldCorrection; ///< Correct for blue tube field effect?
  UInt_t    fMaxNTracks;   ///< Max number of tracks in event to attempt vertex fit
  UInt_t    fMaxNVertices; ///< Max number of vertices to be built before giving up
  Double_t  fMinZVertex;   ///< Lower limit of the Z range for vertices to be saved
  Double_t  fMaxZVertex;   ///< Upper limit of the Z range for vertices to be saved
  Int_t     fCharge;       ///< Charge of vertices to be saved (-999: any charge)
  Double_t  fMaxChi2;      ///< Upper limit of vertex chi2 for vertices to be saved
  VertexLSF fVertexLSF;    ///< The least-squared fitter
  std::vector<SpectrometerTrackVertex> fContainer;

  void BuildVertex(Int_t ind[], Int_t NTracks);
};

#endif
