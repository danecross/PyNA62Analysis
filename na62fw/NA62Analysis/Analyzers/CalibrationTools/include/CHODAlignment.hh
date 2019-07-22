// ---------------------------------------------------------------
//
// History:
//
// Created by Viacheslav Duk (Viacheslav.Duk@cern.ch) 23.08.2018
//
// ---------------------------------------------------------------

#ifndef CHODALIGNMENT_HH
#define CHODALIGNMENT_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TCanvas.h>
#include "TriggerConditions.hh"
#include "NewCHODGeometry.hh"
#include "MUV3Geometry.hh"
#include "DownstreamTrack.hh"
#include "GeometricAcceptance.hh"

class TH1I;
class TH2F;
class TGraph;
class TTree;


class CHODAlignment : public NA62Analysis::Analyzer
{
	public:
  explicit CHODAlignment(NA62Analysis::Core::BaseAnalysis *ba);
  ~CHODAlignment();
  void InitHist();
  void InitOutput() {}
  void DefineMCSimple() {}
  void Process(int iEvent);
  void PostProcess() {}
  void StartOfBurstUser() {}
  void EndOfBurstUser() {}
  void StartOfRunUser() {}
  void EndOfRunUser() {}
  void EndOfJobUser();
  void DrawPlot() {}

protected:

private:
  TriggerConditions* fTriggerConditions;
  Bool_t IsGoodTrack(DownstreamTrack);
  Bool_t fReadingData;
  TH2F *fHCHODCalibrationH;
  TH2F *fHCHODCalibrationV;
};
#endif
