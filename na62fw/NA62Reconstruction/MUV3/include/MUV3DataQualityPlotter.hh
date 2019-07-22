// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-11-26
//
// ---------------------------------------------------------------

#ifndef MUV3DATAQUALITYPLOTTER_H
#define MUV3DATAQUALITYPLOTTER_H 1

#include <stdlib.h>
#include <math.h>
#include <vector>
#include <TCanvas.h>
#include <TStyle.h>
#include <TError.h>

#include "TH1D.h"
#include "TH2F.h"
#include "TProfile.h"
#include "TF1.h"
#include "TLatex.h"
#include "TLine.h"
#include "TArc.h"
#include "TLegend.h"

class MUV3DataQualityPlotter {

public:

  MUV3DataQualityPlotter(std::vector<TH1*>, TString);
  void DrawBoundaries(Int_t);
  void GenerateTileAsymmetry(TH1D*, TH1D*);
  void ConvertProfileToTwoDimensionalMap (TH1D*, TH2F*, TH2F*);
  void BuildPDF();
  virtual ~MUV3DataQualityPlotter() {}

private:
  TString fOutPDFFileName; ///< Name of the output PDF file

  // Primary histograms (pointers passed to the constructor)

  TH1D *fHNEventsProcessedPerBurst;
  TH1D *fHChannelProfile, *fHChannelProfileEOB, *fHTileOR, *fHTileAND;
  TH2F *fHNCandidatesVsL0TriggerBit, *fHNCandidatesVsNoL0TriggerBit;
  TH2F *fHTightCandidateProfileVsL0TriggerBit;
  TH2F *fHTightCandidateProfileVsNoL0TriggerBit;
  TH2F *fHChannelProfileVsBurst, *fHChannelProfileVsBurstEOB;
  TH2F *fHRecoHitTimeWrtReferenceVsReadoutChannelNoT0;
  TH2F *fHRecoHitTimeWrtReferenceVsReadoutChannel;

  TH2F *fHCandidateTimeWrtReferenceNoTileT0VsTile;
  TH2F *fHCandidateTimeWrtReferenceVsTile;
  TH2F *fHCandidateAvgTimeWrtReferenceVsTile;
  TH1D *fHCandidateTimeWrtReferenceNoTileT0;
  TH1D *fHCandidateTimeWrtReference;
  TH1D *fHCandidateAvgTimeWrtReference;

  TH1D *fHTotalPrimitiveCountsEOB, *fHErrorCountsEOB;
  std::vector<TH2F*> fHList;

  // Derived histograms
  TH1D *fHTileAsymmetry, *fHTileAsymmetryEOB;
  TH2F *fHAsym2, *fHAsym2Inner, *fHAsym2EOB, *fHAsym2InnerEOB;

  void GenerateTileAsymmetryVsBurstIDPlots();
};

#endif
