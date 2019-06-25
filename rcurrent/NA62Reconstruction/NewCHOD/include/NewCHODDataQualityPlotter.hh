// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-11-27
//
// ---------------------------------------------------------------

#ifndef NEWCHODDATAQUALITYPLOTTER_H
#define NEWCHODDATAQUALITYPLOTTER_H 1

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
#include "TMath.h"
#include "TGraph.h"

#include "NewCHODGeometry.hh"

class NewCHODDataQualityPlotter {

public:
  NewCHODDataQualityPlotter(std::vector<TH1*>, TString);
  virtual ~NewCHODDataQualityPlotter() {}

  void BuildPDF();
  void FillOccupancyPlotWithChannelIDs(TH1D*);
  void ConvertProfileToTwoDimensionalMap(TH1D*, TH2F*, Int_t);
  void DrawBoundaries(int);
  
private:
  NewCHODGeometry *fGeo;
  TString fOutPDFFileName;  ///< Name of the output PDF file
  TH1D *fHChannelProfile, *fHChannelProfileEOB, *fHTileOR, *fHTileAND;
  TH2F *fHNRecoHitsVsL0TriggerBit, *fHNRecoHitsVsNoL0TriggerBit;
  TH2F *fHTightRecoHitProfileVsL0TriggerBit, *fHTightRecoHitProfileVsNoL0TriggerBit;
  TH2F *fHChannelProfileVsBurst, *fHChannelProfileVsBurstEOB;
  TH2F *fTimeWrtReferenceVsReadoutChannelNoT0; ///< Digi times wrt reference
  TH2F *fTimeWrtReferenceVsReadoutChannel;     ///< Digi times wrt reference, T0-corrected
  TH2F *fHTightRecoHitTimeWrtReferenceVsTile;
  TH2F *fHLooseRecoHitTimeWrtReferenceVsTile;
  TH1D *fHTightRecoHitTimeWrtReference;
  TH1D *fHLooseRecoHitTimeWrtReference;
  TH1D *fHNEventsProcessedPerBurst;
  TH1D *fHTileAsymmetry, *fHTileAsymmetryEOB;
  TH1D *fHTotalPrimitiveCountsEOB, *fHErrorCountsEOB;
  std::vector<TH2F*> fHList;

  void GenerateTileAsymmetryVsBurstIDPlots();
  void PrintBinContents(TH1D *h, Int_t HighChannel, Double_t ScaleFactor, Color_t colour, Double_t max=1.0); ///< Print 1D bin contents on top of a 2D occupancy plot
};

#endif
