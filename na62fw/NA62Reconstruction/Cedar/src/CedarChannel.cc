// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-08-26
//
// ---------------------------------------------------------------

#include "TFile.h"
#include <fstream>
#include "Riostream.h"
#include "TMath.h"
#include "TString.h"

#include "CedarChannel.hh"

CedarChannel::CedarChannel(Int_t GeoChannelID, Int_t ROChannelID, Bool_t FillHistograms) :
  NA62VChannel(GeoChannelID,ROChannelID,FillHistograms,"Cedar"),
  fMinWidth               (10.0),
  fMaxWidth               (15.0),
  fNewMinWidth            (.0),
  fNewMaxWidth            (.0),
  fSlewingCorr_MinWidth   (0),
  fSlewingCorr_MaxWidth   (0),
  fNewSlewingCorr_MinWidth(0),
  fNewSlewingCorr_MaxWidth(0)
{
}

CedarChannel::~CedarChannel () {}

void CedarChannel::EvaluateSlewingCorrection () {

  fNewSlewingCorr_MinWidth = fNewSlewingCorr_MaxWidth = 0.0;

  if (!fEnabled) return;
  if (fHTimeVsWidth->Integral()<100) return;

  // A simple option, should also treat errors correctly
  //fHTimeVsWidth->Fit("pol1", "Q0", "", fNewMinWidth, fNewMaxWidth);
  //fNewSlewingCorr_MinWidth = ((TF1*)fHTimeVsWidth->GetFunction("pol1"))->Eval(fNewMinWidth);
  //fNewSlewingCorr_MaxWidth = ((TF1*)fHTimeVsWidth->GetFunction("pol1"))->Eval(fNewMaxWidth);

  // A less straightforward and possibly less rigorous option, but tested already.

  Int_t nwbins = fHTimeVsWidth->GetNbinsX();
  Double_t wid[nwbins], time[nwbins];
  Int_t npts = 0;
  for (Int_t ibin=1; ibin<=nwbins; ibin++) {
    Double_t w = fHTimeVsWidth->GetXaxis()->GetBinCenter(ibin);
    if (w>fNewMinWidth && w<fNewMaxWidth) {
      wid[npts] = w;
      time[npts] = fHTimeVsWidth->GetBinContent(ibin);
      npts++;
    }
  }
  TGraph *g = new TGraph(npts, wid, time);
  g->Fit("pol1", "Q0", "", fNewMinWidth, fNewMaxWidth);
  fNewSlewingCorr_MinWidth = ((TF1*)g->GetFunction("pol1"))->Eval(fNewMinWidth);
  fNewSlewingCorr_MaxWidth = ((TF1*)g->GetFunction("pol1"))->Eval(fNewMaxWidth);
  delete g;
}

void CedarChannel::SetSlewingCorrectionParameters(Double_t sc_wmin, Double_t sc_wmax) {
  fSlewingCorr_MinWidth = sc_wmin;
  fSlewingCorr_MaxWidth = sc_wmax;
}

// The slewing correction function:
//    f(w) = c1, w<wmin;
//    f(w) = c2, w>wmax;
//    linear function in the interval (wmin;wmax).
//    The values wmin=10 and wmax=15 can be changed from the config file.
// The slewing correction has to be subtracted from the leading time.

Double_t CedarChannel::GetSlewingCorrection (Double_t Width) {
  if (Width <= 0 || Width > 1.e3) return 0; //leading or trailing only
  if (Width <= fMinWidth) return fSlewingCorr_MinWidth;
  if (Width >= fMaxWidth) return fSlewingCorr_MaxWidth;
  return fSlewingCorr_MinWidth +
    (Width-fMinWidth) / (fMaxWidth-fMinWidth) * (fSlewingCorr_MaxWidth-fSlewingCorr_MinWidth);
}
