#include "TH1D.h"
#include "TH2F.h"
#include "TH2F.h"
#include "TGraphErrors.h"
#include "TF1.h"
#include "TFile.h"
#include "TProfile.h"
#include <fstream>
#include "Riostream.h"
#include "TMath.h"

#include "TDCEvent.hh"
#include "TRICHDigi.hh"
#include "TRICHHit.hh"
#include "RICHChannel.hh"

//#define debug_cout(x) std::cout << x
#define debug_cout(x)

RICHChannel::RICHChannel(Int_t GeoChannelID, Int_t ROChannelID, Int_t SeqChannelID, 
    Bool_t FillHistograms,TVector2 CenteredPosition, TVector2 ShiftedPosition):
  NA62VChannel(GeoChannelID,ROChannelID,FillHistograms,"RICH"),
  fSeqChannelID           (SeqChannelID),
  fMinWidth               (16.0),
  fMaxWidth               (23.0),
  fMeanWidth              (0.0),
  fSlewingCorr_MinWidth   (0),
  fSlewingCorr_MaxWidth   (0),
  fNewSlewingCorr_MinWidth(0),
  fNewSlewingCorr_MaxWidth(0),
  fCenteredPosition       (CenteredPosition),
  fShiftedPosition        (ShiftedPosition),
  fHRICHTimeVsWidth       (0)
{
  fGeoChannelID           = GeoChannelID;
  fROChannelID            = ROChannelID;
  if (fFillHistograms) InitHistograms(); 

}

RICHChannel::~RICHChannel(){
  if(fHRICHTimeVsWidth) delete fHRICHTimeVsWidth;
  fHRICHTimeVsWidth=0;
}

void RICHChannel::InitHistograms(){
  NA62VChannel::InitHistograms();
  char name[100];
  sprintf (name,  "Tlead_vs_Width_SeqID_%03d_RO_%03d", fSeqChannelID, fROChannelID);
  fHRICHTimeVsWidth = new TH2F (name, name, 350, 5, 30, 200, -10., 10.);
  fHRICHTimeVsWidth->GetXaxis()->SetTitle("Width, ns");
  fHRICHTimeVsWidth->GetYaxis()->SetTitle("Leading time, ns");
}

void RICHChannel::FillTime (Double_t Width, Double_t UnbiasedDeltaT)
{
  if (!fEnabled) return;
  if (fNHits!=1) return;
  fHTimeVsWidth->Fill(Width,UnbiasedDeltaT); 
  fHRICHTimeVsWidth->Fill(Width,UnbiasedDeltaT); 
}

void RICHChannel::SetSlewingCorrectionParameters (Double_t wmin, Double_t wmax, Double_t sc_wmin, Double_t sc_wmax)
{
  fMinWidth             = wmin;
  fMaxWidth             = wmax;
  fSlewingCorr_MinWidth = sc_wmin;
  fSlewingCorr_MaxWidth = sc_wmax;
}

// The slewing correction function:
//    f(w) = c1, w<wmin;
//    f(w) = c2, w>wmax;
//    linear function in the interval (wmin;wmax).   
//   The interval, for each channel, is defined as meanWidth +/- 3.5
// The slewing correction has to be subtracted from the leading time.

Double_t RICHChannel::GetSlewingCorrection (Double_t Width)
{
  if (Width <= fMinWidth) return fSlewingCorr_MinWidth;
  if (Width >= fMaxWidth) return fSlewingCorr_MaxWidth;
  Double_t LinearFactor = (fSlewingCorr_MaxWidth-fSlewingCorr_MinWidth) / (fMaxWidth-fMinWidth);
  Double_t SlewCorr = (Width-(fMinWidth+fMaxWidth)/2)*LinearFactor; 
  return SlewCorr;
}

void RICHChannel::Write (TFile* fHistoFile) { 
  fHistoFile->cd("RICHMonitor/RICHChannels/SlewingCorr");
  fHRICHTimeVsWidth->Write();
}
