// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-03-20
//
// ---------------------------------------------------------

#include "TFile.h"
#include <fstream>
#include "Riostream.h"
#include "TMath.h"
#include "TString.h"

#include "NA62VChannel.hh"
#include "TDCEvent.hh"

NA62VChannel::NA62VChannel(Int_t GeoChannelID, Int_t ROChannelID, Bool_t FillHistograms, TString Name) :
  fGeoChannelID           (GeoChannelID),
  fROChannelID            (ROChannelID),
  fNHits                  (0),
  fEnabled                (fGeoChannelID>=0),
  fFillHistograms         (FillHistograms),
  fName                   (Name),
  fT0                     (0.0),
  fHTimeVsWidth           (nullptr)
{
  if (!fEnabled) return;

  Reset();
  if (fFillHistograms) InitHistograms();
}

NA62VChannel::~NA62VChannel () {
  if (fHTimeVsWidth) delete fHTimeVsWidth;
}

void NA62VChannel::InitHistograms() {

  fHTimeVsWidth = new TProfile(Form("TimeVsWidthRO%03d", fROChannelID), Form("Time vs Width %03d (RO %03d)", fGeoChannelID, fROChannelID), 250, 0.5*TdcCalib, 250.5*TdcCalib);
  fHTimeVsWidth->GetXaxis()->SetTitle("Width, ns");
  fHTimeVsWidth->GetYaxis()->SetTitle("Leading time, ns");
}

void NA62VChannel::Reset() {
  fNHits = 0;
}

void NA62VChannel::AddHit() {
  if (!fEnabled) return;
  fNHits++;
}

void NA62VChannel::FillTime(Double_t T0CorrectedTime, Double_t Width, Double_t ReferenceTime) {
  if (!fEnabled) return;
  if (fNHits!=1) return;
  if (fabs(T0CorrectedTime-ReferenceTime)<5.0) fHTimeVsWidth->Fill(Width, T0CorrectedTime-ReferenceTime);
}

void NA62VChannel::Write(TFile* HistoFile) {
  if (!fFillHistograms) return;
  if (!fEnabled) return;

  HistoFile->cd(fName+"Monitor/"+fName+"Channels");
  fHTimeVsWidth->Write();
}
