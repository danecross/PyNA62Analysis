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
#include "CHANTIChannel.hh"

CHANTIChannel::CHANTIChannel(Int_t GeoChannelID, Int_t ROChannelID, Bool_t FillHistograms) :
  NA62VChannel(GeoChannelID,ROChannelID,FillHistograms,"CHANTI") {
  //fMinWidth                = 10.0;
  //fMaxWidth                = 15.0;
  //fSlewingCorr_MinWidth    = 0;
  //fSlewingCorr_MaxWidth    = 0;
  //fNewSlewingCorr_MinWidth = 0;
  //fNewSlewingCorr_MaxWidth = 0;
  fHTimeVsDistance         = 0;
  fHDistanceFromSiPM       = 0;
  if (fEnabled && fFillHistograms) InitHistograms(); //CHANTI-specific histos
}

CHANTIChannel::~CHANTIChannel () {
  if(fHTimeVsDistance)   delete fHTimeVsDistance;
  if(fHDistanceFromSiPM) delete fHDistanceFromSiPM;
}

void CHANTIChannel::InitHistograms() {

  Double_t Step = 300./18;
  fHDistanceFromSiPM = new TH1D (Form("DistanceFromSiPM_RO%03d", fROChannelID), Form("Distance From SiPM %03d (RO %03d)", fGeoChannelID, fROChannelID), 19, -0.5*Step, 18.5*Step);
  fHDistanceFromSiPM->GetXaxis()->SetTitle("Dictance from SiPM, mm");
  fHTimeVsDistance = new TProfile(Form("DeltaTimeVsDistanceFromSiPM_RO%03d", fROChannelID), Form("Delta Time Vs Distance From SiPM %03d (RO %03d)", fGeoChannelID, fROChannelID), 19, -0.5*Step, 18.5*Step);
  fHTimeVsDistance->GetXaxis()->SetTitle("Distance from SiPM, mm");
  fHTimeVsDistance->GetYaxis()->SetTitle("Leading time wrt Reference Detector, ns");

}

void CHANTIChannel::FillPosition (Double_t X, Double_t Y, Double_t Time, Double_t ReferenceTime) {
  if (!fEnabled) return;
  Double_t Distance;
  Double_t Length = 150;
  Int_t RingType = (fGeoChannelID%100000)/10000 ;
  Int_t SideID = (fGeoChannelID%10000)/1000;
  if(RingType == 0){
    if(SideID == 0) {
      Distance = Length - Y;
    }
    else {
      Distance = Length + Y;
    }
  }
  else{
    if(SideID == 0) {
      Distance = Length - X;
    }
    else {
      Distance = Length + X;
    }
  }
  fHDistanceFromSiPM->Fill(Distance);
  fHTimeVsDistance->Fill(Distance,Time - ReferenceTime);
}

void CHANTIChannel::SetSlewingCorrectionParameters (Double_t p0, Double_t p1, Double_t p2)
{
  fp0    = p0;
  fp1    = p1;
  fp2    = p2;
}

Double_t CHANTIChannel::GetSlewingCorrection (Double_t Width)
{
  return ( fp2+fp1*TMath::Exp(fp0*Width) );
}

Int_t CHANTIChannel::GetTellID (Int_t ROCH)
{
  return int(ROCH/512 + 1);
}

Int_t CHANTIChannel::GetTDCBID (Int_t ROCH)
{
  return int(ROCH/128 + 1);
}

Int_t CHANTIChannel::GetTDCID (Int_t ROCH)
{
  return int(ROCH/32 + 1);
}

void CHANTIChannel::Write (TFile* HistoFile) {
  if (!fFillHistograms) return;
  if (!fEnabled) return;
  // write common histos
  NA62VChannel::Write(HistoFile);
  // write subdetector-specific histos
  HistoFile->cd("CHANTIMonitor/CHANTIChannels/Position");
  fHDistanceFromSiPM->Write();
  fHTimeVsDistance->Write();
}
