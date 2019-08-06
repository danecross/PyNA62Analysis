// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-08-26
//
// ---------------------------------------------------------------

#include "Riostream.h"
#include "TRegexp.h"

#include "TCedarHit.hh"
#include "TCedarDigi.hh"
#include "TCedarEvent.hh"
#include "TDCBRawDecoder.hh"
#include "CedarDigitizer.hh"
#include "CedarReconstruction.hh"
#include "CedarChannel.hh"
#include "TDCEvent.hh"
#include "TSpecialTriggerEvent.hh"
#include "NA62RecoManager.hh"

#include "CLHEP/Units/SystemOfUnits.h"
using namespace CLHEP;

CedarDigitizer::CedarDigitizer(NA62VReconstruction* Reco) :
  NA62VDigitizer(Reco, "Cedar") {

  fDigiEvent    = new TDCEvent(TCedarDigi::Class());
  fPMTTime_min  = static_cast<CedarReconstruction*>(fReco)->GetPMTTime_min();
  fPMTTime_max  = static_cast<CedarReconstruction*>(fReco)->GetPMTTime_max();
  fTimeResponse = new TF1
    ("CedarTimeResponse", static_cast<CedarReconstruction*>(fReco)->GetPMTTime_Response(), fPMTTime_min, fPMTTime_max);
  fPMTWidth_mean = static_cast<CedarReconstruction*>(fReco)->GetPMTWidth_mean();
  fPMTWidth_sigma = static_cast<CedarReconstruction*>(fReco)->GetPMTWidth_sigma();
  fPMT_Efficiency = static_cast<CedarReconstruction*>(fReco)->GetPMT_Efficiency();
  fPMT_MergeThreshold = static_cast<CedarReconstruction*>(fReco)->GetPMT_MergeThreshold();
}

CedarDigitizer::~CedarDigitizer() {
  if (fTimeResponse) delete fTimeResponse;
}

void CedarDigitizer::StartOfBurst() {
  NA62VDigitizer::StartOfBurst();
}

void CedarDigitizer::EndOfBurst() {
  NA62VDigitizer::EndOfBurst();
}

TDetectorVEvent* CedarDigitizer::ProcessEvent (TDetectorVEvent *tEvent) {

  if (tEvent->GetHits()->GetClass()->InheritsFrom("TVDigi") ||
      tEvent->IsA()->InheritsFrom("TSpecialTriggerEvent"))
    return tEvent;

  TCedarEvent* CedarEvent = static_cast<TCedarEvent*>(tEvent);
  Int_t NHits = CedarEvent->GetNHits();

  fDigiEvent->Clear();
  fDigiEvent->TVEvent::operator=(*static_cast<TVEvent*>(CedarEvent));
  if (!NHits) return fDigiEvent;

  // Loop over MC hits
  for (Int_t iHit = 0; iHit<NHits; iHit++) {

    TCedarHit *Hit = static_cast<TCedarHit*>(CedarEvent->GetHit(iHit));

    // Do we need to apply QE? Negative PMType means QE is already applied
    if (Hit->GetPMType()<0) {
      Hit->SetPMType(-Hit->GetPMType());
    }
    else {
      Double_t Wavelength = 1.986446e-25*joule*m/Hit->GetEnergy();
      if (fRandom->Rndm() > fPMT_Efficiency * QE(Wavelength, Hit->GetPMType())) continue;
    }

    // Convert position ID into RO channel ID
    Int_t ich   = Hit->GetChannelID(); // Position ID
    Int_t iROch =                      // RO channel ID
      static_cast<TDCBRawDecoder*>(static_cast<CedarReconstruction*>(fReco)->GetRawDecoder()->GetDecoder())->GetChannelRO(ich);
    if (iROch<0) continue; // channel not instrumented

    // Hit time with TDC correction (TdcCalib = ClockPeriod/256)
    Double_t Width         = fRandom->Gaus(fPMTWidth_mean*ns, fPMTWidth_sigma*ns);
    Double_t SlewingCorr   = static_cast<CedarChannel*>(fChannels[iROch])->GetSlewingCorrection(Width);
    gRandom = fRandom; // set gRandom to fRandom to ensure reproducibility of GetRandom()

    Double_t FineTime = NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256.;
    Double_t CorrectedTime = Hit->GetTime() + FineTime
      - static_cast<CedarReconstruction*>(fReco)->GetStationMCToF(Hit->GetStationID())
      + fReco->GetT0Correction(Hit->GetChannelID(),Hit->GetStationID())
      + fChannels[iROch]->GetT0() + SlewingCorr + fTimeResponse->GetRandom(fPMTTime_min, fPMTTime_max) * ns;
    Double_t DigitizedTime = (Int_t)(CorrectedTime/ns/TdcCalib)*TdcCalib;

    // Is there a hit in this channel and close in time already?
    Bool_t Merged = kFALSE;
    for (Int_t iDone=0; iDone<fDigiEvent->GetNHits(); iDone++) {
      TCedarDigi *DigiDone = static_cast<TCedarDigi*>(fDigiEvent->GetHit(iDone));
      if (DigiDone->GetChannelID() == ich) { // another Digi exists in this channel
        Double_t HitTimeDifference = DigitizedTime - DigiDone->GetLeadingEdge();
        if (fabs(HitTimeDifference)<fPMT_MergeThreshold*ns) {
          Merged = kTRUE;
          if (HitTimeDifference<0) DigiDone->SetLeadingEdge(DigitizedTime);
        }
      }
    }

    // Create a new Digi
    if (!Merged) {
      TCedarDigi *Digi = static_cast<TCedarDigi*>(fDigiEvent->AddDigi(Hit));
      Digi->DecodeChannelID(); // @@ should become redundant
      Digi->SetLeadingEdge(DigitizedTime);
      Digi->SetTrailingEdge(DigitizedTime + Width);
      Digi->SetDetectedEdge(3); // 3 = both leading and trailing edges exist
    }
  }

  return fDigiEvent;
}

////////////////////////////////////////////////////////////////////////////

Double_t CedarDigitizer::QE (Double_t wavelength, Int_t PMType) {
  if      (PMType==1) return QE_EMI_9820_QB (wavelength);
  else if (PMType==2) return QE_R7400U_03   (wavelength);
  else if (PMType==3) return QE_R9880U_110  (wavelength);
  else if (PMType==4) return QE_R9880U_210  (wavelength);
  return 0;
}

// Hamamatsu R7400U-03 quantum efficiency
// Parameterized by Evgueni, July 2011

Double_t CedarDigitizer::QE_R7400U_03 (Double_t wavelength) {
  Double_t par[10] =
  {-58.41145755814, 1.450540667766, -0.01561331198442,
    9.545010080831e-05, -3.648461145542e-07, 9.047599515597e-10,
    -1.457151808585e-12, 1.471328774241e-15, -8.46121819724e-19,
    2.11384701372e-22};

  Double_t x = wavelength/nm;
  if (x<180) return 0;
  if (x>660) return 0;
  Double_t qe = 0;
  for (int i=0; i<10; i++) qe += par[i]*TMath::Power(x,i);
  if (qe<0) qe = 0;
  return qe;
}

// Hamamatsu R9880U-110 quantum efficiency
// Parameterized by Angela, July 2011

Double_t CedarDigitizer::QE_R9880U_110 (Double_t wavelength) {
  Double_t par[15] =
  {1489.053765000671, -20.61340505642701, 0.09607362916193821,
    -0.000144918944048782, -1.087924475686453e-07, 3.619104979507752e-10,
    2.742092765095943e-13, -1.067200613381487e-15, 6.333980140159196e-19,
    4.675391577876988, 505.1903283978535, 15.37334879108591,
    -23.08738129086531, 358.7521218115685, 53.63424346389683};

  Double_t x  = wavelength/nm;
  Double_t x1 = (x<650) ? x : 650;
  Double_t qe = 0;
  for (int i=0; i<9; i++) qe += par[i]*TMath::Power(x1,i);
  qe += par[9]*TMath::Gaus(x1, par[10], par[11]);
  qe += par[12]*TMath::Gaus(x1, par[13], par[14]);
  qe *= 0.01;
  if (x>650) qe *= (1 - (x-650)/(675-650));
  if (qe<0 || x<200) qe = 0;
  return qe;
}

// Hamamatsu R9880U-210 quantum efficiency
// Parameterized by Evgueni, March 2013

Double_t CedarDigitizer::QE_R9880U_210 (Double_t wavelength) {
  double par[9] =
  {277.3385690654, -5.360192324445, 0.04415739632667,
    -0.0002031054657811, 5.721437395991e-07, -1.012602804374e-09,
    1.100802213492e-12, -6.72600529683e-16, 1.769806940956e-19};

  Double_t x  = wavelength/nm;
  Double_t x1 = (x<680) ? ((x>240) ? x : 240) : 680;
  Double_t qe = 0;
  for (int i=0; i<9; i++) qe += par[i]*TMath::Power(x1,i);
  if (x>680) qe *= (1 - (x-680)/(700-680));
  if (x<240) qe *= (1 - (240-x)/(240-225));
  if (qe<0)  qe = 0;
  return qe;
}

/** Original Cedar PMT quantum efficiency **/

// 1) Lau's parameterization

Double_t CedarDigitizer::QE_EMI_9820_QB_Lau (Double_t wavelength) {
  Double_t wl = wavelength/nm;
  Double_t qe = 0.25 - TMath::Power((wl-400)/500., 2);
  if (qe<0) qe = 0;
  return qe;
}

// 2) Francis's parameterization following the data sheet (March 2013)

Double_t CedarDigitizer::QE_EMI_9820_QB (Double_t wavelength) {

  Double_t wlraw = wavelength/nm;
  Double_t wl = (wlraw < 141.0 ) ? 141.0 : (wlraw > 649.0) ? 649.0 : wlraw;

  Double_t wls[24] =
  {140, 160, 180, 200, 220, 240, 260, 280, 300, 320,
    340, 360, 380, 400, 420, 440, 460, 480, 500, 520,
    540, 560, 600, 650};
  Double_t qes[24] =
  {0, 20, 21.6, 21.6, 21.4, 21.4, 22, 24, 25, 25.6,
    26, 26, 26.4, 26, 24.8, 23.2, 20.8, 18, 15.2, 12,
    8, 5.6, 2, 0};

  unsigned int i = 0;
  while (true) {
    if (wl > wls[i] && wl < wls[i+1])
    { break; }
    else
    { ++i; }
  }
  return 0.01 * (qes[i] + ( wl - wls[i] )
      / ( wls[i+1] - wls[i] ) * ( qes[i+1] - qes[i] ) );
}
