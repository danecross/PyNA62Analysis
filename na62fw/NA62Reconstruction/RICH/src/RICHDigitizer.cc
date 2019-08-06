/********************************************//**
* Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
* Antonino Sergi (sergiant@cern.ch)
* Antonio Cassese (antonio.cassese@cern.ch)
* Francesca Bucci (fbucci@cern.ch), Monica Pepe (monica.pepe@cern.ch)
* Evgueni Goudzovski (Aug 2019): massive cleanup, integration to CDB
************************************************/

#include "Riostream.h"
#include "NA62Utilities.hh"
#include "NA62RecoManager.hh"
#include "TRICHHit.hh"
#include "TRICHDigi.hh"
#include "TDCBRawDecoder.hh"
#include "RICHDigitizer.hh"
#include "RICHReconstruction.hh"
#include "RICHChannel.hh"
#include "TDCEvent.hh"
#include "TSpecialTriggerEvent.hh"
#include "TRICHEvent.hh"
#include "RICHParameters.hh"
#include "CLHEP/Units/SystemOfUnits.h"

using namespace CLHEP;

RICHDigitizer::RICHDigitizer(NA62VReconstruction* Reco) : NA62VDigitizer(Reco, "RICH") {
  fChargeThreshold    = static_cast<RICHReconstruction*>(fReco)->GetChargeThreshold();
  fTimeWidthSigma     = static_cast<RICHReconstruction*>(fReco)->GetTimeWidthSigma();
  fWidthConstant      = static_cast<RICHReconstruction*>(fReco)->GetWidthConstant();
  fNSCChannels        = static_cast<RICHReconstruction*>(fReco)->GetNSCChannels();
  fDigiEvent          = new TDCEvent(TRICHDigi::Class());
  fTimeResponse       = new TF1("TR", "TMath::Gaus(x, 0, 0.161203) + 0.1101571*TMath::Gaus(x, -0.1750211, 0.30081) + 0.0290554*TMath::Exp(1.03047*(0.557182-2*x))*(1-TMath::Erf((0.282630-x)/0.0885416)) + 0.0066920*TMath::Gaus(x, 1.47462, 0.167551)", -3, 2);
  fLandau             = new TF1("fLandau","landau",0,1);
  fLandau->SetParameters(40,0.120,0.04);
  fPolya              = new TF1("fPolya","TMath::Exp(4.7*(TMath::Log(4.7*x/0.165)-x/0.165))/(x*TMath::Gamma(4.7))",0,1);
  fQEcorrection = 1.0;
}

RICHDigitizer::~RICHDigitizer() {
  if (fTimeResponse) {
    delete fTimeResponse;
    fTimeResponse = nullptr;
  }
  if (fLandau) {
    delete fLandau;
    fLandau = nullptr;
  }
  if (fPolya) {
    delete fPolya;
    fPolya = nullptr;
  }
}

void RICHDigitizer::StartOfBurst() {
  NA62VDigitizer::StartOfBurst();

  ////////////////////////////////////////////////////////////////////////
  // Compute the correction to the QE, which includes the NHits-dependence
  // and uncorrects for the R-dependence taken into accout in the NA62MC

  Int_t    RunNumber = NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID();
  Long_t   RunTime   = NA62Utilities::GetInstance()->GetRunTime(RunNumber);
  Double_t Radius    = RICHParameters::GetInstance()->GetElectronRingRadius(RunNumber, RunTime); // [mm]
  Double_t NHits     = RICHParameters::GetInstance()->GetElectronRingNHits(RunNumber, RunTime);
  Double_t RefRadius = RICHParameters::GetInstance()->GetReferenceElectronRingRadius(); // [mm]
  Double_t RefNHits  = RICHParameters::GetInstance()->GetReferenceElectronRingNHits();
  fQEcorrection = NHits/RefNHits/(1.+2.*(Radius-RefRadius)/RefRadius);
}

void RICHDigitizer::EndOfBurst() {
  NA62VDigitizer::EndOfBurst();
}

/********************************************//**
* Converts the MCTruth hits in digitized hits
* Smears leading edge
* Width evaluated with Polya
************************************************/

TDetectorVEvent * RICHDigitizer::ProcessEvent(TDetectorVEvent * tEvent) {
  if (tEvent->GetHits()->GetClass()->InheritsFrom("TVDigi") ||
      tEvent->IsA() == TSpecialTriggerEvent::Class())
    return tEvent;

  TRICHEvent* RICHEvent = static_cast<TRICHEvent*>(tEvent);
  Int_t NHits = RICHEvent->GetNHits();

  fDigiEvent->Clear();
  fDigiEvent->TVEvent::operator=(*static_cast<TVEvent*>(RICHEvent));

  // Loop over MC hits
  for (Int_t iHit=0; iHit<NHits; iHit++) {
    TRICHHit *Hit = static_cast<TRICHHit*>(RICHEvent->GetHit(iHit));

    // Apply PMT quantum efficiency:
    // 0.897 is a factor to synchronize the number of hits between
    // NA62MC and run 6610 data; fQEcorrection is the run-dependent correction
    Double_t Wavelength = 1.986446e-25*joule*m/Hit->GetEnergy();
    if (fRandom->Rndm() > 0.897*fQEcorrection*QE(Wavelength)) continue;

    // Convert position ID into RO channel ID
    Int_t ChannelID = Hit->GetChannelID();       // Position encoded ID
    Int_t SeqChannelID = Hit->GetChannelSeqID(); // Sequential ID
    Int_t iROCh =                                // RO channel ID
      static_cast<TDCBRawDecoder*>(static_cast<RICHReconstruction*>(fReco)->GetRawDecoder()->GetDecoder())->GetChannelRO(ChannelID);
    if (iROCh < 0) continue; // channel not instrumented

    // Hit time with TDC correction (TdcCalib = ClockPeriod/256)
    Double_t q = -100;
    Double_t C = 1.6;
    Double_t ValueLandau = 1e8;
    Double_t ValuePolya = 0;
    Double_t uRandomUniform = 1;
    while (uRandomUniform * C * ValueLandau > ValuePolya) {
      q = fRandom->Landau(0.120,0.04); // In pC
      if(q<0) continue;
      ValueLandau = fLandau->Eval(q);
      if(4.7*(TMath::Log(4.7*q/0.165)-q/0.165)>-700) ValuePolya = fPolya->Eval(q); //cut off to avoid ERANGE error, happening at -745
      else ValuePolya = 0.;
      uRandomUniform = fRandom->Uniform();
    }

    if (q < fChargeThreshold) continue;
    Double_t Width         = 2*fTimeWidthSigma*TMath::Sqrt(2*TMath::Log(q/fChargeThreshold)) + fWidthConstant;
    Double_t SlewingCorr   = static_cast<RICHChannel*>(fChannels[SeqChannelID])->GetSlewingCorrection(Width);
    gRandom = fRandom; // set gRandom to fRandom to ensure reproducibility of GetRandom()
    Double_t FineTime = NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256.;
    Double_t CorrectedTime = Hit->GetTime() + FineTime - static_cast<RICHReconstruction*>(fReco)->GetStationMCToF(Hit->GetStationID())
      + fReco->GetT0Correction(Hit->GetChannelID(),Hit->GetStationID())
      + fChannels[SeqChannelID]->GetT0() + SlewingCorr + fTimeResponse->GetRandom(-2,3) * ns;
    Double_t DigitizedTime = (Int_t)(CorrectedTime/ns/TdcCalib)*TdcCalib;

    // Is there a hit in this channel and close in time already?
    Bool_t Merged = kFALSE;
    Int_t iToMerge = -1;

    for (Int_t iDone = 0; iDone < fDigiEvent->GetNHits(); iDone++) {
      TRICHDigi *DigiDone = static_cast<TRICHDigi*>(fDigiEvent->GetHit(iDone));
      //Int_t irichid_digidone =DigiDone->GetChannelID();
      if (DigiDone->GetChannelSeqID() == SeqChannelID) { // another Digi exists in this channel
        iToMerge = iDone;
        Double_t NewLeadOldTrailDiff = DigitizedTime - DigiDone->GetTrailingEdge();
        Double_t OldLeadNewTrailDiff = DigiDone->GetLeadingEdge() - DigitizedTime + Width;
        if ((NewLeadOldTrailDiff < 0) || (OldLeadNewTrailDiff < 0)) Merged = kTRUE;
      }
    }

    // Create a new Digi
    if (!Merged) {
      TRICHDigi *Digi = static_cast<TRICHDigi*>(fDigiEvent->AddDigi(Hit));
      Digi->DecodeChannelID();
      Digi->SetLeadingEdge(DigitizedTime);
      Digi->SetTrailingEdge(DigitizedTime + Width);
      Digi->SetDetectedEdge(3); // 3 = both leading and trailing edges exist
    }
    else {
      TRICHDigi *Digi = static_cast<TRICHDigi*>(fDigiEvent->GetHit(iToMerge));
      Double_t Leading = Digi->GetLeadingEdge();
      Double_t Trailing = Digi->GetTrailingEdge();
      if (Leading > DigitizedTime) Leading = DigitizedTime;
      if (Trailing < (DigitizedTime + Width)) Trailing = DigitizedTime + Width;
      Digi->SetLeadingEdge(Leading);
      Digi->SetTrailingEdge(Trailing);
    }
  } // end loop on mc hit

  Int_t SuperCellDigiFlag[61*2*2];
  for(Int_t i=0;i<61*2*2;i++){
    SuperCellDigiFlag[i]=0;
  }

  Int_t NDigiPM = fDigiEvent->GetNHits();
  for(Int_t iDigiPM = 0; iDigiPM < NDigiPM ; iDigiPM++){ //loop on PMT digi to create SC

    TRICHDigi *PMDigi = static_cast<TRICHDigi*>(fDigiEvent->GetHit(iDigiPM));
    if(SuperCellDigiFlag[PMDigi->GetSuperCellID()+PMDigi->GetUpDownDiskID()*61+PMDigi->GetDiskID()*61*2]==0) {
      SuperCellDigiFlag[PMDigi->GetSuperCellID()+PMDigi->GetUpDownDiskID()*61+PMDigi->GetDiskID()*61*2]=1;

      TRICHDigi *SCOrDigi = static_cast<TRICHDigi*>(fDigiEvent->AddDigi(PMDigi->GetChannelID()));
      *SCOrDigi = *PMDigi;
      SCOrDigi->SetOrSuperCellID(kOr);
      SCOrDigi->SetPmtID(0); //super cells have PmtID = 0!
      SCOrDigi->EncodeChannelID();
    } // end if new SuperCell
  } // end loop on digi to create SC

  return fDigiEvent;
}

/********************************************//**
* Hamamatsu R7400U-03 quantum efficiency
************************************************/

Double_t RICHDigitizer::QE(Double_t wavelength) {
  Double_t par[10] =
  {-58.41145755814, 1.450540667766, -0.01561331198442,
    9.545010080831e-05, -3.648461145542e-07, 9.047599515597e-10,
    -1.457151808585e-12, 1.471328774241e-15, -8.46121819724e-19,
    2.11384701372e-22};

  Double_t x = wavelength/nm;
  if (x < 180.0) return 0.0;
  if (x > 660.0) return 0.0;
  Double_t qe = 0.0;
  for (Int_t i = 0; i < 10; i++) qe += par[i]*TMath::Power(x,i);
  if (qe < 0.0) qe = 0.0;
  return qe;
}
