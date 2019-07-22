#include "Riostream.h"
#include "TDirectory.h"
#include "TObjArray.h"
#include "TMath.h"
#include "TString.h"

#include "CREAMRawDecoder.hh"
#include "MUV1Digitizer.hh"
#include "MUV1Reconstruction.hh"
#include "MUV1Geometry.hh"
#include "TSpecialTriggerEvent.hh"
#include "FADCEvent.hh"
#include "TMUV1Event.hh"
#include "TMUV1Digi.hh"
#include "TMUV1Hit.hh"
#include "NA62RecoManager.hh"

/// \class MUV1Digitizer
/// \Brief
/// MUV1Digitizer class.
/// \EndBrief
///
/// \Detailed
/// This class uses MC information to calculate number of photo electrons for each channel
/// using given parametrization. This number is used to generate an analog signal of PMT for each channel.
///	Time over threshold method (ToT) is applied and ToT value as digi stored.
/// \EndDetailed

Bool_t PrintDebug = false;

MUV1Digitizer::MUV1Digitizer(NA62VReconstruction* Reco) :
  NA62VDigitizer(Reco, "MUV1") {
  fDigiEvent = new FADCEvent(TMUV1Digi::Class());
  fBirksEnable = static_cast<MUV1Reconstruction*>(Reco)->GetBirksEnable();
  for (Int_t i=0; i<100; i++) fHitsOnChannel[i].Clear();
  /*
  fHistoDir = static_cast<MUV1Reconstruction*>(Reco)->GetPlotDir()->mkdir("Digitizer");
  fHistoDir->cd();
  fhisto_plot[0] = new TH1D ("h0","",16,0,16);
  fhisto_plot[1] = new TH1D ("h1","",16,0,16);
  */

    //The old value = 21.82 is updated due to the updated MUV1 detector geometry description. See more details here:
    //https://indico.cern.ch/event/686720/contributions/2818666/attachments/1573783/2484338/gia_12122017.pdf
    fInvSamplingFraction = 22.6;
}

MUV1Digitizer::~MUV1Digitizer() {}

TDetectorVEvent * MUV1Digitizer::ProcessEvent(TDetectorVEvent * tEvent) {

  if (tEvent->GetHits()->GetClass()->InheritsFrom("TVDigi") || tEvent->IsA() == TSpecialTriggerEvent::Class())
    return tEvent;

  fDigiEvent->Clear();

  TMUV1Event * MUV1Event = static_cast<TMUV1Event*>( tEvent);

  (*(TVEvent*) fDigiEvent) = (*(TVEvent*) MUV1Event);

  Int_t NHits = MUV1Event->GetNHits();
  TClonesArray *Hits = MUV1Event->GetHits();

  for (Int_t i=0; i<100; i++) fHitsOnChannel[i].Clear();

  Double_t FirstHitTime = fReco->GetStationMCToF(0);
  for (Int_t i=0; i<NHits; i++){
    TMUV1Hit *Hit = static_cast<TMUV1Hit*>( Hits->At(i));
    if (Hit->GetChannelID()<1) continue;
    Int_t ChannelID = Hit->GetChannelID();
    if (ChannelID<1) continue;
    Int_t ch = ChannelID - 1;
    fHitsOnChannel[ch].Add(Hit);
  }

  for (Int_t i=0; i<100; i++){
    if (fHitsOnChannel[i].GetEntries()<1) continue;
    UsePhotoelectronParametrization(i,FirstHitTime);
  }
  return fDigiEvent;
}


//	Function calculates photo electrons in each channel using given parametrization
void MUV1Digitizer::UsePhotoelectronParametrization(Int_t Channel, Double_t HitMinTime) {

  const Double_t Nsamples = 16;	// Set the readout window amplitude: 25 ns * Nsamples
  const Double_t Sigma = fRandom->Gaus(35.42,1.596);

  Int_t Side = Channel>50 ? 1 : 0;
  Int_t PMTIDNegativeAxis = 101 + Channel; // Saleve and Bottom side
  Int_t PMTIDPositiveAxis = 201 + Channel; // Jura and Top side
  Int_t HitChannelID = Channel + 1;

  Double_t Eq_Negative = static_cast<MUV1Reconstruction*>(fReco)->GetEq(Side,PMTIDNegativeAxis%50);
  Double_t Eq_Positive = static_cast<MUV1Reconstruction*>(fReco)->GetEq(Side+2,PMTIDPositiveAxis%50);

  /*
  fhisto_plot[0]->Reset();
  fhisto_plot[1]->Reset();
  fhisto_plot[0] -> SetName(Form("Signal_Ev_%d_Ch_%d",fDigiEvent->GetID(),PMTIDNegativeAxis));
  fhisto_plot[1] -> SetName(Form("Signal_Ev_%d_Ch_%d",fDigiEvent->GetID(),PMTIDPositiveAxis));
  fhisto_plot[0] -> SetTitle(fhisto_plot[0]->GetName());
  fhisto_plot[1] -> SetTitle(fhisto_plot[1]->GetName());
  */

  Int_t NHits = fHitsOnChannel[Channel].GetEntries();

  Double_t Parameters[2][3][NHits];

  for (Int_t iHit=0; iHit<NHits; iHit++){

    TMUV1Hit *Hit = static_cast<TMUV1Hit*>( fHitsOnChannel[Channel].At(iHit));

    Double_t HitEnergy = Hit->GetEnergy();
    Double_t HitPositionX = Hit->GetPosition()[0];
    Double_t HitPositionY = Hit->GetPosition()[1];
    Double_t FineTime = NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256.;
    Double_t HitTime = Hit->GetTime() + FineTime - HitMinTime;
    Double_t HitStepLength = Hit->GetStepLength();
    //Double_t HitPositionInScintillator = Hit->GetPositionInScintillator();

    //*---------------------------------------------------------------

    //	Scintillators Category 1
    //	Photo electrons created in center of scintillator per 1 MeV energy deposition
    //	Quantum efficiency of  15% assumed
    Double_t PhotoelectronsPerMeV = 5.939;

    //	Fit parameter of parametrization
    //	N_phe(position, Energy) = PhotoelectronsPerMeV(E)
    //			* (Alpha * exp((position * 0.1) / Lambda1)
    //					+ (1 - Alpha) * exp((position * 0.1) / Lambda2));
    //
    //Double_t Alpha = 0.9997;
    //Double_t Lambda1 = 265.5;
    //Double_t Lambda2 = -32.99;

    //Double_t DistanceHitFringeR =
    //  ((MUV1Geometry::GetInstance()->GetScintLengthStandard() / 10. /*mm->cm*/)
    //   / 2.) - HitPositionInScintillator / 10; // mm
    //Double_t DistanceHitFringeL =
    //  ((MUV1Geometry::GetInstance()->GetScintLengthStandard() / 10. /*mm->cm*/)
    //   / 2.) + HitPositionInScintillator / 10; // mm

    if (HitChannelID == 01 || HitChannelID == 51 || HitChannelID == 44
        || HitChannelID == 94) {
      //Size 2c
      PhotoelectronsPerMeV = 6.329; // Für 15% Quanteneffizienz
      //Alpha = 0.9997;
      //Lambda1 = 265.5;
      //Lambda2 = -32.99;
      //DistanceHitFringeR = ((MUV1Geometry::GetInstance()->GetScintLengthOuter()[0] / 10. /*mm->cm*/ /*225.6*/) / 2.) - HitPositionInScintillator / 10; // mm
      //DistanceHitFringeL = ((MUV1Geometry::GetInstance()->GetScintLengthOuter()[0] / 10. /*mm->cm*/ /*225.6*/) / 2.) + HitPositionInScintillator / 10; // mm
    }
    if (HitChannelID == 02 || HitChannelID == 55 || HitChannelID == 43
        || HitChannelID == 93) {
      //Size 2b
      PhotoelectronsPerMeV = 6.208; // Für 15% Quanteneffizienz
      //Alpha = 0.9997;
      //Lambda1 = 265.5;
      //Lambda2 = -32.99;
      //DistanceHitFringeR = ((MUV1Geometry::GetInstance()->GetScintLengthOuter()[1] / 10. /*mm->cm*/ /*237.6*/) / 2.) - HitPositionInScintillator / 10; // mm
      //DistanceHitFringeL = ((MUV1Geometry::GetInstance()->GetScintLengthOuter()[1] / 10. /*mm->cm*/ /*237.6*/) / 2.) + HitPositionInScintillator / 10; // mm
    }
    if (HitChannelID == 03 || HitChannelID == 53 || HitChannelID == 42
        || HitChannelID == 92) {
      //Size 2a
      PhotoelectronsPerMeV = 6.071; // Für 15% Quanteneffizienz
      //Alpha = 0.9997;
      //Lambda1 = 265.5;
      //Lambda2 = -32.99;
      //DistanceHitFringeR = ((MUV1Geometry::GetInstance()->GetScintLengthOuter()[2] / 10. /*mm->cm*/ /*249.6*/) / 2.) - HitPositionInScintillator / 10; // mm
      //DistanceHitFringeL = ((MUV1Geometry::GetInstance()->GetScintLengthOuter()[2] / 10. /*mm->cm*/ /*249.6*/) / 2.) + HitPositionInScintillator / 10; // mm
    }
    if (HitChannelID == 21 || HitChannelID == 71 || HitChannelID == 24
        || HitChannelID == 74) {
      //Size 4
      PhotoelectronsPerMeV = 8.394; // Für 15% Quanteneffizienz
      //Alpha = 0.999999999;
      //Lambda1 = 258;
      //Lambda2 = -30;
      //DistanceHitFringeR = ((MUV1Geometry::GetInstance()->GetScintLengthMiddleStd() / 10. /*mm->cm*/ /*120.0*/) / 2.) - HitPositionInScintillator / 10; // mm
      //DistanceHitFringeL = ((MUV1Geometry::GetInstance()->GetScintLengthMiddleStd() / 10. /*mm->cm*/ /*120.0*/) / 2.) + HitPositionInScintillator / 10; // mm
    }

    if (HitChannelID == 22 || HitChannelID == 72 || HitChannelID == 23
        || HitChannelID == 73) {
      //Size 3

      PhotoelectronsPerMeV = 8.412; // Für 15% Quanteneffizienz
      //Alpha = 0.999999999;
      //Lambda1 = 258;
      //Lambda2 = -30;
      //DistanceHitFringeR = ((MUV1Geometry::GetInstance()->GetScintLengthMiddleOuter() / 10. /*mm->cm*/ /*123.6*/) / 2.) - HitPositionInScintillator / 10; // mm
      //DistanceHitFringeL = ((MUV1Geometry::GetInstance()->GetScintLengthMiddleOuter() / 10. /*mm->cm*/ /*123.6*/) / 2.) + HitPositionInScintillator / 10; // mm
    }

    if ((HitChannelID > 17 && HitChannelID < 21) || (HitChannelID > 24 && HitChannelID < 28) || (HitChannelID > 68 && HitChannelID < 71) || (HitChannelID > 74 && HitChannelID < 77)){

      //Size 3

      PhotoelectronsPerMeV = 7.513; // Für 15% Quanteneffizienz (Extrapolation)
      //Alpha = 0.9997;
      //Lambda1 = 265.5;
      //Lambda2 = -32.99;
      //DistanceHitFringeR = ((MUV1Geometry::GetInstance()->GetScintLengthStandard() / 20. /*mm->cm*/ /*130.8*/) / 2.) - HitPositionInScintillator / 10; // mm
      //DistanceHitFringeL = ((MUV1Geometry::GetInstance()->GetScintLengthStandard() / 20. /*mm->cm*/ /*130.8*/) / 2.) + HitPositionInScintillator / 10; // mm
    }

    //*--------------------------------------------------------
    //	Deposited energy is corrected for Birks effect. Birks constant B_k = 0.151*mm/MeV choosen.
    if(fBirksEnable==1){
      HitEnergy = BirksCorrection(HitEnergy, HitStepLength, 0.151);
    }

    Double_t PositionOfHit = 0;
    if (HitChannelID < 50) {
      PositionOfHit = HitPositionY;
    } else {
      PositionOfHit = HitPositionX;
    }

    //	Function PhotoElectronsInScintillator calculates number of photo electrons seen by one PMT for this scintillator
    Double_t PhotonsNegativeAxis = HitEnergy*PhotoelectronsPerMeV*static_cast<MUV1Reconstruction*>(fReco)->GetQPosAttenuation(PMTIDNegativeAxis,PositionOfHit)/Eq_Negative * (5.939/PhotoelectronsPerMeV);
    Double_t PhotonsPositiveAxis = HitEnergy*PhotoelectronsPerMeV*static_cast<MUV1Reconstruction*>(fReco)->GetQPosAttenuation(PMTIDPositiveAxis,PositionOfHit)/Eq_Positive * (5.939/PhotoelectronsPerMeV);

    Double_t NPhe_Positive = static_cast<Int_t> (PhotonsPositiveAxis);
    if (fRandom->Rndm()<(PhotonsPositiveAxis - NPhe_Positive)) NPhe_Positive++;

    Double_t Charge_Positive = fInvSamplingFraction * 4.7e+6 * NPhe_Positive * 1.602e-7;

    Double_t NPhe_Negative = static_cast<Int_t> (PhotonsNegativeAxis);
    if (fRandom->Rndm()<(PhotonsNegativeAxis - NPhe_Negative)) NPhe_Negative++;

    Double_t Charge_Negative = fInvSamplingFraction * 4.7e+6 * NPhe_Negative * 1.602e-7;

    Parameters[0][0][iHit] = Charge_Negative;
    Parameters[0][1][iHit] = HitTime - static_cast<MUV1Reconstruction*>(fReco)->GetTPosDelay(PMTIDNegativeAxis,PositionOfHit);
    Parameters[0][2][iHit] = Sigma;

    Parameters[1][0][iHit] = Charge_Positive;
    Parameters[1][1][iHit] = HitTime - static_cast<MUV1Reconstruction*>(fReco)->GetTPosDelay(PMTIDPositiveAxis,PositionOfHit);
    Parameters[1][2][iHit] = Sigma;

    //	Deleting values for Scintillators 17-28 and 71-74 on the blind side

    if ((HitChannelID > 17 && HitChannelID < 28)
        || (HitChannelID > 68 && HitChannelID < 77)) {
      if (PositionOfHit < 0) {
        Parameters[1][0][iHit] = 0.;
      } else {
        Parameters[0][0][iHit] = 0.;
      }
    }
  }

  TMUV1Digi* Digi[2];

  Digi[0] = static_cast<TMUV1Digi*>( fDigiEvent -> AddDigi(static_cast<TMUV1Hit*>(fHitsOnChannel[Channel].At(0))));

  Digi[0] -> SetChannelID(PMTIDNegativeAxis);
  Digi[0] -> DecodeChannelID();

  Digi[1] = static_cast<TMUV1Digi*>( fDigiEvent -> AddDigi(static_cast<TMUV1Hit*>(fHitsOnChannel[Channel].At(0))));
  Digi[1] -> SetChannelID(PMTIDPositiveAxis);
  Digi[1] -> DecodeChannelID();

  Double_t T0[2];
  T0[0] = static_cast<MUV1Reconstruction*>(fReco)->GetT0(Side,PMTIDNegativeAxis%50);
  T0[1] = static_cast<MUV1Reconstruction*>(fReco)->GetT0(Side+2,PMTIDPositiveAxis%50);

  Bool_t Saturated[2] = {0,0}, Underflow[2] = {0,0};
  Double_t Sample_time[2];

  Double_t Offset[2] = { fReco->GetT0Correction(Digi[0]),fReco->GetT0Correction(Digi[1])};

  for (Int_t iSample = 0; iSample < Nsamples; iSample++){
    Double_t Signal[2];

    for (Int_t iCh=0; iCh<2; iCh++){
      Sample_time[iCh] =  iSample*ClockPeriod + fRandom->Gaus(0,1) - Offset[iCh] - 0.5*Nsamples*ClockPeriod - T0[iCh];

      Signal[iCh] = 0.;

      for (Int_t iHit = 0; iHit < NHits; iHit++)
        Signal[iCh] += Parameters[iCh][0][iHit] * AnalogSignal(Sample_time[iCh],Parameters[iCh][1][iHit],Parameters[iCh][2][iHit]);

      Signal[iCh] *= 16384./2000.;
      Signal[iCh] += 1000 + fRandom->Gaus(0.,3.);

      if (Signal[iCh] <= 0 ) {
        Signal[iCh] = 0;
        Underflow[iCh] = true;
      }
      if (Signal[iCh] > 16383) {
        Signal[iCh] = 16383;
        Saturated[iCh] = true;
      }

      Signal[iCh] = static_cast<Int_t>( Signal[iCh] );

      Digi[iCh]->AddSample(Signal[iCh]);
      //fhisto_plot[iCh] -> Fill(iSample,Signal[iCh]);
    }
  }

  Digi[0]->SetFlags((Saturated[0] ? 1 : 0) * (1<<kCREAMSaturationBit) + (Underflow[0] ? 1 : 0) * (1<<kCREAMUnderflowBit));
  Digi[1]->SetFlags((Saturated[1] ? 1 : 0) * (1<<kCREAMSaturationBit) + (Underflow[1] ? 1 : 0) * (1<<kCREAMUnderflowBit));

  /*
  if (PrintDebug){
    fHistoDir->cd();
    fhisto_plot[0]->Write();
    fhisto_plot[1]->Write();
  }
  */
}


//	Correction of deposited energy for the Birks effect
Double_t MUV1Digitizer::BirksCorrection(Double_t Energy, Double_t StepLength,
    Double_t BirksConstant) {

  if(StepLength==0) return Energy;

  Double_t dEdX = Energy / StepLength;
  Double_t CorrectedEnergy = (dEdX / (1 + (BirksConstant * dEdX))) * StepLength;

  if(CorrectedEnergy!=CorrectedEnergy || CorrectedEnergy < 0) return Energy;
  return CorrectedEnergy;
}

//	Parametrization
Double_t MUV1Digitizer::PhotoElectronsInScintillator(Double_t edep, Double_t position,
    Double_t PhotoelectronsPerMeV, Double_t Alpha, Double_t Lambda1,
    Double_t Lambda2, bool near) {
  Int_t sign = 0;
  if (near == true /*Calculating for scintillator near to hit*/) {
    sign = +1;
  } else {
    sign = -1;
  } // /*Calculating for scintillator far away to hit*/

  //	Number of photo electrons seen by PMT, if particle hits center of scintillator
  Double_t PhotoElectronsCenterCalculated = edep * PhotoelectronsPerMeV;

  //	Correction of value PhotoElectronsCenterCalculated for position.
  Double_t PhotoElectronsPositionCorrected = PhotoElectronsCenterCalculated
    * (Alpha * exp(sign * (position * 0.1) / Lambda1)
        + (1 - Alpha) * exp(sign * (position * 0.1) / Lambda2));

  return PhotoElectronsPositionCorrected;
}

//		Parametrisation of Signal through shaper and cream
Double_t MUV1Digitizer::AnalogSignal(Double_t T, Double_t Center, Double_t Sigma){
  if (TMath::Abs(T-Center)/Sigma > 10.) return 0;
  double val = (TMath::Gaus(T,Center,Sigma,1) - 0.05*TMath::Gaus(T,Center+3*Sigma,2.*Sigma,1));
  return val;
}
