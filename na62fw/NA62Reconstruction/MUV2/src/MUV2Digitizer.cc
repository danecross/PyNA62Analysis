#include "Riostream.h"
#include "TDirectory.h"
#include "TObjArray.h"
#include "TMath.h"
#include "TString.h"
#include "CREAMRawDecoder.hh"
#include "MUV2Digitizer.hh"
#include "MUV2Reconstruction.hh"
#include "MUV2Geometry.hh"
#include "TSpecialTriggerEvent.hh"
#include "FADCEvent.hh"
#include "TMUV2Event.hh"
#include "TMUV2Digi.hh"
#include "TMUV2Hit.hh"
#include "NA62RecoManager.hh"

/// \class MUV2Digitizer
/// \Brief
/// First implementation of the MUV2 digitizer.
/// \n
/// Takes the MC Hits and convert them into Digi to be readout by the Reconstruction.
/// \n
/// Fixed readout window (16 ADC points * ~25 ns = 400 ns) centered on the time of the first hit in the event.
/// \n
/// The center of the window can be change using the Offset variable at line 57 in ns.
/// \n
/// In the same way the width of the window can be changed using the variable Nsample in a range between 8 and 32 points at line 56.
/// \EndBrief

MUV2Digitizer::MUV2Digitizer(NA62VReconstruction* Reco) : NA62VDigitizer(Reco, "MUV2"), fHit(nullptr) {

  fDigiEvent = new FADCEvent(TMUV2Digi::Class());
  fBirksEnable = static_cast<MUV2Reconstruction*>(Reco)->GetBirksEnable();
  for (int i=0; i<4; i++){
    for (int j=0; j<22; j++) fHitsOnChannel[i][j].Clear();
  }

  //The old value = 39.58 is updated due to the updated MUV2 detector geometry description. See more details here:
  //https://indico.cern.ch/event/686720/contributions/2818666/attachments/1573783/2484338/gia_12122017.pdf
  fInvSamplingFraction = 42.0;
}

MUV2Digitizer::~MUV2Digitizer() {}

TDetectorVEvent * MUV2Digitizer::ProcessEvent(TDetectorVEvent * tEvent) {

  if (tEvent->GetHits()->GetClass()->InheritsFrom("TVDigi")
      || tEvent->IsA() == TSpecialTriggerEvent::Class())
    return tEvent;

  const Double_t calibration = 2.24; //fC/MeV, calibration factor
  const Double_t Nsamples = 16;	// Set the readout window amplitude: ~25 ns * Nsamples

  fDigiEvent->Clear();

  TMUV2Event * MUV2Event = static_cast<TMUV2Event*>(tEvent);
  (*(TVEvent*) fDigiEvent) = (*(TVEvent*) MUV2Event);

  Int_t NHits = MUV2Event->GetNHits();
  TClonesArray *Hits = MUV2Event->GetHits();

  for (int i=0; i<4; i++){
    for (int j=0; j<22; j++) fHitsOnChannel[i][j].Clear();
  }

  Double_t FirstHitTime = 0.;
  for (Int_t i=0; i<NHits; i++){

    TMUV2Hit *Hit = static_cast<TMUV2Hit*>( Hits->At(i));
    if (Hit->GetChannelID()<1) continue;
    if (i==0 || Hit->GetTime()<FirstHitTime) FirstHitTime = Hit->GetTime();

    Int_t ChannelID = Hit->GetChannelID();
    Int_t RecoChannelID = 100;

    if (ChannelID<1) continue;

    if ( ChannelID > 50 ) RecoChannelID += 50;
    if (Hit->GetPosition()[(ChannelID > 50 ? 0 : 1)] > 0) RecoChannelID += 100;

    if      ( RecoChannelID < 150 ) RecoChannelID+=(ChannelID+1)/2;
    else if ( RecoChannelID < 200 ) RecoChannelID+=(ChannelID-50)/2;
    else if ( RecoChannelID < 250 ) RecoChannelID+=(ChannelID)/2;
    else                            RecoChannelID+=(ChannelID-49)/2;

    Int_t side = (RecoChannelID-100)/50;
    Int_t ch = (RecoChannelID)%50 - 1;

    fHitsOnChannel[side][ch].Add(Hit);
  }

  for (Int_t iSide=0; iSide<4; iSide++){
    for (Int_t iCh=0; iCh<22; iCh++){

      Int_t NChannelHits = fHitsOnChannel[iSide][iCh].GetEntries();

      if (NChannelHits<1) continue;
      Double_t Parameters[3][NChannelHits];

      Double_t Sigma = fRandom->Gaus(35.42,1.596);
      Double_t TotalEnergy = 0., MeanTime = 0.;
      TVector3 Position0(0.0, 0.0, 0.0);
      Int_t ChannelID = 101 + iCh + iSide*50;
      for (Int_t iHit=0; iHit<NChannelHits; iHit++){

        TMUV2Hit *Hit = static_cast<TMUV2Hit*>( fHitsOnChannel[iSide][iCh].At(iHit));

        Double_t Energy = Hit->GetEnergy();
        Double_t FineTime = NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256.;
        Double_t Time = Hit->GetTime() + FineTime - FirstHitTime;

        TotalEnergy += Energy;
        MeanTime += Energy*Time;
        Position0 += Energy*Hit->GetPosition();
        Energy *= fInvSamplingFraction;

        Double_t Position = (iSide%2) ?
	  Hit->GetPosition().Y() : Hit->GetPosition().X();

        Time += static_cast<MUV2Reconstruction*>(fReco)->GetTPosDelay(ChannelID,Position);

        Double_t Eff_Energy = Energy * static_cast<MUV2Reconstruction*>(fReco)->GetQPosAttenuation(ChannelID,Position) / static_cast<MUV2Reconstruction*>(fReco)->GetEq(iSide,iCh+1);
        Double_t Charge = 1.42 * Eff_Energy * calibration;

        Parameters[0][iHit] = Charge;
        Parameters[1][iHit] = Time;
        Parameters[2][iHit] = Sigma;
      }

      Bool_t Saturated = false, Underflow = false;

      fHit = static_cast<TMUV2Hit*>( fHitsOnChannel[iSide][iCh].At(0));
      fHit -> SetTime(MeanTime/TotalEnergy);
      fHit -> SetPosition(Position0 * (1.0/TotalEnergy));
      fHit -> SetEnergy(TotalEnergy);

      TMUV2Digi* Digi = static_cast<TMUV2Digi*>( fDigiEvent->AddDigi(fHit));
      Digi -> SetChannelID(100 + iSide*50 + iCh + 1);
      Digi -> DecodeChannelID();

      Double_t Samples;
      Double_t Offset = static_cast<MUV2Reconstruction*>(fReco)->GetT0(iSide,iCh+1) + fReco->GetT0Correction(Digi);

      for (Int_t iSample=0; iSample<Nsamples; iSample++) {

        Samples = 1000 + fRandom->Gaus(0.,3.); //Baseline

        Double_t SampleTime = (iSample - Nsamples/2)*ClockPeriod + fRandom->Gaus(0,1) - Offset;

        for (Int_t iHit=0; iHit<NChannelHits; iHit++){

          Double_t ADC_counts = Parameters[0][iHit] * AnalogSignal(SampleTime, Parameters[1][iHit],Parameters[2][iHit]);

          ADC_counts *= 16384./2000.; //Conversion from mV to ADC

          Samples += ADC_counts;
        }

        Samples = static_cast <Int_t> (Samples); //Cast for readout: only int possible

        //Saturation and Underflow threatment
        if (Samples >= 16383){ Samples = 16383; Saturated = true; }
        if (Samples <= 0){ Samples = 0; Underflow = true; }

        Digi->AddSample(Samples);
      }

      UInt_t Flag = 0;
      if (Saturated) Flag += (1<<kCREAMSaturationBit); //Saturation Flag
      if (Underflow) Flag += (1<<kCREAMUnderflowBit); //Underflow Flag
      Digi->SetFlags(Flag);
    }
  }
  return fDigiEvent;
}

//		Parametrisation of Signal through shaper and cream
Double_t MUV2Digitizer::AnalogSignal(Double_t T, Double_t Center, Double_t Sigma){

  if (TMath::Abs(T-Center)/Sigma > 10.) return 0;
  double val = (TMath::Gaus(T,Center,Sigma,1) - 0.05*TMath::Gaus(T,Center+3*Sigma,2.*Sigma,1));
  return val;
}

//      Correction of deposited energy for the Birks effect
Double_t MUV2Digitizer::BirksCorrection(Double_t Energy, Double_t StepLength,
    Double_t BirksConstant) {

  if(StepLength==0) return Energy;

  Double_t dEdX = Energy / StepLength;

  Double_t CorrectedEnergy = (dEdX / (1 + (BirksConstant * dEdX)))
    * StepLength;


  if(CorrectedEnergy!=CorrectedEnergy || CorrectedEnergy < 0) return Energy;

  return CorrectedEnergy;

}
