#include "Riostream.h"
#include "TMath.h"

#include <fstream>
#include <string>

#include "TLKrHit.hh"
#include "LKrDigitizer.hh"
#include "LKrReconstruction.hh"
#include "FADCEvent.hh"
#include "FADCVHit.hh"
#include "TLKrDigi.hh"
#include "LKrChannelID.hh"
#include "LKrGeometry.hh"
#include "TRecoLKrEvent.hh"
#include "TRecoLKrHit.hh"
#include "TLKrEvent.hh"
#include "TRecoLKrCandidate.hh"
#include "TLKrMicroCellHit.hh"
#include "LKrParameters.hh"
#include "LKrCommon.hh"
#include "TSpecialTriggerEvent.hh"
#include "NA62RecoManager.hh"
#include "NA62ConditionsService.hh"

#include "TFile.h"
#include "TVector3.h"

#include "CLHEP/Units/SystemOfUnits.h"
using namespace CLHEP;

#define dec000 dec000_
#define type_of_call
extern "C" {void type_of_call dec000(UInt_t&, UInt_t&, UInt_t*, UInt_t*, UInt_t&);}
//#define SLM 1
LKrDigitizer::LKrDigitizer(NA62VReconstruction* Reco) : NA62VDigitizer(Reco, "LKr") {
  fDigiEvent = new FADCEvent(TLKrDigi::Class());

  // Set the noise parameters
  fSigmaCorrNoise[0] = 1;
  fSigmaCorrNoise[1] = 0.1;
  fSigmaCorrNoise[2] = 0.04;
  fSigmaCorrNoise[3] = 0.017;
  fSigmaUncorrNoise[0] = 3;
  fSigmaUncorrNoise[1] = 0.3;
  fSigmaUncorrNoise[2] = 0.12;
  fSigmaUncorrNoise[3] = 0.05;

  // Slopes for gain range definition
  fSlopes[0] = 0.0033;
  fSlopes[1] = 0.0096;
  fSlopes[2] = 0.0250;
  fSlopes[3] = 0.0650;

  // Read the text file with the pulse shape bits/constants
  Int_t iDigitizationBin=0;
  if(NA62ConditionsService::GetInstance()->Open("LKr-digishape.dat")==kSuccess){
    while(NA62ConditionsService::GetInstance()->Get("LKr-digishape.dat") >> fReadShape[iDigitizationBin]) iDigitizationBin++;
    NA62ConditionsService::GetInstance()->Close("LKr-digishape.dat");
  }
  else {
    std::cerr << "[LKrDigitizer] Error: LKr-digishape.dat not found!" << std::endl;
    _exit(kConditionFileNotFound);
  }

  // LKr Parameters
  fPar = LKrParameters::GetInstance();

  // Pointer to the ADC common
  fAdcCommon = LKrCommon::GetInstance();
}


LKrDigitizer::~LKrDigitizer(){}

TDetectorVEvent * LKrDigitizer::ProcessEvent(TDetectorVEvent * tEvent){
  if(tEvent->GetHits()->GetClass()->InheritsFrom("TVDigi") || tEvent->IsA() == TSpecialTriggerEvent::Class()) return tEvent;
  if(tEvent->IsA() != TLKrEvent::Class()) NA62VReconstruction::Exception(Form("%s is not a %s", tEvent->IsA()->GetName(), TLKrEvent::Class()->GetName()));

  // Pulse shape
  const UInt_t   NSamples = 8;
  const Int_t    MaxSample = 5; // Max in sample 5
  const Int_t    PeakPulseOffset = 218; // Max of the full shape is at 218 ns
  const Double_t MaxShapePulse = 0.714800; // maximum value of the typical pulse shape
  const Double_t MinShapePulse = -0.629300;
  const Double_t EnergyCurrentRatio = 1./2.55; // nominal one: 2.5 muA / GeV
  const Double_t MeanCorrNoise = 0.;
  const Double_t MeanUncorrNoise = 0.;
  const Double_t DigitizationBinWidth = ClockPeriod/25.; // ns

  Double_t FineTime = NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256.;
  if(!fReco->GetStationT0(0)){ // no T0s provided
    static_cast<LKrReconstruction*>(fReco)->SetStationT0(0,MaxSample*ClockPeriod); // Force Maximum to be where it should be
    fPar->DefineDataType(false); // MC
    fPar->Fill(static_cast<LKrReconstruction*>(fReco));
  }

  // LKr Event
  TLKrEvent * LKrEvent = static_cast<TLKrEvent*>(tEvent);
  UInt_t NLKrHits = LKrEvent->GetNHits();
  fDigiEvent->Clear("C");
  (*(TVEvent*)fDigiEvent)=(*(TVEvent*)LKrEvent);
  static_cast<FADCEvent*>(fDigiEvent)->SetFADCID(10);
  static_cast<FADCEvent*>(fDigiEvent)->SetNSamples(NSamples);
  // Loop over LKr Hits to sort them in a vector for each channel
  std::vector<std::vector<std::vector<Int_t>>> LKrHitsPerChannel(128);
  for(Int_t ix = 0; ix<128; ix++){
    LKrHitsPerChannel[ix].resize(128);
  }
  for(UInt_t iLKrHit = 0; iLKrHit < NLKrHits; iLKrHit++){
    TLKrHit * LKrHit = static_cast<TLKrHit*>(LKrEvent->GetHit(iLKrHit));
    LKrHit->DecodeChannelID();
    LKrHitsPerChannel[LKrHit->GetXCellID()][LKrHit->GetYCellID()].push_back(iLKrHit);
  }
  // Loop over the channels
  for(UInt_t ix = 0; ix< 128; ix++){
    for(UInt_t iy = 0; iy< 128; iy++){
      // ADC counts computation
      Int_t igain = 0;
      Int_t flag = 0;
#ifdef SLM
      UInt_t vgain[8];
      UInt_t adcSamples[8];
#endif
      Double_t eightSamples[8];
      Double_t currentSample[8];
      Int_t maxgain = 0;
      for(UInt_t iSample = 0; iSample < NSamples; iSample++) {
        eightSamples[iSample] = 0;
        currentSample[iSample] = 0;
      }
      // Loop over the MC hits inside every channel
      if(!LKrHitsPerChannel[ix][iy].size()) continue;
      for(UInt_t iLKrHit = 0; iLKrHit < LKrHitsPerChannel[ix][iy].size(); iLKrHit++){
        TLKrHit * LKrHit = static_cast<TLKrHit*>(LKrEvent->GetHit(LKrHitsPerChannel[ix][iy].at(iLKrHit)));
        LKrHit->DecodeChannelID();
        Double_t current = LKrHit->GetCurrent();
#ifdef SLM
        Double_t edep = LKrHit->GetEnergy();
#endif
        Double_t HitTime = LKrHit->GetTime()+FineTime-(static_cast<LKrReconstruction*>(fReco))->GetStationMCToF(LKrHit->GetStationID())
          + fReco->GetT0Correction(LKrHit->GetChannelID(),LKrHit->GetStationID()) + fPar->GetCellT0(ix,iy);
        // Additional hit time smearing
        HitTime+=fRandom->Gaus(0.,fPar->GetMCHitTimeSmearing());

#ifdef SLM
        // Choose the gain range on the basis of the total deposited energy
        for (Int_t jsw=0; jsw<3; jsw++) {
          Double_t pedestal = jsw==0 ? fPar->GetPedRef(ix,iy) : fPar->GetCalOffset(ix,iy,jsw);
          fESwitch[jsw] = (950.-pedestal)*fSlopes[jsw]*1000;
        }
        // Find the sample with the maximum and the maximum current
        maxgain = chooseGain(edep);
        Double_t maxCurrent = 0;
        Double_t maxSample = -1;
        if (maxgain>0){
          for(UInt_t iSample=0; iSample<NSamples; iSample++){
            Double_t PulseTime = iSample*ClockPeriod-HitTime;
            Int_t NewIndex = PeakPulseOffset+PulseTime/ns/DigitizationBinWidth;
            Double_t PulseHeight = fReadShape[0]; // for NewIndex<0
            if(NewIndex>=2999)  PulseHeight = fReadShape[2999];
            else if(NewIndex>=0) {
              // Interpolate pulse shape (bins are 1 ns wide!)
              Double_t dx=PulseTime/ns/DigitizationBinWidth-(Int_t)(PulseTime/ns/DigitizationBinWidth); // defined from 0 to 1
              PulseHeight = fReadShape[NewIndex] + dx*(fReadShape[NewIndex+1]-fReadShape[NewIndex]);
            }
            currentSample[iSample] += (PulseHeight-MinShapePulse)*current/(MaxShapePulse-MinShapePulse);
            if (currentSample[iSample]>maxCurrent){
              maxCurrent = currentSample[iSample];
              maxSample = iSample;
            }
          }
        } else maxSample = 0;
#endif
        for (UInt_t iSample=0; iSample<NSamples; iSample++){
          Double_t PulseTime = iSample*ClockPeriod-HitTime;
          Int_t NewIndex = PeakPulseOffset+PulseTime/ns/DigitizationBinWidth;
          Double_t PulseHeight = fReadShape[0]; // for NewIndex<0
          if(NewIndex>=2999)  PulseHeight = fReadShape[2999];
          else if(NewIndex>=0) {
            // Interpolate pulse shape (bins are 1 ns wide!)
            Double_t dx=PulseTime/ns/DigitizationBinWidth-(Int_t)(PulseTime/ns/DigitizationBinWidth); // defined from 0 to 1
            PulseHeight = fReadShape[NewIndex] + dx*(fReadShape[NewIndex+1]-fReadShape[NewIndex]);
          }
#ifdef SLM
          if (iSample>=maxSample-2) igain = maxgain;            // Set the gain of the samples around the maximum
#else
          igain=maxgain;
#endif
          Double_t calConst = fPar->GetCalSteig(ix,iy,igain);
          Double_t fadc=0.;
          if (calConst>0.) fadc = EnergyCurrentRatio/(calConst*1000);
          //else             fadc = EnergyCurrentRatio/(.004*1000); // Fake calibration constant
          else fadc = 0.;
          currentSample[iSample] += (PulseHeight-MinShapePulse)*(current*fadc)/(MaxShapePulse-MinShapePulse); // Compute the ADC counts
        }
      }//------ end of loop on the hits
      Int_t imaxCount = 0;
      Int_t maxCount = 0;
      Int_t minCount = 99999;
      for (UInt_t iSample=0; iSample<NSamples; iSample++){
        Double_t noiseUncorr = fRandom->Gaus(MeanUncorrNoise,fSigmaUncorrNoise[igain]);
        Double_t noiseCorr = fRandom->Gaus(MeanCorrNoise,fSigmaCorrNoise[igain]);
        Double_t ped = igain==0 ? fPar->GetPedRef(ix,iy) : fPar->GetCalOffset(ix,iy,igain); // Compute the pedestals cell by cell
        Double_t count = currentSample[iSample]+noiseUncorr+noiseCorr+ped; // Add noise and pedestals to the ADC counts
        // range is chosen automatically for igain=0, but then the saturation is at 16384
        Int_t icount = (Int_t)(count+0.5);
#ifdef SLM
        if (icount>=1023) {icount = 1023; flag = (1<<kCREAMSaturationBit);}// Saturation
#else
        if (icount>=16383) {icount = 16383; flag = (1<<kCREAMSaturationBit);}// Saturation
#endif
#ifdef SLM
        vgain[iSample] = (UInt_t)igain;
        eightSamples[iSample] = icount+10000*igain;
        adcSamples[iSample] = icount;
#else
        eightSamples[iSample] = icount;
#endif
        if (icount>maxCount) {maxCount = icount; imaxCount = iSample;} // Look for the sample with the maximum counts
        if (icount<minCount) minCount = icount; // Look for the sample with the minimum counts
      }
      // Choose the Zero Suppression algorithm
      if (fPar->GetZSAlgorithm()==1){
        if (maxgain==0 && (maxCount-minCount)<20) continue; // Applied during the technical run of 2012
      }
      if (!((fAdcCommon->GetKAF()->KAFIHEA[iy][ix])&0x4)) continue; //  Dead cells
      // Save digitized hits
      LKrChannelID channel;
      channel.SetXCellID(ix);
      channel.SetYCellID(iy);
      TLKrDigi * LKrDigi = static_cast<TLKrDigi*>(fDigiEvent->AddDigi(channel.EncodeChannelID()));
      LKrDigi->DecodeChannelID();
      for (UInt_t iSample=0; iSample<NSamples; iSample++) LKrDigi->AddSample(eightSamples[iSample]);
#ifdef SLM
      // Adjust the flags for the digital filter
      dec000(ix,iy,adcSamples,vgain,NSamples);
      LKrDigi->SetADCPeakEnergy((Double_t)fAdcCommon->GetADC()->PEAKENE);
      LKrDigi->SetADCPeakTime((Double_t)fAdcCommon->GetADC()->PEAKTIME-1); // Warning! peaktime = maxsample -> -1 from fortran to c++: samples in fortran 1:8, in c 0:7
      LKrDigi->SetPeakTime((Double_t)(fAdcCommon->GetADC()->PEAKTIME-1)*ClockPeriod);
      LKrDigi->SetQuality(fAdcCommon->GetADC()->IQUALITY);
      LKrDigi->SetFlags(fAdcCommon->GetADC()->IFLAG);
#else
      LKrDigi->SetADCPeakEnergy((Double_t)maxCount);
      LKrDigi->SetADCPeakTime((Double_t)imaxCount);
      LKrDigi->SetPeakTime((Double_t)imaxCount*ClockPeriod);
      LKrDigi->SetQuality(1);
      LKrDigi->SetFlags(flag);
#endif
    }
  }
  return fDigiEvent;
}

#ifdef SLM
Int_t LKrDigitizer::chooseGain(Double_t e){
  if (e<=fESwitch[0]) return 0;
  if (e<=fESwitch[1]) return 1;
  if (e<=fESwitch[2]) return 2;
  return 3;
}
#endif
