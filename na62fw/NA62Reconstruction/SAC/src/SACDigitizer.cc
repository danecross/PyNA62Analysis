#include "Riostream.h"
#include "TSACHit.hh"
#include "TSACDigi.hh"
#include "TSACEvent.hh"
#include "TDCEvent.hh"
#include "SACDigitizer.hh"
#include "SACReconstruction.hh"
#include "TSpecialTriggerEvent.hh"
#include "NA62RecoManager.hh"
#include "TMath.h"
#define ELECTRON_CHARGE 1.6025e-19

SACDigitizer::SACDigitizer(NA62VReconstruction* Reco) :
  NA62VDigitizer(Reco, "SAC"),
  fBinWidth(0.25), // in ns: it is the bin width
  fNGammaPerMeV(10000.), // MeV-1
  fImpedence(50.),
  fTau(4.5), // ns
  fEpsilonReach(0.05),
  fEpsilonPhotocatode(0.15),
  fPMTGain(0.5E6),
  fNtau(15),
  fNBinRange(fTRange/fBinWidth),
  fLightSuppressionFactor(10*2.5),

  fThLow(0.005),
  fThHigh(0.010),
  fHyst(1.25E-3), // V
  fHyTimeC(300.), // ns
  fNorm(1.0/(1.0-TMath::Exp(-fBinWidth/fTau))),

  fShareFactor(0.25),
  fShareSigma(0.04),
  fMakeDigiHistos(kFALSE)
{

  fTRange = 2*fNtau*fTau;  // time range of primary photoelectron emission
// Parametrize number of primary photoelectrons as: TF1* constantFun = new TF1("cF","[0]*exp(-0.5*(x-[1])/[2]+[3]*exp(-(x-[1])/[4]))",0.,50.);

  // SAC curve:
  fEmissionPars[0] = 1.7/75.; // to be multiplied by E[MeV] * fEpsilonReach * fEpsilonPhotocatode
  fEmissionPars[1] = 3.42;
  fEmissionPars[2] = 5    ;
  fEmissionPars[3] = -0.35;
  fEmissionPars[4] = 1.627;

  fDigiEvent = new TDCEvent(TSACDigi::Class());

  InitHisto();
}

void SACDigitizer::InitHisto(){
  if (fMakeDigiHistos) {
    fHRiseTime            = new TH2F("SACSignalRiseTime","MC Energy vs total RiseTime",100,0.,50.,100,0.,10000.);
    fHMCEnergyCorrelation = new TH2F("SACMCEnergyCorrelation","MC Energy vs signal maximum",100,0.,10.,100,0.,10000.);
    fHMCEnergyVsTotLow    = new TH2F("SACMCEnergyVsTotLow","MC Energy vs low-threshold ToT",400,0.,200.,100,0.,10000.);
    fHMCEnergyVsTotHigh   = new TH2F("SACMCEnergyVsTotHigh","MC Energy vs high-threshold ToT",400,0.,200.,100,0.,10000.);
    fHNTotLow  = new TH2F("SACnTOTLow","Number of TOTs in the event",20,0,20,4,-0.5,3.5);
    fHNTotHigh = new TH2F("SACnTOTHigh","Number of TOTs in the event",20,0,20,4,-0.5,3.5);
  }
}

void SACDigitizer::CloseHisto(){
  if (fMakeDigiHistos) {
    fHRiseTime            ->Write();
    fHMCEnergyCorrelation ->Write();
    fHMCEnergyVsTotLow    ->Write();
    fHMCEnergyVsTotHigh   ->Write();
    fHNTotLow->Write();
    fHNTotHigh->Write();
    delete fHRiseTime           ;
    delete fHMCEnergyCorrelation;
    delete fHMCEnergyVsTotLow   ;
    delete fHMCEnergyVsTotHigh  ;
    delete fHNTotLow;
    delete fHNTotHigh;
  }
  //  delete fHistoFile;
}

void SACDigitizer::FillHisto() {

  if (fMakeDigiHistos==0) return;

  for (Int_t ich=0; ich<4; ich++) {
    if (fChHit[ich] == 0) continue;

    fHRiseTime->Fill(fTSignalMax[ich],fTotEnergy[ich]);
    fHMCEnergyCorrelation->Fill(fSignalMax[ich],fTotEnergy[ich]);

    Int_t nTots = 0;
    for (Int_t j=1; j<(Int_t) fEdgesLow[ich].size(); j++){
      if (fEdgeTypesLow[ich].at(j) == -1 && fEdgeTypesLow[ich].at(j-1) == 1){
	nTots++;
	fHMCEnergyVsTotLow->Fill(fEdgesLow[ich].at(j)-fEdgesLow[ich].at(j-1),fTotEnergy[ich]);
      }
    }
    fHNTotLow->Fill(nTots,ich);

    nTots = 0;
    for (Int_t j=1; j<(Int_t) fEdgesHigh[ich].size(); j++){
      if (fEdgeTypesHigh[ich].at(j) == -1 && fEdgeTypesHigh[ich].at(j-1) == 1){
	nTots++;
	fHMCEnergyVsTotHigh->Fill(fEdgesHigh[ich].at(j)-fEdgesHigh[ich].at(j-1),fTotEnergy[ich]);
      }
    }
    fHNTotHigh->Fill(nTots,ich);
  }

  fHRiseTime            ->Write();
  fHMCEnergyCorrelation ->Write();
  fHMCEnergyVsTotLow ->Write()  ;
  fHMCEnergyVsTotHigh->Write()  ;
  fHNTotLow->Write();
  fHNTotHigh->Write();
}

SACDigitizer::~SACDigitizer(){
  CloseHisto();
}

void SACDigitizer::StartOfBurst() {
  NA62VDigitizer::StartOfBurst();
}

void SACDigitizer::EndOfBurst() {
  NA62VDigitizer::EndOfBurst();
}

TDetectorVEvent * SACDigitizer::ProcessEvent(TDetectorVEvent * tEvent){

  if (tEvent->GetHits()->GetClass()->InheritsFrom("TVDigi") ||
      tEvent->IsA() == TSpecialTriggerEvent::Class())
    return tEvent;

  TSACEvent * SACEvent = static_cast<TSACEvent*>(tEvent);
  Int_t NHits = SACEvent->GetNHits();

  fDigiEvent->Clear();
  fDigiEvent->TVEvent::operator=(*static_cast<TVEvent*>(SACEvent));

  if (!NHits) return fDigiEvent;

  TClonesArray& hitArray = *(SACEvent->GetHits());

  // Find minimum and maximum hit times

  fTOffset=0;
  Double_t tMax=0;
  for (Int_t i=0; i<NHits; i++) {
    TSACHit* hit = static_cast<TSACHit*>( hitArray[i]);
    if (i==0) {
      fTOffset = hit->GetTime();
      tMax = fTOffset;
    }
    else {
      if (hit->GetTime() > tMax) tMax = hit->GetTime();
      if (hit->GetTime() < fTOffset) fTOffset = hit->GetTime();
    }
  }
  if (tMax > fTOffset+10000) { // Protection against unphysical times
    //std::cout << "[SACDigitizer] Warning, tMax too high: " << tMax << std::endl;
    tMax = fTOffset+10000;
  }
  fTOffset = fTOffset - fTau; // shift minimum time to avoid signal starting above threshold
  tMax = tMax + fTRange; // add time to include the emission of photoelectrons

  // Prepare arrays to store the photoelectron distribution
  Int_t nTimeBins = (tMax-fTOffset)/fBinWidth + 1;
  Int_t** primPE = new Int_t*[4];
  for (Int_t i=0; i<4; i++) {
    primPE[i] = new Int_t[nTimeBins];
    for (Int_t j=0; j< nTimeBins; j++) primPE[i][j] = 0.;
    fChHit[i] = 0;
    fTotEnergy[i] = 0;
  }

  for (Int_t i=0; i<NHits; i++) {
    TSACHit* hit = static_cast<TSACHit*>( hitArray[i]);
    Int_t iBin = (hit->GetTime()-fTOffset)/fBinWidth;
    if (iBin<0 || iBin>= nTimeBins) {
      //std::cout << "[SACDigitizer] Warning, iBin<0 or iBin >= nTimeBins" << std::endl;
      continue;
    }

    // Share the energy among the 4 SAC channels
    for (Int_t ich = 0; ich < 4; ich++) {
      Double_t energy = hit->GetEnergy()*fRandom->Gaus(fShareFactor,fShareSigma);
      if (energy < 0) continue;
      if (fChHit[ich] == 0) fChHit[ich] = i+1;
      fTotEnergy[ich] += energy;

      for (Int_t jBin = 0; jBin < fNBinRange; jBin++) {
	Double_t timeX = (jBin+0.5)*fBinWidth - fEmissionPars[1];
	Double_t npeAv = energy / fLightSuppressionFactor * fNGammaPerMeV * fEpsilonReach * fEpsilonPhotocatode * fEmissionPars[0] * exp(-0.5*timeX/fEmissionPars[2] + fEmissionPars[3]*exp(-timeX/fEmissionPars[4]));
//	if (npeAv < 10 && npeAv > 0.01) primPE[ich][iBin+jBin] += fRandom->Poisson(npeAv);
//	else if (npeAv >= 10) primPE[ich][iBin+jBin] += npeAv + TMath::Sqrt(npeAv)*fRandom->Gaus(0,1);
	primPE[ich][iBin+jBin] += npeAv;
      }
    }
  }

  Double_t ToV = ( fImpedence * ELECTRON_CHARGE ) / ( fBinWidth * 1.e-9 ) ; // fBinWidth is ns
  Int_t IsAleading = 1;
  Int_t IsAtrailing = -1;
  Double_t dumpFactor = TMath::Exp(-fBinWidth/fTau);
  Int_t signalTimeBins = nTimeBins + fNtau*fTau/fBinWidth;  // extend the time range to include signal decay

  // Prepare arrays to store signal distribution
  Double_t** signalGenerated = new Double_t*[4];
  for (Int_t ich=0; ich<4; ich++) {
    if (fChHit[ich] == 0) continue;
    signalGenerated[ich] = new Double_t[signalTimeBins];
    for (Int_t j = 0; j<signalTimeBins; j++) signalGenerated[ich][j]=0;
  }

  // Zero the edge arrays
  for (Int_t ich=0; ich<4; ich++) {
    fEdgesLow[ich].clear()    ;
    fEdgeTypesLow[ich].clear();
    fEdgesHigh[ich].clear()   ;
    fEdgeTypesHigh[ich].clear();
  }

  // Generate signal and evaluate crossing times

  for (Int_t ich = 0; ich<4; ich++) {
    if (fChHit[ich] == 0) continue;

    Int_t aboveThLo = 0;
    Int_t aboveThHi = 0;
    Double_t thresholdLo = fThLow;  // default value
    Double_t thresholdHi = fThHigh; // default value
    Double_t timeLo; // time passed above Lo threshold
    Double_t timeHi; // time passed above Hi threshold

    signalGenerated[ich][0] = primPE[ich][0] * fPMTGain * ToV / fNorm;
    fSignalMax[ich] = signalGenerated[ich][0];

    if (signalGenerated[ich][0] > thresholdLo) {
      std::cout << "[SACDigitizer] Warning: Signal starts above threshold Lo: V= " << signalGenerated[ich][0] << " thr = " << thresholdLo << std::endl;
      aboveThLo = 1;
      thresholdLo = fThLow - fHyst;
      timeLo = 0; // initialize
    }
    if (signalGenerated[ich][0] > thresholdHi) {
      std::cout << "[SACDigitizer] Warning: Signal starts above threshold Hi: V= " << signalGenerated[ich][0] << " thr = " << thresholdHi << std::endl;
      aboveThHi = 1;
      thresholdHi = fThHigh - fHyst;
      timeHi = 0; // initialize
    }

    for (Int_t iBin=1; iBin<signalTimeBins; iBin++) {
      if (iBin < nTimeBins) signalGenerated[ich][iBin] = ( primPE[ich][iBin] * fPMTGain * ToV / fNorm + signalGenerated[ich][iBin-1] * dumpFactor );
      else signalGenerated[ich][iBin] = signalGenerated[ich][iBin-1] * dumpFactor;

      if (signalGenerated[ich][iBin] > fSignalMax[ich]) {
	fSignalMax[ich] = signalGenerated[ich][iBin];
	fTSignalMax[ich] = iBin*fBinWidth;
      }

      ////////////  Low Th

      if (aboveThLo) {
	timeLo += fBinWidth;
	thresholdLo = fThLow - fHyst*exp(-timeLo/fHyTimeC); // update threshold taking into account feedback circuit time constant

	if (signalGenerated[ich][iBin] < thresholdLo) {
	  aboveThLo = 0;
	  fEdgesLow[ich].push_back( fTOffset + iBin*fBinWidth );
	  fEdgeTypesLow[ich].push_back( IsAtrailing );
	  thresholdLo = fThLow;
	}
      }
      else {
	if (signalGenerated[ich][iBin] > thresholdLo) {
	  aboveThLo = 1;
	  fEdgesLow[ich].push_back( fTOffset + iBin*fBinWidth );
	  fEdgeTypesLow[ich].push_back( IsAleading );
	  thresholdLo = fThLow - fHyst;
	  timeLo = 0;
	}
      }

      ////////////  High Th

      if (aboveThHi) {
	timeHi += fBinWidth;
	thresholdHi = fThHigh - fHyst*exp(-timeHi/fHyTimeC); // update threshold taking into account feedback circuit time constant

	if (signalGenerated[ich][iBin] < thresholdHi) {
	  aboveThHi = 0;
	  fEdgesHigh[ich].push_back( fTOffset + iBin*fBinWidth );
	  fEdgeTypesHigh[ich].push_back( IsAtrailing );

	  thresholdHi = fThHigh;
	}
      }
      else {
	if (signalGenerated[ich][iBin] > thresholdHi) {
	  aboveThHi = 1;
	  fEdgesHigh[ich].push_back( fTOffset + iBin*fBinWidth );
	  fEdgeTypesHigh[ich].push_back( IsAleading );

	  thresholdHi = fThHigh - fHyst;
	  timeHi = 0;
	}
      }
    }
  }

  // Write digi output

  for (Int_t ich=0; ich<4; ich++) {
    if (fChHit[ich] == 0) continue;

    Double_t FineTime = NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256.;

    TSACHit* hit = static_cast<TSACHit*>( hitArray[fChHit[ich]-1]);

    int openDigi = 0;
    TSACDigi* DigiOld = NULL;
    for (UInt_t j=0; j<fEdgesLow[ich].size(); j++) {

      if (fEdgeTypesLow[ich].at(j) == IsAleading) { // Create a new Digi starting from leading edge

	TSACDigi *Digi = static_cast<TSACDigi*>(fDigiEvent->AddDigi());
	Digi->SetMCTrackID( hit->GetMCTrackID() );
	Digi->SetMCHit( hit );
	Digi->SetChannelID(ich);
	Digi->DecodeChannelID();
	Digi->SetLeadingEdge(fEdgesLow[ich].at(j)+FineTime-static_cast<SACReconstruction*>(fReco)->GetStationMCToF(0)+fReco->GetT0Correction(ich,0));
	Digi->SetDetectedEdge(1);
	DigiOld = Digi;
	openDigi = 1;
      }
      else {
	if (openDigi == 0) {
	  TSACDigi *Digi = static_cast<TSACDigi*>(fDigiEvent->AddDigi()); // Create a new Digi starting from trailing edge
	  Digi->SetMCTrackID(hit->GetMCTrackID());
	  Digi->SetMCHit(hit);
	  Digi->SetChannelID(ich);
	  Digi->DecodeChannelID();
	  Digi->SetLeadingEdge(fEdgesLow[ich].at(j)+FineTime-static_cast<SACReconstruction*>(fReco)->GetStationMCToF(0)+fReco->GetT0Correction(ich,0));
	  Digi->SetDetectedEdge(2);
	}
	else { // Insert trailing edge and close Digi
	  DigiOld->SetTrailingEdge(fEdgesLow[ich].at(j)+FineTime-static_cast<SACReconstruction*>(fReco)->GetStationMCToF(0)+fReco->GetT0Correction(ich,0));
	  DigiOld->SetDetectedEdge(3);
	  openDigi = 0;
	}
      }
    }

    openDigi = 0;
    DigiOld = NULL;

    for (UInt_t j=0; j<fEdgesHigh[ich].size(); j++) {

      if (fEdgeTypesHigh[ich].at(j) == IsAleading) {
	TSACDigi *Digi = static_cast<TSACDigi*>(fDigiEvent->AddDigi());
	Digi->SetMCTrackID( hit->GetMCTrackID() );
	Digi->SetMCHit( hit );
	Digi->SetChannelID(1000+ich);
	Digi->DecodeChannelID();
	Digi->SetLeadingEdge(fEdgesHigh[ich].at(j)+FineTime-static_cast<SACReconstruction*>(fReco)->GetStationMCToF(0)+fReco->GetT0Correction(ich,0));
	Digi->SetDetectedEdge(1);
	DigiOld = Digi;
	openDigi = 1;
      }
      else {
	if (openDigi == 0) {
	  TSACDigi *Digi = static_cast<TSACDigi*>(fDigiEvent->AddDigi());
	  Digi->SetMCTrackID( hit->GetMCTrackID() );
	  Digi->SetMCHit( hit );
	  Digi->SetChannelID(1000+ich);
	  Digi->DecodeChannelID();
	  Digi->SetLeadingEdge(fEdgesHigh[ich].at(j)+FineTime-static_cast<SACReconstruction*>(fReco)->GetStationMCToF(0)+fReco->GetT0Correction(ich,0));
	  Digi->SetDetectedEdge(2);
	}
	else {
	  DigiOld->SetTrailingEdge(fEdgesHigh[ich].at(j)+FineTime-static_cast<SACReconstruction*>(fReco)->GetStationMCToF(0)+fReco->GetT0Correction(ich,0));
	  DigiOld->SetDetectedEdge(3);
	  openDigi = 0;
	}
      }
    }
  }

  FillHisto();

  for (Int_t ich=0; ich<4; ich++) {
    if (fChHit[ich] == 0) continue;
    delete[] signalGenerated[ich];
  }
  delete[] signalGenerated;
  for (Int_t i=0; i<4; i++) {
    delete[] primPE[i];
  }
  delete[] primPE;
  return fDigiEvent;
}
