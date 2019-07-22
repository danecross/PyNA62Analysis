/// \class MUV3Digitizer
/// \Brief
/// MUV3 digitizer: converts MUV3 MC hits into Digis
/// \EndBrief
/// \Detailed
/// Conversion of MUV3 MC hits into Digis. The simple digitizer (default) generates a Digi
/// if the energy desposit in a counter is above threshold, of if the energy is deposited
/// in a PMT window. The detailed digitizer (not complete) simulates the light collection
/// and the CFD (constant fraction discriminator) operation. The digitizer is selected, and
/// its parameters are defined, in the config/MUV3.conf file.
/// \author Riccardo Fantechi, Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

///////////////////////////////////////////////////////////////////////////////
//  MUV3 Digitization (the detailed digitizer) R. Fantechi Version 1 27.09.2013
//  Updated 3.8.2014
//  Correct problem in Collection efficiency calculation
//  Debug flags now in the config file
//  To be done:
//
//  Geometry
//  T0
//  Handle collection on central cells
//  Events out of window
//  Multiple leadings

#include "MUV3Digitizer.hh"
#include "NA62RecoManager.hh"
#include "NA62ConditionsService.hh"

#include "CLHEP/Units/SystemOfUnits.h"
using namespace CLHEP;

MUV3Digitizer::MUV3Digitizer(NA62VReconstruction* Reco) : NA62VDigitizer(Reco, "MUV3") {

  fDigiEvent    = new TDCEvent(TMUV3Digi::Class());
  fMode         = 1;
  fHistoFlag    = 0;
  fDebugFlag    = 0;
  fCFDDebugFlag = 0;
  fGeo = MUV3Geometry::GetInstance();

  fEnergyDepositThreshold = fChannelTimeResolution = fChannelMergeThreshold = -999;
  fLightCollectionMatrixAFileName = "";
  fLightCollectionMatrixBFileName = "";
  fSignalShapeFileName = "";

  // Read configuration file
  ParseConfFile(static_cast<MUV3Reconstruction*>(fReco)->GetConfigFileName());
  // PrintParameters();

  // Initialize the detailed digitizer
  if (fMode==2) {
    fNBinCFDDelay      = (Int_t) (fCFDDelay/fDeltaTime);
    fNBins             = (Int_t) (fTimeInterval/fDeltaTime);
    fNBinCFDPulseWidth = (Int_t) (fCFDPulseWidth/fDeltaTime);
    fAnalogSignal      = new Double_t[fNBins];
    if (fDebugFlag) {
      std::cout << "CFD delay in bins:       " << fNBinCFDDelay << std::endl;
      std::cout << "CFD pulse Width in bins: " << fNBinCFDPulseWidth << std::endl;
    }

    ReadLightCollectionMatrices(fLightCollectionMatrixAFileName, fLightCollectionMatrixBFileName);
    ReadReferenceSignalShape(fSignalShapeFileName);

    for (Int_t iPMT=0; iPMT<2; iPMT++) {
      for (Int_t iCell=0; iCell<152; iCell++) {
	fTransitTime[iPMT][iCell] = fRandom->Gaus(fTransitTimeMean, fTransitTimeSpread);
      }
    }
  }
}

void MUV3Digitizer::ParseConfFile (TString ConfFileName) {

  std::ifstream confFile(ConfFileName.Data());
  if (!confFile.is_open()) {
    perror(ConfFileName); exit(kWrongConfiguration);
  }

  TString Line;
  while (Line.ReadLine(confFile)) {
    if (Line.BeginsWith("#")) continue;
    else if (Line.BeginsWith("DigitizerMode")) {
      fMode = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("EnergyDepositThreshold")) {
      fEnergyDepositThreshold = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof() * MeV;
      continue;
    }
    else if (Line.BeginsWith("ChannelTimeResolution")) {
      fChannelTimeResolution = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof() * ns;
      continue;
    }
    else if (Line.BeginsWith("ChannelMergeThreshold")) {
      fChannelMergeThreshold = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof() * ns;
      continue;
    }
    else if (Line.BeginsWith("LightCollectionMatrixAFileName")) {
      TObjArray* l = Line.Tokenize(" ");
      fLightCollectionMatrixAFileName = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("LightCollectionMatrixBFileName")) {
      TObjArray* l = Line.Tokenize(" ");
      fLightCollectionMatrixBFileName = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("SignalShapeFileName")) {
      TObjArray* l = Line.Tokenize(" ");
      fSignalShapeFileName = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("PMTransitTimeMean")) {
      fTransitTimeMean = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof() * ns;
      continue;
    }
    else if (Line.BeginsWith("PMTransitTimeSpread")) {
      fTransitTimeSpread = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof() * ns;
      continue;
    }
    else if (Line.BeginsWith("QuantumEfficiency")) {
      fQuantumEff = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof();
      continue;
    }
    else if (Line.BeginsWith("CableDelay")) {
      fCableDelay = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof() * ns;
      continue;
    }
    else if (Line.BeginsWith("CFDDelay")) {
      fCFDDelay = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof() * ns;
      continue;
    }
    else if (Line.BeginsWith("CFDAttenuation")) {
      fCFDAttenuation = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof();
      continue;
    }
    else if (Line.BeginsWith("CFDThreshold")) {
      fCFDThreshold = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof();
      continue;
    }
    else if (Line.BeginsWith("CFDPulseWidth")) {
      fCFDPulseWidth = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof() * ns;
      continue;
    }
    else if (Line.BeginsWith("TimeInterval")) {
      fTimeInterval = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof() * ns;
      continue;
    }
    else if (Line.BeginsWith("DeltaTime")) {
      fDeltaTime = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof() * ns;
      continue;
    }
    else if (Line.BeginsWith("PhotonsPerMeV")) {
      fPhotonsPerMeV = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof();
      continue;
    }
    else if (Line.BeginsWith("DigiHistogramFlag")) {
      fHistoFlag = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("DigiDebugFlag")) {
      fDebugFlag = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("DigiCFDDebugFlag")) {
      fCFDDebugFlag = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atoi();
      continue;
    }
  }
  confFile.close();

  // Sanity checks (parameters of the simple digitizer)
  if (fEnergyDepositThreshold<0) {
    std::cout << "[MUV3Digitizer] Error: invalid threshold specified"<< std::endl;
    exit(kWrongConfiguration);
  }
  if (fChannelTimeResolution<0) {
    std::cout << "[MUV3Digitizer] Error: invalid channel time resolution specified"<< std::endl;
    exit(kWrongConfiguration);
  }
  if (fChannelMergeThreshold<0) {
    std::cout << "[MUV3Digitizer] Error: invalid hit merge threshold specified"<< std::endl;
    exit(kWrongConfiguration);
  }
}

/////////////////////////////////////////////////////////
// Read the reference signal shape: 502 bins, 200 ps step

void MUV3Digitizer::ReadReferenceSignalShape(TString FileName) {
  NA62ConditionsService::GetInstance()->Open(FileName);
  Double_t dummy;
  fNBinsShape = 0;
  while (NA62ConditionsService::GetInstance()->Get(FileName) >> dummy >> fShape[fNBinsShape]) fNBinsShape++;
  NA62ConditionsService::GetInstance()->Close(FileName);

  if (fHistoFlag) {
    for (int i=0; i<fNBinsShape; i++) {
      static_cast<MUV3Reconstruction*>(fReco)->GetHReferenceShape()->SetBinContent(i+1, fShape[i]);
    }
  }
}

////////////////////////////////////////////////////////////////////////////
// Read the light collection matrices from Italo.
// They contain light collection efficiencies as a function of impact point.
// A matrix for each PMT consists of 22*22=484 efficiency values.

void MUV3Digitizer::ReadLightCollectionMatrices(TString FileName1, TString FileName2) {
  Double_t conv = 4.5e7;

  NA62ConditionsService::GetInstance()->Open(FileName1);
  Double_t *m = &fCollectionMatrix[0][0][0];
  while (NA62ConditionsService::GetInstance()->Get(FileName1) >> *m) {*m /= conv; m++;}
  NA62ConditionsService::GetInstance()->Close(FileName1);

  NA62ConditionsService::GetInstance()->Open(FileName2);
  m = &fCollectionMatrix[1][0][0];
  while (NA62ConditionsService::GetInstance()->Get(FileName2) >> *m) {*m /= conv; m++;}
  NA62ConditionsService::GetInstance()->Close(FileName2);

  if (fDebugFlag) {
    for (int i=0; i<22; i++) {
      for (int j=0; j<22; j++) {
	std::cout << i <<" "<<j<<" "<<fCollectionMatrix[0][i][j]<<" "<<fCollectionMatrix[1][i][j]<< std::endl;
      }
    }
  }
  if (fHistoFlag) {
    for (int i=0; i<22; i++) {
      for (int j=0; j<22; j++) {
	static_cast<MUV3Reconstruction*>(fReco)->GetHCollectionMatrixA()->
	  Fill(i+0.5, j+0.5, fCollectionMatrix[0][i][j]);
	static_cast<MUV3Reconstruction*>(fReco)->GetHCollectionMatrixB()->
	  Fill(i+0.5, j+0.5, fCollectionMatrix[1][i][j]);
      }
    }
  }
}

/////////////////////////////////////////////////
// Print the parameters of the detailed digitizer

void MUV3Digitizer::PrintParameters() {
  std::cout << "MUV3 digitizer: configuration parameters"        << std::endl;
  std::cout << "Transit time mean [ns]   " << fTransitTimeMean   << std::endl;
  std::cout << "Transit time spread [ns] " << fTransitTimeSpread << std::endl;
  std::cout << "Quantum efficiency       " << fQuantumEff        << std::endl;
  std::cout << "Cable delay [ns]         " << fCableDelay        << std::endl;
  std::cout << "CFD   delay [ns]         " << fCFDDelay          << std::endl;
  std::cout << "CFD   attenuation        " << fCFDAttenuation    << std::endl;
  std::cout << "CFD   threshold          " << fCFDThreshold      << std::endl;
  std::cout << "CFD   pulse width [ns]   " << fCFDPulseWidth     << std::endl;
  std::cout << "Time interval [ns]       " << fTimeInterval      << std::endl;
  std::cout << "Delta time [ns]          " << fDeltaTime         << std::endl;
  std::cout << "Photons per MeV          " << fPhotonsPerMeV     << std::endl;
  std::cout << "Histograms flag          " << fHistoFlag         << std::endl;
  std::cout << "Debug flag               " << fDebugFlag         << std::endl;
  std::cout << "CFD Debug flag           " << fCFDDebugFlag      << std::endl;
}

////////////////
// Process event

TDetectorVEvent* MUV3Digitizer::ProcessEvent (TDetectorVEvent* tEvent) {

  if(tEvent->GetHits()->GetClass()->InheritsFrom("TVDigi") ||
     tEvent->IsA() == TSpecialTriggerEvent::Class())
    return tEvent;

  TMUV3Event* MUV3Event = static_cast<TMUV3Event*>(tEvent);
  Int_t NHits = MUV3Event->GetNHits();

  fDigiEvent->Clear();
  (*(TVEvent*)fDigiEvent)=(*(TVEvent*)MUV3Event);

  if (fDebugFlag) std::cout << "Nhits " << NHits << std::endl;
  if (!NHits) return fDigiEvent;
  if      (fMode==1) RunSimpleDigitizer(MUV3Event);
  else if (fMode==2) RunDetailedDigitizer(MUV3Event);
  return fDigiEvent;
}

///////////////////////
// The simple digitizer

void MUV3Digitizer::RunSimpleDigitizer (TMUV3Event* MUV3Event) {

  // Loop over MC hits
  Int_t NHits = MUV3Event->GetNHits();
  for (Int_t iHit=0; iHit<NHits; iHit++) {
    TMUV3Hit *Hit = static_cast<TMUV3Hit*>(MUV3Event->GetHit(iHit));
    Bool_t CherenkovHit = (Hit->GetEnergy()<0);
    if (!CherenkovHit && Hit->GetEnergy()<fEnergyDepositThreshold) continue; // threshold simulation

    // Convert position ID into RO channel ID
    Int_t ich   = Hit->GetChannelID(); // Position ID
    Int_t iROch =                      // RO channel ID
      static_cast<TDCBRawDecoder*>(static_cast<MUV3Reconstruction*>(fReco)->GetRawDecoder()->GetDecoder())->GetChannelRO(ich);
    if (iROch<0) continue; // channel not instrumented

    // Hit time with TDC correction (TdcCalib = ClockPeriod/256)
    Double_t dT = fRandom->Gaus(0, fChannelTimeResolution);
    Double_t FineTime = NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256.;
    Double_t CorrectedTime = Hit->GetTime() + FineTime - static_cast<MUV3Reconstruction*>(fReco)->GetStationMCToF(Hit->GetStationID())
      + fReco->GetT0Correction(Hit->GetChannelID(),Hit->GetStationID()) + fChannels[iROch]->GetT0() + dT;
    if (CherenkovHit) CorrectedTime -= 3.0*ns;

    Double_t TrailingTime          = fRandom->Gaus(CorrectedTime+35*ns, 1*ns);
    Double_t DigitizedTime         = (Int_t)(CorrectedTime/ns/TdcCalib)*TdcCalib;
    Double_t DigitizedTrailingTime = (Int_t)(TrailingTime/ns/TdcCalib)*TdcCalib;

    // Is there a hit in this channel and close in time already?
    Bool_t Merged = kFALSE;
    for (Int_t iDone=0; iDone<fDigiEvent->GetNHits(); iDone++) {
      TMUV3Digi *DigiDone = static_cast<TMUV3Digi*>(fDigiEvent->GetHit(iDone));
      if (DigiDone->GetChannelID() == ich) { // another Digi exists in this channel
	Double_t HitTimeDifference = DigitizedTime - DigiDone->GetLeadingEdge();
	if (fabs(HitTimeDifference)<fChannelMergeThreshold) {
	  Merged = kTRUE;
	  if (HitTimeDifference<0) DigiDone->SetLeadingEdge(DigitizedTime);
	}
      }
    }

    // Create a new Digi
    if (!Merged) {
      TMUV3Digi *Digi = static_cast<TMUV3Digi*>(fDigiEvent->AddDigi(Hit));
      Digi->SetLeadingEdge(DigitizedTime);
      Digi->SetTrailingEdge(DigitizedTrailingTime);
      Digi->SetDetectedEdge(kBothEdges);
    }
  }
}

/////////////////////////
// The detailed digitizer

void MUV3Digitizer::RunDetailedDigitizer (TMUV3Event* MUV3Event) {

  Int_t NHits = MUV3Event->GetNHits();
  if (fHistoFlag) static_cast<MUV3Reconstruction*>(fReco)->GetHNHits()->Fill(NHits);
  for (Int_t i=0; i<400; i++) fHitChannels[i] = 0;

  //////////////////////////////////
  // Identify which channels are hit

  Int_t NHitChannels = 0;
  for (Int_t iHit=0; iHit<NHits; iHit++) {
    TMUV3Hit *Hit = static_cast<TMUV3Hit*>(MUV3Event->GetHit(iHit));
    if (Hit->GetEnergy()<0) continue; // reject hits in PMT windows for the time being
    Int_t iChannel = Hit->GetChannelID();
    if (!fHitChannels[iChannel]) {
      fHitChannels[iChannel] = 1;
      NHitChannels++;
    }
  }
  if (fHistoFlag) static_cast<MUV3Reconstruction*>(fReco)->GetHNChannels()->Fill(NHitChannels);

  ////////////////////////////
  // Main loop on the channels

  for (Int_t iCell=0; iCell<152; iCell++) {
    for (Int_t iPMT=0; iPMT<2; iPMT++) {
      int iChannel = iCell + 200*iPMT;

      if (!fHitChannels[iChannel]) continue;

      // Convert position ID into RO channel ID
      Int_t iROch =
              static_cast<TDCBRawDecoder*>(static_cast<MUV3Reconstruction*>(fReco)->
	 GetRawDecoder()->GetDecoder())->GetChannelRO(iChannel);
      if (iROch<0) continue; // channel not instrumented

      // Compute minimum, maximum and average hit times in a channel
      Int_t NHitsInChannel = 0;
      Double_t MinimumTime = 0.0, MaximumTime = 0.0, AverageTime = 0.0;
      for (Int_t iHit=0; iHit<NHits; iHit++) {
	TMUV3Hit *Hit = static_cast<TMUV3Hit*>(MUV3Event->GetHit(iHit));
	if (Hit->GetChannelID() != iChannel) continue;
	if (Hit->GetEnergy()<0) continue; // reject hits in PMT windows for the time being

	if (!NHitsInChannel) {
	  MinimumTime = MaximumTime = Hit->GetTime();
	}
	else {
	  if (Hit->GetTime()<MinimumTime) MinimumTime = Hit->GetTime();
	  if (Hit->GetTime()>MaximumTime) MaximumTime = Hit->GetTime();
	}
	AverageTime += Hit->GetTime();
	NHitsInChannel++;
      }
      AverageTime /= (1.0*NHitsInChannel);

      if (fDebugFlag) {
	std::cout << "Channel " << iChannel << " average time " << AverageTime << std::endl;
	std::cout << "Minimum time " << MinimumTime <<" Maximum time " << MaximumTime << std::endl;
      }

      // Clear signal vector and setup the extremes of the time interval
      for (Int_t i=0; i<fNBins; i++) fAnalogSignal[i] = 0.0;
      fStartTime = MinimumTime - 2*fCFDDelay + 0.08*ns + fCableDelay; // 0.08 ns = max collection time
      fEndTime   = fStartTime + fTimeInterval;
      if (fEndTime < MaximumTime) std::cout << "Hits outside signal time window" << std::endl;

      // Loop on the hits in the given channel
      Double_t CellEnergy = 0.0;
      for (Int_t iHit=0; iHit<NHits; iHit++) {
	TMUV3Hit *Hit = static_cast<TMUV3Hit*>(MUV3Event->GetHit(iHit));
	if (Hit->GetChannelID() != iChannel) continue;

	Double_t Energy   = Hit->GetEnergy();
	if (Energy<0) continue; // reject hits in PMT windows for the time being
	CellEnergy += Energy;

	Double_t x        = Hit->GetPosition().x();
	Double_t y        = Hit->GetPosition().y();
	Double_t nPhotons = Energy*fPhotonsPerMeV;

	// Compute the number of photoelectrons for this hit and the arrival time at the CFD input
	Double_t nMeanPhotoElectrons = nPhotons * CollectionEfficiency(iPMT, x, y, iCell) * fQuantumEff;
	Double_t nPhotoElectrons     = 1.0*fRandom->Poisson(nMeanPhotoElectrons);
	Double_t CollectionTime      = GetCollectionTime(iPMT, x, y, iCell);
	Double_t CorrectedTime       =
	  Hit->GetTime() + CollectionTime + fCableDelay + fTransitTime[iPMT][iCell];
	IncrementAnalogSignal(nPhotoElectrons, CorrectedTime);
      }

      // Do the CFD signal processing
      Times.clear();
      DoCFD();

      // Fill the Digi(s)
      for (UInt_t i=0; i<Times.size(); i++) {

	TMUV3Digi *Digi = static_cast<TMUV3Digi*>( fDigiEvent->AddDigi());
	Digi->SetChannelID(iChannel); // Digi carries the geometric ID
	Double_t dt = fReco->GetT0Correction(Digi) + fChannels[iROch]->GetT0();
	Digi->SetLeadingEdge(Times.at(i).LeadingTime + dt);
	Digi->SetTrailingEdge(Times.at(i).TrailingTime + dt);
	Digi->SetDetectedEdge(kBothEdges);
	//Digi->SetMCPhotoelectrons(0);
	//Digi->SetEnergy(0);

	if (fHistoFlag) {

	  if (iPMT==0) {
	      static_cast<MUV3Reconstruction*>(fReco)->GetHEventMinusCFDTimePM0_all()->
	      Fill(Times.at(i).LeadingTime - MinimumTime);
	    if (NHits==1)
	        static_cast<MUV3Reconstruction*>(fReco)->GetHEventMinusCFDTimePM0()->
		Fill(Times.at(i).LeadingTime - MinimumTime);
	    static_cast<MUV3Reconstruction*>(fReco)->GetHTimevsEnergyPM0()->
	      Fill(CellEnergy,Times.at(i).LeadingTime - MinimumTime);
	    static_cast<MUV3Reconstruction*>(fReco)->GetHTimevsNHitsPM0()->
	      Fill(NHits, Times.at(i).LeadingTime - MinimumTime);
	    static_cast<MUV3Reconstruction*>(fReco)->GetHCFDTimePM0()->
	      Fill(Times.at(i).LeadingTime);
	  }
	  if (iPMT==1) {
	      static_cast<MUV3Reconstruction*>(fReco)->GetHEventMinusCFDTimePM1_all()->
	      Fill(Times.at(i).LeadingTime - MinimumTime);
	    if (NHits==1)
	        static_cast<MUV3Reconstruction*>(fReco)->GetHEventMinusCFDTimePM1()->
		Fill(Times.at(i).LeadingTime - MinimumTime);
	    static_cast<MUV3Reconstruction*>(fReco)->GetHTimevsEnergyPM1()->
	      Fill(CellEnergy,Times.at(i).LeadingTime - MinimumTime);
	    static_cast<MUV3Reconstruction*>(fReco)->GetHTimevsNHitsPM1()->
	      Fill(NHits, Times.at(i).LeadingTime - MinimumTime);
	    static_cast<MUV3Reconstruction*>(fReco)->GetHCFDTimePM1()->
	      Fill(Times.at(i).LeadingTime);
	  }
	}
      } // end of Digi loop

      if (fDebugFlag) {
	if (Times.size()>0) {
	  std::cout << "Leading  times - Nleading " << Times.size() << " ";
	  for (UInt_t i=0; i<Times.size(); i++)
	    std::cout << Times.at(i).LeadingTime << " " << Times.at(i).LeadingTime - MinimumTime << " ";
	  std::cout << std::endl;
	  std::cout << "Trailing times " ;
	  for (UInt_t i=0; i<Times.size(); i++)
	    std::cout << Times.at(i).TrailingTime << " ";
	  std::cout << std::endl;
	  std::cout << "Cell energy " << CellEnergy << std::endl;
	}
      }
      if (fHistoFlag) {
	if (CellEnergy>0.0)
	  static_cast<MUV3Reconstruction*>(fReco)->GetHEnergyPerCell()->Fill(CellEnergy);
      }
    } // end of PMT loop
  } // end of cell loop

  if (fDebugFlag || fHistoFlag) {
    Double_t Etot = 0.0;
    for (Int_t iHit=0; iHit<NHits; iHit++) {
      TMUV3Hit *Hit = static_cast<TMUV3Hit*>(MUV3Event->GetHit(iHit));

      if (Hit->GetEnergy()<0) continue; // reject hits in PMT windows for the time being
      if (Hit->GetChannelID()>=200) continue; // avoid duplication

      Etot += Hit->GetEnergy();
      if (fDebugFlag) {
	std::cout << Hit->GetTime() << " " << Hit->GetEnergy() << " " <<
	  Hit->GetPosition().x() << " " << Hit->GetPosition().y() << " " <<
	  Hit->GetPosition().z() << " " << Hit->GetChannelID() << " " <<
	  Hit->GetMCTrackID() << std::endl;
      }
    }
    if (fDebugFlag) std::cout << "Energy " << Etot << std::endl << std::endl;
    if (fHistoFlag) static_cast<MUV3Reconstruction*>(fReco)->GetHEnergy()->Fill(Etot);
  }
}

///////////////////////////////////////////////////////////////////////
// Compute the collection efficiency starting from Italo's matrices and
// the hit distance from the PMs

Double_t MUV3Digitizer::CollectionEfficiency (Int_t PM_no, Double_t x, Double_t y, Int_t iCell) {
  Double_t tx = x - fGeo->GetTileCentreX(iCell);
  Double_t ty = y - fGeo->GetTileCentreY(iCell);
  Double_t ttx = tx;
  Double_t tty = ty;
  if (iCell==147 || iCell==148) {
    ttx = 0.7071*(tx+ty);
    tty = 0.7071*(ty-tx);
  }
  if (iCell==145 || iCell==150) {
    ttx = 0.7071*(tx-ty);
    tty = 0.7071*(tx+ty);
  }
  if (iCell==144 || iCell==149) {
    ttx =  ty;
    tty = -tx;
  }
  tx = ttx;
  ty = tty;
  //Int_t ix = (Int_t) (tx/220.) + 11;
  //Int_t iy = (Int_t) (ty/220.) + 11;
  Int_t ix = (Int_t) ((tx+110.)/10.);
  Int_t iy = (Int_t) ((ty+110.)/10.);

  if (fDebugFlag) {
    std::cout << "Icell " << iCell << " x " << x << " y " << y << std::endl ;
    std::cout << "x tile centre " << fGeo->GetTileCentreX(iCell) << " shifted x " << tx << std::endl ;
    std::cout << "y tile centre " << fGeo->GetTileCentreY(iCell) << " shifted y " << ty << std::endl ;
    std::cout << "PM " << PM_no <<" ix iy "<<ix<<" "<<iy<<" Collection efficiency " << fCollectionMatrix [PM_no][iy][ix] << std::endl;
  }

  return fCollectionMatrix [PM_no][iy][ix];
}

//////////////////////////////////////////////////////////////////
// Compute the time correction due to the different distances from
// the different points of the scintillator to the PM surface

Double_t MUV3Digitizer::GetCollectionTime (Int_t iPM, Double_t x, Double_t y, Int_t iCell) {
  //  Double_t PM_x[2] = {26.51,-26.51};
  //  Double_t PM_y[2] = {26.51,-26.51};
  Double_t PM_z = 200.0;
  Double_t tx   = x - fGeo->GetTileCentreX(iCell);
  Double_t ty   = y - fGeo->GetTileCentreY(iCell);
  Double_t PM_x = fGeo->GetPMOffsetX (iCell, iPM);
  Double_t PM_y = fGeo->GetPMOffsetY (iCell, iPM);
  Double_t time = (tx-PM_x)*(tx-PM_x) + (ty-PM_y)*(ty-PM_y) + PM_z*PM_z;
  return sqrt(time)/300.0;
}

////////////////////////////////////////////////////////////////////////////////////////////
// Accumulate reference shapes for each hit, scaled by photoelectrons and offset by the time

void MUV3Digitizer::IncrementAnalogSignal (Double_t nPhotoElectrons, Double_t Time) {

  if (fDebugFlag) {
    std::cout << " Photoelectrons " << nPhotoElectrons << " Time " << Time << std::endl;
    std::cout << "Start time " << fStartTime << " End time " << fEndTime << std::endl;
  }
  Int_t ibin = (Int_t) ((Time - fStartTime)/fDeltaTime);
  Int_t ncopy = fNBinsShape;
  if (ibin+fNBinsShape>fNBins) ncopy = fNBins - ibin;
  if (fDebugFlag) {
    std::cout << "ncopy " << ncopy << " ibin " << ibin << std::endl;
  }
  for (Int_t i=0; i<ncopy; i++) fAnalogSignal[ibin+i] += fShape[i]*nPhotoElectrons;
}

//////////////////////////////////////////////////////
// Simulate constant fraction discriminator processing

void MUV3Digitizer::DoCFD() {
  Double_t LeadingSum = 0, LeadingSumOld = 0;
  Int_t ibinLeading = 0, ibinTrailing;
  Double_t fTDCTimeBin = 0.097;

  if (fCFDDebugFlag) std::cout << "CFD processing" << std::endl;
  Double_t CFDSumOld = 0.0;
  Int_t    CFDDead   = 0;

  // Compute the diference between the delayed signal and the inverted, attenuated one
  for (Int_t i=fNBinCFDDelay; i<fNBins; i++) {

    // Arm the CFD if above threshold
    Double_t CFDSum = fAnalogSignal[i-fNBinCFDDelay] - fAnalogSignal[i]/fCFDAttenuation;
    Int_t CFDArmed  = (fAnalogSignal[i]<fCFDThreshold);

    if (fCFDDebugFlag) {
      if (i>250 && i<600) {
	std::cout << " bin sig sum sumold armed dead " << i << " " << fAnalogSignal[i] << " ";
	std::cout << CFDSum << " " << CFDSumOld << " " << CFDArmed;
	std::cout << " " << CFDDead << std::endl;
      }
    }

    // When the sum is passing across the zero, store the bin number and the two consecutive sums
    if (CFDSum<0.0 && CFDSumOld>0.0) {
      if (!CFDDead && CFDArmed) {
	ibinLeading = i;
	CFDDead = 1;
	LeadingSum = CFDSum;
	LeadingSumOld = CFDSumOld;
      }
    }

    // As soon as the CFD is not armed any longer and the out pulse width has passed
    // Compute the time of the leading edge, interpolating inside the bin
    // Compute the trailing time (without interpolationfor the moment)
    // Truncate times at the TDC bin size

    if (CFDDead && i-ibinLeading==fNBinCFDPulseWidth) {
      TDCTime tt;
      CFDDead = false;
      ibinTrailing = i;
      Double_t t = fStartTime + (ibinLeading-1)*fDeltaTime;
      t = t + fDeltaTime * LeadingSumOld /(LeadingSumOld + abs(LeadingSum));
      t = fTDCTimeBin * (Int_t) (t/fTDCTimeBin);
      tt.LeadingTime = t;
      t = fStartTime + ibinTrailing*fDeltaTime;
      t = fTDCTimeBin * (Int_t) (t/fTDCTimeBin);
      tt.TrailingTime = t;
      Times.push_back(tt);
    }
    CFDSumOld = CFDSum;
  }
}
