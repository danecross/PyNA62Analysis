/// \class MUV0Digitizer
/// \Brief
/// MUV0 digitizer: converts MUV0 MC hits into Digis
/// \EndBrief
/// \Detailed
/// Conversion of MUV0 MC hits into Digis. A Digi is generated if the energy desposit
/// in a counter is above threshold. If several MC hits in a counter are close in time,
/// a single Digi is generated for these MC hits. The digitizer parameters are defined in the config/MUV0.conf file.
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "MUV0Digitizer.hh"
#include "NA62RecoManager.hh"

MUV0Digitizer::MUV0Digitizer(NA62VReconstruction* Reco) : NA62VDigitizer(Reco, "MUV0") {
  fDigiEvent = new TDCEvent(TMUV0Digi::Class());
  fEnergyDepositThresholdLow = fEnergyDepositThresholdHigh = -999;
  fChannelTimeResolution = fChannelMergeThreshold = -999;

  // Read configuration file
  ParseConfFile(static_cast<MUV0Reconstruction*>(fReco)->GetConfigFileName());
}

void MUV0Digitizer::ParseConfFile (TString ConfFileName) {

  std::ifstream confFile(ConfFileName.Data());
  if (!confFile.is_open()) {
    perror(ConfFileName); exit(kWrongConfiguration);
  }

  TString Line;
  while (Line.ReadLine(confFile)) {
    if (Line.BeginsWith("#")) continue;
    else if (Line.BeginsWith("EnergyDepositThresholdLow")) {
      fEnergyDepositThresholdLow = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof() * MeV;
      continue;
    }
    else if (Line.BeginsWith("EnergyDepositThresholdHigh")) {
      fEnergyDepositThresholdHigh = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof() * MeV;
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
  }
  confFile.close();

  // Sanity checks (parameters of the simple digitizer)
  if (fEnergyDepositThresholdLow<0) {
    std::cout << "[MUV0Digitizer] Error: invalid low threshold specified"<< std::endl;
    exit(kWrongConfiguration);
  }
  if (fEnergyDepositThresholdHigh<0) {
    std::cout << "[MUV0Digitizer] Error: invalid high threshold specified"<< std::endl;
    exit(kWrongConfiguration);
  }
  if (fChannelTimeResolution<0) {
    std::cout << "[MUV0Digitizer] Error: invalid channel time resolution specified"<< std::endl;
    exit(kWrongConfiguration);
  }
  if (fChannelMergeThreshold<0) {
    std::cout << "[MUV0Digitizer] Error: invalid hit merge threshold specified"<< std::endl;
    exit(kWrongConfiguration);
  }
}

TDetectorVEvent* MUV0Digitizer::ProcessEvent(TDetectorVEvent * tEvent){

  if (tEvent->GetHits()->GetClass()->InheritsFrom("TVDigi") ||
      tEvent->IsA() == TSpecialTriggerEvent::Class())
    return tEvent;

  TMUV0Event* MUV0Event = static_cast<TMUV0Event*>(tEvent);
  Int_t NHits = MUV0Event->GetNHits();

  fDigiEvent->Clear();
  (*(TVEvent*)fDigiEvent)=(*(TVEvent*)MUV0Event);

  if (!NHits) return fDigiEvent;

  /////////////////////////////////////////////////////////////
  // Loop over MC hits
  // Each signal is discriminated by a low and a high threshold

  for (Int_t iHit=0; iHit<NHits; iHit++) {
    TMUV0Hit *Hit = static_cast<TMUV0Hit*>(MUV0Event->GetHit(iHit));
    for (Int_t j=0; j<2; j++) {

      Double_t Threshold = (j==0) ? fEnergyDepositThresholdLow : fEnergyDepositThresholdHigh;
      if (Hit->GetEnergy()<Threshold) continue; // threshold simulation

      // Convert position ID into RO channel ID
      Int_t ich   = Hit->GetChannelID()+10*j; // Position ID
      Int_t iROch =                           // RO channel ID
              static_cast<TDCBRawDecoder*>(static_cast<MUV0Reconstruction*>(fReco)->GetRawDecoder()->GetDecoder())->GetChannelRO(ich);
      if (iROch<0) continue; // channel not instrumented

      // Hit time with TDC correction (TdcCalib = ClockPeriod/256)
      Double_t dT = fRandom->Gaus(0, fChannelTimeResolution);
      Double_t FineTime = NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256.;
      Double_t CorrectedTime = Hit->GetTime() + FineTime - static_cast<MUV0Reconstruction*>(fReco)->GetStationMCToF(Hit->GetStationID())
        + fReco->GetT0Correction(Hit->GetChannelID(),Hit->GetStationID()) + fChannels[iROch]->GetT0() + dT;    
      Double_t TrailingTime          = fRandom->Gaus(CorrectedTime+35*ns, 1*ns);
      Double_t DigitizedTime         = (Int_t)(CorrectedTime/ns/TdcCalib)*TdcCalib;
      Double_t DigitizedTrailingTime = (Int_t)(TrailingTime/ns/TdcCalib)*TdcCalib;

      // Is there a hit in this channel and close in time already?
      Bool_t Merged = kFALSE;
      for (Int_t iDone=0; iDone<fDigiEvent->GetNHits(); iDone++) {
	TMUV0Digi *DigiDone = static_cast<TMUV0Digi*>(fDigiEvent->GetHit(iDone));
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
	TMUV0Digi *Digi = static_cast<TMUV0Digi*>(fDigiEvent->AddDigi(Hit));
	Digi->SetChannelID(ich);
	Digi->DecodeChannelID();
	Digi->SetLeadingEdge(DigitizedTime);
	Digi->SetTrailingEdge(DigitizedTrailingTime);
	Digi->SetDetectedEdge(kBothEdges);
      }
    }
  }
  return fDigiEvent;
}
