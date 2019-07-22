// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-27
//
// ---------------------------------------------------------------  

/// \class NewCHODDigitizer
/// \Brief
/// NewCHOD digitizer: converts NewCHOD MC hits into Digis
/// \EndBrief
/// \Detailed
/// Conversion of NewCHOD MC hits into Digis. A Digi is generated if the energy deposit
/// in a counter is above threshold. If several MC hits in a counter are close in time,
/// a single Digi is generated for these MC hits. The digitizer parameters are defined in the config/NewCHOD.conf file.
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "NewCHODDigitizer.hh"
#include "NA62RecoManager.hh"
#include "TNewCHODEvent.hh"

#include "CLHEP/Units/SystemOfUnits.h"
using namespace CLHEP;

NewCHODDigitizer::NewCHODDigitizer(NA62VReconstruction* Reco) : NA62VDigitizer(Reco, "NewCHOD") {
  fDigiEvent = new TDCEvent(TNewCHODDigi::Class());
  fGeo       = NewCHODGeometry::GetInstance();
  fEnergyDepositThreshold = fChannelTimeResolution = fChannelMergeThreshold = -999;

  // Read configuration file
  ParseConfFile(static_cast<NewCHODReconstruction*>(fReco)->GetConfigFileName());
}

void NewCHODDigitizer::ParseConfFile (TString ConfFileName) {
  std::ifstream confFile(ConfFileName.Data());
  if (!confFile.is_open()) {
    perror(ConfFileName); exit(kWrongConfiguration);
  }

  TString Line;
  while (Line.ReadLine(confFile)) {
    if (Line.BeginsWith("#")) continue;
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
  }
  confFile.close();

  // Sanity checks
  if (fEnergyDepositThreshold<0) {
    std::cout << "[NewCHODDigitizer] Error: invalid threshold specified"<< std::endl;
    exit(kWrongConfiguration);
  }
  if (fChannelTimeResolution<0) {
    std::cout << "[NewCHODDigitizer] Error: invalid channel time resolution specified"<< std::endl;
    exit(kWrongConfiguration);
  }
  if (fChannelMergeThreshold<0) {
    std::cout << "[NewCHODDigitizer] Error: invalid hit merge threshold specified"<< std::endl;
    exit(kWrongConfiguration);
  }
}

////////////////
// Process event

TDetectorVEvent* NewCHODDigitizer::ProcessEvent (TDetectorVEvent* tEvent) {

  if (tEvent->GetHits()->GetClass()->InheritsFrom("TVDigi") ||
      tEvent->IsA() == TSpecialTriggerEvent::Class())
    return tEvent;

  TNewCHODEvent* NewCHODEvent = static_cast<TNewCHODEvent*>(tEvent);
  Int_t NHits = NewCHODEvent->GetNHits();

  fDigiEvent->Clear();
  (*(TVEvent*)fDigiEvent)=(*(TVEvent*)NewCHODEvent);

  if (!NHits) return fDigiEvent;

  // Loop over MC hits
  for (Int_t iHit=0; iHit<NHits; iHit++) {
    TNewCHODHit *Hit = static_cast<TNewCHODHit*>(NewCHODEvent->GetHit(iHit));
    if (Hit->GetEnergy()<fEnergyDepositThreshold) continue; // threshold simulation

    for (Int_t j=0; j<2; j++) { // Two SiPMs connected to tile: MC hit replication into two Digis

      // Convert position ID into RO channel ID
      Int_t ich   = Hit->GetChannelID()+50*j; // Position ID
      Int_t iROch =                           // RO channel ID
	static_cast<TDCBRawDecoder*>(static_cast<NewCHODReconstruction*>(fReco)->GetRawDecoder()->GetDecoder())->GetChannelRO(ich);
      if (iROch<0) continue; // channel not instrumented

      // Hit time with TDC correction (TdcCalib = ClockPeriod/256)
      Double_t dT = fRandom->Gaus(0, fChannelTimeResolution);
      Double_t FineTime = NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256.;
      Double_t CorrectedTime = Hit->GetTime() + FineTime
	- static_cast<NewCHODReconstruction*>(fReco)->GetStationMCToF(Hit->GetStationID())
        + fReco->GetT0Correction(Hit->GetChannelID(),Hit->GetStationID()) + fChannels[iROch]->GetT0() + dT;    

      Double_t TrailingTime          = fRandom->Gaus(CorrectedTime+35*ns, 1.5*ns);
      Double_t DigitizedTime         = (Int_t)(CorrectedTime/ns/TdcCalib)*TdcCalib;
      Double_t DigitizedTrailingTime = (Int_t)(TrailingTime/ns/TdcCalib)*TdcCalib;

      // Is there a hit in this channel and close in time already?
      Bool_t Merged = kFALSE;
      for (Int_t iDone=0; iDone<fDigiEvent->GetNHits(); iDone++) {
	TNewCHODDigi *DigiDone = static_cast<TNewCHODDigi*>(fDigiEvent->GetHit(iDone));
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
	TNewCHODDigi *Digi = static_cast<TNewCHODDigi*>(fDigiEvent->AddDigi(Hit));
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
