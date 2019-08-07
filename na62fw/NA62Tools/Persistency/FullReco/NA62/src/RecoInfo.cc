// ---------------------------------------------------------------
// History:
//
// Variables added  Karim Massri (karim.massri@cern.ch) 2016-12-08
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-11-18
//
// ---------------------------------------------------------------

/// \class RecoInfo
/// \Brief
/// Container of NA62Reconstruction settings
/// \EndBrief

#include "RecoInfo.hh"

#include <iostream>
#include <iomanip>
#include <stdlib.h>
#include "TemplateFunctions.hh"

ClassImp(RecoInfo)

RecoInfo::RecoInfo() {
  Clear();
}

void RecoInfo::Clear(Option_t*) {
  fRevision               = "";
  fNReadEvents            = 0;
  fNProcessedEvents       = 0;
  fNSkippedEvents         = 0;
  fNCriticalEvents        = 0;
  fNPhysicsTriggerEvents  = 0;
  fNControlTriggerEvents  = 0;
  fNPeriodicTriggerEvents = 0;
  fNSpecialTriggerEvents  = 0;
  fKaonRate               = 0;
  fKaonRateError          = 0;
  fChokeONTime            = 0;

  fRunID.clear();
  fBurstID.clear();
  fBurstTime.clear();
}

void RecoInfo::Print(Option_t*) const {
	std::cout << "RecoInfo" << std::endl;
	std::cout << std::setw(30) << "Revision:"                  << fRevision                  << std::endl;
	if(fRunID.size()>0)
		std::cout << "Input files:" << std::endl;
	for(unsigned int iFile=0; iFile<fRunID.size(); ++iFile)
		std::cout << "  Run: " << fRunID[iFile] << "   BurstID: " << fBurstID[iFile] << "   Timestamp: " << fBurstTime[iFile] << std::endl;

	std::cout << std::setw(30) << "NReadEvents:"               << fNReadEvents               << std::endl;
	std::cout << std::setw(30) << "NProcessedEvents:"          << fNProcessedEvents          << std::endl;
	std::cout << std::setw(30) << "NSkippedEvents:"            << fNSkippedEvents            << std::endl;
	std::cout << std::setw(30) << "fNCriticalEvents:"          << fNCriticalEvents           << std::endl;
	std::cout << std::setw(30) << "fNPhysicsTriggerEvents:"    << fNPhysicsTriggerEvents     << std::endl;
	std::cout << std::setw(30) << "fNControlTriggerEvents:"    << fNControlTriggerEvents     << std::endl;
	std::cout << std::setw(30) << "fNPeriodicTriggerEvents:"   << fNPeriodicTriggerEvents    << std::endl;
	std::cout << std::setw(30) << "fNSpecialTriggerEvents:"    << fNSpecialTriggerEvents     << std::endl;
	std::cout << std::setw(30) << "KaonRate:"                  << fKaonRate                  << std::endl;
	std::cout << std::setw(30) << "KaonRateError:"             << fKaonRateError             << std::endl;
	std::cout << std::setw(30) << "ChokeONTime:"               << fChokeONTime               << std::endl;
}

void RecoInfo::UpdateUniqueAttributes(RecoInfo& s) {
  fRevision               = s.fRevision;
  fNReadEvents            += s.fNReadEvents;
  fNProcessedEvents       += s.fNProcessedEvents;
  fNSkippedEvents         += s.fNSkippedEvents;
  fNCriticalEvents        += s.fNCriticalEvents;
  fNPhysicsTriggerEvents  += s.fNPhysicsTriggerEvents;
  fNControlTriggerEvents  += s.fNControlTriggerEvents;
  fNPeriodicTriggerEvents += s.fNPeriodicTriggerEvents;
  fNSpecialTriggerEvents  += s.fNSpecialTriggerEvents;
  fKaonRate               = fKaonRate*(fNProcessedEvents-s.fNProcessedEvents)/((Double_t)fNProcessedEvents);
  fKaonRate               += s.fKaonRate*s.fNProcessedEvents/((Double_t)fNProcessedEvents);
  fKaonRateError          = pow(fKaonRateError*(fNProcessedEvents-s.fNProcessedEvents)/((Double_t)fNProcessedEvents),2.);
  fKaonRateError          +=pow(s.fKaonRateError*s.fNProcessedEvents/((Double_t)fNProcessedEvents),2.);
  fKaonRateError          = sqrt(fKaonRateError);
  fChokeONTime            += s.fChokeONTime;
}

void RecoInfo::MergeJobAttributes(RecoInfo& s) {
	if(!all_equal(fRunID.size(), fBurstID.size(), fBurstTime.size())){
		std::cout << "Warning: merging RecoInfo job attributes with inconsistent number of entries." << std::endl;
	}
	fRunID.insert(fRunID.end(), s.fRunID.begin(), s.fRunID.end());
	fBurstID.insert(fBurstID.end(), s.fBurstID.begin(), s.fBurstID.end());
	fBurstTime.insert(fBurstTime.end(), s.fBurstTime.begin(), s.fBurstTime.end());
}

void RecoInfo::UpdateAndMergeAttributes(RecoInfo& s) {
	UpdateUniqueAttributes(s);
	MergeJobAttributes(s);
}
