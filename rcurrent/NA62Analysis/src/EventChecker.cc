/*
 * EventChecker.cc
 *
 *  Created on: 13 Feb 2017
 *      Author: ncl
 */

#include "EventChecker.hh"
#include "EventHeader.hh"

#include "Verbose.hh"
#include "ConfigSettings.hh"

#include <iostream>
#include <TH1I.h>
namespace NA62Analysis {

EventChecker::EventChecker(std::set<std::string> detectors, TString rev) :
		Verbose("EventChecker"),
		fDetFlag(0),
		fRevision(rev)
	{
	/// \MemberDescr
	///	\param detectors: Set of detectors to ignore. The possible values are Cedar, GigaTracker,
	/// CHANTI, LAV, Spectrometer, CHOD, NewCHOD, RICH, IRC, L0, L1, LKr, MUV0, MUV1, MUV2, MUV3, SAC, SAV, HAC
	/// \param rev: Revision used for the reconstruction of the data
	///
	/// Constructor.
	/// \EndMemberDescr

	UInt_t oldfDetFlag;

	std::set<TString> checkedSystems{"Cedar", "GTK", "CHANTI", "LAV", "Straw", "CHOD", "NewCHOD",
		"RICH", "IRC", "L0", "L1", "LKr", "MUV0", "MUV1", "MUV2", "MUV3", "SAC", "SAV", "HAC"};

	std::cout << "[EventChecker] Systems ignored:";
	if(Configuration::ConfigSettings::CLI::fNoCheckEvents){
		fDetFlag = 0xffffffff; // mask all detectors for event checks
		for(auto systemName : checkedSystems)
			GetStream() << " " << systemName << "(Ok)";
		checkedSystems.clear();
	}
	else{
		for (auto detector : detectors) {
			oldfDetFlag = fDetFlag;
			GetStream() << " " << detector;
			if (TString(detector).CompareTo("Cedar", TString::kIgnoreCase) == 0)
				fDetFlag |= 0x1 << kCedar;
			else if (TString(detector).CompareTo("GTK", TString::kIgnoreCase) == 0)
				fDetFlag |= 0x1 << kGigaTracker;
			else if (TString(detector).CompareTo("CHANTI", TString::kIgnoreCase) == 0)
				fDetFlag |= 0x1 << kCHANTI;
			else if (TString(detector).CompareTo("LAV", TString::kIgnoreCase) == 0)
				fDetFlag |= 0x1 << kLAV;
			else if (TString(detector).CompareTo("Straw", TString::kIgnoreCase) == 0)
				fDetFlag |= 0x1 << kSpectrometer;
			else if (TString(detector).CompareTo("CHOD", TString::kIgnoreCase) == 0)
				fDetFlag |= 0x1 << kCHOD;
			else if (TString(detector).CompareTo("NewCHOD", TString::kIgnoreCase) == 0)
				fDetFlag |= 0x1 << kNewCHOD;
			else if (TString(detector).CompareTo("RICH", TString::kIgnoreCase) == 0)
				fDetFlag |= 0x1 << kRICH;
			else if (TString(detector).CompareTo("IRC", TString::kIgnoreCase) == 0)
				fDetFlag |= 0x1 << kIRC;
			else if (TString(detector).CompareTo("L0", TString::kIgnoreCase) == 0)
				fDetFlag |= 0x1 << kL0TP;
			else if (TString(detector).CompareTo("L1", TString::kIgnoreCase) == 0)
				fDetFlag |= 0x1 << kL1TP;
			else if (TString(detector).CompareTo("LKr", TString::kIgnoreCase) == 0)
				fDetFlag |= 0x1 << kLKr;
			else if (TString(detector).CompareTo("MUV0", TString::kIgnoreCase) == 0)
				fDetFlag |= 0x1 << kMUV0;
			else if (TString(detector).CompareTo("MUV1", TString::kIgnoreCase) == 0)
				fDetFlag |= 0x1 << kMUV1;
			else if (TString(detector).CompareTo("MUV2", TString::kIgnoreCase) == 0)
				fDetFlag |= 0x1 << kMUV2;
			else if (TString(detector).CompareTo("MUV3", TString::kIgnoreCase) == 0)
				fDetFlag |= 0x1 << kMUV3;
			else if (TString(detector).CompareTo("SAC", TString::kIgnoreCase) == 0)
				fDetFlag |= 0x1 << kSAC;
			else if (TString(detector).CompareTo("SAV", TString::kIgnoreCase) == 0)
				fDetFlag |= 0x1 << kSAV;
			else if (TString(detector).CompareTo("HAC", TString::kIgnoreCase) == 0)
				fDetFlag |= 0x1 << kHAC;
			if(fDetFlag!=oldfDetFlag)
				GetStream() << "(Ok)";
			else
				GetStream() << "(Fail)";
			checkedSystems.erase(detector);
		}
	}
	GetStream() << std::endl;
	std::cout << "[EventChecker] Systems checked:";
	for(auto systemName : checkedSystems)
		GetStream() << " " << systemName;
	GetStream() << std::endl;
	if(Configuration::ConfigSettings::CLI::fInvertBadEvent)
		std::cout << "[EventChecker] Inverting output logic" << std::endl;

	//These should be always good
	//fEnabledDetFlag |= 0x1 << kL0TP;
	//fEnabledDetFlag |= 0x1 << kL1TP;
	//fEnabledDetFlag |= 0x1 << kL2EB;

	TString names[23] =
	  {"Total", "Dummy", "Cedar", "GigaTracker", "CHANTI", "LAV",
	   "Spectrometer", "CHOD", "RICH", "IRC", "LKr", "MUV1", "MUV2", "MUV3",
	   "SAC", "NewCHOD", "HAC", "L0TP", "L1TP", "L2EB", "DIM", "MUV0", "SAV"};
	fFailedEventsMasked = new TH1I
	  ("BadEventsMonitorMasked", "Events removed by bad event checks", 23, 0, 23);
        fFailedEventsTotal = new TH1I
	  ("BadEventsMonitorTotal", "Bad event monitor: all events", 23, 0, 23);
	for (Int_t i=0; i<23; i++) {
	  fFailedEventsMasked->GetXaxis()->SetBinLabel(i+1, names[i]);
	  fFailedEventsTotal-> GetXaxis()->SetBinLabel(i+1, names[i]);
	}
}

EventChecker::~EventChecker() {
	/// \MemberDescr
	/// Default destructor
	/// \EndMemberDescr

	fFailedEventsMasked->Write();
	fFailedEventsTotal->Write();
}

bool EventChecker::IsGoodEvent(EventHeader *header) {
	/// \MemberDescr
	/// \param header: Pointer to EventHeader
	/// \return True if the event is good for processing (no bad quality bit at all, or no bad quality bit for the checked detectors)
	/// \EndMemberDescr
	if (header == nullptr)
		return true;
	//The following line are enabled only to check the flag is good for used detectors
	//NARKD-464 requests that the check should fail if any detector is in error, not only the used ones.
	if (header->GetRunID() < 4174 && fRevision == "r1411") { // Check on LKr disabled for 2015 data with r1411 (LKr flag not set properly, fixed in r1433)
	//fEnabledDetFlag &= ~(0x1<<kLKr);
		fDetFlag |= 0x1 << kLKr;
	}
	if (header->GetRunID() < 6278) { // Check on GTK disabled for all the runs before sample A in 2016
		//fEnabledDetFlag &= ~(0x1<<kGigaTracker);
		fDetFlag |= 0x1 << kGigaTracker;
	}
	//return (header->GetEventQualityMask()&fEnabledDetFlag)==0;

	fFailedEventsMasked->Fill(0);
	fFailedEventsTotal->Fill(0);
	for(int bit=0; bit<21; ++bit){
		if(header->GetEventQualityMask() & (0x1<<bit))
			fFailedEventsTotal->Fill(bit+1);
	}

	//Instead, following NARKD-464, return true if the whole word is 0, or if it is 0 ignoring the detectors specified in fEnabledDetFlag
	//The meaning of the flag changes wrt to the previous case (list of detectors to check). Here this is the list of detectors to ignore.
	bool isGood = (header->GetEventQualityMask() & (~fDetFlag)) == 0;

	if(isGood) return true;
	for(int bit=0; bit<21; ++bit){
		if(header->GetEventQualityMask() & (~fDetFlag & 0x1<<bit))
			fFailedEventsMasked->Fill(bit+1); // First bin is the total
	}

	return false;
}

bool EventChecker::BadQualityMask(EventHeader* header, DetectorID det) {
	/// \MemberDescr
	/// \param header: Pointer to EventHeader
	/// \param det: DetectorID of the detector to check
	/// \return True if the quality bit for the specified detector is bad.
	/// \EndMemberDescr
	if(header == nullptr)
		return false;
	return (header->GetEventQualityMask() & (0x1<<det)) != 0;
}


int EventChecker::GetTotalSkippedEvents() {
	return fFailedEventsMasked->Integral(2, 23);
}
} /* namespace NA62Analysis */
