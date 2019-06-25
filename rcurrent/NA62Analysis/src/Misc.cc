/*
 * Misc.cc
 *
 *  Created on: Mar 8, 2016
 *      Author: nlurkin
 */

#include "Misc.hh"

#include <cstdlib>
#include <fstream>
#include <TString.h>
#include <iostream>

#include <TFile.h>
#include <TTree.h>
#include "EventHeader.hh"

namespace NA62Analysis {

std::string GetNA62AnalysisPath() {
	char* val = std::getenv("ANALYSISFW_PATH");
	if (val == nullptr)
		return std::string(".");

	return std::string(val);
}

std::string GetUserAnalysisPath() {
	char* val = std::getenv("ANALYSISFW_USERDIR");
	if (val == nullptr)
		return std::string(".");

	return std::string(val);
}

std::pair<Double_t,Double_t> GetMemoryUsage() {
	/// \MemberDescr
	/// \return std:::pair<Double_t,Double_t> containing the virtual,resident memory usage
	/// in Mbytes.
	/// \MemberDescr
	std::ifstream file;
	file.open("/proc/self/stat");
	TString datum;
	double virtualMemory = 0.;
	double residentMemory = 0.;
	if (file.is_open()) {
		for (int i = 0; i < 23; i++)
			datum.ReadToDelim(file, ' ');
		//virtual memory is in bytes
		virtualMemory = datum.Atoll() / (1024. * 1024.);
		datum.ReadToDelim(file, ' ');
		//resident memory is in pages (4kb each)
		residentMemory = datum.Atoll()*4 / (1024.);
	}
	file.close();
	return std::make_pair(virtualMemory, residentMemory);
}

bool GetValueFromString(std::string s, int& ref) {
	/// \MemberDescr
	/// \param s string
	/// \param ref Variable to fill with the value found in the string
	/// \return true in case of success.
	///
	/// Fill the variable passed by reference with the value found in the string
	/// \MemberDescr
	char* endptr;
	int val = strtol(s.data(), &endptr, 0);
	if (!*endptr) {
		ref = val;
		return true;
	}
	return false;
}

bool GetValueFromString(std::string s, long& ref) {
	/// \MemberDescr
	/// \param s string
	/// \param ref Variable to fill with the value found in the string
	/// \return true in case of success.
	///
	/// Fill the variable passed by reference with the value found in the string
	/// \MemberDescr
	char* endptr;
	unsigned int val = strtoul(s.data(), &endptr, 0);
	if (!*endptr) {
		ref = val;
		return true;
	}
	return false;
}

bool GetValueFromString(std::string s, float& ref) {
	/// \MemberDescr
	/// \param s string
	/// \param ref Variable to fill with the value found in the string
	/// \return true in case of success.
	///
	/// Fill the variable passed by reference with the value found in the string
	/// \MemberDescr
	char* endptr;
	double val = strtof(s.data(), &endptr);
	if (!*endptr) {
		ref = val;
		return true;
	}
	return false;
}

bool GetValueFromString(std::string s, double& ref) {
	/// \MemberDescr
	/// \param s string
	/// \param ref Variable to fill with the value found in the string
	/// \return true in case of success.
	///
	/// Fill the variable passed by reference with the value found in the string
	/// \MemberDescr
	char* endptr;
	double val = strtod(s.data(), &endptr);
	if (!*endptr) {
		ref = val;
		return true;
	}
	return false;
}

bool GetCurrentBurstInfo(TFile* fd, struct BurstID &b){
	/// \MemberDescr
	/// \param fd : File descriptor of an open and readable file where we look for burst info
	/// \param b : reference to a BurstID struct which is going to be filled with the burst info (if found)
	/// \return true in case of success.
	///
	/// Fill the variable passed by reference with the burst info found in the EventHeader
	/// \MemberDescr
	TTree *rawHeaderTree = static_cast<TTree*>(fd->Get("Reco"));
	if(!rawHeaderTree) return false;
	b.fNEventsInTree = rawHeaderTree->GetEntries();
	if(rawHeaderTree->GetEntries()==0) return false;

	EventHeader *rh = new EventHeader();
	rawHeaderTree->SetBranchAddress("EventHeader", &rh);
	rawHeaderTree->GetEntry(0);

	b.fBurstID = rh->GetBurstID();
	b.fBurstTime= rh->GetBurstTime();
	b.fRunID = rh->GetRunID();

	delete rh;
	return true;
}

} /* namespace NA62Analysis */
