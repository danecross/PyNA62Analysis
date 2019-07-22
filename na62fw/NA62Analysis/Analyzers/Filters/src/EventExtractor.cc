// ---------------------------------------------------------------
//
// History:
//
// Created by Nicolas Lurkin (nicolas.lurkin@cern.ch) 2017-10-14
//
// ---------------------------------------------------------------
#include "EventExtractor.hh"

#include <stdlib.h>
#include <iostream>

#include "EventHeader.hh"

bool operator<(const EventID &lhs, const EventID &rhs) {
	if(lhs.runID != rhs.runID) return lhs.runID < rhs.runID;
	else if(lhs.burstID != rhs.burstID) return lhs.burstID < rhs.burstID;
	else return lhs.timestamp < rhs.timestamp;
}

bool operator==(const EventID &lhs, const EventID &rhs) {
	if(lhs.runID != rhs.runID) return false;
	else if(lhs.burstID != rhs.burstID) return false;
	else return lhs.timestamp == rhs.timestamp;
}

/// \class eventExtractor
/// \Brief
/// Reads a list of events from the FilterList parameter and outputs a filtered file containing only the
/// specified events.
/// The FilterList parameter must point to a text file with one event per line in the following format:
/// runNumber burstNumber eventTimestamp
/// Lines starting with '#' are considered as comments and discarded.
/// \EndBrief
///
/// \Detailed
/// \EndDetailed

EventExtractor::EventExtractor(Core::BaseAnalysis *ba) : Analyzer(ba, "eventExtractor")
{
	RequestAllRecoTrees();
	RequestAllMCTrees();

	AddParam("FilterList", &fFilterListFile, "");
}

void EventExtractor::InitHist(){
    // Read the FilterList file
	std::string line;
	if(fFilterListFile.CompareTo("")!=0){
		std::ifstream fd(fFilterListFile.Data());
		while(std::getline(fd, line)){
			if(line.compare("#") == 0) continue;
			int runID, burstID, timestamp;
			std::istringstream iss(line);
			iss >> runID >> burstID >> timestamp;
			fFilterList.push_back(EventID(runID, burstID, timestamp));
		}
	}
}

void EventExtractor::Process(int ){
    // Retrieve the event info
	int runID = GetRunID();
	int burstID = GetBurstID();
	int timestamp = GetEventHeader()->GetTimeStamp();

	// Check if we have a corresponding event in the filter list and accept it
	auto it = std::find(fFilterList.begin(), fFilterList.end(), EventID(runID, burstID, timestamp));
	if(it!=fFilterList.end() || fFilterList.size()==0) FilterAccept();

}

EventExtractor::~EventExtractor(){
}
