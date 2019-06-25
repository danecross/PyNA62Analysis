// ---------------------------------------------------------------
//
// History:
//
// Created by Nicolas Lurkin (nicolas.lurkin@cern.ch) 2017-10-14
//
// ---------------------------------------------------------------
#ifndef EVENTEXTRACTOR_HH
#define EVENTEXTRACTOR_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"

struct EventID{
    EventID() : runID(), burstID(), timestamp() {}
    EventID(int r, int b, int t): runID(r), burstID(b), timestamp(t) {}
    int runID, burstID, timestamp;
};

bool operator<(const EventID &lhs, const EventID &rhs);
bool operator==(const EventID &lhs, const EventID &rhs);

class EventExtractor : public NA62Analysis::Analyzer
{
public:
    explicit EventExtractor(NA62Analysis::Core::BaseAnalysis *ba);
    ~EventExtractor();
    void InitHist();
    void InitOutput() {}
    void DefineMCSimple() {}
    void ProcessSpecialTriggerUser(int , unsigned int ) {};
    void Process(int iEvent);
    void PostProcess() {}
    void StartOfBurstUser() {}
    void EndOfBurstUser() {}
    void StartOfRunUser() {}
    void EndOfRunUser() {}
    void EndOfJobUser() {}
    void DrawPlot() {}
protected:
    TString fFilterListFile;
    std::vector<EventID> fFilterList;

};
#endif
