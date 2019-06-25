// ---------------------------------------------------------
//
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2019-03-25
//
// ---------------------------------------------------------

#ifndef MCEOPDEBUGGER_HH
#define MCEOPDEBUGGER_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"

class TH1I;
class TH2F;
class TGraph;
class TTree;


class MCEopDebugger : public NA62Analysis::Analyzer
{
  public:
    explicit MCEopDebugger(NA62Analysis::Core::BaseAnalysis *ba);
    ~MCEopDebugger();
    void InitHist();
    void InitOutput();
    void DefineMCSimple();
    void ProcessSpecialTriggerUser(int iEvent, unsigned int triggerType);
    void Process(int iEvent);
    void PostProcess();
    void StartOfBurstUser();
    void EndOfBurstUser();
    void StartOfRunUser();
    void EndOfRunUser();
    void EndOfJobUser();
    void DrawPlot();
  protected:


};
#endif
