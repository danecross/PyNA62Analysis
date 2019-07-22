#ifndef CHECKTRIGGER_HH
#define CHECKTRIGGER_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TCanvas.h>

class CheckTrigger : public NA62Analysis::Analyzer
{
public:
  CheckTrigger(NA62Analysis::Core::BaseAnalysis *ba);
  ~CheckTrigger();
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
  void PrepareOutputs();
protected:
  bool fReadingData;

  unsigned int fTrigger_Pinunu;

  //output
  bool fEventPassedTrigger;
  double fTriggerTime;

  //parameters
  int fWhichTrigger;
};
#endif
