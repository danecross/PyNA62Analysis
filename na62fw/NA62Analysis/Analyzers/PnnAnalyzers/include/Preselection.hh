#ifndef PRESELECTION_HH
#define PRESELECTION_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"

class Preselection : public NA62Analysis::Analyzer {
public:
  Preselection(NA62Analysis::Core::BaseAnalysis *ba);
  ~Preselection();
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
  void ValidateOutputs();

protected:
  bool fReadingData;
  bool fPreselectedEvent;
  int fMinCHODHits;
  int fMinLKrHits;
  int fMaxLKrHits;
  int fMinSTRAWCandidates;
  int fMaxSTRAWCandidates;
};
#endif
