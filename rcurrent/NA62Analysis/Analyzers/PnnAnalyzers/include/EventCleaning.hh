#ifndef EVENTCLEANING_HH
#define EVENTCLEANING_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"

class EventCleaning : public NA62Analysis::Analyzer {
public:
  EventCleaning(NA62Analysis::Core::BaseAnalysis *ba);
  ~EventCleaning();
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

  //output
  bool fGoodEvent;

  //parameters
  bool verb;
  int fSpecificCharge;
  double fCutSlopeDiffX;
  double fCutSlopeDiffY;
  double fCutPatternRecognQuality;
  int fCutMinNHits;
  int fCutMaxNHits;
  double fkMinX;
  double fqMinX;
  double fkMaxX;
  double fqMaxX;
  double fkMinY;
  double fqMinY;
  double fkMaxY;
  double fqMaxY;
  double fCutTimeDiffCHODGTK;
  double fCutTimeDiffKTAGGTK;
};
#endif
