#ifndef FAKETRACKSELECTION_HH
#define FAKETRACKSELECTION_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"

class TRecoSpectrometerEvent;

class FakeTrackSelection : public NA62Analysis::Analyzer {
public:
  FakeTrackSelection(NA62Analysis::Core::BaseAnalysis *ba);
  ~FakeTrackSelection();
  void InitHist();
  void InitOutput();
  void DefineMCSimple();
  void ProcessSpecialTriggerUser(int iEvent, unsigned int triggerType);
  void Process(int);
  void PostProcess();
  void StartOfBurstUser();
  void EndOfBurstUser();
  void StartOfRunUser();
  void EndOfRunUser();
  void EndOfJobUser();
  void DrawPlot();
  void PrepareOutputs(int);
  void ValidateOutputs();
  bool has_common_hit(int);

protected:
  TRecoSpectrometerEvent *STRAWEvent;
  bool fReadingData;

  //output
  std::vector<bool> fFakeTracks;

  //parameters
  int fCutMinNSTRAWChambers;
  double fCutChi2;
  bool verb;
};
#endif
