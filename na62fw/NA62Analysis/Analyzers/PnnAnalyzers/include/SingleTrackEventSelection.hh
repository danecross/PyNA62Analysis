#ifndef SINGLETRACKEVENTSELECTION_HH
#define SINGLETRACKEVENTSELECTION_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include "GigaTrackerRecoAlgorithm.hh"
#include "MatchingRG.hh"

class SingleTrackEventSelection : public NA62Analysis::Analyzer {
public:
  SingleTrackEventSelection(NA62Analysis::Core::BaseAnalysis *ba);
  ~SingleTrackEventSelection();
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
  GigaTrackerRecoAlgorithm *fGTKReco;
  MatchingRG *fMatchingRG;
  bool fSelected;
  int fTrackID;
  double fTrackTime;
  int fTrackCHODAssocID;
  int fTrackNewCHODAssocID;
  double fTrackRICHAssocTime;
  int fTrackRICHsrAssocID;
  int fTrackLKrAssocID;
  int fTrackKTAGAssocID;
  int fTrackGTKAssocID;

  //parameters
  bool UseGTK;
  int fCutMaxGTKAssoc;
  int fCutMaxNTracks;
  double fCutMinCDA;
};
#endif
