#ifndef KAONDECAYSELECTION_HH
#define KAONDECAYSELECTION_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TCanvas.h>

class TRecoGigaTrackerCandidate;

class KaonDecaySelection : public NA62Analysis::Analyzer
{
public:
  KaonDecaySelection(NA62Analysis::Core::BaseAnalysis *ba);
  ~KaonDecaySelection();
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
  bool IsGoodCandidate(TRecoGigaTrackerCandidate*);
protected:
  bool fReadingData;
  double fZGTK3;
  double fZSTRAW1;
  double fZTRIM5;
  double fZB6end;
  double fZB6start;
  double fZB5start;
  double fZB5end;
  double fB;
  double fTRIM5kick;
  double fTRIM5halfwidth;

  //output
  bool fDecaySelected;

  //parameters
  double fCutMinGTKMomentum;
  double fCutMaxGTKMomentum;
  double fMinSlopeX;
  double fMaxSlopeX;
  double fMinSlopeY;
  double fMaxSlopeY;
  double fMaxThetaCorr;
  double fCutMinXTrackAtGTK3;
  double fCutMinYTrackAtGTK3;
  double fCutMinDistTrackHitGTK3;
  double fSTRAW1CenterX;
  double fkMax1Rstraw1;
  double fqMax1Rstraw1;
  double fkMax2Rstraw1;
  double fqMax2Rstraw1;
  double fkMinRstraw1;
  double fqMinRstraw1;
  double fCutMaxVertexZ;
  double fCutToT;
  double fCutTimeDiffTrackCHANTI;
  double fCutAdditionalVertexMaxZ;
  double fCutAdditionalVertexMinZ;
  double fCutMaxXTrim5;
  double fCutMaxYTrim5;
  double fkMax3Rstraw1;
  double fqMax3Rstraw1;
  bool fUseCutRStraw1VsVertexZ;
  bool fUseCutTrim5;
  bool fUseCutToT;
  bool fUseCutChantiAssoc;
  bool fUseCutAdditionalVertex;
};
#endif
