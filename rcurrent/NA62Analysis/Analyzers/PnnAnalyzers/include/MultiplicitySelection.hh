#ifndef MULTIPLICITYSELECTION_HH
#define MULTIPLICITYSELECTION_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"

class TRecoCHODEvent;

class MultiplicitySelection : public NA62Analysis::Analyzer {
public:
  MultiplicitySelection(NA62Analysis::Core::BaseAnalysis *ba);
  ~MultiplicitySelection();
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
  bool MergedLikeLKrCluster(double, TVector3);
  void LKrExtraActivity(std::vector<int>&, double, TVector3);
  void NewCHODExtraActivity(std::vector<int>&, double, int);

  double fZLKr;
  double fZNewCHOD;
  double fZCHODH;
  double fZCHODV;
  double fSlabWidth[128];
  double fLV[128];
  double fSlewSlope[128][16];
  double fSlewConst[128][16];
  double fSlabCenter[128]; // x coordinate for 0<=i<64, y coordinate for 64<=i<128
  double fOffsetX;
  double fOffsetY;

  TRecoCHODEvent *fCHODEvent;
  int fLKrID;
  bool fReadingData;

  //output
  bool fMultiplicity;

  //parameters
  int fMaxNCHODSlabs;
  double fCutLKrEnergy;
  double fCutTimeDiffTrackHAC;
  double fCutMaxTimeDiffTrackMUV0;
  double fCutMinTimeDiffTrackMUV0;
  bool verb;
  double fCutMaxDistanceCHODLKrCoincidence;
  double fCutTimeDiffCHODLKrCoincidence;
  double fCHODSlabHalfWidth;
  double fCutTimeDiffCHODNewCHODCoincidence;
  double fCutMaxXDistanceCHODNewCHODCoincidence;
  double fCutMaxYDistanceCHODNewCHODCoincidence;
  double fCutMaxXDistanceNewCHODLKrCoincidence;
  double fCutMaxYDistanceNewCHODLKrCoincidence;
  double fCutTimeDiffTrackCHODSlab;
  double fCutMinDistanceLKrTrackMergedLikeLKrCluster;
  double fCutMaxDistanceLKrTrackMergedLikeLKrCluster;
  double fCutTimeDiffTrackLKrMergedLikeLKrCluster;
  double fCutEnergyLKrExtra;
  double fCutLowEnergyLKrExtra;
  double fCutHighEnergyLKrExtra;
  double fCutTimeDiffForLowEnergyLKrExtra;
  double fCutMinTimeDiffForMiddleEnergyLKrExtra;
  double fCutMaxTimeDiffForMiddleEnergyLKrExtra;
  double fCutTimeDiffForHighEnergyLKrExtra;
  double fCutMinDistanceLKrExtra;
  double fCutMinDistanceNewCHODExtra;
  double fCutTimeDiffNewCHODExtra;

};
#endif
