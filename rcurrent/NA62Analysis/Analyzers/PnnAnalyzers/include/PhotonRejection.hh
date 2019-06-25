#ifndef PHOTONREJECTION_HH
#define PHOTONREJECTION_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include "LKrAuxClusterReco.hh"

class PhotonRejection : public NA62Analysis::Analyzer {
public:
  PhotonRejection(NA62Analysis::Core::BaseAnalysis *ba);
  ~PhotonRejection();
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
  bool fPhotons;

  bool fReadingData;
  double fZLKr;

  //parameters
  bool verb;
  double fLAVMinTimeCut;
  double fLAVMaxTimeCut;
  double fIRCMinTimeCut;
  double fIRCMaxTimeCut;
  double fSACMinTimeCut;
  double fSACMaxTimeCut;
  double fIRCMinToT;
  double fIRCMaxToT;
  double fCutTimeDiffIRCTrack;
  double fSACMinToT;
  double fSACMaxToT;
  double fCutTimeDiffSACTrack1;
  double fCutTimeDiffSACTrack2;
  double fCutTimeDiffSACTrack3;
  double fCutTimeDiffSACTrackMin;
  double fCutTimeDiffSACTrackMax;
  double fCutMinEnergySAVFADCHit;
  double fCutTimeDiffSAVFADCHit;
};
#endif
