#ifndef PHOTONREJECTIONLKR_HH
#define PHOTONREJECTIONLKR_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include "LKrAuxClusterReco.hh"
#include <TCanvas.h>

class PhotonRejectionLKr : public NA62Analysis::Analyzer
{
public:
  PhotonRejectionLKr(NA62Analysis::Core::BaseAnalysis *ba);
  ~PhotonRejectionLKr();
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

  double fZLKr;
  LKrAuxClusterReco *fLKrAuxClusterReco;
  bool fReadingData;

  //parameters
  bool verb;
  double fCutMinDistLKrClusterTrack;
  double fOffsetLKrStandardCandidate;
  double fOffsetLKrAuxCandidate;
  double fLKrSigmaA;
  double fLKrSigmaB;
  double fLKrSigmaC;
  double fLKrClusterEnergy1;
  double fLKrClusterEnergy2;
  double fLKrClusterEnergy3;
  double fLKrClusterEnergy4;
  double fLKrClusterTimeDiff1;
  double fLKrClusterTimeDiff2;
  double fLKrClusterTimeDiffSigma1;
  double fLKrClusterTimeDiffSigma2;
  double fLKrClusterTimeDiffSigma3;
  double fLKrClusterTimeDiffSigma4;
  double fLKrClusterMinPosX;
  double fLKrClusterMaxPosX;
  double fLKrClusterMaxPosY;
};
#endif
