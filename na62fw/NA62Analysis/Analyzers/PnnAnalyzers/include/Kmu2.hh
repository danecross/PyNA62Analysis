#ifndef KMU2_HH
#define KMU2_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include "DownstreamTrack.hh"
#include "LKrAuxClusterReco.hh"
#include "SpectrometerRICHAssociationOutput.hh"
#include "SpectrometerLKrAssociationOutput.hh"
#include "SpectrometerMUV3AssociationOutput.hh"
#include "StrawSegmentAlgorithm.hh"
#include "TRecoSpectrometerCandidate.hh"
#include "CalorimeterCluster.hh"
#include "TwoLinesCDA.hh"
#include <TCanvas.h>

class Kmu2 : public NA62Analysis::Analyzer
{
public:
  Kmu2(NA62Analysis::Core::BaseAnalysis *ba);
  ~Kmu2();
  void InitHist();
  void InitOutput();
  void DefineMCSimple();
  void Process(int iEvent);
  void StartOfBurstUser();
  void EndOfBurstUser();
  void StartOfRunUser();
  void EndOfRunUser();
  void EndOfJobUser();
  void PostProcess();
  void DrawPlot();
  void PrepareOutputs();
  void ValidateOutputs();
  bool isMuon(double, double);

private:
  bool fReadingData;
  TwoLinesCDA fTwoLinesCDA;

  double fMuonProb;
  double fPionProb;
  double fLKrEnergy;
  double fTotalEnergy;
  double fDMIP;
  double fExtraMUV1Energy;
  double fExtraMUV2Energy;
  double fMUV1Energy;
  double fMUV2Energy;
  double fLKrSeedEnergy;
  double fLKrNCells;
  SpectrometerRICHAssociationOutputSingleRing *fSpecRICHsr;
  SpectrometerMUV3AssociationOutput *fSpecMUV3;
  double fZMUV3;
  TRecoMUV3Event *fMUV3Event;
  std::unique_ptr<StrawSegmentAlgorithm> fSegmentsAlgo;

  //parameters
  bool UseGTK;
  double fCutTrackMomMin;
  double fCutTrackMomMax;
  double fCutMatchedGTKQuality;
  double fCutEoP;
  double fCutTimeDiffMUV3;
  double fCutDMIP;
  double fMaxTotalExtraEnergy;
  double fCutMuonProbability;

  bool fEventSelected;
  TVector3 fKmu2NomKaonMom;
  TVector3 fKmu2NomTrackMom;
  TVector3 fKmu2KaonMom;
  TVector3 fKmu2TrackMom;
  int fKmu2KaonID;
  int fKmu2TrackID;
  double fKmu2RICHMomentum;
};
#endif
