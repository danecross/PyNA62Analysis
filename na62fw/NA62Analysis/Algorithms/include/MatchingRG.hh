#ifndef MATCHINGRG_HH
#define MATCHINGRG_HH

#include <stdlib.h>
#include <vector>
#include "Algorithm.hh"
#include "SpectrometerNewCHODAssociationOutput.hh"
#include "SpectrometerRICHAssociationOutput.hh"
#include "TRecoSpectrometerCandidate.hh"
#include "TRecoGigaTrackerCandidate.hh"
#include "TRecoGigaTrackerEvent.hh"
#include "TwoLinesCDA.hh"
#include "BlueTubeTracker.hh"
#include <TLorentzVector.h>
#include <TVector3.h>
#include <TString.h>
#include <TCanvas.h>

using namespace std;

class MatchingRG : public Algorithm {
public:
  MatchingRG(BaseAnalysis *ba, Analyzer *ana, const string &name = "MatchingRG");
  virtual ~MatchingRG();
  void Init(TString);
  void Process(TRecoGigaTrackerEvent*, TRecoSpectrometerCandidate*, double, double, double, bool, TString);
  std::vector<int> GetMatchedGTKIDs();
  std::vector<double> GetGTKTimes();
  std::vector<TVector3> GetVertices();
  std::vector<TVector3> GetTrackMomentaAtVertices();
  std::vector<TVector3> GetGTKMomentaAtVertices();
  std::vector<TVector3> GetTrackPositionsAtVertices();
  std::vector<TVector3> GetGTKPositionsAtVertices();
  std::vector<double> GetMatchingQuality1();
  std::vector<double> GetMatchingQuality2();
  std::vector<double> GetMatchingQualityP();
  std::vector<std::pair<double, double>> GetMatchingParts1();
  std::vector<std::pair<double, double>> GetMatchingParts2();
  std::vector<std::pair<double, double>> GetMatchingPartsP();
  TVector3 MomAfterKick(TVector3, double);
  TVector3 GetVertex(TVector3, TVector3, TVector3, TVector3);
  void ApplyBlueTube(int, TVector3, TVector3, double, TVector3*, TVector3*);
  bool IsBeamParticle(TRecoGigaTrackerCandidate*, bool, TString);
  void FindBestGTK(TRecoGigaTrackerCandidate*, TRecoSpectrometerCandidate*, double, double, int, bool, TString);
  void FindBestPU(std::vector<int>, std::vector<double>, int, std::vector<int>&, std::vector<double>&);
  void SetMinD(double);
  void SetMinD1(double);
  void SetMinD2(double);
  void PrepareDefaultOutputs();
  void RestoreDefaultOutputs();
  void EraseDefaultOutputs();
  bool AreSimilar(int, int, bool, TString);
  void DiscriminantNormalization();
  void EvaluateDiscriminant(double, double, double, double&, double&, double&, double&, double&, double&, double&, double&, double&, double&, double&, double&, double&, double&, double&);
  double EvaluateCondition(double, double, double);

private:
  TF1 *fCDA;
  TF1 *fDT1;
  TF1 *fDT2;
  TF1 *fCDA_p;
  TF1 *fDT_p;
  int fMaxNInTimeWithKTAG;
  double fCutSimilarRatioCDA;
  double fCutSimilarRatioDT;
  double fChi2;
  double fDeltaT;
  double fDeltaD1;
  double fMinD;
  double fMinD1;
  double fMinD2;
  double fMaxChi2Event;
  double fMaxDeltaT;
  double fMaxCDA;

  std::vector<int> fGTKID;
  std::vector<double> fGTKTime;
  std::vector<TVector3> fVertex;
  std::vector<TVector3> fTrackMomentum;
  std::vector<TVector3> fGTKMomentum;
  std::vector<TVector3> fTrackPosition;
  std::vector<TVector3> fGTKPosition;
  std::vector<double> fMatchingQuality1;
  std::vector<double> fMatchingQuality2;
  std::vector<double> fMatchingQualityP;
  std::vector<std::pair<double, double>> fMatchingParts1;
  std::vector<std::pair<double, double>> fMatchingParts2;
  std::vector<std::pair<double, double>> fMatchingPartsP;
  std::vector<double> fPDFKaonCDA;
  std::vector<double> fPDFPileupCDA;
  std::vector<double> fPDFKaonDT1;
  std::vector<double> fPDFKaonDT2;
  std::vector<double> fPDFPileupDT;
  double fIntPDFKaonCDA;
  double fIntPDFPileupCDA;
  double fIntPDFKaonDT1;
  double fIntPDFKaonDT2;
  double fIntPDFPileupDT;

  TwoLinesCDA fTwoLinesCDA;
  BlueTubeTracker *ftracker;
};
#endif
