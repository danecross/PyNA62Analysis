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
#include <TLorentzVector.h>
#include <TVector3.h>
#include <TString.h>
#include <TCanvas.h>

using namespace std;

class MatchingRG : public Algorithm {
public:
  MatchingRG(BaseAnalysis *ba, Analyzer *ana, const string &name = "MatchingRG");
  virtual ~MatchingRG();
  void InitForProcess(TString);
  void InitForFinalSelection(TString);
  void Process(TRecoGigaTrackerEvent*, TRecoSpectrometerCandidate*, double, double, double, bool, TString);
  void FinalSelection(double, double, bool, TString);
  std::vector<int> GetMatchedGTKIDs(){return fGTKID;};
  std::vector<double> GetGTKTimes(){return fGTKTime;};
  std::vector<TVector3> GetVertices(){return fVertex;};
  std::vector<double> GetCDA(){return fDistAtVertex;};
  std::vector<TVector3> GetTrackMomentaAtVertices(){return fTrackMomentum;};
  std::vector<TVector3> GetGTKMomentaAtVertices(){return fGTKMomentum;};
  std::vector<TVector3> GetTrackPositionsAtVertices(){return fTrackPosition;};
  std::vector<TVector3> GetGTKPositionsAtVertices(){return fGTKPosition;};
  std::vector<double> GetMatchingQuality1(){return fMatchingQuality1;};
  std::vector<double> GetMatchingQuality2(){return fMatchingQuality2;};
  std::vector<double> GetMatchingQualityP(){return fMatchingQualityP;};
  std::vector<std::pair<double, double>> GetMatchingParts1(){return fMatchingParts1;};
  std::vector<std::pair<double, double>> GetMatchingParts2(){return fMatchingParts2;};
  std::vector<std::pair<double, double>> GetMatchingPartsP(){return fMatchingPartsP;};
  int GetBeamParticlesInTime(){return fBeamParticlesInTime;};

  void SetMatchedGTKIDs(const std::vector<int> &p){fGTKID = p;};
  void SetGTKTimes(const std::vector<double> &p){fGTKTime = p;};
  void SetVertices(const std::vector<TVector3> &p){fVertex = p;};
  void SetCDA(const std::vector<double> &p){fDistAtVertex = p;};
  void SetTrackMomentaAtVertices(const std::vector<TVector3> &p){fTrackMomentum = p;};
  void SetGTKMomentaAtVertices(const std::vector<TVector3> &p){fGTKMomentum = p;};
  void SetTrackPositionsAtVertices(const std::vector<TVector3> &p){fTrackPosition = p;};
  void SetGTKPositionsAtVertices(const std::vector<TVector3> &p){fGTKPosition = p;};
  void SetMatchingQuality1(const std::vector<double> &q){fMatchingQuality1 = q;};
  void SetMatchingQuality2(const std::vector<double> &q){fMatchingQuality2 = q;};
  void SetMatchingQualityP(const std::vector<double> &q){fMatchingQualityP = q;};
  void GetMatchingParts1(const std::vector<std::pair<double, double>> &p){fMatchingParts1 = p;};
  void GetMatchingParts2(const std::vector<std::pair<double, double>> &p){fMatchingParts2 = p;};
  void GetMatchingPartsP(const std::vector<std::pair<double, double>> &p){fMatchingPartsP = p;};
  void GetBeamParticlesInTime(int n){fBeamParticlesInTime = n;};

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
  std::vector<double> fDistAtVertex;
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
  int fBeamParticlesInTime;
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
};
#endif
