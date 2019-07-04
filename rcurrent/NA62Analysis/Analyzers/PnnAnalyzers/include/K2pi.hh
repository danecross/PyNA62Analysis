#ifndef K2PI_HH
#define K2PI_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include "SpectrometerRICHAssociationOutput.hh"
#include "SpectrometerLKrAssociationOutput.hh"
#include "SpectrometerMUV3AssociationOutput.hh"
#include "SpectrometerMUV12AssociationOutput.hh"
#include "CalorimeterCluster.hh"
#include "TwoLinesCDA.hh"
#include "DownstreamTrack.hh"
#include <TCanvas.h>
#include <TString.h>

class TRecoLKrEvent;
class TRecoMUV3Event;

class K2pi : public NA62Analysis::Analyzer
{
public:
  K2pi(NA62Analysis::Core::BaseAnalysis *ba);
  ~K2pi();
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
  bool isGoodExpectedPiPlus(TLorentzVector, TVector3, const std::pair<Int_t, Int_t>&);
  bool isPion(double, double);
  bool ExtraPhotonsInLKr(TRecoSpectrometerCandidate*, int, int, double);
  bool ArePhotonsInRICHAcceptance(const std::pair<Int_t, Int_t>&, TVector3);

private:
  bool fReadingData;

  TwoLinesCDA fTwoLinesCDA;
  double fZMUV3;
  TLorentzVector fKaonNom;
  double fZLKr;
  double fZRICHfront;
  double fZRICHback;
  TRecoLKrEvent *fLKrEvent;
  TRecoMUV3Event *fMUV3Event;

  //request outputs
  double fDMIP;
  double fMuonProb;
  double fPionProb;
  double fMUV1Energy;
  double fMUV2Energy;
  double fExtraMUV1Energy;
  double fExtraMUV2Energy;
  double fTotalEnergy;
  double fLKrSeedEnergy;
  double fLKrEnergy;
  TVector2 fLKrAssocPos;
  int fLKrNCells;
  SpectrometerRICHAssociationOutput *fSpecRICH;
  SpectrometerRICHAssociationOutputSingleRing *fSpecRICHsr;

  //output
  bool fEventSelected;
  TVector3 fK2piNomTrackMom;
  TVector3 fK2piNomKaonMom;
  TVector3 fK2piTrackMom;
  TVector3 fK2piKaonMom;
  int fK2piKaonID;
  int fK2piTrackID;
  double fK2piRICHMomentum;

  TVector3 fVertex;
  TLorentzVector fkaon;
  TLorentzVector fkaonN;
  TVector3 fposN;
  TLorentzVector fpion;
  TLorentzVector fpionR;
  TLorentzVector fPi0;

  //parameters
  bool UseGTK;
  double fCutTrackMomMin;
  double fCutTrackMomMax;
  double fCutMatchedGTKQuality;
  double fCutMinDiPionMass;
  double fCutMaxDiPionMass;
  double fOffsetLKrCandidateToExpPiPlus;
  double fOffsetLKrHitToExpPiPlus;
  double fCutTimeDiffPi0CHOD;
  double fCutEoP;
  double fOffsetLKrCandidateExtraPhotons;
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
  double fCutTimeDiffMUV3;
  double fMaxTotalExtraEnergy;
  double fCutRICHMaxLikelihood;
  double fCutMinRICHMass;
  double fCutMaxRICHMass;
  double fCutMinNeutralVertexZ;
  double fCutMaxNeutralVertexZ;
};
#endif
