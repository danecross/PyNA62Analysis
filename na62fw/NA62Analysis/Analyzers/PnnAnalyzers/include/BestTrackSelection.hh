#ifndef BESTTRACKSELECTION_HH
#define BESTTRACKSELECTION_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include "SpectrometerNewCHODAssociationOutput.hh"
#include "SpectrometerCHODAssociationOutput.hh"
#include "SpectrometerLKrAssociationOutput.hh"
#include "SpectrometerRICHAssociationOutput.hh"
#include "TRecoSpectrometerCandidate.hh"
#include "MatchingRG.hh"
#include "FakeTrackSelection.hh"
#include "GigaTrackerRecoAlgorithm.hh"
#include "BlueTubeTracker.hh"
#include <TCanvas.h>
#include <TVector3.h>
#include "TwoLinesCDA.hh"

class TRecoSpectrometerEvent;
class TRecoCHODEvent;
class TRecoLKrEvent;

class BestTrackSelection : public NA62Analysis::Analyzer
{
public:
  BestTrackSelection(NA62Analysis::Core::BaseAnalysis *ba);
  ~BestTrackSelection();
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
  void RestoreDefaultOutputs();
  std::pair<bool, bool> is_multi_track(int, std::vector<bool>&);
  bool AcceptanceOK(TRecoSpectrometerCandidate*);
  bool HasCHODAssoc(double, int, double&, int&);
  bool HasNewCHODAssoc(int, TVector2, double, double, double&, int&);
  bool HasLKrStandardAssoc(TVector2, double, double&, double&, double&, int&, TVector2&, int&);
  bool HasLKrCellAssoc(TVector2, double, double&, double&, double&, int&, TVector2&);

private:
  BlueTubeTracker *ftracker;
  TwoLinesCDA fTwoLinesCDA;
  TClonesArray *fSpecCalo;
  TRecoSpectrometerEvent *fSTRAWEvent;
  MatchingRG *fMatchingRG;
  GigaTrackerRecoAlgorithm *fGTKReco;
  FakeTrackSelection *fFakeTrackSelection;
  TRecoCHODEvent *fCHODEvent;
  TRecoLKrEvent *fLKrEvent;
  std::vector<SpectrometerNewCHODAssociationOutput> fSpecNewCHOD;
  std::vector<SpectrometerCHODAssociationOutput> fSpecCHOD;
  double tTrigger;
  bool fReadingData;

  //output
  std::vector<bool> fBestTracks;
  std::vector<double> fBestTrackTime;
  std::vector<double> fBestTrackSTRAWTime;
  std::vector<double> fBestTrackCHODTime;
  std::vector<double> fBestTrackNewCHODTime;
  std::vector<int> fCHODAssocID;
  std::vector<int> fNewCHODAssocID;
  std::vector<bool> fRICHStandardAssoc;
  std::vector<bool> fRICHSingleAssoc;
  std::vector<double> fBestTrackRICHTime;
  std::vector<bool> fLKrStandardAssoc;
  std::vector<int> fLKrAssocID;
  std::vector<bool> fLKrChargedAssoc;
  std::vector<double> fBestTrackLKrTime;
  std::vector<double> fLKrAssocEnergy;
  std::vector<double> fLKrAssocSeedEnergy;
  std::vector<int> fLKrAssocNCells;
  std::vector<TVector2> fLKrAssocPosition;
  std::vector<double> fBestTrackKTAGTime;
  std::vector<int> fGTKAssocID;
  std::vector<double> fBestTrackGTKTime;
  std::vector<TVector3> fGTKAssocVertex;
  std::vector<std::vector<TVector3>> fGTKAllAssocVertices;
  std::vector<double> fGTKAssocQuality1;
  std::vector<double> fGTKAssocQuality2;
  std::vector<TVector3> fGTKAssocMomentum;
  std::vector<TVector3> fGTKAssocTrackMomentum;
  std::vector<int> fGTKNAssocs;
  std::vector<TVector3> fNomVertex;
  std::vector<TVector3> fBestTrackNomMomentum;
  std::vector<TVector3> fNomKaonMomentum;

  //parameters
  bool UseGTK;
  int fCutNStrawChambers;
  double fCutTrackChi2;
  double fCutMomDiffBefAftFit;
  bool fWantNoMultitracks;
  bool fRejectBroadMultitracks;
  bool fRejectFullMultitracks;
  double fCutBroadMultitrackCDA;
  double fCutFullMultitrackCDA;
  double fCutMultitrackMinZ;
  double fCutMultitrackMaxZ;
  double fCutMultitrackTimeDiff;
  double fCutTimeDiffSTRAWCHOD;
  double fCHODAssocSigmaX;
  double fCHODAssocSigmaT;
  double fCutDCHOD;
  double fCutTimeDiffTriggerCHOD;
  double fCutTimeDiffNewCHODHitCHOD;
  double fSigmaPosNewCHOD;
  double fSigmaTimeNewCHOD;
  double fCutDiscriminantNewCHOD;
  double fCutTimeDiffCHODNewCHOD;
  double fCutTimeDiffRICHCHOD;
  double fOffsetLKrStandard;
  double fCutMaxDistLKrStandard;
  double fCutTimeDiffLKrStandardSTRAW;
  double fCutTimeDiffLKrStandardCHOD;
  double fOffsetLKrCell;
  double fCutMaxDistLKrCell;
  double fCutTimeDiffLKrCellSTRAW;
  double fCutMinEnergy;
  double fCutTimeDiffLKrClusterSTRAW;
  double fCutMaxDistLKr;
  double fCutTimeDiffLKrSTRAW;
  double fCutTimeDiffLKrCHOD;
  double fWeightCHOD;
  double fWeightLKr;
  double fWeightRICH;
  double fWeightSTRAW;
  int fCutMinNSectorsKTAG;
  double fCutTimeDiffCedarDownstreamData;
  double fCutTimeDiffCedarDownstreamMC;
};
#endif
