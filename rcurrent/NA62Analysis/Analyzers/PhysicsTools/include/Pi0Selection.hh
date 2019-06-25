// ---------------------------------------------------------------
//
// History:
//
// Updated by Zuzana Kucerova (zuzana.kucerova@cern.ch) 2017-10-27
// Created by Karim Massri (karim.massri@cern.ch) 2016-11-15
//
// ---------------------------------------------------------------

#ifndef PI0SELECTION_HH
#define PI0SELECTION_HH

#include <stdlib.h>
#include <vector>
#include <utility>
#include "Analyzer.hh"
#include "TriggerConditions.hh"
#include "EnergyCluster.hh"
#include <TCanvas.h>
#include <TF1.h>

class TH1I;
class TH2F;
class TGraph;
class TTree;

struct Pi0SelectionOutput {
  TLorentzVector fMomentum; ///< Pi0 momentum = sum of 2 gamma candidate momenta
  Double_t fTime;           ///< Pi0 time = average time of LKr clusters
  TVector3 fPosition;       ///< Pi0 position = reconstructed neutral vertex position
  Int_t fGTKID;             ///< ID of the time-associated GTK track
  std::pair<TLorentzVector,TLorentzVector> fGammaMomenta; ///< Pair of reconstructed photon momenta forming the pi0 candidate
  std::pair<Int_t, Int_t> fClustersID;                    ///< Pair of photon cluster IDs
};

class Pi0Selection : public NA62Analysis::Analyzer {
public:
  explicit Pi0Selection(NA62Analysis::Core::BaseAnalysis *ba);
  ~Pi0Selection();
  void InitHist();
  void InitOutput();
  void DefineMCSimple(){};
  void Process(Int_t);
  void PostProcess();
  void StartOfBurstUser();
  void EndOfBurstUser();
  void StartOfRunUser();
  void EndOfRunUser();
  void EndOfJobUser();
  void DrawPlot();

  static TLorentzVector PhotonMomentum(const EnergyCluster& c1, const TVector3& vertex);
  static Double_t ComputeDiPhotonMass(const EnergyCluster& c1, const EnergyCluster& c2, const TVector3 vertex);

private:
  Int_t FindBestKTAG(Double_t);
  Int_t FindBestGTK(Int_t, Double_t);
  TVector3 Pi0Vertex(const EnergyCluster& c1, const EnergyCluster& c2, Int_t gtkID = -1);

  Bool_t fReadingData;          ///< Reading data or my own output?
  TriggerConditions *fTriggerConditions;
  Double_t fLKrStartPos;

  // Outputs
  Bool_t fEventSelected;
  std::vector<Pi0SelectionOutput> fSelectedPi0;
  std::vector<EnergyCluster> fEMClusters;

  // Parameters
  Bool_t   fUseGTK;
  Bool_t   fCheckSpectrometerAssociation;
  Bool_t   fCheckLKrAcceptance;
  Bool_t   fCheckIsolation;
  Double_t fCutMaxTimeDiffClusterTrigger;
  Double_t fCutMinDistToDeadCell;
  Double_t fCutMinClusterEnergy;
  Double_t fCutMaxClusterTimeDiff;
  Double_t fCutMinClusterDist;
  Double_t fCutMaxTotalEnergy;
  Double_t fCutMinTotalEnergy;
  Int_t    fCutMinNSectorsKTAG;
  Double_t fCutTimeDiffGTKKTAG;
  Double_t fCutMaxChi2GTKCandidate;
  Double_t fCutMaxVertexZ;
  Double_t fCutMinVertexZ;

  // static members: computation of vertex
  static TF1 fVertexZFunction;
  static Double_t VertexZFunctionFormula(Double_t*, Double_t*);
};
#endif
