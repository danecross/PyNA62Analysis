#ifndef ViewHitCollector_H
#define ViewHitCollector_H 1

#include <vector>

#include "TH1F.h"
#include "TH2F.h"
#include "TMath.h"
#include "TString.h"

#include "SpectrometerGeometry.hh"
#include "SpectrometerParameters.hh"
#include "StrawHitCollector.hh"
#include "Straw.hh"
#include "Cluster.hh"
#include "TRecoSpectrometerEvent.hh"

class ViewHitCollector
{
public:
  ViewHitCollector(Int_t chamberID, Int_t viewID);
  virtual ~ViewHitCollector();

  void InitHistograms(TString subDir);
  void SaveHistograms(TString subDir);

  void AddHit(TRecoSpectrometerHit*,const Double_t&,const Double_t&,const Double_t&,const Int_t&);
  void AddCluster(Int_t,TRecoSpectrometerHit**,Double_t*);

  StrawHitCollector *GetPlane(Int_t jPlane) {return fPlaneHitCollector.at(jPlane);}
  Int_t GetNHit() {return (Int_t)fHitView.size();}
  TRecoSpectrometerHit *GetHit(Int_t j) {return fHitView.at(j);}
  Int_t GetNcluster() {return (Int_t)fcluster.size();}
  Cluster *Getcluster(Int_t j)  {return fcluster.at(j);}

  Int_t GetChamberID() {return fChamberID;};
  void  SetChamberID(Int_t val) {fChamberID = val;};
  Int_t GetViewID() {return fViewID;};
  void  SetViewID(Int_t val) {fViewID = val;};

  void Reset();

  void ReconstructHitPositions(TRecoSpectrometerEvent *event);
  Double_t ChooseTheCombination(Straw**,Double_t*,Double_t*,Double_t*);
  Double_t Chi2LinearFit(Double_t *, Double_t *, Double_t *, Double_t *);

  Double_t SigmaRadius(Double_t);
  Bool_t Pairing(Double_t *, Int_t *, Int_t);

  std::vector<Int_t> fHitFlag;

private:
  SpectrometerParameters *fPar;
  SpectrometerGeometry *fGeo;
  std::vector<StrawHitCollector *> fPlaneHitCollector; ///< Vector of pointers to the collection of reconstructed tube-hits in the view-planes.
  std::vector<TRecoSpectrometerHit*> fHitView; ///< vector of saved TRecoSpectrometerHit hits.
  std::vector<Cluster *> fcluster; ///< vector of saved clusters.
  Int_t fChamberID; ///< ID of the chamber (0-3)
  Int_t fViewID;    ///< ID of the view in the chamber (0-3)

  // Histograms created created for each view hit collector
  TH1F *fHTTime3;
  TH1F *fHTTime2;
  TH1F *fHTTime;
  TH1F *fProva[4][4];
  TH2F *fHIDvsID[4][4];
  TH1F *fHPairTime[4][4];
  TH1F *fHPairRadius[4][4];
  TH1F *fHTripleTime[4];
  TH1F *fHTripleRadius[4];
  TH2F *fHTripletChi2;
  TH2F *fHTripleSlope;
  TH2F *fHDoubleRR01_3;
  TH2F *fHDoubleRR02_3;
  TH2F *fHDoubleRR03_3;
  TH2F *fHDoubleRR12_3;
  TH2F *fHDoubleRR13_3;
  TH2F *fHDoubleRR23_3;
  TH1F *fHYCoorTot3;
  TH1F *fHYCoorTot2;
  TH1F *fHYCoorTot;
  TH2F *fHDoubleRR01;
  TH2F *fHDoubleRR02;
  TH2F *fHDoubleRR03;
  TH2F *fHDoubleRR12;
  TH2F *fHDoubleRR13;
  TH2F *fHDoubleRR23;
  // Global reconstruction histograms pointers, created in SpectrometerReconstruction, filled in ViewHitCollector::ReconstructHitPositions
  TH1F *fRecoHitWireSum2;
  TH2F *fRecoHitWireSum22;
  TH2F *fRecoHitWireSum23;
  TH2F *fRecoHitWireSum24;
  TH2F *fRecoHitWireSum25;
  TH2F *fHSlope;
  TH2F *fRecoHitWireSum2VsMagicT0;
  TH2F *fRecoHitWireSum2VsROMezzanine;

};
#endif
