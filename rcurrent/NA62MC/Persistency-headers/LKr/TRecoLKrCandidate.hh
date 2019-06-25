#ifndef TRecoLKrCandidate_H
#define TRecoLKrCandidate_H

#include "TRecoVCandidate.hh"
#include "TClonesArray.h"
#include "TRecoLKrHit.hh"
#include "TLKrHit.hh"

class TRecoLKrCandidate : public TRecoVCandidate {

public:

  TRecoLKrCandidate();
  ~TRecoLKrCandidate(){}

  void Clear(Option_t* = "");

  Int_t GetId() const { return fId; }
  void SetId(Int_t val) { fId=val; }
  Int_t GetNCells() const { return fNCells; }
  void SetNCells(Int_t val) { fNCells=val; }
  Int_t GetIdSeed() const { return fIdSeed; }
  void SetIdSeed(Int_t val) { fIdSeed=val; }
  Double_t GetClusterEnergy() const { return fClusterEnergy; }
  void SetClusterEnergy(Double_t val) { fClusterEnergy=val; }
  Double_t GetClusterEnergyError() const { return fClusterEnergyError; }
  void SetClusterEnergyError(Double_t val) { fClusterEnergyError=val; }
  Int_t GetClusterStatus() const { return fClusterStatus; }
  void SetClusterStatus(Int_t val) { fClusterStatus=val; }
  Double_t GetClusterX() const { return fClusterX; }
  void SetClusterX(Double_t val) { fClusterX=val; }
  Double_t GetClusterY() const { return fClusterY; }
  void SetClusterY(Double_t val) { fClusterY=val; }
  Double_t GetClusterRMSX() const { return fClusterRMSX; }
  void SetClusterRMSX(Double_t val) { fClusterRMSX=val; }
  Double_t GetClusterRMSY() const { return fClusterRMSY; }
  void SetClusterRMSY(Double_t val) { fClusterRMSY=val; }
  Double_t GetClusterTime() const { return fClusterTime; }
  void SetClusterTime(Double_t val) { fClusterTime=val; }
  Double_t GetClusterChi2RMS() const { return fClusterChi2RMS; }
  void SetClusterChi2RMS(Double_t val) { fClusterChi2RMS=val; }
  Double_t GetClusterTimeLateralCell() const { return fClusterTimeLateralCell; }
  void SetClusterTimeLateralCell(Double_t val) { fClusterTimeLateralCell=val; }
  Double_t GetClusterDDeadCell() const { return fClusterDDeadCell; }
  void SetClusterDDeadCell(Double_t val) { fClusterDDeadCell=val; }
  Double_t GetClusterUEnergy() const { return fClusterUEnergy; }
  void SetClusterUEnergy(Double_t val) { fClusterUEnergy=val; }
  Double_t GetClusterEnoise() const { return fClusterEnoise; }
  void SetClusterEnoise(Double_t val) { fClusterEnoise=val; }
  Double_t GetCluster77Energy() const { return fCluster77Energy; }
  void SetCluster77Energy(Double_t val) { fCluster77Energy=val; }
  Double_t GetSpaceChargeCorr() const { return fSpaceChargeCorr; }
  void SetSpaceChargeCorr(Double_t val) { fSpaceChargeCorr=val; }
  Double_t GetClusterKe3Energy() const { return fClusterKe3Energy; }
  void SetClusterKe3Energy(Double_t val) { fClusterKe3Energy=val; }
  Double_t GetCluster77Enoise() const { return fCluster77Enoise; }
  void SetCluster77Enoise(Double_t val) { fCluster77Enoise=val; }
  Double_t GetClusterUTime() const { return fClusterUTime; }
  void SetClusterUTime(Double_t val) { fClusterUTime=val; }
  Int_t GetN77Cells() const { return fN77Cells; }
  void SetN77Cells(Int_t val) { fN77Cells=val; }
  Int_t GetId77Cell(Int_t i) const { return fId77Cell[i]; }
  void SetId77Cell(Int_t i,Int_t val) { fId77Cell[i]=val; }
  Int_t GetFlag77Cell(Int_t i) const { return fFlag77Cell[i]; }
  void SetFlag77Cell(Int_t i,Int_t val) { fFlag77Cell[i]=val; }
  Double_t GetEnergy77Cell(Int_t i) const { return fEnergy77Cell[i]; }
  void SetEnergy77Cell(Int_t i,Double_t val) { fEnergy77Cell[i]=val; }
  Double_t GetTime77Cell(Int_t i) const { return fTime77Cell[i]; }
  void SetTime77Cell(Int_t i,Double_t val) { fTime77Cell[i]=val; }
  Double_t GetClusterSeedEnergy() const { return fClusterSeedEnergy; }
  void SetClusterSeedEnergy(Double_t val) { fClusterSeedEnergy=val; }

private:
  Int_t    fId;
  Int_t    fNCells; 
  Int_t    fIdSeed; 
  Double_t fClusterEnergy;
  Double_t fClusterEnergyError;
  Int_t    fClusterStatus;
  Double_t fClusterX;
  Double_t fClusterY;
  Double_t fClusterRMSX;
  Double_t fClusterRMSY;
  Double_t fClusterTime;
  Double_t fClusterChi2RMS;
  Double_t fClusterTimeLateralCell;
  Double_t fClusterDDeadCell;
  Double_t fClusterUEnergy;
  Double_t fClusterEnoise;
  Double_t fCluster77Energy;
  Double_t fSpaceChargeCorr;
  Double_t fClusterKe3Energy;
  Double_t fCluster77Enoise;
  Double_t fClusterUTime;
  Int_t    fN77Cells;
  Int_t    fId77Cell[50];
  Int_t    fFlag77Cell[50];
  Double_t fEnergy77Cell[50];
  Double_t fTime77Cell[50];
  Double_t fClusterSeedEnergy;

  ClassDef(TRecoLKrCandidate,1);
};
#endif
