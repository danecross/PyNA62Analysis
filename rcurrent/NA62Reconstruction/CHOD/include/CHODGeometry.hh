#ifndef CHODGeometry_H
#define CHODGeometry_H 1
#include "TROOT.h"
#include "TVector3.h"

class CHODGeometry
{

public:

  CHODGeometry();
  static CHODGeometry* GetInstance();

private:

  static CHODGeometry* fInstance;

private:

  void CreateGeometry();

public:
  Int_t GetNumberOfChannel() {return fNumberOfChannel;};
  TVector3 GetScintillatorPosition(Int_t iCounter) {return fScintillatorPosition[iCounter];}; 
  Int_t GetNScintillatorCounter() {return fNScintillatorCounter;};

  Double_t GetSlabLength(Int_t iCounter) {return fSlabLength[iCounter];};
  Double_t GetSlabWidth(Int_t iCounter) {return fSlabWidth[iCounter];};


private:
  Int_t fNumberOfChannel;
  TVector3 fScintillatorPosition[16];
  Double_t fSlabLength[16];
  Double_t fSlabWidth[128];
  Int_t fNScintillatorCounter;

};
#endif
