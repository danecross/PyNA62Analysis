#ifndef RICHSINGLERINGTRKSEEDEDFIT_HH
#define RICHSINGLERINGTRKSEEDEDFIT_HH 

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TCanvas.h>
#include "TVector3.h"
#include "TLorentzVector.h"
#include "TMinuit.h"

using namespace std;

class TH1I;
class TH2F;
class TGraph;
class TTree;
class TRecoRICHEvent;
class TRecoRICHCandidate;
class TRecoRICHHit;

class RICHSingleRingTrkSeededFit{
public:


  static RICHSingleRingTrkSeededFit* GetInstance();
  RICHSingleRingTrkSeededFit();
  ~RICHSingleRingTrkSeededFit();
  void InitOutput();
 
  //Bool_t Chi2Fit(Int_t, TRecoRICHEvent*,TRecoRICHCandidate*,TVector3);
  Bool_t Chi2Fit(TRecoRICHEvent*,TRecoRICHCandidate*, TVector3, Int_t &, Double_t &, Double_t &, Double_t &, Double_t &, Double_t &);


  static void RingChi2FCN(Int_t &, Double_t *, Double_t &, Double_t *, Int_t);
  Double_t RingChi2(Double_t *);

  Int_t MirrorSurface(Double_t,Double_t);
 /////////////////////////                                                                                                               
  // RICH Ring variables                                                                                                          
  Int_t    GetTimeCandIndex()                                 { return fTimeCandIndex;                 }
  void     SetTimeCandIndex(Int_t val)                        { fTimeCandIndex = val;                  }

  Double_t    GetRadius()                               { return fRadius;                 }
  void     SetRadius(Double_t val)                      { fRadius = val;                  }
  Double_t    GetRadiusErr()                               { return fRadiusErr;                 }   // not used for now
  void     SetRadiusErr(Double_t val)                      { fRadiusErr = val;                  }

  Double_t    GetCenterX()                             { return fCenterX;                 }
  void     SetCenterX(Double_t val)                      { fCenterX = val;                  }
  Double_t    GetCenterY()                             { return fCenterY;                 }
  void     SetCenterY(Double_t val)                      { fCenterY = val;                  }

  Double_t    GetCenterXErr()                             { return fCenterXErr;                 }  // not used for now
  void     SetCenterXErr(Double_t val)                      { fCenterXErr = val;                  }
  Double_t    GetCenterYErr()                             { return fCenterYErr;                 }  // not used for now
  void     SetCenterYErr(Double_t val)                      { fCenterYErr = val;                  }

  Double_t    GetRingChi2()                             { return fRingChi2;                 }
  void     SetRingChi2(Double_t val)                    { fRingChi2 = val;                  }
  Double_t    GetRingTime()                               { return fRingTime;                 }  // the same as the Time Candidate
  void     SetRingTime(Double_t val)                      { fRingTime = val;                  }
  Double_t    GetMass()                                 { return fMass;                 }
  void     SetMass(Double_t val)                        { fMass = val;                  }
  Double_t    GetMomentum()                                 { return fMomentum;                 }
  void     SetMomentum(Double_t val)                        { fMomentum = val;                  }
  Int_t    GetNHits()                                 { return fNHits;                 }    // the same as the Time Candidate
  void     SetNHits(Int_t val)                        { fNHits = val;                  }    
private: 
  Bool_t UpdateRingFit(TRecoRICHCandidate*,TVector3,TVector3);
  Int_t fNPars;
  TMinuit *fFitter;
  Int_t fTimeCandIndex;
  Double_t fRadius;
  Double_t fRadiusErr;
  Double_t fCenterX;
  Double_t fCenterY;
  Double_t fCenterXErr;
  Double_t fCenterYErr;
  Double_t fRingChi2;
  Double_t fRingTime;
  Double_t fMass;
  Double_t fMomentum;
  Int_t fNHits;
  Double_t fMirrorPosition[20][2];  ///< Mirror centre potisions (sequential numbering)
  Double_t fMirrorAlignment[25][2]; ///< Mirror alignment parameters
  Int_t    fMirrorSide[25];         ///< 0=Jura mirror, 1=Saleve mirror, -999: mirror does not exist
  Int_t    fMirrorNumber[20];       ///< Mapping of standard mirror numbers to contiguous numbers
  int fjMax;
  int fIdBad;
};
#endif
