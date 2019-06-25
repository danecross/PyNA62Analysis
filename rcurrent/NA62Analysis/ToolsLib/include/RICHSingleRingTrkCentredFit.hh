#ifndef RICHSINGLERINGTRKCENTREDFIT_HH
#define RICHSINGLERINGTRKCENTREDFIT_HH

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
class TRecoSpectrometerCandidate;

class RICHSingleRingTrkCentredFit{
public:

  static RICHSingleRingTrkCentredFit* GetInstance();
  RICHSingleRingTrkCentredFit();
  ~RICHSingleRingTrkCentredFit();
  void InitOutput();

  Bool_t Chi2Fit(TRecoRICHEvent*,TRecoRICHCandidate*, TRecoSpectrometerCandidate*, Int_t &, Double_t &, Double_t &, Double_t &);

  static void RingChi2FCN(Int_t &, Double_t *, Double_t &, Double_t *, Int_t);
  Double_t RingChi2(Double_t *);

  Int_t MirrorSurface(Double_t,Double_t);

private:
  Bool_t UpdateRingFit(TRecoRICHCandidate*,TVector3,TVector3);
  Int_t fNPars;
  TMinuit *fFitter;
  Double_t fMirrorPosition[20][2];  ///< Mirror centre potisions (sequential numbering)
  Double_t fMirrorAlignment[25][2]; ///< Mirror alignment parameters
  Int_t    fMirrorSide[25];         ///< 0=Jura mirror, 1=Saleve mirror, -999: mirror does not exist
  Int_t    fMirrorNumber[20];       ///< Mapping of standard mirror numbers to contiguous numbers
  Int_t fjMax;
};
#endif
