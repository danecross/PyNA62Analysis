#ifndef TALKPrimMon_H
#define TALKPrimMon_H 1

#include <bitset>

#include "VPrimMon.hh"
#include "TH1F.h"
#include "TH1D.h"
#include "TF1.h"
#include "TTree.h"
#include "TPaveText.h"
#include "TLine.h"
#include "TGraph.h"
#include "TPrimitive.hh"

class TALKPrimMon : public VPrimMon {

public:

  TALKPrimMon(TRootBrowser*, TString);
  virtual ~TALKPrimMon();
  virtual void Update(TTree *tree, TString TimeRunBurst, TString time);
  
private:

  TTree *fTree;
  TH1F  *fHPrimTime;
  TH1F  *fHPrimTimeZoom1;
  TH1F  *fHPrimTimeZoom2;
  TH1F  *fHPrimDelta;
  TH1F  *fHPrimDeltaTS;
  TH1F  *fHPrimID;
  TH1F  *fHTimeBits;
  TH1F  *fHFineTime;
  TH1F  *fHTimeStamp16 ;
  TH1F  *fHTimeStamp128 ;
  TH1F  *fHTimeStamp1024 ;
  TH1F  *fHTimeStamp8192 ;
  TH1F  *fHFineTimeStamp128 ;
  TH1D  *fHPrimTimeFFT;
  TH1D  *fHPrimFFT;

  TPaveText *fPText;
  TPaveText *fPTextTime;
  Int_t NBurst;
  
  TPrimitive* fTPrimitive;

  void Rescale(TH1F*) ;

};

#endif
