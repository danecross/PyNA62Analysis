#ifndef IRCPrimMon_H
#define IRCPrimMon_H 1

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

class IRCPrimMon : public VPrimMon {

public:

  IRCPrimMon(TRootBrowser*, TString);
  virtual ~IRCPrimMon();
  virtual void Update(TTree *tree, TString TimeRunBurst, TString time);
  
private:

  TTree *fTree;
  TH1F  *fHPrimTime;
  TH1F  *fHPrimTimeZoom1;
  TH1F  *fHPrimTimeZoom2;
  TH1D  *fHPrimTimeForEffSpill;
  TH1F  *fHPrimDelta;
  TH1F  *fHPrimDeltaTS;
  TH1F  *fHPrimID;
  TH1F  *fHPrimMask;
  TH1F  *fHTimeBits;
  TH1F  *fHFineTime;
  TH1F  *fHTimeStamp16 ;
  TH1F  *fHTimeStamp128 ;
  TH1F  *fHTimeStamp1024 ;
  TH1F  *fHTimeStamp8192 ;
  TH1F  *fHMultiTimeStamp16 ;
  TH1F  *fHMultiTimeStamp128 ;
  TH1F  *fHMultiTimeStamp1024 ;
  TH1F  *fHMultiTimeStamp8192 ;
  TH1F  *fHFineTimeStamp128 ;
  TH1D  *fHPrimTimeFFT;
  TH1D  *fHPrimFFT;
  TH1D  *fHPrimTimeFFT1;
  TH1D  *fHPrimFFT1;
  TH1D  *fHPrimTimeFFT2;
  TH1D  *fHPrimFFT2;
  TH1D  *fHPrimTimeFFT3;
  TH1D  *fHPrimFFT3;
  TH1D  *fHPrimTimeFFT4;
  TH1D  *fHPrimFFT4;
  TH1D  *fHPrimTimeFFT5;
  TH1D  *fHPrimFFT5;

  TPaveText *fPText;
  TPaveText *fPTextTime;
  Int_t NBurst;
  
  TPrimitive* fTPrimitive;

  void Rescale(TH1F*) ;

};

#endif
