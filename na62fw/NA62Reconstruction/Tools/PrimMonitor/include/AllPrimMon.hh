#ifndef ALLPrimMon_H
#define ALLPrimMon_H 1

#include <bitset>

#include "VPrimMon.hh"
#include "TH1F.h"
#include "TH1D.h"
#include "TH2F.h"
#include "TF1.h"
#include "TTree.h"
#include "TPaveText.h"
#include "TLine.h"
#include "TGraph.h"
#include "TMultiGraph.h"
#include "TLegend.h"
#include "TPrimitive.hh"

class AllPrimMon : public VPrimMon {

public:

  AllPrimMon(TRootBrowser*, TString);
  virtual ~AllPrimMon();
  virtual void Update(TTree *lavtree, TTree *richtree, TTree *chodtree, TTree *muvtree, TTree *talktree, TTree *irctree,TTree *lkrtree, TString TimeRunBurst, TString time);
  
private:

  TH1D  *fHIRCLAVCorr;
  TH1D  *fHIRCCHODCorr;
  TH1D  *fHIRCRICHCorr;
  TH1D  *fHIRCMUVCorr;
  TH1D  *fHIRCTALKCorr;
  TH1D  *fHIRCLKRCorr;

  TH1D  *fHRICHCHODCorr;
  TH1D  *fHRICHMUVCorr;
  TH1D  *fHRICHLKRCorr;
  TH1D  *fHRICHTALKCorr;
  TH1D  *fHRICHLAVCorr;

  TH1D  *fHMUVOffsetMTP;
  TH1D  *fHLAVOffsetMTP;
  TH1D  *fHLKROffsetMTP;
  TH1D  *fHIRCOffsetMTP;
  TH1D  *fHCHODOffsetMTP;
  TH1D  *fHRICHOffsetMTP;

  TH2F  *fHMUVSendVsTimeStamp;
  TH2F  *fHLAVSendVsTimeStamp;
  TH2F  *fHLKRSendVsTimeStamp;
  TH2F  *fHIRCSendVsTimeStamp;
  TH2F  *fHCHODSendVsTimeStamp;
  TH2F  *fHRICHSendVsTimeStamp;

  //TGraph *fGIRCRICHCorr;

  TPaveText *fPText;
  TLegend   *fLegCorr1;
  TLegend   *fLegCorr2;
  TLegend   *fLegMTP;

  Int_t   NBurst;

  TPrimitive* fLAVprimitive;
  TPrimitive* fMUVprimitive;
  TPrimitive* fCHODprimitive;
  TPrimitive* fRICHprimitive;
  TPrimitive* fIRCprimitive;
  TPrimitive* fLKRprimitive;
  TPrimitive* fTALKprimitive;

  void Rescale(TH1F*) ;



};

#endif

