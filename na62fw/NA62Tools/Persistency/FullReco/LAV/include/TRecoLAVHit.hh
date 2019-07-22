// --------------------------------------------------------------
// History:
//
// 2015-03-19 T. Spadaro (tommaso.spadaro@lnf.infn.it)
// - promoting c++ variables to root types whenever possible
// - add doxygen-compliant documentation
// - add method to retrieve span in azimuth of a given block
// 2015-01-22 Totally modified and revised by T. Spadaro and E. Leonardi
// 2012-01-04 Modified by Vito Palladino
//        RecoHit contains now a TClonesArray with all the RecoDigis
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
// 
// --------------------------------------------------------------

#ifndef TRecoLAVHit_H
#define TRecoLAVHit_H 1

#include "TClonesArray.h"

#include "TRecoVHit.hh"
#include "LAVDefinitions.hh"
#include "LAVChannelID.hh"
#include "TVector3.h"
#include "TMath.h"

using namespace std;

class TRecoLAVHit : public TRecoVHit, public LAVChannelID {
  
public:
  
  TRecoLAVHit();
  ~TRecoLAVHit();
  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();

  void SetLeadingEdgeLow(Double_t edgeTime){fLeadingEdgeLow = edgeTime; fEdgeMask |= 1;}
  void SetLeadingEdgeHigh(Double_t edgeTime){fLeadingEdgeHigh = edgeTime; fEdgeMask |= 2;}
  void SetTrailingEdgeHigh(Double_t edgeTime){fTrailingEdgeHigh = edgeTime; fEdgeMask |= 4;}
  void SetTrailingEdgeLow(Double_t edgeTime){fTrailingEdgeLow = edgeTime; fEdgeMask |= 8;}
  
  Double_t GetLeadingEdgeLow(){if (fEdgeMask & 1) {return fLeadingEdgeLow;} else {return 0;}}
  Double_t GetLeadingEdgeHigh(){if (fEdgeMask & 2) {return fLeadingEdgeHigh;} else {return 0;}} 
  Double_t GetTrailingEdgeHigh(){if (fEdgeMask & 4) {return fTrailingEdgeHigh;} else {return 0;}}
  Double_t GetTrailingEdgeLow(){if (fEdgeMask & 8) {return fTrailingEdgeLow;} else {return 0;}}

  Int_t GetEdgeMask(){return fEdgeMask;}

  void GetBlockPosition(TVector3 &); ///< Returns block position, expressed in mm
  static void GetBlockPositionFromCh(Int_t, TVector3 &);
  static Int_t  GetBlockIDFromPhi(Double_t, Int_t, Int_t); 
  static Double_t  GetBlockPhiSpan(Int_t);
private:

  Int_t fEdgeMask; ///< Mask for the edges present: bit 0 --> LeadingLow; 1 --> LeadingHigh; 2-->TrailingHigh; 3-->TrailingLow

  Double_t fLeadingEdgeLow; ///< Time of leading low, subtracted of the trigger time only 
  Double_t fTrailingEdgeLow;///< Time of leading high, subtracted of the trigger time only 
  Double_t fLeadingEdgeHigh;///< Time of trailing high, subtracted of the trigger time only 
  Double_t fTrailingEdgeHigh;///< Time of trailing low, subtracted of the trigger time only 
  
  ClassDef(TRecoLAVHit,1);
  
};
#endif
