#ifndef TRecoHACHit_H
#define TRecoHACHit_H

#include "TRecoVHit.hh"
#include "HACChannelID.hh"
class TRecoHACHit : public TRecoVHit , public HACChannelID {

public:

  TRecoHACHit();
  ~TRecoHACHit(){};
  
  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();

public:

  void SetEdgeMask(Int_t val) {fEdgeMask =val;} 
  
  void SetLeadingEdge0(Double_t LedgeTDC){fLeadingEdge0 = LedgeTDC; fEdgeMask |= 0x1;}     // Saves T0 subtracted Leading edge for thr0  and set the mask bit 0
  void SetLeadingEdge1(Double_t LedgeTDC){fLeadingEdge1 = LedgeTDC; /*(fLeadingEdge1==-999.)?(fEdgeMask &= ~(1<<1)):(*/fEdgeMask |= 0x2;} // Saves T0 subtracted Leading edge for thr1  and set the mask bit 1
  void SetLeadingEdge2(Double_t LedgeTDC){fLeadingEdge2 = LedgeTDC; /*(fLeadingEdge2==-999.)?(fEdgeMask &= ~(1<<2)):(*/fEdgeMask |= 0x4;} // Saves T0 subtracted Leading edge for thr2  and set the mask bit 2
  void SetLeadingEdge3(Double_t LedgeTDC){fLeadingEdge3 = LedgeTDC; /*(fLeadingEdge3==-999.)?(fEdgeMask &= ~(1<<3)):(*/fEdgeMask |= 0x8;} // Saves T0 subtracted Leading edge for thr3  and set the mask bit 3
  
  Double_t GetLeadingEdge0()const{return (fLeadingEdge0);}
  Double_t GetLeadingEdge1()const{return (fLeadingEdge1);}
  Double_t GetLeadingEdge2()const{return (fLeadingEdge2);}
  Double_t GetLeadingEdge3()const{return (fLeadingEdge3);}
  
    
  void SetTrailingEdge0(Double_t LedgeTDC){fTrailingEdge0 = LedgeTDC; fEdgeMask |= 0x10;}  // Saves T0 subtracted Trailing edge for thr0 and set the mask bit 4
  void SetTrailingEdge1(Double_t LedgeTDC){fTrailingEdge1 = LedgeTDC; /*(fTrailingEdge1==-999.)?(fEdgeMask &= ~(1<<5)):(*/fEdgeMask |= 0x20;}  // Saves T0 subtracted Trailing edge for thr1 and set the mask bit 5 
  void SetTrailingEdge2(Double_t LedgeTDC){fTrailingEdge2 = LedgeTDC; /*(fTrailingEdge2==-999.)?(fEdgeMask &= ~(1<<6)):(*/fEdgeMask |= 0x40;}  // Saves T0 subtracted Trailing edge for thr2 and set the mask bit 6
  void SetTrailingEdge3(Double_t LedgeTDC){fTrailingEdge3 = LedgeTDC; /*(fTrailingEdge3==-999.)?(fEdgeMask &= ~(1<<7)):(*/fEdgeMask |= 0x80;} // Saves T0 subtracted Trailing edge for thr3 and set the mask bit 7
  
  Double_t GetTrailingEdge0()const{return (fTrailingEdge0);}
  Double_t GetTrailingEdge1()const{return (fTrailingEdge1);}
  Double_t GetTrailingEdge2()const{return (fTrailingEdge2);}
  Double_t GetTrailingEdge3()const{return (fTrailingEdge3);}
  
  Bool_t IsGrade0()const{return (fEdgeMask&0x11) == 0x11;} // 1st threshold passed
  Bool_t IsGrade1()const{return (fEdgeMask&0x22) == 0x22;} // 2nd threshold passed
  Bool_t IsGrade2()const{return (fEdgeMask&0x44) == 0x44;} // 3rd threshold passed
  Bool_t IsGrade3()const{return (fEdgeMask&0x88) == 0x88;} // 4th threshold passed
  Bool_t IsGrade(Int_t ithr);
  
  Int_t GetEdgeMask()const{return fEdgeMask;}
  
  Bool_t HasTDCsInOrder() const; // when FALSE there is a tdc arrival exception - Channel Time and Charge are calculated accordingly in HASCReconstruction
  
  Double_t GetChargeModuleSection()             { return fChargeModuleSection;} // [pC] See HACReconstruction.cc
  void     SetChargeModuleSection(Double_t val) { fChargeModuleSection = val; } 
  
  Double_t GetToTsumm()             { return fToTsumm;} // [ns] this is the Summ of all dt's corresponding to each surpassed threshold
  void     SetToTsumm(Double_t val) { fToTsumm = val; } 
  
  Double_t GetToT(Int_t ithr);
  Double_t GetLeadingEdge(Int_t ithr);
  Double_t GetTrailingEdge(Int_t ithr);
  Int_t    GetHighestThreshold();
  
  void SetLeadingEdge(Int_t ithr, Double_t fLeadTime);
  void SetTrailingEdge(Int_t ithr, Double_t fTrailTime);
  
private:
  
  Int_t fEdgeMask; // Mask for the edges present: bit 0-3 Leading, 4-7-Trailing
  Double_t fLeadingEdge0, fLeadingEdge1, fLeadingEdge2, fLeadingEdge3;
  Double_t fTrailingEdge0, fTrailingEdge1, fTrailingEdge2, fTrailingEdge3;
  Double_t fChargeModuleSection, fToTsumm; // See HACReconstruction.cc
  ClassDef(TRecoHACHit,1);
};
#endif
