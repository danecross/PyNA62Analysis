#ifndef EVENTVETOES_HH
#define EVENTVETOES_HH

#include "TMath.h"
#include "TVector2.h"
#include "TVector3.h"
#include <iostream>
#include <vector>

#include "TRecoCHANTICandidate.hh"
#include "TRecoLAVCandidate.hh"
#include "TIRCHit.hh"
#include "TSACHit.hh"

class EventVetoes {

public:

  EventVetoes();
  ~EventVetoes() {}
  void Clear();

  //////////////////////
  // Bool Matching

  Bool_t inline GetLAVTrigMatching() {return fLAVTrigMatching;};
  void inline SetLAVTrigMatching(Bool_t val) {fLAVTrigMatching = val;};

  Bool_t inline GetSAVTrigMatching() {return fSAVTrigMatching;};
  void inline SetSAVTrigMatching(Bool_t val) {fSAVTrigMatching = val;};
  
  Bool_t inline GetCHANTITrigMatching() {return fCHANTITrigMatching;};
  void inline SetCHANTITrigMatching(Bool_t val) {fCHANTITrigMatching = val;};
  

  //////////////////////
  // Pointers candidates
 
  Int_t GetNCHANTICandidates()                                       { return fCHANTICandidates.size();  };
  TRecoCHANTICandidate*          GetCHANTICandidate(UInt_t);
  void AddCHANTICandidate        (TRecoCHANTICandidate* val)         { fCHANTICandidates.push_back(val); };
 
  Int_t GetNLAVCandidates()                                          { return fLAVCandidates.size();  };
  TRecoLAVCandidate*          GetLAVCandidate(UInt_t);
  void AddLAVCandidate        (TRecoLAVCandidate* val)               { fLAVCandidates.push_back(val); };
 
  Int_t GetNIRCHit()                                                 { return fIRCHits.size();  };
  TIRCHit*          GetIRCHit(UInt_t);
  void AddIRCHit        (TIRCHit* val)                               { fIRCHits.push_back(val); };
 
  Int_t GetNSACHit()                                                 { return fSACHits.size();  };
  TSACHit*          GetSACHit(UInt_t);
  void AddSACHit        (TSACHit* val)                               { fSACHits.push_back(val); };

private:

  std::vector<TRecoCHANTICandidate*> fCHANTICandidates;
  std::vector<TRecoLAVCandidate*> fLAVCandidates;
  std::vector<TIRCHit*> fIRCHits;
  std::vector<TSACHit*> fSACHits;
  Bool_t fLAVTrigMatching;
  Bool_t fSAVTrigMatching;
  Bool_t fCHANTITrigMatching;
};

#endif
