// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2019-04-09
//
// ---------------------------------------------------------------

#ifndef TSLIMMUV3CANDIDATE_H
#define TSLIMMUV3CANDIDATE_H

#include <RtypesCore.h>
#include <vector>
#include "TVector3.h"
#include "NA62Global.hh"
#include "TSlimRecoVCandidate.hh"

class TRecoMUV3Candidate;

class TSlimRecoMUV3Candidate : public TSlimRecoVCandidate {

public:
  TSlimRecoMUV3Candidate();
  explicit TSlimRecoMUV3Candidate(TRecoMUV3Candidate*);
  virtual ~TSlimRecoMUV3Candidate() {}

  // Setters for members
  void SetTime(Float_t val)     { fTime  = val;       }
  void SetTime1(Float_t val)    { fTime1 = val;       }
  void SetTime2(Float_t val)    { fTime2 = val;       }
  void SetChannel1(Short_t val) { fChannel1 = val;    }
  void SetChannel2(Short_t val) { fChannel2 = val;    }
  void SetType(Char_t val)      { fType = val;        }

  // Getters for members
  Float_t GetTime()  const      { return fTime;       }
  Float_t GetTime1() const      { return fTime1;      }
  Float_t GetTime2() const      { return fTime2;      }
  Short_t GetChannel1() const   { return fChannel1;   }
  Short_t GetChannel2() const   { return fChannel2;   }
  Char_t  GetType() const       { return fType;       }

  // Derived methods
  Short_t  GetTileID()          { return fChannel1%200;                  }
  Bool_t   IsTight()            { return (fType==kTightCandidate);       }
  Bool_t   IsLoose()            { return (fType==kLooseCandidate);       }
  Bool_t   IsLooseMasked()      { return (fType==kLooseMaskedCandidate); }
  Bool_t   IsInner()            { return ((fChannel1%200)>=144);         }
  Bool_t   IsOuter()            { return ((fChannel1%200)< 144);         }
  Double_t GetAverageTime(); ///< Average time of the two hits (for tight candidates only) [ns]
  Double_t GetDeltaTime();   ///< Difference of time between two hits (for tight candidates only) [ns]
  Double_t GetX();
  Double_t GetY();
  Double_t GetZ()               { return 246800.0;                       }
  TVector3 GetPosition();

  // Conversion functions
  virtual void FromReco(TRecoVCandidate*);
  virtual void ToReco  (TRecoVCandidate*);

private:
  Char_t  fType;       ///< kTightCandidate, kLooseCandidate, kLooseMaskedCandidate, kUndefinedCandidate
  Short_t fChannel1;   ///< Valid IDs: 0-151 except 65,66,77,78; 200-351 except 265,266,277,278
  Short_t fChannel2;   ///< Valid IDs: -1 (for loose candidates); 0-151 except 65,66,77,78; 200-351 except 265,266,277,278
  Float_t fTime;       ///< Candidate time [ns]
  Float_t fTime1;      ///< Time of the 1st hit [ns]
  Float_t fTime2;      ///< Time of the 2nd hit [ns]; -999 for loose candidates

  ClassDef(TSlimRecoMUV3Candidate, 1)
};

#endif
