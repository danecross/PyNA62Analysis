// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2019-04-09
//
// ---------------------------------------------------------------

#ifndef TSLIMRECONEWCHODCANDIDATE_H
#define TSLIMRECONEWCHODCANDIDATE_H

#include <RtypesCore.h>
#include "NA62Global.hh"
#include "TSlimRecoVCandidate.hh"
#include "TVector3.h"

class TRecoNewCHODHit;
class TRecoVHit;

class TSlimRecoNewCHODCandidate : public TSlimRecoVCandidate {

public:
  TSlimRecoNewCHODCandidate();
  explicit TSlimRecoNewCHODCandidate(TRecoNewCHODHit*);
  virtual ~TSlimRecoNewCHODCandidate() {}

  // Setters for members
  void SetTime1(Float_t val)    { fTime1 = val;       }
  void SetTime2(Float_t val)    { fTime2 = val;       }
  void SetChannel1(Short_t val) { fChannel1 = val;    }
  void SetChannel2(Short_t val) { fChannel2 = val;    }
  void SetType(Char_t val)      { fType = val;        }

  // Getters for members
  Float_t GetTime1() const      { return fTime1;      }
  Float_t GetTime2() const      { return fTime2;      }
  Short_t GetChannel1() const   { return fChannel1;   }
  Short_t GetChannel2() const   { return fChannel2;   }
  Char_t  GetType() const       { return fType;       }

  // Derived methods
  Float_t  GetTime() { return (fType==kTightCandidate) ? 0.5*(fTime1+fTime2) : fTime1; }
  Int_t    GetTileID();
  Int_t    GetSeqTileID();
  Int_t    GetQuadrantID();
  Double_t GetX();
  Double_t GetY();
  Double_t GetZ()        { return 238131.5;                         }
  TVector3 GetPosition() { return TVector3(GetX(), GetY(), GetZ()); }

  // Conversion functions
  virtual void FromReco(TRecoVCandidate*) {}
  virtual void ToReco  (TRecoVCandidate*) {}

  virtual void FromReco(TRecoVHit*);
  virtual void ToReco  (TRecoVHit*);

private:
  Char_t  fType;       ///< kTightCandidate, kLooseCandidate, kLooseMaskedCandidate, kUndefinedCandidate
  Short_t fChannel1;   ///< Valid IDs: 101-138, 201-238, 301-338, 401-438
  Short_t fChannel2;   ///< Valid IDs: 151-188, 251-288, 351-388, 451-488; or -1 for loose hits
  Float_t fTime1;      ///< Time of the 1st channel [ns]
  Float_t fTime2;      ///< Time of the 2nd channel [ns]; -999 for loose hits

  ClassDef(TSlimRecoNewCHODCandidate, 1)
};

#endif
