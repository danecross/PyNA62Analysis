// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------

#ifndef TRecoMUV3Candidate_H
#define TRecoMUV3Candidate_H

#include "TRecoVCandidate.hh"

class TRecoMUV3Candidate : public TRecoVCandidate {

public:

  TRecoMUV3Candidate();
  ~TRecoMUV3Candidate() {}

  void Clear(Option_t* = "");

  Double_t GetX ()                       { return fX;                 }
  void     SetX (Double_t x)             { fX=x;                      }
  Double_t GetY ()                       { return fY;                 }
  void     SetY (Double_t y)             { fY=y;                      }
  Double_t GetZ ()                       { return fZ;                 }
  void     SetZ (Double_t z)             { fZ=z;                      }
  TVector3 GetPosition()                 { return TVector3(fX,fY,fZ); }

  Int_t    GetTileID()                   { return fTileID;       }
  void     SetTileID(Int_t val)          { fTileID = val;        }
  Int_t    GetChannel1()                 { return fChannel1;     }
  Int_t    GetChannel2()                 { return fChannel2;     }
  void     SetChannel1(Double_t val)     { fChannel1 = val;      }
  void     SetChannel2(Double_t val)     { fChannel2 = val;      }
  Int_t    GetROChannel1()               { return fROChannel1;   }
  Int_t    GetROChannel2()               { return fROChannel2;   }
  void     SetROChannel1(Double_t val)   { fROChannel1 = val;    }
  void     SetROChannel2(Double_t val)   { fROChannel2 = val;    }

  Double_t GetTime1()                    { return fTime1;        }
  Double_t GetTime2()                    { return fTime2;        }
  void     SetTime1(Double_t val)        { fTime1 = val;         }
  void     SetTime2(Double_t val)        { fTime2 = val;         }
  Double_t GetTime1NoT0()                { return fTime1NoT0;    }
  Double_t GetTime2NoT0()                { return fTime2NoT0;    }
  void     SetTime1NoT0(Double_t val)    { fTime1NoT0 = val;     }
  void     SetTime2NoT0(Double_t val)    { fTime2NoT0 = val;     }
  Double_t GetTimeNoTileT0()             { return fTimeNoTileT0; }
  void     SetTimeNoTileT0(Double_t val) { fTimeNoTileT0 = val;  }
  Double_t GetTimeNoT0()                 { return fTimeNoT0;     }
  void     SetTimeNoT0(Double_t val)     { fTimeNoT0 = val;      }
  Int_t    GetType()                     { return fType;         }
  void     SetType(Int_t val)            { fType  = val;         }

  Bool_t   IsTight()                     { return (fType==kTightCandidate);       }
  Bool_t   IsLoose()                     { return (fType==kLooseCandidate);       }
  Bool_t   IsLooseMasked()               { return (fType==kLooseMaskedCandidate); }
  Bool_t   IsInner()                     { return (fTileID>=144);                 }
  Bool_t   IsOuter()                     { return (fTileID< 144);                 }

  Double_t GetDeltaTime();
  Double_t GetDeltaTimeNoT0();
  Double_t GetAverageTime();

private:

  Double_t fX, fY, fZ;
  Int_t    fTileID;
  Int_t    fChannel1, fChannel2;
  Int_t    fROChannel1, fROChannel2;
  Double_t fTime1, fTime2, fTime1NoT0, fTime2NoT0, fTimeNoTileT0, fTimeNoT0;
  Int_t    fType;  ///< tight, loose, loose masked, undefined; see MUV3.conf for documentation

  ClassDef(TRecoMUV3Candidate,1);
};

#endif
