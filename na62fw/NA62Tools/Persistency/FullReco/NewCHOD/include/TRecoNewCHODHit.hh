// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-22
//
// ---------------------------------------------------------------

#ifndef TRecoNewCHODHit_H
#define TRecoNewCHODHit_H

#include "TRecoVHit.hh"
#include "NewCHODChannelID.hh"

class TRecoNewCHODHit : public TRecoVHit, public NewCHODChannelID {

public:

  TRecoNewCHODHit();
  ~TRecoNewCHODHit() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();

  Double_t GetX ()                    { return fX;                 }
  void     SetX (Double_t x)          { fX = x;                    }
  Double_t GetY ()                    { return fY;                 }
  void     SetY (Double_t y)          { fY = y;                    }
  Double_t GetZ ()                    { return fZ;                 }
  void     SetZ (Double_t z)          { fZ = z;                    }
  TVector3 GetPosition()              { return TVector3(fX,fY,fZ); }

  void     SetChannel1(Int_t val)     { fChannel1 = val;    }
  void     SetChannel2(Int_t val)     { fChannel2 = val;    }
  Int_t    GetChannel1()              { return fChannel1;   }
  Int_t    GetChannel2()              { return fChannel2;   }
  void     SetROChannel1(Int_t val)   { fROChannel1 = val;  }
  void     SetROChannel2(Int_t val)   { fROChannel2 = val;  }
  Int_t    GetROChannel1()            { return fROChannel1; }
  Int_t    GetROChannel2()            { return fROChannel2; }

  void     SetTime1(Double_t val)     { fTime1 = val;       }
  void     SetTime2(Double_t val)     { fTime2 = val;       }
  Double_t GetTime1()                 { return fTime1;      }
  Double_t GetTime2()                 { return fTime2;      }
  void     SetTime1NoT0(Double_t val) { fTime1NoT0 = val;   }
  void     SetTime2NoT0(Double_t val) { fTime2NoT0 = val;   }
  void     SetTimeNoT0 (Double_t val) { fTimeNoT0  = val;   }
  Double_t GetTime1NoT0()             { return fTime1NoT0;  }
  Double_t GetTime2NoT0()             { return fTime2NoT0;  }
  Double_t GetTimeNoT0()              { return fTimeNoT0;   }
  Double_t GetEarliestTime();
  Double_t GetDeltaTime();

  void     SetType(Int_t val)         { fType  = val;       }
  Int_t    GetType()                  { return fType;       }

private:

  Int_t fType;     ///< Tight, loose, or loose masked; see NewCHOD.conf for documentation  
  Int_t fChannel1; ///< "geometric" ID of the first channel
  Int_t fChannel2; ///< "geometric" ID of the second channel
  Int_t fROChannel1;
  Int_t fROChannel2;
  Double_t fX, fY, fZ;
  Double_t fTime1, fTime2;
  Double_t fTime1NoT0, fTime2NoT0, fTimeNoT0;

  ClassDef(TRecoNewCHODHit,1);
};
#endif
