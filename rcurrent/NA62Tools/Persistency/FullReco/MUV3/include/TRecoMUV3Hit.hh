// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
// Updated: Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-11-02
//
// --------------------------------------------------------------

#ifndef TRecoMUV3Hit_H
#define TRecoMUV3Hit_H

#include "TRecoVHit.hh"
#include "MUV3ChannelID.hh"

class TRecoMUV3Hit : public TRecoVHit, public MUV3ChannelID {

public:

  TRecoMUV3Hit();
  ~TRecoMUV3Hit() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();

  Int_t    GetDetectedEdge()                 { return fDetectedEdge;     }
  void     SetDetectedEdge(Int_t val)        { fDetectedEdge = val;      }
  Double_t GetLeadingTime()                  { return fLeadingTime;      }
  void     SetLeadingTime(Double_t val)      { fLeadingTime = val;       }
  Double_t GetTrailingTime()                 { return fTrailingTime;     }
  void     SetTrailingTime(Double_t val)     { fTrailingTime = val;      }
  Double_t GetLeadingTimeNoT0()              { return fLeadingTimeNoT0;  }
  void     SetLeadingTimeNoT0(Double_t val)  { fLeadingTimeNoT0 = val;   }
  Double_t GetTrailingTimeNoT0()             { return fTrailingTimeNoT0; }
  void     SetTrailingTimeNoT0(Double_t val) { fTrailingTimeNoT0 = val;  }
  Double_t GetTimeNoT0()                     { return fTimeNoT0;         }
  void     SetTimeNoT0(Double_t val)         { fTimeNoT0 = val;          }
  int      GetROChannelID()                  { return fROChannelID;      }
  void     SetROChannelID(Int_t val)         { fROChannelID = val;       }

private:
  Int_t    fDetectedEdge;
  Double_t fLeadingTime, fTrailingTime;
  Double_t fLeadingTimeNoT0, fTrailingTimeNoT0, fTimeNoT0;
  Int_t    fROChannelID;

  ClassDef(TRecoMUV3Hit,1);
};
#endif
