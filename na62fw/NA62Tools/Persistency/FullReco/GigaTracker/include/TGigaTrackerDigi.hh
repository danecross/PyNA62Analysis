// --------------------------------------------------------------
// History:
//
// Created by Massimiliano Fiorini (Massimiliano.Fiorini@cern.ch) 2011-04-11
//
// --------------------------------------------------------------
#ifndef TGigaTrackerDigi_H
#define TGigaTrackerDigi_H

#include "TDCVHit.hh"
#include "GigaTrackerChannelID.hh"
#include "TVector3.h"

class TGigaTrackerDigi : public TDCVHit, public GigaTrackerChannelID {

public:
  
  TGigaTrackerDigi();
  ~TGigaTrackerDigi(){}
  
  void Clear(Option_t* = "");

  Int_t EncodeChannelID(); //must be here because of TDCVHit
  void  DecodeChannelID(); //must be here because of TDCVHit

  Int_t GetStationID() { return GetStationNo(); } //must be here because of TDCVHit


  UInt_t         GetSourceId()                              { return fSourceId;          };
  void           SetSourceId(UInt_t value)                  { fSourceId = value;         };

  UInt_t         GetFrameCounter()                          { return fFrameCounter;      };
  void           SetFrameCounter(UInt_t value)              { fFrameCounter = value;     };

  UInt_t         GetPixelAddress()                          { return fPixelAddress;      };
  void           SetPixelAddress(UInt_t value)              { fPixelAddress = value;     };

  UInt_t         GetHitArbiterAddress()                     { return fHitArbiterAddress; };
  void           SetHitArbiterAddress(UInt_t value)         { fHitArbiterAddress = value;};

  UInt_t         GetPileUpAddress()                         { return fPileUpAddress;     };
  void           SetPileUpAddress(UInt_t value)             { fPileUpAddress = value;    };

  Bool_t         GetIsPileUp()                              { return fIsPileUp;     };
  void           SetIsPileUp(Bool_t value)                  { fIsPileUp = value;    };

  UInt_t         GetLeadingSelector()                       { return fLeadingSelector;   };
  void           SetLeadingSelector(UInt_t value)           { fLeadingSelector = value;  };

  UInt_t         GetLeadingCoarse()                         { return fLeadingCoarse;     };
  void           SetLeadingCoarse(UInt_t value)             { fLeadingCoarse = value;    };

  UInt_t         GetLeadingFine()                           { return fLeadingFine;       };
  void           SetLeadingFine(UInt_t value)               { fLeadingFine = value;      };

  UInt_t         GetTotSelector()                           { return fTotSelector;       };
  void           SetTotSelector(UInt_t value)               { fTotSelector = value;      };

  UInt_t         GetTotCoarse()                             { return fTotCoarse;         };
  void           SetTotCoarse(UInt_t value)                 { fTotCoarse = value;        };

  UInt_t         GetTotFine()                               { return fTotFine;           };
  void           SetTotFine(UInt_t value)                   { fTotFine = value;          };

  Float_t        GetDelay()                                 { return fDelay;             };
  void           SetDelay(Float_t value)                    { fDelay = value;            };

  Float_t        GetAbsLeadingEdge()                        { return fAbsLeadingEdge;    };
  void           SetAbsLeadingEdge(Float_t value)           { fAbsLeadingEdge = value;   };

  Float_t        GetAbsTrailingEdge()                       { return fAbsTrailingEdge;   };
  void           SetAbsTrailingEdge(Float_t value)          { fAbsTrailingEdge = value;  };

  Double_t       GetTime()                                  { return fTime;              };
  void           SetTime(Double_t value)                    { fTime = value;             };



private:

  Float_t fDelay;
  Float_t fAbsLeadingEdge;
  Float_t fAbsTrailingEdge;
  UInt_t  fSourceId;
  UInt_t  fFrameCounter;
  
  UInt_t  fPixelAddress;
  UInt_t  fHitArbiterAddress;
  UInt_t  fPileUpAddress;
  
  UInt_t  fLeadingSelector;
  UInt_t  fLeadingCoarse;
  UInt_t  fLeadingFine;
  
  UInt_t  fTotSelector;
  UInt_t  fTotCoarse;
  UInt_t  fTotFine;

  Double_t fTime;

  // This member is last in the list: it has been introduced in v2 of the class
  Bool_t fIsPileUp;
  
  ClassDef(TGigaTrackerDigi,2);

};
#endif
