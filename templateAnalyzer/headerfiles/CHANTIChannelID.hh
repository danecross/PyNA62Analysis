// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2011-01-31
//
// --------------------------------------------------------------
#ifndef CHANTIChannelID_H
#define CHANTIChannelID_H
#include "Rtypes.h"

class CHANTIChannelID {

    public:

  CHANTIChannelID();
  virtual ~CHANTIChannelID() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void DecodeChannelID(Int_t);

    public:

  Int_t    GetStationID()                   { return 0;              }
  Int_t    GetPlaneID()                     { return fPlaneID;       }
  void     SetPlaneID(Int_t value)          { fPlaneID = value;      }
  Int_t    GetRingType()                    { return fRingType;      }
  void     SetRingType(Int_t value)         { fRingType = value;     }
  Int_t    GetRingID()                      { return fRingType + 2*fPlaneID; }
  Int_t    GetSideID()                      { return fSideID;        }
  void     SetSideID(Int_t value)           { fSideID = value;       }
  Int_t    GetBarID()                       { return fBarID;         }
  void     SetBarID(Int_t value)            { fBarID = value;        }

    private:

  Int_t      fPlaneID;
  Int_t      fRingType;
  Int_t      fSideID;
  Int_t      fBarID;

  ClassDef(CHANTIChannelID,1);
};
#endif
