// --------------------------------------------------------------
// History:
//
// Bob Velghe (bob.velghe@cern.ch) 2014-11-13
//  - Add UID field
//
// Created by Massimiliano Fiorini (Massimiliano.Fiorini@cern.ch) 2009-10-26
//
// --------------------------------------------------------------
#ifndef GigaTrackerChannelID_H
#define GigaTrackerChannelID_H
#include "Rtypes.h"
#include "TVector3.h"

class GigaTrackerChannelID {
public:
  GigaTrackerChannelID();
  GigaTrackerChannelID(const GigaTrackerChannelID &);
  virtual ~GigaTrackerChannelID();
  void Clear(Option_t* = "");
  Double_t GetPixelXPosition();
  Double_t GetPixelYPosition();
  TVector3 GetRawPosition();
  
public:
  Int_t                GetStationNo() const;
  void                 SetStationNo(Int_t value);
  
  Int_t                GetPixelID() const;
  void                 SetPixelID(Int_t value);
  
  Int_t                GetChipID() const;
  void                 SetChipID(UInt_t value)           {fChipID = value;        };

  UInt_t               GetqChipID();
  
  Int_t                GetChipPixelID() const           {return fChipPixelID;    };
  void                 SetChipPixelID(Int_t value)      {fChipPixelID = value;   };
  
  void                 DecodeChannelID(Long_t);
  UInt_t               EncodeChannelID();

  UInt_t               GetColumn();
  UInt_t               GetRow();


private:
  Int_t fStationNo;
  Int_t fChipPixelID; 

  UInt_t fChipID;

  ClassDef(GigaTrackerChannelID,1);
};
#endif
