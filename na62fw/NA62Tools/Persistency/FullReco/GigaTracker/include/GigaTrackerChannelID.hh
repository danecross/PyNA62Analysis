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
  struct chIDDecoded {  Int_t fStationNo, fChipPixelID; UInt_t fChipID; };

  GigaTrackerChannelID();
  GigaTrackerChannelID(const GigaTrackerChannelID &);
  virtual ~GigaTrackerChannelID();
  void Clear(Option_t* = "");
  Double_t GetPixelXPosition();
  Double_t GetPixelYPosition();
  TVector3 GetRawPosition();
  static TVector3 GetRawPosition(Int_t stationNo, Double_t pixelX, Double_t pixelY);
  
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
  
  static chIDDecoded   DecodeChannelID_Static(Long_t);
  void                 DecodeChannelID(Long_t);
  UInt_t               EncodeChannelID();

  UInt_t               GetColumn();
  UInt_t               GetRow();
  static UInt_t        GetColumn(Int_t ChipID, Int_t ChipPixelID);
  static UInt_t        GetRow(Int_t ChipID, Int_t ChipPixelID);


private:
  Int_t fStationNo;
  Int_t fChipPixelID; 

  UInt_t fChipID;

  ClassDef(GigaTrackerChannelID,1);
};
#endif
