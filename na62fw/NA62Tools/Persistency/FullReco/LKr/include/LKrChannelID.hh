// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-08
//
// --------------------------------------------------------------
#ifndef LKrChannelID_H
#define LKrChannelID_H
#include "Rtypes.h"

class LKrChannelID {

  public:

    struct chIDDecoded { Int_t fXCellID, fYCellID, fCPDID, fCPDChannelID; };
    LKrChannelID();
    LKrChannelID(Int_t, Int_t); //old, it should be removed eventually
    virtual ~LKrChannelID() {}

    void Clear(Option_t* = "");

    Int_t EncodeChannelID();      // returns position ID
    void  DecodeChannelID(Int_t); // converts position ID into XCellIDs, YCellIDs
    static struct chIDDecoded DecodeChannelID_Static(Int_t ChannelID);
    Int_t GetCPDID()                   { return fCPDID;         } //old, it should be removed eventually
    void  SetCPDID(Int_t value)        { fCPDID = value;        } //old, it should be removed eventually
    Int_t GetCPDChannelID()            { return fCPDChannelID;  } //old, it should be removed eventually
    void  SetCPDChannelID(Int_t value) { fCPDChannelID = value; } //old, it should be removed eventually
    Int_t GetXCellID()          { return fXCellID; }
    void  SetXCellID(Int_t val) { fXCellID = val;  }
    Int_t GetYCellID()          { return fYCellID; }
    void  SetYCellID(Int_t val) { fYCellID = val;  }

  private:

    Int_t fCPDID;        //old, it should be removed eventually
    Int_t fCPDChannelID; //old, it should be removed eventually
    Int_t fXCellID;
    Int_t fYCellID;

    ClassDef(LKrChannelID,1);
};
#endif
