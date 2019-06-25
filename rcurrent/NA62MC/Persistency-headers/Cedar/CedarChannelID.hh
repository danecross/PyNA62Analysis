// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2014-04-10
//
// ---------------------------------------------------------------

#ifndef CedarChannelID_H
#define CedarChannelID_H

#include "Rtypes.h"

class CedarChannelID {

public:

  CedarChannelID();
  virtual ~CedarChannelID() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();      // returns position ID
  void  DecodeChannelID(Int_t); // converts position ID into sector, row, cone IDs 

  Int_t GetSectorID()          { return fSectorID; }
  void  SetSectorID(Int_t val) { fSectorID = val;  }
  Int_t GetRowID()             { return fRowID;    }
  void  SetRowID(Int_t val)    { fRowID = val;     }
  Int_t GetConeID()            { return fConeID;   }
  void  SetConeID(Int_t val)   { fConeID = val;    }

private:

  Int_t fSectorID;
  Int_t fRowID;
  Int_t fConeID;

  ClassDef(CedarChannelID,1);
};
#endif
