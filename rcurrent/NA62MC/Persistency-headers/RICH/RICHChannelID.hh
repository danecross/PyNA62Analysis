// --------------------------------------------------------------
// History:
//
// Created by Francesca Bucci (francesca.bucci@cern.ch) 2014-06-19
//
// --------------------------------------------------------------

#ifndef RICHChannelID_H
#define RICHChannelID_H
#include "Rtypes.h"

class RICHChannelID {

    public:

  RICHChannelID();
  virtual ~RICHChannelID() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void DecodeChannelID(Int_t);
  Int_t GetChannelSeqID();

    public:

  Int_t                GetDiskID()                                        { return fDiskID;                       };
  void                 SetDiskID(Int_t value)                             { fDiskID = value;                      };

  Int_t                GetUpDownDiskID()                                  { return fUpDownDiskID;                 };
  void                 SetUpDownDiskID(Int_t value)                       { fUpDownDiskID = value;                };

  Int_t                GetSuperCellID()                                   { return fSuperCellID;                  };
  void                 SetSuperCellID(Int_t value)                        { fSuperCellID = value;                 };

  Int_t                GetOrSuperCellID()                                 { return fOrSuperCellID;                };
  void                 SetOrSuperCellID(Int_t value)                      { fOrSuperCellID = value;               };

  Int_t                GetPmtID()                                         { return fPmtID;                        };
  void                 SetPmtID(Int_t value)                              { fPmtID = value;                       };


    protected:

  Int_t     fDiskID;   ///< 0 if hit position on Jura, 1 if on Saleve
  Int_t     fUpDownDiskID;  ///< 0 if hit position on the Up half of the disk, 1 if on the Down half of the disk     
  Int_t     fSuperCellID;  ///<  ID number of the SuperCell to whom the hit belongs (from 0 to 121)
  Int_t     fOrSuperCellID;  ///< 0 if hit coming from a single PM, 1 if coming from a SuperCell (logic OR of 8 adjacent PMs)
  Int_t     fPmtID;       ///< ID number of the hit PM inside the SuperCell (from 0 to 7)

        ClassDef(RICHChannelID,1);
};
#endif
