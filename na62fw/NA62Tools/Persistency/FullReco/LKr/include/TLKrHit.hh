// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//            Evelina Marinova (Evelina.Marinova@cern.ch)
// --------------------------------------------------------------
#ifndef TLKrHit_H
#define TLKrHit_H

#include "TDetectorVHit.hh"
#include "LKrChannelID.hh"

class TLKrHit : public TDetectorVHit, public LKrChannelID  {

    public:

        TLKrHit();
        ~TLKrHit();

        void Clear(Option_t* = "");

        Int_t EncodeChannelID();
        void  DecodeChannelID();

        Int_t GetStationID() { return 0; }


    public:

        Int_t                GetNSlices()                                       { return fNSlices;                      };
        void                 SetNSlices(Int_t value)                            { fNSlices = value;                     };
        Int_t                GetSlicesInX()                                     { return fSlicesInX;                    };
        void                 SetSlicesInX(Int_t value)                          { fSlicesInX = value;                   };
        Int_t                GetSlicesInY()                                     { return fSlicesInY;                    };
        void                 SetSlicesInY(Int_t value)                          { fSlicesInY = value;                   };

////<<        TClonesArray *       GetMicroCellData()                                 { return fMicroCellData;                };
////<<        void                 SetMicroCellData(TClonesArray * value)             { fMicroCellData = value;               };

        Double_t             GetCurrent() { return fCurrent; };
        void                 SetCurrent(Double_t value) { fCurrent = value; }; 

    protected:

        Int_t fNSlices ;
        Int_t fSlicesInX ;
        Int_t fSlicesInY ;
        Double_t fCurrent ; 

////<<        TClonesArray * fMicroCellData;

        ClassDef(TLKrHit,1);
};
#endif
