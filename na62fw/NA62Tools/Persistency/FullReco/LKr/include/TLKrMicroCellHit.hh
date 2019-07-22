// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//            Evelina Marinova (Evelina.Marinova@cern.ch)
// --------------------------------------------------------------
#ifndef TLKrMicroCellHit_H
#define TLKrMicroCellHit_H
#include "TObject.h"


class TLKrMicroCellHit : public TObject {

    public:

        TLKrMicroCellHit();
        ~TLKrMicroCellHit();

        void Clear(Option_t* = "");

        Int_t                GetXIndex()                                        { return fXIndex;                       };
        void                 SetXIndex(Int_t value)                             { fXIndex = value;                      };
        Int_t                GetYIndex()                                        { return fYIndex;                       };
        void                 SetYIndex(Int_t value)                             { fYIndex = value;                      };
        Int_t                GetZIndex()                                        { return fZIndex;                       };
        void                 SetZIndex(Int_t value)                             { fZIndex = value;                      };
        Float_t              GetEnergyFraction()                                { return fEnergyFraction;               };
        void                 SetEnergyFraction(Float_t value)                   { fEnergyFraction = value;              };

    protected:

        Int_t fXIndex;
        Int_t fYIndex;
        Int_t fZIndex;
        Float_t fEnergyFraction;

        ClassDef(TLKrMicroCellHit,1);
};
#endif
