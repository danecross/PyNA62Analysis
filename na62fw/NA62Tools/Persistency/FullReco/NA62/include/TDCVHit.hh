// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2007-03-30
//
// --------------------------------------------------------------
#ifndef TDCVHit_H
#define TDCVHit_H
#include "TVDigi.hh"
#include "Riostream.h"

class TDCVHit : public TVDigi {

    public:

        TDCVHit();
        explicit TDCVHit(Int_t);
        explicit TDCVHit(TVHit* MCHit);
        virtual ~TDCVHit(){};
        void Clear(Option_t* = "");
        virtual void UpdateReferenceTime(Double_t value){ fLeadingEdge -= value; fTrailingEdge -= value; };
        Int_t Compare(const TObject *obj) const;
        Bool_t IsSortable() const { return kTRUE; }
        virtual Double_t GetTime() { return fLeadingEdge; };
        //virtual Int_t GetStationID() { cerr << "ERROR: GetStationID() must be overloaded in the concrete implementetion of Digi" << endl; return 0; };

    public:

        inline Int_t                GetDetectedEdge() const                      { return fDetectedEdge;                 };
        inline void                 SetDetectedEdge(Int_t value)                 { fDetectedEdge = value;                };
        inline Double_t             GetLeadingEdge() const                       { return fLeadingEdge;                  };
        inline void                 SetLeadingEdge(Double_t value)               { fLeadingEdge = value;                 };
        inline Double_t             GetTrailingEdge() const                      { return fTrailingEdge;                 };
        inline void                 SetTrailingEdge(Double_t value)              { fTrailingEdge = value;                };
        inline Int_t                GetFPGAID() const                            { return fFPGAID;                       };
        inline void                 SetFPGAID(Int_t value)                       { fFPGAID = value;                      };
        inline Int_t                GetSlot() const                              { return fSlot;                         };
        inline void                 SetSlot(Int_t value)                         { fSlot = value;                        };
        inline Double_t             GetSlotTS() const                            { return fSlotTS;                       };
        inline void                 SetSlotTS(Double_t value)                    { fSlotTS = value;                      };

        inline void                 UpdateDetectedEdge(Int_t e)                  { fDetectedEdge |= e;                   };

    protected:

        Double_t fLeadingEdge;
        Double_t fTrailingEdge;

    private:

        Int_t   fDetectedEdge; // Bits: 1=Leading,2=Trailing
        Int_t fFPGAID;
        Int_t fSlot;
        Double_t fSlotTS;

        ClassDef(TDCVHit,1);
};
#endif
