// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-08
//
// --------------------------------------------------------------
#ifndef TDetectorVHit_H
#define TDetectorVHit_H

#include "TVector3.h"
#include "TVHit.hh"

class TDetectorVHit : public TVHit {

    public:

        TDetectorVHit();
        TDetectorVHit(const TDetectorVHit &);
        explicit TDetectorVHit(Int_t);
        virtual ~TDetectorVHit(){};
        void Clear(Option_t* = "");
        virtual void UpdateReferenceTime(Double_t value){ fTime -= value; };
        void Print(Option_t* option="") const;

    public:

        TVector3             GetPosition() const                                { return fPosition;                     };
        void                 SetPosition(TVector3 value)                        { fPosition = value;                    };
        Double_t             GetEnergy() const                                  { return fEnergy;                       };
        void                 SetEnergy(Double_t value)                          { fEnergy = value;                      };
        void                 AddEnergy(Double_t value)                          { fEnergy += value;                     };
        Double_t             GetTime() const                                    { return fTime;                         };
        void                 SetTime(Double_t value)                            { fTime = value;                        };

    private:

        TVector3   fPosition;
        Double_t   fEnergy;
        Double_t   fTime;

        ClassDef(TDetectorVHit,1);
};
#endif
