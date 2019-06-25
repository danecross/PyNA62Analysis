// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoRICHHit_H
#define TRecoRICHHit_H

#include "TRecoVHit.hh"
#include "TRICHDigi.hh"
#include "RICHChannelID.hh"

class TRecoRICHHit : public TRecoVHit, public RICHChannelID {

  public:

    TRecoRICHHit();
    explicit TRecoRICHHit(Int_t iCh);
    explicit TRecoRICHHit(TRICHDigi *);
    ~TRecoRICHHit(){};

    void Clear(Option_t* = "");

    Int_t EncodeChannelID();
    void  DecodeChannelID();


 public:
    Double_t             GetHitQuality()                                    { return fHitQuality;                   };
    void                 SetHitQuality(Double_t value)                      { fHitQuality = value;                  };
    Double_t             GetTimeWidth()                                     { return fTimeWidth;                    };
    void                 SetTimeWidth(Double_t value)                       { fTimeWidth = value;                   };
    Int_t                GetROChannelID()                                   { return fROChannelID;                 };
    void                 SetROChannelID(Int_t value)                        { fROChannelID = value;                };
    Double_t             GetPtolemy()                                       { return fPtolemy;                      };
    void                 SetPtolemy(Double_t value)                         { fPtolemy = value;                     };
    Bool_t               GetIsOnCircle()                                    { return fIsOnCircle;                   };
    void                 SetIsOnCircle(Bool_t value)                        { fIsOnCircle = value;                  };
    TVector3             GetFitPosition()                                   { return fFitPosition;                  };        
    void                 SetFitPosition(TVector3 value)                     { fFitPosition = value;                 };

  private:

  Double_t fHitQuality;
    Double_t fTimeWidth;
    Int_t    fROChannelID;
    Double_t fPtolemy;
    Bool_t fIsOnCircle; //!  Transient data member
    TVector3 fFitPosition; ///< HitPosition corrected including mirror inclination


    ClassDef(TRecoRICHHit,1);
};
#endif
