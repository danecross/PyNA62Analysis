// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-05-05
//
// --------------------------------------------------------------
#ifndef MUV1HitsCluster_H
#define MUV1HitsCluster_H

#include "TObject.h"
#include "TClonesArray.h"
#include "TRecoMUV1Hit.hh"

class MUV1HitsCluster : public TObject {

  public:

    MUV1HitsCluster();
    void AddHit(TRecoMUV1Hit*);
    void Clear(Option_t* option ="");

  public:

    Int_t                GetNHits()                                         { return fNHits;                        };
    void                 SetNHits(Int_t value)                              { fNHits = value;                       };

    TRecoMUV1Hit **      GetHits()                                          { return fHits.data();                  };

    TVector3             GetPosition()                                      { return fPosition;                     };
    void                 SetPosition(TVector3 value)                        { fPosition = value;                    };

    Double_t             GetTime()                                          { return fTime; }
    void                 SetTime(Double_t time)                             { fTime = time; }

    Double_t             GetCharge()                                        { return fCharge; }
    void                 SetCharge(Double_t charge)                         { fCharge = charge; }

    Double_t             GetAmplitude()                                     { return fAmplitude; }
    void                 SetAmplitude(Double_t amplitude)                   { fAmplitude = amplitude; }

    Double_t             GetX()                                             { return fCoordX;       }
    void                 SetX(Double_t x)                                   { fCoordX = x; fPosition[0] = x;}

    Double_t             GetY()                                             { return fCoordY;       }
    void                 SetY(Double_t y)                                   { fCoordY = y; fPosition[1] = y;}

    Double_t             GetSigmaT()                                        { return fSigmaT; }
    Double_t             GetSigmaA()                                        { return fSigmaA; }
    Double_t             GetSigmaQ()                                        { return fSigmaQ; }
    Double_t             GetSigmaX()                                        { return fSigmaX; }
    Double_t             GetSigmaY()                                        { return fSigmaY; }

    void                 UpdateValue();


  private:

    Int_t      fNHits;
    std::vector <TRecoMUV1Hit *> fHits;

    TVector3   fPosition;
    Int_t fNHitsSide[2];

    Double_t fTime, fSigmaT;
    Double_t fCharge, fSigmaQ;
    Double_t fAmplitude, fSigmaA;
    Double_t fCoordX, fSigmaX;
    Double_t fCoordY, fSigmaY;
};
#endif
