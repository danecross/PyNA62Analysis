// TLAVHit.hh
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
// 2009-03-02 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - add LAV specific hit information
// 2010-03-15 - Domenico Di Filippo (difilippo@na.infn.it)
//   - Optical photons on photocatode
//
// First implementation of LAVChannelID and revision by T. Spadaro and E. Leonardi
// --------------------------------------------------------------
#ifndef TLAVHit_H
#define TLAVHit_H

#include "TDetectorVHit.hh"
#include "LAVChannelID.hh"

#define __TLAVHit_MAX_PHOTONS__ 10000

class TLAVHit : public TDetectorVHit, public LAVChannelID {

public:

        TLAVHit();
        ~TLAVHit(){};

        void Clear(Option_t* = "");

        Int_t EncodeChannelID();
        void DecodeChannelID();
        Int_t GetStationID() { return GetLAVID(); }

        TVector3 GetLocalPosition()                { return fLocalPosition; };
        void     SetLocalPosition(TVector3 value)  { fLocalPosition = value; };
        TVector3 GetLocalDirection()               { return fLocalDirection; };
        void     SetLocalDirection(TVector3 value) { fLocalDirection = value; };
        Double_t GetBeta()                         { return fBeta; };
        void     SetBeta(Double_t value)           { fBeta = value; };
        Double_t GetStepLength()                   { return fStepLength; };
        void     SetStepLength(Double_t value)     { fStepLength = value; };

        void      SetPhotonsNumber(Int_t value)    { fPhotonsNumber = value;
                                                     if (fPhotonsNumber > __TLAVHit_MAX_PHOTONS__)
                                                        fPhotonsNumber = __TLAVHit_MAX_PHOTONS__;
                                                     if (fPhotonsNumber < 0)
                                                        fPhotonsNumber = 0; };
        Int_t     GetPhotonsNumber()               { return fPhotonsNumber; };
        Float_t* GetPhotonsEnergy()                { return fPhotonsEnergy; };
        Float_t* GetPhotonsTime()                  { return fPhotonsTime; };

        void Print(Option_t * option = "" )  const;

    private:
        
    protected:

        TVector3   fLocalPosition;
        TVector3   fLocalDirection;
        Double_t   fBeta;
        Double_t   fStepLength;

        Int_t      fPhotonsNumber;
        Float_t    fPhotonsEnergy[__TLAVHit_MAX_PHOTONS__];
        Float_t    fPhotonsTime[__TLAVHit_MAX_PHOTONS__];

        ClassDef(TLAVHit,2);

};
#endif
