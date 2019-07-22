// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//
// --------------------------------------------------------------
#ifndef TSpectrometerDigi_H
#define TSpectrometerDigi_H

//mod//#include "TDCVHit.hh"
#include "SpectrometerChannelID.hh"
#include "SRBVHit.hh"

//class TSpectrometerDigi : public TDCVHit, public SpectrometerChannelID {
class TSpectrometerDigi : public SRBVHit, public SpectrometerChannelID {

    public:

//        TSpectrometerDigi() : TDCVHit(){}
//        TSpectrometerDigi(Int_t iCh) : TDCVHit(iCh){}
        TSpectrometerDigi() : SRBVHit(), SpectrometerChannelID(), fHitID(0) {}
        explicit TSpectrometerDigi(Int_t iCh) : SRBVHit(iCh), SpectrometerChannelID(), fHitID(0) {}
        ~TSpectrometerDigi(){}
        void Clear(Option_t* = "");
        Bool_t IsSortable() const { return kTRUE; }
        Int_t Compare(const TObject *obj) const {Int_t res = TVHit::Compare(obj); if(res == 0){ return TDCVHit::Compare(obj);
                                                                                              } else {return res;}}
        Int_t EncodeChannelID();
        void  DecodeChannelID();

        //Int_t GetStationID() { return GetChamberID(); }
        Int_t GetStationID() { return 0; }

    public:
        Int_t GetHitID() {return fHitID;};
        void SetHitID(Int_t val) {fHitID=val;};

    private:
        Int_t fHitID;

        ClassDef(TSpectrometerDigi,1);
};
#endif
