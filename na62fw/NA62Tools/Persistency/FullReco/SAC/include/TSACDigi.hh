// --------------------------------------------------------------
// History:
//
// Created by T Spadaro (tommaso.spadaro@cern.ch) 2015-05-14
//
// --------------------------------------------------------------
#ifndef TSACDigi_H
#define TSACDigi_H 1

#include "TDCVHit.hh"
#include "SACChannelID.hh"

class TSACDigi : public TDCVHit, public SACChannelID {

  public:

    TSACDigi();// : TDCVHit(){}
    explicit TSACDigi(Int_t iCh) : TDCVHit(iCh), SACChannelID(iCh%1000){}
    explicit TSACDigi(TVHit*);
    ~TSACDigi() {}

    void Clear(Option_t* = "");

    Int_t EncodeChannelID();
    void  DecodeChannelID();

    Int_t GetStationID() { return 0; }

    Int_t Compare(const TObject *obj) const;
    Bool_t IsSortable() const { return kTRUE; }
    //Int_t      GetPMTID                ()  {return  SACChannelID::EncodeChannelID();}
    //Int_t      GetChannelID            ()const  {return  fChannelID;}
    Int_t      GetCorrespondingLowHighChannelId()const;

    Int_t GetThresholdType() const { return fChannelID/1000; } //Low Threshold: 0, High Threshold: 1

    Bool_t     HasLeadingEdge          ()       {return  GetDetectedEdge()&1;}
    Bool_t     HasTrailingEdge         ()       {return  GetDetectedEdge()&2;}

  private:
    //    Int_t fPMTID;
    //   Bool_t fIsLowThresholdChannel;

    ClassDef(TSACDigi,1);
};
#endif
