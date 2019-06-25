// --------------------------------------------------------------
// History:
//
// Created by T Spadaro (tommaso.spadaro@cern.ch) 2015-05-14
//
// --------------------------------------------------------------
#ifndef TIRCDigi_H
#define TIRCDigi_H

#include "TDCVHit.hh"
#include "IRCChannelID.hh"

class TIRCDigi : public TDCVHit, public IRCChannelID {

  public:

    TIRCDigi();// : TDCVHit(){}
    explicit TIRCDigi(Int_t iCh) : TDCVHit(iCh), IRCChannelID(iCh%1000){}
    explicit TIRCDigi(TVHit*);
    ~TIRCDigi() {}

    void Clear(Option_t* = "");

    Int_t EncodeChannelID();
    void  DecodeChannelID();

    Int_t GetStationID() { return (fChannelID%1000)/100; }

    Int_t Compare(const TObject *obj) const;
    Bool_t IsSortable() const { return kTRUE; }
    //Int_t      GetPMTID   ()   {  return   IRCChannelID::EncodeChannelID();}
    //Int_t      GetChannelID            ()const  {return  fChannelID;}
    Int_t      GetCorrespondingLowHighChannelId()const;

    Int_t GetThresholdType() const { return fChannelID/1000; } //Low Threshold: 0, High Threshold: 1

    Bool_t     HasLeadingEdge          ()       {return  GetDetectedEdge()&1;}
    Bool_t     HasTrailingEdge         ()       {return  GetDetectedEdge()&2;}

  private:
    //  Int_t fPMTID;
    //  Bool_t fIsLowThresholdChannel;

    ClassDef(TIRCDigi,1);
};
#endif
