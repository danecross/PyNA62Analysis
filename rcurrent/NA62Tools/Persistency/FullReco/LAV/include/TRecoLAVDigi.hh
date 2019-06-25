// --------------------------------------------------------------
// History:
//
// 2015-03-19 T. Spadaro and E. Leonardi: this class is obsolete and will be removed
// Created by Vito Palladno and Tommaso Spadaro (vito.palladino@cern.ch tommaso.spadaro@cern.ch) 2011-10-11
//
// 
//
// --------------------------------------------------------------


#ifndef TRecoLAVDigi_H
#define TRecoLAVDigi_H 1

#include "TObject.h"

#include "LAVDefinitions.hh"

class TRecoLAVDigi : public TObject {

public:

  TRecoLAVDigi();
  ~TRecoLAVDigi(){};

  void Clear(Option_t* = "");

  void  SetThType( Int_t ThType ) { fThType = ThType; }  // 1 High -1 Low 0 not assigned 
  Int_t GetThType()               { return fThType; }

  void SetNToT(Int_t NToT)       { 
    if(NToT>MAX_EDGES)
      fNToT = MAX_EDGES;
    else
      fNToT = NToT; }
  
  Int_t GetNToT()                { return fNToT; }

  void SetToT(Double_t* ToT)    { for(Int_t i=0; i<MAX_EDGES; i++) fToT[i] = ToT[i]; }
  Double_t* GetToT()            { return fToT; }
  
  void SetCharge(Double_t* Q)   { for(Int_t i=0; i<MAX_EDGES; i++) fQ[i] = Q[i]; }
  Double_t* GetCharge()         { return fQ; }
  
  void SetTime(Double_t* Time)  { for(Int_t i=0; i<MAX_EDGES; i++) fTime[i] = Time[i]; }
  Double_t* GetTime()           { return fTime; }

  void SetStatus(Int_t* Status)  { for(Int_t i=0; i<MAX_EDGES; i++) fStatus[i] = Status[i]; }
  Int_t* GetStatus()             { return fStatus; }

private:

  Int_t fThType;

  Int_t fNToT;

  Double_t fToT[MAX_EDGES];
  Double_t fTime[MAX_EDGES];
  Double_t fQ[MAX_EDGES];
  Int_t fStatus[MAX_EDGES];

  ClassDef(TRecoLAVDigi,1);

};
#endif


