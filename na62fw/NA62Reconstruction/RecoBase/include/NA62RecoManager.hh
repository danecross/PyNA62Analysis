// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2019-04-25
//
// ---------------------------------------------------------------

#ifndef NA62RecoManager_H
#define NA62RecoManager_H 1

#include "EventHeader.hh"
#include "TSpecialTriggerEvent.hh"

class NA62RecoManager {

public:

  static NA62RecoManager* GetInstance();
  EventHeader* GetEventHeader(){ return fEventHeader; }
  void InitNA62ConditionsService(TString);
  void StoreDIMBlock(const TString, UInt_t*, UInt_t, TSpecialTriggerEvent*);

private:

  NA62RecoManager();
  ~NA62RecoManager();
  static NA62RecoManager* fInstance;
  EventHeader* fEventHeader;
};

#endif
