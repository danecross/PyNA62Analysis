// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2019-04-25
//
// ---------------------------------------------------------------

#ifndef NA62RecoManager_H
#define NA62RecoManager_H 1

#include "EventHeader.hh"

class NA62RecoManager {

  public:

    static NA62RecoManager* GetInstance();
    EventHeader* GetEventHeader(){ return fEventHeader; }

  private:

    NA62RecoManager();
    ~NA62RecoManager();

    static NA62RecoManager* fInstance;

    EventHeader* fEventHeader;
};

#endif
