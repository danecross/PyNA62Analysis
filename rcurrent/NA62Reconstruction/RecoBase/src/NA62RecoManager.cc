// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2019-04-25
//
// ---------------------------------------------------------------

#include "NA62RecoManager.hh"
#include <iostream>

NA62RecoManager* NA62RecoManager::fInstance = nullptr;

NA62RecoManager* NA62RecoManager::GetInstance(){
  if(!fInstance) fInstance = new NA62RecoManager();
  return fInstance;
}

NA62RecoManager::NA62RecoManager(){
  fEventHeader = new EventHeader();
} 

NA62RecoManager::~NA62RecoManager(){
  if(fEventHeader){
    delete fEventHeader;
    fEventHeader=nullptr;
  }
}
