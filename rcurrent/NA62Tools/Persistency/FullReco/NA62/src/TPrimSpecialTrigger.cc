// --------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-11-30
//
// --------------------------------------------------------------
#include "TPrimSpecialTrigger.hh"

#include <iostream>
ClassImp(TPrimSpecialTrigger)

PrimRegister::PrimRegister(TString Label, UInt_t Value){
  Clear();
  fLabel = Label;
  fValue = Value;
}

void PrimRegister::Clear(Option_t* /*option*/){
  fLabel="";
  fValue = 0;
}

PrimCounter::PrimCounter(const PrimCounter &c) : TObject(c), fLabel(c.fLabel), fChannelIDs(c.fChannelIDs), fValues(c.fValues){
}

PrimCounter::PrimCounter(TString Label, Int_t ChannelID, UInt_t Value){
  Clear();
  fLabel = Label;
  fChannelIDs.push_back(ChannelID);
  fValues.push_back(Value);
}

void PrimCounter::Clear(Option_t* /*option*/){
  fLabel="";
  fChannelIDs.clear();
  fValues.clear();
}

void PrimCounter::AddEntry(Int_t ChannelID, UInt_t Value) {
  fChannelIDs.push_back(ChannelID);
  fValues.push_back(Value);
}

TPrimSpecialTrigger::TPrimSpecialTrigger(): TSpecialTrigger(){
  Clear();
}

void TPrimSpecialTrigger::Clear(Option_t* option){
  TSpecialTrigger::Clear(option);
  fRegisters.clear();
  fCounters.clear();
}

Int_t TPrimSpecialTrigger::FindRegister(TString Label){
  for(UInt_t iRegister=0;iRegister<fRegisters.size();iRegister++){
    if(!fRegisters[iRegister].GetLabel().CompareTo(Label)) return iRegister;
  }
  return -1;
}

Int_t TPrimSpecialTrigger::FindCounter(TString Label){
  for(UInt_t iCounter=0;iCounter<fCounters.size();iCounter++){
    if(!fCounters[iCounter].GetLabel().CompareTo(Label)) return iCounter;
  }
  return -1;
}

void TPrimSpecialTrigger::AddRegister(TString Label, UInt_t Value){
  Int_t iRegister = FindRegister(Label);
  if(iRegister>=0){ // Register named Label found
    fRegisters[iRegister].SetValue(Value); // Overwrite [it should never happen!]
  }
  else fRegisters.push_back(PrimRegister(Label,Value)); // Add new register
}

void TPrimSpecialTrigger::AddCounter(TString Label, Int_t ChannelID, UInt_t Value){
  Int_t iCounter = FindCounter(Label);
  if(iCounter>=0){ // Counter named Label found
    fCounters[iCounter].AddEntry(ChannelID,Value);
  }
  else fCounters.push_back(PrimCounter(Label,ChannelID,Value)); // Add new register
}

PrimRegister* TPrimSpecialTrigger::GetRegister(TString Label){
  Int_t Index = FindRegister(Label);
  if (Index<0) return 0;
  return GetRegister(Index);
}

PrimCounter* TPrimSpecialTrigger::GetCounter(TString Label){
  Int_t Index = FindCounter(Label);
  if (Index<0) return 0;
  return GetCounter(Index);
}
