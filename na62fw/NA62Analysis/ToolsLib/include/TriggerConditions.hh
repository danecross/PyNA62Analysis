// -------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)     2016-10-09
// L1 interface added Angela Romano (axr@hep.ph.bham.ac.uk) 2016-11-24
//
// -------------------------------------------------------------------

#ifndef TRIGGERCONDITIONS_HH
#define TRIGGERCONDITIONS_HH

#include <fstream>
#include <iostream>
#include <TVector3.h>
#include "TObjString.h"
#include "TObjArray.h"
#include "L0TPData.hh"
#include "L1TPData.hh"
#include "EventHeader.hh"

#define TC_NRUNS 20000
#define TC_NBITS 16

class TriggerConditions {

public:
  static TriggerConditions* GetInstance();

  void    ParseL0InputFile();
  void    ParseL0VacantBitsFile();   ///< Read the list of vacant runs/bits from the DB
  void    ParseL0VariableBitsFile(); ///< Read the list of L0 bits variable during a run from the DB
  Int_t   GetNumberOfL0Conditions()   { return fL0ConditionNames.size(); }
  TString GetL0ConditionName(Int_t i) { return fL0ConditionNames[i];     }
  Int_t   L0TriggerCondition(Int_t run, Int_t bit) { return fL0TriggerCondition[run][bit]; }
  Int_t   GetL0TriggerID(TString);
  Int_t   GetTriggerBit(Int_t Run, Int_t L0TriggerID);

  // Check the data type inclusively
  Bool_t  IsPhysicsTrigger       (L0TPData*);
  Bool_t  IsControlTrigger       (L0TPData*);
  Bool_t  IsPeriodicTrigger      (L0TPData*);
  Bool_t  IsCalibrationTrigger   (L0TPData*);
  Bool_t  IsLKrCalibrationTrigger(L0TPData*);
  Bool_t  IsRandomTrigger        (L0TPData*);

  // Check the data type exclusively
  Bool_t  IsPhysicsTriggerOnly       (L0TPData*);
  Bool_t  IsControlTriggerOnly       (L0TPData*);
  Bool_t  IsPeriodicTriggerOnly      (L0TPData*);
  Bool_t  IsCalibrationTriggerOnly   (L0TPData*);
  Bool_t  IsLKrCalibrationTriggerOnly(L0TPData*);
  Bool_t  IsRandomTriggerOnly        (L0TPData*);

  Bool_t  L0TriggerEnabled(Int_t Run, Int_t L0TriggerID);
  Bool_t  L0TriggerOn(Int_t Run, L0TPData* L0Packet, Int_t L0TriggerID); ///< Check by trigger ID
  Bool_t  L0TriggerBitOn(L0TPData* L0Packet, Int_t Bit); ///< Check by trigger bit
  Int_t   GetL0TriggerDownscaling(Int_t Run, Int_t L0TriggerID);

  Int_t   GetL1AutopassDownscaling(Int_t Run);
  TString GetL1TriggerConditionName(Int_t Run, Int_t L0TriggerID);
  Int_t   GetL1TriggerDownscaling(Int_t Run, Int_t L0TriggerID);
  Int_t   GetL1TriggerStatus(Int_t Run, Int_t L0TriggerID); ///< L1 trigger status: 0=ok, 1=bad

  Bool_t  ControlTriggerEnabled(Int_t Run);
  Int_t   GetControlTriggerDownscaling(Int_t Run);

  void    PrintL0(); ///< A detailed printout
  void    PrintTriggersForRun(Int_t Run); ///< Print L0/L1 trigger chains enabled for a given run
  void    PrintRunsForTrigger(Int_t L0TriggerID, Int_t MinRun=0, Int_t MaxRun=TC_NRUNS-1); ///< Print all runs with a certain L0 trigger enabled
  void    CheckMissingBitsForRun(Int_t Run, Int_t MinBit=0, Int_t MaxBit=TC_NBITS-1); ///< Check for L0 bits not known to the database
  void    PrintConflicts(); ///< Print runs/L0/L1 bits with multiple entries in the database
  void    PrintComments();  ///< Print comments found in the DB text file (lines starting with "#")

  void    ParseL1InputFile();
  Int_t   GetNumberOfL1Algorithms(Int_t Run, L1TPData* L1Packet, Int_t L0TriggerID);
  TString GetL1AlgorithmName(Int_t Run, L0TPData* L0Packet, L1TPData* L1Packet, Int_t L0TriggerID, Int_t AlgoNumber);
  Bool_t  L1TriggerOn(Int_t Run, L1TPData* L1Packet, Int_t L0TriggerID, Int_t AlgoNumber=-1);
  Bool_t  L1TriggerBypass(EventHeader*);
  Bool_t  L1TriggerAutopass(EventHeader*);
  Bool_t  AnyL1TriggerInFlaggingMode(EventHeader*); ///< Is any of the L1 algorithms that processed an event in flagging mode?
  Bool_t  L1TriggerInFlaggingMode(Int_t Run, L1TPData* L1Packet, Int_t L0TriggerID, Int_t AlgoNumber=-1); ///< Is a particular L1 algorithm in flagging mode?

private:
  TriggerConditions();
  ~TriggerConditions() {}

  TString fL0InputFileName1, fL0InputFileName2, fL0InputFileName3, fL1InputFileName;
  Int_t   fL0TriggerCondition[TC_NRUNS][TC_NBITS];    ///< Meanings of L0 trigger bits for each run
  Int_t   fL0TriggerDownscaling[TC_NRUNS][TC_NBITS];  ///< Downscaling factors of L0 triggers for each run
  Int_t   fControlTriggerDetectorID[TC_NRUNS];        ///< Detector ID for the control trigger for each run
  Int_t   fControlTriggerDownscaling[TC_NRUNS];       ///< Downscaling factors of control trigger for each run
  Int_t   fL1TriggerCondition[TC_NRUNS][TC_NBITS];    ///< Meanings of L1 trigger masks for each run
  Int_t   fL1TriggerDownscaling[TC_NRUNS][TC_NBITS];  ///< Downscaling factors of L1 triggers for each run
  Int_t   fL1TriggerStatus[TC_NRUNS][TC_NBITS];       ///< L1 status flag: 0=ok; 1=bad, do not use

  std::vector<TString>fL0ConditionNames; ///< Valid L0 trigger condition names
  std::vector<TString>fL0Comments;       ///< L0 Comments in the DB text file (lines starting with "#")
  std::vector<TString>fL1ConditionNames; ///< Valid L1 trigger condition names
  std::vector<TString>fL1Comments;       ///< L1 Comments in the DB text file (lines starting with "#")
  std::vector<TString>fL1AlgoNames;      ///< Conversion of L1 algorithm IDs to names
};

#endif
