// ----------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)        2016-10-09
// L1 interface added by Angela Romano (axr@hep.ph.bham.ac.uk) 2016-11-24
//
// ----------------------------------------------------------------------

/// \class TriggerConditions
/// \Brief
/// Interface to the trigger conditions database
/// \EndBrief
/// \Detailed
/// Interface to the database of L0 trigger conditions used for each run number,
/// and the corresponding L1 algorithms.
/// If an L0 or L1 bit changes meaning during the run according to the database,
/// this is considered to be a "conflict", the information about that bit for that run is ignored,
/// and the bit is considered to be inactive for that run.
/// Below is an example of checking of an L0 trigger is active during a run and if it is on
/// for a given event.
/// \code
/// // Obtain the trigger condition code once in the constructor (this is "slow")
/// #include "TriggerConditions.hh"
/// ...
/// RequestL0Data();
/// RequestL1Data();
/// ...
/// fTrigger_CHOD = TriggerConditions::GetInstance()->GetL0TriggerID("CHOD");
/// ...
/// // Check the trigger type for each event in the Process() method
/// Bool_t PhysicsData = TriggerConditions::GetInstance()->IsPhysicsTrigger(GetL0Data());
/// Bool_t ControlData = TriggerConditions::GetInstance()->IsControlTrigger(GetL0Data());
/// ...
/// // Then check the trigger condition (for physics triggers) for each event in the Process() method
/// Int_t RunNumber = GetRunID();
/// Bool_t Enabled = TriggerConditions::GetInstance()->L0TriggerEnabled(RunNumber, fTrigger_CHOD);
/// Bool_t On  = TriggerConditions::GetInstance()->L0TriggerOn(RunNumber, GetL0Data(), fTrigger_CHOD);
/// Bool_t DS  = TriggerConditions::GetInstance()->GetL0TriggerDownscaling(RunNumber, fTrigger_CHOD);
/// Bool_t DS2 = TriggerConditions::GetInstance()->GetControlTriggerDownscaling(RunNumber);
/// \endcode
/// L0TriggerOn() always returns TRUE for MC events.
/// To print out the list of known L0 trigger conditions, run intervals for each condition,
/// and trigger conditions (both L0 and L1) used for a certain run, use
/// \code
/// TriggerConditions::GetInstance()->PrintL0();
/// TriggerConditions::GetInstance()->PrintTriggersForRun(RunNumber);
/// TriggerConditions::GetInstance()->PrintRunsForTrigger(TriggerID);
/// \endcode
/// Interface to the database of L1 trigger conditions:
/// L1 trigger is automatically defined by the selected L0 trigger.
/// Once the user requires specific L0 trigger conditions the corresponding L1 trigger conditions are applied on data.
/// For each event, given a L0 trigger condition, the user can check if the corresponding L1 trigger condition is on.
/// The L0 trigger condition (for example fTrigger_CHOD) is retrieved as explained above,
/// below is an example of checking the L1 trigger response (the whole chain of algorithms):
/// \code
/// Bool_t On = TriggerConditions::GetInstance()->L1TriggerOn(GetRunID(), GetL1Data(), fTrigger_CHOD);
/// \endcode
/// Here is an example of retreiving the reponse of a particular L1 algorithm:
/// \code
/// Bool_t On = TriggerConditions::GetInstance()->L1TriggerOn(GetRunID(), GetL1Data(), fTrigger_CHOD, AlgoNumber);
/// \endcode
/// Note that if an L1 algorithm produces a negative response, then all subsequent algorithms are not called,
/// and the above check will return zero for non-called algorithms.
/// Periodics, control and special (SOB and EOB) triggers are bypassed at L1.
/// For efficiency measurement a global portion of L1 bandwidth is autopassed at L1,
/// the L1 trigger verdict is registered but not applied.
/// Below is an example of how to check whether an event belongs to the bypass/autopass category,
/// or was processed in flagging mode.
/// \code
/// Bool_t bypass = TriggerConditions::GetInstance()->L1TriggerBypass(GetEventHeader());
/// Bool_t autopass = TriggerConditions::GetInstance()->L1TriggerAutopass(GetEventHeader());
/// Bool_t flagging = TriggerConditions::L1TriggerInFlaggingMode(GetEventHeader());
/// \endcode
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \author Angela Romano (axr@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "TriggerConditions.hh"
#include "NA62ConditionsService.hh"
#include "NA62Exceptions.hh"

using namespace std;
using namespace NA62Analysis;

static TriggerConditions* fInstance = 0;

TriggerConditions* TriggerConditions::GetInstance() {
  if (!fInstance) fInstance = new TriggerConditions();
  return fInstance;
}

TriggerConditions::TriggerConditions() :
  fL0InputFileName1("L0TriggerConditions.dat"),
  fL0InputFileName2("L0VacantBits.dat"),
  fL0InputFileName3("L0VariableBits.dat"),
  fL1InputFileName ("L1TriggerConditions.dat")
{
  for (Int_t i=0; i<TC_NRUNS; i++) {
    for (Int_t j=0; j<TC_NBITS; j++) {
      fL0TriggerCondition[i][j] = -1; // -1: missing information, -2: vacant, -3: variable, -999: conflict
      fL0TriggerDownscaling[i][j] = 0;
      fL1TriggerCondition[i][j] = -1;
      fL1TriggerDownscaling[i][j] = 0;
      fL1TriggerStatus[i][j] = 0; // 0=ok, 1=bad (do not use)
    }
    fControlTriggerDetectorID[i] = -999; // -999 means unknown, -1 means variable downscaling
    fControlTriggerDownscaling[i] = -999; // -999 means no control trigger, -1 means variable downscaling
  }
  fL0ConditionNames.clear();
  fL1ConditionNames.clear();
  fL0Comments.clear();
  fL1Comments.clear();

  fL1AlgoNames = {"CHOD", "RICH", "KTAG", "LAV", "IRCSAC", "STRAW", "MUV3", "NewCHOD"};

  ParseL0InputFile();
  ParseL0VacantBitsFile();
  ParseL0VariableBitsFile();
  ParseL1InputFile();
  //PrintConflicts();
}

/////////////////////////////////////////////////////////////////
// Read the run-dependent L0 trigger conditions from the database

void TriggerConditions::ParseL0InputFile() {
  if (fL0ConditionNames.size()) return; // input file already processed

  if (NA62ConditionsService::GetInstance()->Open(fL0InputFileName1)!=kSuccess) {
    cout <<"[TriggerConditions] Input file " << fL0InputFileName1 << " not found" << endl;
    throw Core::ReadException();
  }

  TString Line;
  Int_t NEntriesRead = 0, NEntriesReadTotal = 0, NConflicts = 0;
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fL0InputFileName1))) {
    if (Line.BeginsWith("#")) { // collect the comments
      if (Line.Length()>1) fL0Comments.push_back(Line);
      continue;
    }
    NEntriesReadTotal++;
    TObjArray *l = Line.Tokenize(" ");
    Int_t   Run1 = ((TObjString*)(l->At(0)))->GetString().Atoi();
    Int_t   Run2 = ((TObjString*)(l->At(1)))->GetString().Atoi();
    Int_t   Bit  = ((TObjString*)(l->At(2)))->GetString().Atoi();
    TString Cond = ((TObjString*)(l->At(3)))->GetString();
    Int_t   DS   = ((TObjString*)(l->At(4)))->GetString().Atoi();
    if (l->GetLast()==5) DS = -1; // downscaling varies during the run
    delete l;
    if (Cond=="Control") { // Control trigger
      for (Int_t iRun=Run1; iRun<=Run2; iRun++) {
	if (iRun<0 || iRun>=TC_NRUNS) continue; // run number out of range
	fControlTriggerDetectorID[iRun] = Bit;
	fControlTriggerDownscaling[iRun] = DS;
      }
      NEntriesRead++;
    }
    else { // Physics trigger
      if (Bit<0 || Bit>=TC_NBITS) continue; // invalid trigger bit
      vector<TString>::iterator it = find(fL0ConditionNames.begin(), fL0ConditionNames.end(), Cond);
      if (it == fL0ConditionNames.end()) { // add a new trigger condition to the list
	fL0ConditionNames.push_back(Cond);
	it = fL0ConditionNames.end() - 1;
      }
      Int_t index = distance(fL0ConditionNames.begin(), it);
      for (Int_t iRun=Run1; iRun<=Run2; iRun++) {
	if (iRun<0 || iRun>=TC_NRUNS) continue; // run number out of range
	if (fL0TriggerCondition[iRun][Bit] != -1) { // bit is occupied by another condition
	  fL0TriggerCondition[iRun][Bit] = -999; // -999 means a conflict
	  NConflicts++;
	}
	else { // bit is not occupied by another condition as expected
	  fL0TriggerCondition[iRun][Bit] = index;
	  fL0TriggerDownscaling[iRun][Bit] = DS;
	}
      }
      NEntriesRead++;
    }
  }
  NA62ConditionsService::GetInstance()->Close(fL0InputFileName1);
  cout << "[TriggerConditions] Found " << fL0ConditionNames.size() <<
    " L0 trigger conditions and " << NEntriesRead << " valid entries" << endl;
  if (NConflicts) cout << "[TriggerConditions] Found " << NConflicts << " L0 conflicts" << endl;
  if (NEntriesRead != NEntriesReadTotal)
    cout << "[TriggerConditions] Found " << NEntriesReadTotal-NEntriesRead << " L0 invalid entries" << endl;
}

//////////////////////////////////////////////////////////////////////
// Read the information about vacant L0 trigger bits from the database

void TriggerConditions::ParseL0VacantBitsFile() {

  if (NA62ConditionsService::GetInstance()->Open(fL0InputFileName2)!=kSuccess) {
    cout <<"[TriggerConditions] Input file " << fL0InputFileName2 << " not found" << endl;
    throw Core::ReadException();
  }

  TString Line;
  Int_t NEntriesRead = 0, NConflicts = 0;
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fL0InputFileName2))) {
    if (Line.BeginsWith("#")) continue;
    NEntriesRead++;
    TObjArray *l = Line.Tokenize(" ");
    Int_t Run1 = ((TObjString*)(l->At(0)))->GetString().Atoi();
    Int_t Run2 = ((TObjString*)(l->At(1)))->GetString().Atoi();
    for (Int_t i=2; i<=l->GetLast(); i++) {
      Int_t Bit = ((TObjString*)(l->At(i)))->GetString().Atoi();
      if (Bit<0 || Bit>=TC_NBITS) continue; // invalid trigger bit
      for (Int_t iRun=Run1; iRun<=Run2; iRun++) {
	if (iRun<0 || iRun>=TC_NRUNS) continue; // run number out of range
	if (fL0TriggerCondition[iRun][Bit] != -1) { // bit is occupied by another condition
	  fL0TriggerCondition[iRun][Bit] = -999; // -999 means a conflict
	  NConflicts++;
	}
	else { // bit is not occupied by another condition as expected
	  fL0TriggerCondition[iRun][Bit] = -2; // -2 means vacant
	}
      }
    }
    delete l;
  }
  NA62ConditionsService::GetInstance()->Close(fL0InputFileName2);
  cout << "[TriggerConditions] Found " << NEntriesRead << " vacant bit entries" << endl;
  if (NConflicts) cout << "[TriggerConditions] Found " << NConflicts << "L0 conflicts" << endl;
}

////////////////////////////////////////////////////////////////////////
// Read the information about variable L0 trigger bits from the database

void TriggerConditions::ParseL0VariableBitsFile() {

  if (NA62ConditionsService::GetInstance()->Open(fL0InputFileName3)!=kSuccess) {
    cout <<"[TriggerConditions] Input file " << fL0InputFileName3 << " not found" << endl;
    throw Core::ReadException();
  }

  TString Line;
  Int_t NEntriesRead = 0, NConflicts = 0;
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fL0InputFileName3))) {
    if (Line.BeginsWith("#")) continue;
    NEntriesRead++;
    TObjArray *l = Line.Tokenize(" ");
    Int_t Run1 = ((TObjString*)(l->At(0)))->GetString().Atoi();
    Int_t Run2 = ((TObjString*)(l->At(1)))->GetString().Atoi();
    for (Int_t i=2; i<=l->GetLast(); i++) {
      Int_t Bit = ((TObjString*)(l->At(i)))->GetString().Atoi();
      if (Bit<0 || Bit>=TC_NBITS) continue; // invalid trigger bit
      for (Int_t iRun=Run1; iRun<=Run2; iRun++) {
	if (iRun<0 || iRun>=TC_NRUNS) continue; // run number out of range
	if (fL0TriggerCondition[iRun][Bit] != -1) { // bit is occupied by another condition
	  fL0TriggerCondition[iRun][Bit] = -999; // -999 means a conflict
	  NConflicts++;
	}
	else { // bit is not occupied by another condition as expected
	  fL0TriggerCondition[iRun][Bit] = -3; // -3 means vacant
	}
      }
    }
    delete l;
  }
  NA62ConditionsService::GetInstance()->Close(fL0InputFileName3);
  cout << "[TriggerConditions] Found " << NEntriesRead << " variable bit entries" << endl;
  if (NConflicts) cout << "[TriggerConditions] Found " << NConflicts << " L0 conflicts" << endl;
}

/////////////////////////////////////////////////////////////////
// Read the run-dependent L1 trigger conditions from the database

void TriggerConditions::ParseL1InputFile() {
  if (fL1ConditionNames.size()) return; // input file already processed

  if (NA62ConditionsService::GetInstance()->Open(fL1InputFileName)!=kSuccess) {
    cout <<"[TriggerConditions] Input file " << fL1InputFileName << " not found" << endl;
    throw Core::ReadException();
  }

  TString Line;
  Int_t NEntriesRead = 0, NEntriesReadTotal = 0, NConflicts = 0;
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fL1InputFileName))) {
    if (Line.BeginsWith("#")) { // collect the comments
      if (Line.Length()>1) fL1Comments.push_back(Line);
      continue;
    }
    NEntriesReadTotal++;
    TObjArray *l = Line.Tokenize(" ");
    if (l->GetLast()!=4) {
      cout <<"[TriggerConditions] Invalid entry in L1 input file: " << Line << endl;
      l->Delete();
      delete l;
      continue;
    }
    Int_t   Run  = ((TObjString*)(l->At(0)))->GetString().Atoi();
    Int_t   Bit  = ((TObjString*)(l->At(1)))->GetString().Atoi();
    TString Cond = ((TObjString*)(l->At(2)))->GetString();
    Int_t   DS   = ((TObjString*)(l->At(3)))->GetString().Atoi();
    Int_t   Flag = ((TObjString*)(l->At(4)))->GetString().Atoi();

    l->Delete();
    delete l;
    if (Bit<0 || Bit>=TC_NBITS) continue; // invalid L0 trigger bit
    vector<TString>::iterator it =
      find(fL1ConditionNames.begin(), fL1ConditionNames.end(), Cond);
    if (it == fL1ConditionNames.end()) { // add a new trigger condition to the list
      fL1ConditionNames.push_back(Cond);
      it = fL1ConditionNames.end() - 1;
    }
    Int_t index = distance(fL1ConditionNames.begin(), it);
    if (Run<0 || Run>=TC_NRUNS) continue; // run number out of range
    if (fL1TriggerCondition[Run][Bit] != -1) { // bit is occupied by another condition
      fL1TriggerCondition[Run][Bit] = -999; // -999 means a conflict
      NConflicts++;
    }
    else { // bit is not occupied by another condition as expected
      fL1TriggerCondition[Run][Bit] = index;
      fL1TriggerDownscaling[Run][Bit] = DS;
      fL1TriggerStatus[Run][Bit] = Flag;
    }
    NEntriesRead++;
  }
  NA62ConditionsService::GetInstance()->Close(fL1InputFileName);
  cout << "[TriggerConditions] Found " << fL1ConditionNames.size() <<
    " L1 trigger conditions and " << NEntriesRead << " valid entries" << endl;
  if (NConflicts) cout << "[TriggerConditions] Found " << NConflicts << " L1 conflicts" << endl;
  if (NEntriesRead != NEntriesReadTotal)
    cout << "[TriggerConditions] Found " << NEntriesReadTotal-NEntriesRead << " L1 invalid entries" << endl;
}

//////////////////////////////////////////////////////////////////////////////////////////////
// Print trigger conflicts: multiple trigger conditions occupying the same mask in a given run.
// This happens e.g. when a trigger definition is changed during a run.
// The total number of conflicts could be smaller then reported by ParseL0InputFile()
// and other parse methods because there could be multiple conflicts
// affecting the same trigger bit.

void TriggerConditions::PrintConflicts() {
  Int_t NL0Conflicts = 0;
  Int_t NL1Conflicts = 0;
  for (Int_t iRun=0; iRun<TC_NRUNS; iRun++) {
    for (UInt_t iBit=0; iBit<TC_NBITS; iBit++) {
      if (fL0TriggerCondition[iRun][iBit]==-999) {
	NL0Conflicts++;
	cout << "[TriggerConditions] L0 Conflict: run " << iRun << " bit " << iBit << endl;
      }
      if (fL1TriggerCondition[iRun][iBit]==-999) {
	NL1Conflicts++;
	cout << "[TriggerConditions] L1 Conflict: run " << iRun << " bit " << iBit << endl;
      }
    }
  }
  if (NL0Conflicts) cout << "[TriggerConditions] Unique L0 trigger conflicts found: " << NL0Conflicts << endl;
  else              cout << "[TriggerConditions] No L0 trigger conflicts found" << endl;
  if (NL1Conflicts) cout << "[TriggerConditions] Unique L1 trigger conflicts found: " << NL1Conflicts << endl;
  else              cout << "[TriggerConditions] No L1 trigger conflicts found" << endl;
}

///////////////////////////////////////////
// Print trigger comments from run database

void TriggerConditions::PrintComments() {
  cout << "[TriggerConditions] Printing L0 trigger comments in the database" << endl;
  for (UInt_t i=0; i<fL0Comments.size(); i++) {
    cout << fL0Comments[i] << endl;
  }
  cout << "[TriggerConditions] Printing L1 trigger comments in the database" << endl;
  for (UInt_t i=0; i<fL1Comments.size(); i++) {
    cout << fL1Comments[i] << endl;
  }
}

//////////////////////////////////////////////////////////////////////////
// Translate a human-readable L0 trigger name into a trigger ID.
// Control trigger has ID=0, the rest are standard L0 triggers.
// This procedure is "slow" and should be done once per trigger condition,
// i.e. in the constructor of your analyser.

Int_t TriggerConditions::GetL0TriggerID(TString TriggerName) {
  vector<TString>::iterator it = find(fL0ConditionNames.begin(), fL0ConditionNames.end(), TriggerName);
  if (it == fL0ConditionNames.end()) {
    cout << "[TriggerConditions] Warning: L0 trigger condition " << TriggerName << " unknown" << endl;
    return -1;
  }
  return distance(fL0ConditionNames.begin(), it);
}

/////////////////////////////////////////////////////////////////////////////////////
// Return the trigger bit for a given run and L0TriggerID.
// -1 returned if this trigger condition was not employed in this run

Int_t TriggerConditions::GetTriggerBit(Int_t Run, Int_t L0TriggerID) {
  if (L0TriggerID<0) return -1; // unknown trigger condition
  Int_t bit = -1; // No trigger bit exists for this trigger condition - search to see if one exists...
  for (Int_t i=0; i<TC_NBITS; i++) if (fL0TriggerCondition[Run][i]==L0TriggerID) bit = i; 
  return bit; //if bit!=-1 (0,1,2...15) this is the trigger bit number...
}

/////////////////////////////////////////////////////////
// Checks of all possible data type conditions.
// For MC, DataType==0xFF: all these methods return true.

Bool_t TriggerConditions::IsPhysicsTrigger(L0TPData* L0Packet) {
  if (L0Packet==nullptr) return false;
  return (L0Packet->GetDataType() & 0x1);
}
Bool_t TriggerConditions::IsPhysicsTriggerOnly(L0TPData* L0Packet) {
  if (L0Packet==nullptr) return false;
  return (L0Packet->GetDataType() == 0x1);
}
Bool_t TriggerConditions::IsControlTrigger(L0TPData* L0Packet) {
  if (L0Packet==nullptr) return false;
  return (L0Packet->GetDataType() & 0x10);
}
Bool_t TriggerConditions::IsControlTriggerOnly(L0TPData* L0Packet) {
  if (L0Packet==nullptr) return false;
  return (L0Packet->GetDataType() == 0x10);
}
Bool_t TriggerConditions::IsPeriodicTrigger(L0TPData* L0Packet) {
  if (L0Packet==nullptr) return false;
  return (L0Packet->GetDataType() & 0x2);
}
Bool_t TriggerConditions::IsPeriodicTriggerOnly(L0TPData* L0Packet) {
  if (L0Packet==nullptr) return false;
  return (L0Packet->GetDataType() == 0x2);
}
Bool_t TriggerConditions::IsCalibrationTrigger(L0TPData* L0Packet) {
  if (L0Packet==nullptr) return false;
  return (L0Packet->GetDataType() & 0x4);
}
Bool_t TriggerConditions::IsCalibrationTriggerOnly(L0TPData* L0Packet) {
  if (L0Packet==nullptr) return false;
  return (L0Packet->GetDataType() == 0x4);
}
Bool_t TriggerConditions::IsLKrCalibrationTrigger(L0TPData* L0Packet) {
  if (L0Packet==nullptr) return false;
  return (L0Packet->GetDataType() & 0x8);
}
Bool_t TriggerConditions::IsLKrCalibrationTriggerOnly(L0TPData* L0Packet) {
  if (L0Packet==nullptr) return false;
  return (L0Packet->GetDataType() == 0x8);
}
Bool_t TriggerConditions::IsRandomTrigger(L0TPData* L0Packet) {
  if (L0Packet==nullptr) return false;
  return (L0Packet->GetDataType() & 0x20);
}
Bool_t TriggerConditions::IsRandomTriggerOnly(L0TPData* L0Packet) {
  if (L0Packet==nullptr) return false;
  return (L0Packet->GetDataType() == 0x20);
}

/////////////////////////////////////////////////////////////////////////
// Check if a L0 trigger condition (ID) is used in a given run.
// The trigger condition can be retrieved with the GetL0TriggerID method.

Bool_t TriggerConditions::L0TriggerEnabled(Int_t Run, Int_t L0TriggerID) {
  if (L0TriggerID<0) return false; // unknown trigger condition
  Bool_t Result = false;
  for (Int_t i=0; i<TC_NBITS; i++) Result |= (fL0TriggerCondition[Run][i]==L0TriggerID);
  return Result;
}

/////////////////////////////////////////////////////////////////////////////////////
// Return the L0 trigger downscaling for a given run.
// Zero returned if the condition is disabled, and -1 if the downscaling is variable.

Int_t TriggerConditions::GetL0TriggerDownscaling(Int_t Run, Int_t L0TriggerID) {
  if (L0TriggerID<0) return 0; // unknown trigger condition
  Int_t bit = -1;
  for (Int_t i=0; i<TC_NBITS; i++) if (fL0TriggerCondition[Run][i]==L0TriggerID) bit = i;
  if (bit==-1) return 0;
  return fL0TriggerDownscaling[Run][bit];
}

//////////////////////////////////////////////////////
// Return the L0 autopass downscaling for a given run.

Int_t TriggerConditions::GetL1AutopassDownscaling(Int_t /*Run*/) {
  return 50; // hard-coded for the moment; it was fixed to 50 in 2016
}

/////////////////////////////////////////////////////////////////////////////////////////
// Return the name of the L1 algorithm applied for a given L0 trigger bit in a given run.
// These names are read from the CDB

TString TriggerConditions::GetL1TriggerConditionName(Int_t Run, Int_t L0TriggerID) {
  if (L0TriggerID<0) return "none"; // unknown L0 trigger condition
  Int_t bit = -1;
  for (Int_t i=0; i<TC_NBITS; i++) if (fL0TriggerCondition[Run][i]==L0TriggerID) bit = i;
  if (bit==-1) return "none"; // this L0 trigger is not enabled for this run
  if (fL1TriggerCondition[Run][bit]<0) return "none";
  return fL1ConditionNames[fL1TriggerCondition[Run][bit]];
}

///////////////////////////////////////////////////////////////////////////////////////////////
// Return the downscaling of the L1 algorithm applied for a given L0 trigger bit in a given run

Int_t TriggerConditions::GetL1TriggerDownscaling(Int_t Run, Int_t L0TriggerID) {
  if (L0TriggerID<0) return 0; // unknown L0 trigger condition
  Int_t bit = -999;
  for (Int_t i=0; i<TC_NBITS; i++) if (fL0TriggerCondition[Run][i]==L0TriggerID) bit = i;
  if (bit==-1) return -999; // this L0 trigger is not enabled for this run
  if (fL1TriggerCondition[Run][bit]<0) return -999;
  return fL1TriggerDownscaling[Run][bit];
}

//////////////////////////////////////////////////////////////////////////////////////////
// Return the status of the L1 algorithm applied for a given L0 trigger bit in a given run
// (0=ok, 1=bad)
Int_t TriggerConditions::GetL1TriggerStatus(Int_t Run, Int_t L0TriggerID) {
  if (L0TriggerID<0) return 0; // unknown L0 trigger condition
  Int_t bit = -999;
  for (Int_t i=0; i<TC_NBITS; i++) if (fL0TriggerCondition[Run][i]==L0TriggerID) bit = i;
  if (bit==-1) return -999; // this L0 trigger is not enabled for this run
  if (fL1TriggerCondition[Run][bit]<0) return -999;
  return fL1TriggerStatus[Run][bit];
}

//////////////////////////////////////////////////////////////////////
// Check if a L0 trigger condition is satisfied for a given event.
// If the trigger is not enabled during a run, then FALSE is returned.

Bool_t TriggerConditions::L0TriggerOn(Int_t Run, L0TPData* L0Packet, Int_t L0TriggerID) {
  if (L0Packet==nullptr) return false; // L0TP data packet is a null pointer
  if (L0TriggerID<0) return false; // unknown L0 trigger condition
  Int_t  DataType = L0Packet->GetDataType();
  Bool_t PhysicsData = DataType & 0x1;
  if (!PhysicsData) return false; // event is not flagged by physics triggers
  Int_t TriggerFlags = L0Packet->GetTriggerFlags();
  Int_t bit = -1;
  for (Int_t i=0; i<TC_NBITS; i++) if (fL0TriggerCondition[Run][i]==L0TriggerID) bit = i;
  if (bit==-1) return false; // this L0 trigger condition is not enabled
  return (TriggerFlags>>bit) & 1;
}

/////////////////////////////////////////////////////
// Check if a L0 trigger bit is ON for a given event.

Bool_t TriggerConditions::L0TriggerBitOn(L0TPData* L0Packet, Int_t Bit) {
  if (L0Packet==nullptr) return false; // L0TP data packet is a null pointer
  Int_t  DataType = L0Packet->GetDataType();
  Bool_t PhysicsData = DataType & 0x1;
  if (!PhysicsData) return false; // event is not flagged by physics triggers
  Int_t TriggerFlags = L0Packet->GetTriggerFlags();
  return (TriggerFlags>>Bit) & 1;
}

//////////////////////////////////////////////////////////////////////
// The number of individual L1 algorithms applied for a certain L0 bit

Int_t TriggerConditions::GetNumberOfL1Algorithms(Int_t Run, L1TPData* L1Packet, Int_t L0TriggerID) {
  if (L1Packet==nullptr) return 0; // L1TP data packet is a null pointer
  if (L1Packet->GetTimeStamp()==0xFFFFFFFFFFFFFFFF) return 0; // for MC
  if (L0TriggerID<0) return 0; // unknown trigger condition
  Int_t bit = -1;
  for (Int_t i=0; i<TC_NBITS; i++) if (fL0TriggerCondition[Run][i]==L0TriggerID) bit = i;
  if (bit==-1) return 0; // this L0 trigger condition is not enabled
  std::vector<L1MaskBlock> L1Infos = L1Packet->GetL0Masks();
  for (UInt_t iMask=0; iMask<L1Infos.size(); iMask++) {
    if ((Int_t)L1Infos[iMask].GetL0MaskID() != bit) continue;
    std::vector<L1AlgoBlock> L1algos = L1Infos[iMask].GetL1Algorithms(); // retrieve L1 data block
    return L1algos.size();
  }
  return 0;
}

/////////////////////////////////////////////////////////////////
// The name of the i-th L1 algorithm applied for a certain L0 bit

TString TriggerConditions::GetL1AlgorithmName(Int_t Run, L0TPData* L0Packet, L1TPData* L1Packet, Int_t L0TriggerID, Int_t AlgoNumber) {
  if (L1Packet==nullptr) return "none"; // L1TP data packet is a null pointer
  if (L1Packet->GetTimeStamp()==0xFFFFFFFFFFFFFFFF) return "none"; // for MC
  if (L0TriggerID<0) return "none"; // unknown trigger condition
  if (!L0TriggerOn(Run, L0Packet, L0TriggerID)) return "unknown"; // this L0 trigger is on active in this event; L1 info not filled
  Int_t bit = -1;
  for (Int_t i=0; i<TC_NBITS; i++) if (fL0TriggerCondition[Run][i]==L0TriggerID) bit = i;
  if (bit==-1) return "none"; // this L0 trigger condition is not enabled
  std::vector<L1MaskBlock> L1Infos = L1Packet->GetL0Masks();
  for (UInt_t iMask=0; iMask<L1Infos.size(); iMask++) {
    if ((Int_t)L1Infos[iMask].GetL0MaskID() != bit) continue;
    std::vector<L1AlgoBlock> L1algos = L1Infos[iMask].GetL1Algorithms(); // retrieve L1 data block
    if ((UInt_t)AlgoNumber >= L1algos.size()) return "none";
    TString Logic = (L1algos[AlgoNumber].GetL1AlgoFlags() & 4) ? "" : "n"; // positive or veto?
    return Logic + fL1AlgoNames[L1algos[AlgoNumber].GetL1AlgoID()];
  }
  return "none";
}

//////////////////////////////////////////////////////////////////
// Check if a L0 trigger condition is satisfied for a given event.
// This works for physics triggers only.

Bool_t TriggerConditions::L1TriggerOn(Int_t Run, L1TPData* L1Packet, Int_t L0TriggerID, Int_t AlgoNumber) {
  if (L1Packet==nullptr) return false; // L1TP data packet is a null pointer
  if (L1Packet->GetTimeStamp()==0xFFFFFFFFFFFFFFFF) return true; // for MC
  if (L0TriggerID<0) return false; // unknown trigger condition
  Int_t bit = -1;
  for (Int_t i=0; i<TC_NBITS; i++) if (fL0TriggerCondition[Run][i]==L0TriggerID) bit = i;
  if (bit==-1) return false; // this L0 trigger condition is not enabled
  std::vector<L1MaskBlock> L1Infos = L1Packet->GetL0Masks();
  Int_t L1TriggerWord = -1;
  for (UInt_t iMask=0; iMask<L1Infos.size(); iMask++) {
    if ((Int_t)L1Infos[iMask].GetL0MaskID() != bit) continue;
    if (AlgoNumber==-1) { // check the whole chain of L1 algorithms
      L1TriggerWord = L1Infos[iMask].GetL1TriggerWord();
      return (L1TriggerWord & 1);
    }
    else { // check a particular L1 algorithm
      std::vector<L1AlgoBlock> L1algos = L1Infos[iMask].GetL1Algorithms(); // retrieve L1 data block
      if ((UInt_t)AlgoNumber >= L1algos.size()) return false;
      Int_t AlgoQuality = L1algos[AlgoNumber].GetL1QualityFlags();

      // Patch to retrieve correct L1 KTAG verdict for Run Number < 7969
      Bool_t AlgoLogic = L1algos[AlgoNumber].GetL1AlgoFlags() & (1<<kL1KTAG);
      std::vector<UInt_t> l1data = L1algos[AlgoNumber].GetL1DataWords();
      if (Run<7969 && L1algos[AlgoNumber].GetL1AlgoID()==kL1KTAG) {
	if (AlgoLogic) return ((UInt_t)l1data[0]>4);
	else return !((UInt_t)l1data[0]>4);
      }
      else return (AlgoQuality & 1);
    }
  }
  return false;
}

// Meanings of L1 bits:
// Bool_t bitA = (L1Word &   1); // event is passing at least a L1 trigger mask
// Bool_t bitD = (L1Word &   8); // l1 timeout flag is on, event must be discarded
// Bool_t bitE = (L1Word &  16); // event selected by a L0 mask with no L1 applied 
// Bool_t bitF = (L1Word &  32); // bypassed events (control, special, periodics)
// Bool_t bitG = (L1Word &  64); // event selected by a trigger mask with L1 in flagging
// Bool_t bitH = (L1Word & 128); // autopassed event

// L1 bypass event (control, special, periodics)
Bool_t TriggerConditions::L1TriggerBypass(EventHeader* EvtHdr) {
  Int_t L1Word = (EvtHdr->GetTriggerType() & 0x00FF00) >> 8;
  return (L1Word & 32);
}

// L1 autopass event (in 2016, they are 2% of the total L0 input)
Bool_t TriggerConditions::L1TriggerAutopass(EventHeader* EvtHdr) {
  Int_t L1Word = (EvtHdr->GetTriggerType() & 0x00FF00) >> 8;
  return (L1Word & 128);
}

// At least one L1 algorithm that processed an event worked in flagging mode
Bool_t TriggerConditions::AnyL1TriggerInFlaggingMode(EventHeader* EvtHdr) {
  Int_t L1Word = (EvtHdr->GetTriggerType() & 0x00FF00) >> 8;
  return (L1Word & 64);
}

Bool_t TriggerConditions::L1TriggerInFlaggingMode(Int_t Run, L1TPData* L1Packet, Int_t L0TriggerID, Int_t AlgoNumber) {
  if (L1Packet==nullptr) return false; // L1TP data packet is a null pointer
  if (L1Packet->GetTimeStamp()==0xFFFFFFFFFFFFFFFF) return true; // for MC
  if (L0TriggerID<0) return false; // unknown trigger condition
  Int_t bit = -1;
  for (Int_t i=0; i<TC_NBITS; i++) if (fL0TriggerCondition[Run][i]==L0TriggerID) bit = i;
  if (bit==-1) return false; // this L0 trigger condition is not enabled
  std::vector<L1MaskBlock> L1Infos = L1Packet->GetL0Masks();
  Int_t L1TriggerFlags = -1;
  for (UInt_t iMask=0; iMask<L1Infos.size(); iMask++) {
    if ((Int_t)L1Infos[iMask].GetL0MaskID() != bit) continue;
    if (AlgoNumber==-1) { // check the whole chain of L1 algorithms
      L1TriggerFlags = L1Infos[iMask].GetL1Flags();
      return (L1TriggerFlags & (1<<6));
    }
    else { // check a particular L1 algorithm
      std::vector<L1AlgoBlock> L1algos = L1Infos[iMask].GetL1Algorithms(); // retrieve L1 data block
      if ((UInt_t)AlgoNumber >= L1algos.size()) return false;
      Bool_t AlgoFlag = L1algos[AlgoNumber].GetL1AlgoFlags() & (1<<4);
      return AlgoFlag;
    }
  }
  return false;
}

// Check if the control trigger is enabled in a given run (i.e. found in the input file)
Bool_t TriggerConditions::ControlTriggerEnabled(Int_t Run) {
  return (fControlTriggerDetectorID[Run]!=-999);
}

//////////////////////////////////////////////////////////////////////////////
// Return the control trigger downscaling.
// -999 if control trigger is disabled, and -1 if the downscaling is variable.

Int_t TriggerConditions::GetControlTriggerDownscaling(Int_t Run) {
  return fControlTriggerDownscaling[Run];
}

//////////////////////////////////////////////////////////
// Print L0+L1 trigger chains enabled for a particular run

void TriggerConditions::PrintTriggersForRun(Int_t Run) {
  cout << "[TriggerConditions] Trigger conditions for run " << Run << endl;
  for (Int_t i=0; i<TC_NBITS; i++) {
    if (fL0TriggerCondition[Run][i]<0) continue;
    Int_t DS = fL0TriggerDownscaling[Run][i];
    cout <<
      "[TriggerConditions] Run "<<Run<<" L0 bit " << i << ": " <<
      fL0ConditionNames[fL0TriggerCondition[Run][i]] << " (DS=";
    if (DS>0) cout << DS; else cout << "var"; // DS=0 means variable downscaling during the run
    cout << ")";
    if (fL1TriggerCondition[Run][i]>=0) {
      cout <<" --> L1: " << fL1ConditionNames[fL1TriggerCondition[Run][i]] << " (DS=" <<
	fL1TriggerDownscaling[Run][i] << " Status=" << fL1TriggerStatus[Run][i] << ")";
    }
    cout << endl;
  }
  Int_t DS = fControlTriggerDownscaling[Run];
  cout << "[TriggerConditions] Control trigger: ";
  if (DS>0) cout << "DS= "<< DS;
  else if (DS==-1)   cout << "variable downscaling";
  else if (DS==-999) cout << "disabled"; // no control trigger in this run (or not in the offline DB)
  cout << endl;
}

//////////////////////////////////////////////////////////
// Print runs with a certaint L0 trigger condition enabled

void TriggerConditions::PrintRunsForTrigger(Int_t L0TriggerID, Int_t MinRun, Int_t MaxRun) {
  if (L0TriggerID<0) return; // unknown trigger condition
  cout << "[TriggerConditions] Runs with enabled L0 trigger " << fL0ConditionNames[L0TriggerID] << endl;
  for (Int_t iRun=MinRun; iRun<=MaxRun; iRun++) {
    if (!L0TriggerEnabled(iRun, L0TriggerID)) continue;
    Int_t bit = -1;
    for (Int_t i=0; i<TC_NBITS; i++) if (fL0TriggerCondition[iRun][i]==L0TriggerID) bit = i;
    Int_t DS = GetL0TriggerDownscaling(iRun, L0TriggerID);
    cout << "[TriggerConditions] Run " << iRun << " L0 bit " << bit << ": " <<
      fL0ConditionNames[L0TriggerID] << " (DS=";
    if (DS>0) cout << DS; else cout << "var"; // DS=0 means variable downscaling during the run
    cout <<")";

    if (fL1TriggerCondition[iRun][bit]>=0) {
      cout <<" --> L1: " << fL1ConditionNames[fL1TriggerCondition[iRun][bit]] << " (DS=" <<
	fL1TriggerDownscaling[iRun][bit] << " Status=" << fL1TriggerStatus[iRun][bit] << ")";
    }
    cout << endl;
  }
}

//////////////////////////////////////////////////////
// Check for missing L0 bits not known to the database

void TriggerConditions::CheckMissingBitsForRun(Int_t Run, Int_t MinBit, Int_t MaxBit) {
  Int_t NMissingBits = 0;
  for (Int_t i=MinBit; i<=MaxBit; i++) {
    if (fL0TriggerCondition[Run][i]==-1) NMissingBits++; // -1: missing info, -2: vacant, -3: variable, -999: conflict
  }
  if (NMissingBits>0) {
    cout << "[TriggerConditions] Run " << Run << " has missing L0 bits:";
    for (Int_t i=MinBit; i<=MaxBit; i++) {
      if (fL0TriggerCondition[Run][i]==-1) cout << " " << i;
    }
    cout << endl;
  }
}

///////////////////////
// Detailed L0 printout

void TriggerConditions::PrintL0() {
  cout << "[TriggerConditions] Printing L0 trigger conditions found in the database" << endl;
  for (UInt_t i=0; i<fL0ConditionNames.size(); i++) {
    cout << fL0ConditionNames[i] << endl;
  }

  cout << "[TriggerConditions] Printing run intervals for each L0 trigger condition" << endl;
  for (UInt_t iTrigger=0; iTrigger<fL0ConditionNames.size(); iTrigger++) {
    cout << "Trigger condition #"<<iTrigger<<" ("<<fL0ConditionNames[iTrigger] << ") is enabled in runs:" << endl;
    Bool_t InInterval = false;
    Int_t FirstRun = 0;
    for (Int_t iRun=0; iRun<TC_NRUNS; iRun++) {
      if (L0TriggerEnabled(iRun, iTrigger)) {
	if (!InInterval) {
	  InInterval = true;
	  FirstRun = iRun;
	}
      }
      else {
	if (InInterval) {
	  InInterval = false;
	  cout << FirstRun << "-" << iRun-1 << endl;
	}
      }
    }
  }

  cout << "Control trigger is enabled in runs:" << endl;
  Bool_t InInterval = false;
  Int_t FirstRun = 0;
  for (Int_t iRun=0; iRun<TC_NRUNS; iRun++) {
    if (GetControlTriggerDownscaling(iRun)!=999) { // -999: disabled, -1: variable; otherwise it is positive
      if (!InInterval) {
	InInterval = true;
	FirstRun = iRun;
      }
    }
    else {
      if (InInterval) {
	InInterval = false;
	cout << FirstRun << "-" << iRun-1 << endl;
      }
    }
  }
}
