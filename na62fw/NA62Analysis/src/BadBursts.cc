// ------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)    2016-03-14
// Modified by Nicolas Lurkin (nlurkin@cern.ch)            2016-03-30
// Migration to CDB by Karim Massri (karim.massri@cern.ch) 2018-12-04 
//
// ------------------------------------------------------------------

/// \class BadBursts
/// \Brief
/// Interface to the bad bursts database
/// \EndBrief
/// \Detailed
/// The bad bursts database is located in the conditions database.
/// This class provides the user methods IsBad() and IsGood().
/// These methods are used by InputTree::SkipBadBurst() to skip bad bursts.<br><br>
/// Some systems can be masked in the bad burst check via the --no-check-[system] command line arguments.
/// Also, the SystemsToCheck and SystemsToIgnore data cards in the compilation configuration file
/// can be used enable and disable systems for the bad burst check.
/// Internally, this is handled via the "bad burst masks" specifying which systems
/// are to be checked; the bit encoding is defined in NA62Global.hh.<br><br>
/// The bad burst database can be also queried for each individual subsystem
/// (regardless of the bad burst mask settings) from analyzers as follows:
/// \code
/// Bool_t BadBurst_LAV = BadBursts::GetInstance()->IsBadForSubsystem(GetBurstID(), kLAV, GetWithMC());
/// \endcode
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "BadBursts.hh"
#include "TObjArray.h"
#include "ConfigSettings.hh"
#include "NA62ConditionsService.hh"

using namespace std;

namespace NA62Analysis {

static BadBursts* fInstance = nullptr;

BadBursts* BadBursts::GetInstance() {
  if (!fInstance) fInstance = new BadBursts();
  return fInstance;
}

BadBursts::BadBursts() :
  Verbose("BadBursts"), fFileName("BadBursts.dat"), fCDBTag(""), fRunNumber(0), fSystemNames(64, ""),
  // By default, bad bursts are checked for all subsystems.
  // Note that bit #1 (dummy) is disabled. It is not a subsystem,
  // but a flag meaning that the source of information is NumberOfHitsMonitor.
  fSystemsActive(0xFFFFFFFFFFFFFFFE),
  fNoSkipBadBurst(Configuration::ConfigSettings::CLI::fNoSkipBadBurst),
  fNoUseBadBurst (Configuration::ConfigSettings::CLI::fNoUseBadBurst) {

  // System names must follow the IDs defined in NA62Tools/include/NA62Global.hh
  fSystemNames[kCedar       ] = "Cedar";
  fSystemNames[kGigaTracker ] = "GTK";
  fSystemNames[kCHANTI      ] = "CHANTI";
  fSystemNames[kLAV         ] = "LAV";
  fSystemNames[kSpectrometer] = "Straw";
  fSystemNames[kCHOD        ] = "CHOD";
  fSystemNames[kRICH        ] = "RICH";
  fSystemNames[kIRC         ] = "IRC";
  fSystemNames[kLKr         ] = "LKr";
  fSystemNames[kMUV1        ] = "MUV1";
  fSystemNames[kMUV2        ] = "MUV2";
  fSystemNames[kMUV3        ] = "MUV3";
  fSystemNames[kSAC         ] = "SAC";
  fSystemNames[kNewCHOD     ] = "NewCHOD";
  fSystemNames[kHAC         ] = "HAC";
  fSystemNames[kL0TP        ] = "L0TP";
  fSystemNames[kL1TP        ] = "L1TP";
  fSystemNames[kL2EB        ] = "L2EB";
  fSystemNames[kDIM         ] = "DIM";
  fSystemNames[kMUV0        ] = "MUV0";
  fSystemNames[kSAV         ] = "SAV";
  fSystemNames[kProcessing  ] = "Processing";
  // L0 sub-systems
  fSystemNames[32+kL0CHOD   ] = "L0CHOD";
  fSystemNames[32+kL0RICH   ] = "L0RICH";
  fSystemNames[32+kL0LAV    ] = "L0LAV";
  fSystemNames[32+kL0MUV3   ] = "L0MUV3";
  fSystemNames[32+kL0NewCHOD] = "L0NewCHOD";
  fSystemNames[32+kL0TALK   ] = "L0TALK";
  fSystemNames[32+kL0Calo   ] = "L0Calo";
  fNSystems = fSystemNames.size();
}

// Disable bad burst checks for a list of systems
void BadBursts::MaskSystems(std::set<std::string> noCheckList) {
  if (Configuration::ConfigSettings::CLI::fNoSkipBadBurst)
    fSystemsActive = 0x0; // mask all systems
  else {
    for (auto system : noCheckList) {
      Bool_t SystemFound = false;
      for (UInt_t i=0; i<fNSystems; i++) {
        if (system==fSystemNames[i]) {
          fSystemsActive &= ~((ULong64_t)1<<i);
          SystemFound = true;
        }
      }
      if (!SystemFound) {
        cout << normal() << "Invalid systems to be ignored specified: " << system << endl;
      }
    }
  }
}

////////////////////////////////////
// Read bad burst lists from the CDB

void BadBursts::InitBadBurstsList() {

  // Clear lists
  fBadBurstIDList.clear();
  fBadBurstIDList_mask.clear();

  if (fNoUseBadBurst) {
    cout << normal() << "Bad burst database is not read (--no-use-badburst flag)" << endl;
    return;
  }

  Int_t EntriesFound = 0;
  if(NA62ConditionsService::GetInstance()->Open(fFileName)==kSuccess) { // protect against reading the file regardless of the -eN options (N>0)
    TString Line;
    while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fFileName))) {
      if (Line.BeginsWith("#")) continue;
      TObjArray *l = Line.Tokenize(" ");
      Int_t n=0;
      if (((TObjString*)(l->At(0)))->GetString() == "B") n++; // pre-v1.1.0 format
      Int_t RunNumber = ((TObjString*)(l->At(n)))->GetString().Atoi();
      Int_t BurstID   = ((TObjString*)(l->At(n+1)))->GetString().Atoi();
      ULong64_t Mask  = strtol(((TObjString*)(l->At(n+2)))->GetString(), NULL, 16); // hex
      if (RunNumber!=fRunNumber) {
        cout << normal() << "Error: corrupted bad burst database" << endl;
        exit(kGenericError);
      }
      fBadBurstIDList.push_back(BurstID);
      fBadBurstIDList_mask.push_back(Mask);
      EntriesFound++;
      delete l;
    }
    NA62ConditionsService::GetInstance()->Close(fFileName);
  }

  cout << normal() << "Found " << EntriesFound << " bad bursts for tag " <<
    NA62ConditionsService::GetInstance()->GetCDBTag() << " run " << fRunNumber << endl;
}

void BadBursts::Print() {
  vector<TString> SystemsChecked, SystemsIgnored;
  for (UInt_t i=0; i<fNSystems; i++) {
    if (!fSystemNames[i].Length()) continue;
    if (fSystemsActive & (ULong64_t)1<<i) SystemsChecked.push_back(fSystemNames[i]);
    else SystemsIgnored.push_back(fSystemNames[i]);
  }
  std::sort(SystemsChecked.begin(), SystemsChecked.end());
  std::sort(SystemsIgnored.begin(), SystemsIgnored.end());
  if (TestLevel(Verbosity::kNormal)) {
    cout << normal() << "Systems checked";
    if      (!SystemsChecked.size()) cout << ": none";
    else if (!SystemsIgnored.size()) cout << ": all";
    else {
      cout << " (" << SystemsChecked.size() << "):";
      for (auto system : SystemsChecked) cout << " " << system;
    }

    cout << endl << normal() << "Systems ignored";
    if      (!SystemsChecked.size()) cout << ": all";
    else if (!SystemsIgnored.size()) cout << ": none";
    else {
      cout << " (" << SystemsIgnored.size() << "):";
      for (auto system : SystemsIgnored) cout << " " << system;
    }
    cout << endl;
  }

  if (Configuration::ConfigSettings::CLI::fInvertBadBurst)
    cout << normal() << "Output logic is inverted" << endl;
}

///////////////////////////////////////////////////////////////////
// Bad burst by EITHER burst time OR run number and burst ID:
// The corresponding IsGood() is used by InputTree::SkipBadBurst().

Bool_t BadBursts::IsBad(UInt_t BurstID) {

  // Update the bad burst list in case CDB tag or run number changes
  if (fCDBTag    != NA62ConditionsService::GetInstance()->GetCDBTag() ||
      fRunNumber != NA62ConditionsService::GetInstance()->GetCurrentRunID()) {
    fCDBTag    = NA62ConditionsService::GetInstance()->GetCDBTag();
    fRunNumber = NA62ConditionsService::GetInstance()->GetCurrentRunID();
    InitBadBurstsList();
  }

  // Look for burstID in the BadBurstID list
  UInt_t posBurstID =
    find(fBadBurstIDList.begin(), fBadBurstIDList.end(), BurstID) - fBadBurstIDList.begin();
  ULong64_t BadBurstMask = 0;
  if (posBurstID<fBadBurstIDList.size()) { // burst found in the BadBurstID list
    BadBurstMask = fBadBurstIDList_mask[posBurstID];
  }

  // Debug
  //cout << "BadBurstCheck:    "; for (Int_t i=0; i<64; i++) cout << ((fSystemsActive>>i)&1); cout << endl;
  //cout << "ThisBurst(in CDB):"; for (Int_t i=0; i<64; i++) cout << ((BadBurstMask>>i)&1);   cout << endl;

  return (BadBurstMask & fSystemsActive);
}

Bool_t BadBursts::IsGood(UInt_t BurstID) {
  return !IsBad(BurstID);
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Check a particular subsystem: DetectorID is defined in NA62Global.hh.
// This method is provided for users running multiple analyzers wih different bad burst conditions.

Bool_t BadBursts::IsBadForSubsystem(UInt_t BurstID, DetectorID id, Bool_t isMC) {
  if (isMC) return false;
  ULong64_t SystemsActive = fSystemsActive;
  fSystemsActive = (1<<id);
  Bool_t check = IsBad(BurstID);
  fSystemsActive = SystemsActive;
  return check;
}

Bool_t BadBursts::IsGoodForSubsystem(UInt_t BurstID, DetectorID id, Bool_t isMC) {
  return !IsBadForSubsystem(BurstID, id, isMC);
}

} /* namespace NA62Analysis */
