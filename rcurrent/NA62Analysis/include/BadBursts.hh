// ------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)    2016-03-14
//
// ------------------------------------------------------------------

#ifndef BADBURSTS_HH
#define BADBURSTS_HH

#include <set>
#include "TString.h"
#include "Verbose.hh"
#include "EventHeader.hh"

namespace NA62Analysis {

class BadBursts: public Verbose {

public:
  static BadBursts* GetInstance();

  Bool_t IsBad (UInt_t BurstID);
  Bool_t IsGood(UInt_t BurstID);
  Bool_t IsBadForSubsystem (UInt_t BurstID, DetectorID id, Bool_t isMC);
  Bool_t IsGoodForSubsystem(UInt_t BurstID, DetectorID id, Bool_t isMC);

  void MaskSystems(std::set<std::string> noCheckList); ///< Mask some systems from bad burst checks

  // Direct access to the flags with information about active and masked systems, for expert use only
  ULong64_t GetSystemsActive()              { return fSystemsActive; }
  void      SetSystemsActive(ULong64_t val) { fSystemsActive = val;  }

  void Print(); ///< Print informarion about systems checked and ignored for bad burst checks

private:

  BadBursts();
  ~BadBursts() {}
  void InitBadBurstsList();

  TString fFileName;  ///< Input file name on CDB
  TString fCDBTag;    ///< CDB tag (=sw revision the data is reconstructed with); bad burst list depends on it
  Int_t   fRunNumber; ///< Current run number; bad burst list depends on it

  std::vector<TString> fSystemNames; ///< Subsystem names, see NA62Tools/include/NA62Global.hh
  UInt_t    fNSystems;               ///< Number of subsystems
  ULong64_t fSystemsActive;  ///< Bit mask defining active subsystems, as defined in NA62Tools/includeNA62Global.hh
  Bool_t    fNoSkipBadBurst; ///< Is bad burst skipping disabled by user via --no-check-badburst-all or otherwise?
  Bool_t    fNoUseBadBurst;  ///< Is reading of bad burst DB disabled by user via --no-use-badburst?

  std::vector<Short_t>   fBadBurstIDList;      ///< List of bad burst IDs
  std::vector<ULong64_t> fBadBurstIDList_mask; ///< The corresponding system masks
};

} /* namespace NA62Analysis */

#endif
