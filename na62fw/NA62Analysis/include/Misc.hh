/*
 * Misc.hh
 *
 *  Created on: Mar 8, 2016
 *      Author: nlurkin
 */

#ifndef INCLUDE_MISC_HH_
#define INCLUDE_MISC_HH_

#include <string>
#include <Rtypes.h>

class TFile;

namespace NA62Analysis {

  /// \class BurstID
  /// \Brief
  /// Struct used to pass burst information between functions
  /// \EndBrief
  /// \Detailed
  /// Struct used to pass burst information between functions
  /// \EndDetailed
struct BurstID {
    UInt_t fBurstID;       ///< Burst ID
    UInt_t fRunID;         ///< Run ID
    UInt_t fBurstTime;     ///< Burst time
    UInt_t fNEventsInTree; ///< Number of events in the tree
};

std::string GetNA62AnalysisPath();
std::string GetUserAnalysisPath();

std::pair<Double_t,Double_t> GetMemoryUsage();

bool GetValueFromString(std::string s, int& ref);
bool GetValueFromString(std::string s, long& ref);
bool GetValueFromString(std::string s, float& ref);
bool GetValueFromString(std::string s, double& ref);

bool GetCurrentBurstInfo(TFile* fd, struct BurstID &b);

} /* namespace NA62Analysis */

#endif /* INCLUDE_MISC_HH_ */
