/*
 * EventChecker.hh
 *
 *  Created on: 13 Feb 2017
 *      Author: ncl
 */

#ifndef INCLUDE_EVENTCHECKER_HH_
#define INCLUDE_EVENTCHECKER_HH_

#include <set>
#include <TString.h>
#include "Verbose.hh"
#include "NA62Global.hh"

class EventHeader;
class TH1I;

namespace NA62Analysis {

/// \class EventChecker
/// \Brief
/// Class used to check if the event is good.
/// \EndBrief
///
/// \Detailed
/// This is checking the Quality mask of the event. The bit of specific detectors can be
/// ignored if requested.
/// \EndDetailed
class EventChecker : public Verbose{
public:
	EventChecker(std::set<std::string> detectors, TString rev);
	virtual ~EventChecker();

	bool IsGoodEvent(EventHeader *header);
	void SetRevision(TString rev){
		/// \MemberDescr
		/// \param rev: Softwaree revision used to reconstruct currently read data
		///
		/// Set the current software revision for the read file.
		/// \EndMemberDescr
		fRevision = rev;
	}
	int GetTotalSkippedEvents();

	static bool BadQualityMask(EventHeader *header, DetectorID det);
private:

	UInt_t  fDetFlag;      ///< Flag of detectors that should not be checked
	TString fRevision;     ///< Software revision
	TH1I*   fFailedEventsMasked; ///< Histograms to monitor bad events (ignoring bad events for masked detectors)
	TH1I*   fFailedEventsTotal; ///< Histograms to monitor bad events (including bad events for masked detectors)
};

} /* namespace NA62Analysis */

#endif /* INCLUDE_EVENTCHECKER_HH_ */
