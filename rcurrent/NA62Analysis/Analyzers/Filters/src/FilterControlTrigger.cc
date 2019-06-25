// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-07-12
//
// ---------------------------------------------------------------

/// \class FilterControlTrigger
/// \Brief
/// Filter events triggered with the control trigger
/// \EndBrief
/// \Detailed
/// Events triggered with the control trigger (data) or all events (MC) are filtered.
/// A downscaling is controllable by the user can be applied;
/// events with timestamp divisible by the downscaling are filtered out. Example of use:
/// \code
/// ./MyApplication -i <input_file> -o <output_file> -p "FilterControlTrigger:Downscaling=10" --filter
/// \endcode
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include "TriggerConditions.hh"
#include "FilterControlTrigger.hh"
#include "Event.hh"
#include "Persistency.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

FilterControlTrigger::FilterControlTrigger(Core::BaseAnalysis *ba) :
  Analyzer(ba, "FilterControlTrigger") {
  RequestAllMCTrees();
  RequestAllRecoTrees();
  AddParam("Downscaling", &fDownscaling, 1); // by default, no downscaling
}

void FilterControlTrigger::Process(Int_t) {
  Bool_t ControlTrigger = TriggerConditions::GetInstance()->IsControlTrigger(GetL0Data());
  if (!ControlTrigger) return;
  if (GetEventHeader()->GetTimeStamp() % fDownscaling) return;
  FilterAccept();
}
