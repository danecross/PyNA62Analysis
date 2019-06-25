// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-11-28
//
// ---------------------------------------------------------------

/// \class FilterAll
/// \Brief
/// Filter all events into the output
/// \EndBrief
/// \Detailed
/// Convenient for filtering out a specific event from a file. Example of use:
/// \code
/// ./MyApplication -i <input_file> -o <output_file> --start 100 -n 1 --filter
/// \endcode
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include "FilterAll.hh"
#include "Event.hh"
#include "Persistency.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

FilterAll::FilterAll(Core::BaseAnalysis *ba) :
  Analyzer(ba, "FilterAll") {
  RequestAllMCTrees();
  RequestAllRecoTrees();
  AddParam("Downscaling", &fDownscaling, 1); // by default, no downscaling
}

void FilterAll::Process(Int_t) {
  if (GetEventHeader()->GetTimeStamp() % fDownscaling) return;
  FilterAccept();
}
