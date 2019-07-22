#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "Event.hh"
#include "RequestAllMCTrees.hh"
#include "Persistency.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class RequestAllMCTrees
/// \Brief
/// Simply request all known MC TTrees and branches. Can be used for filtering in order to export everything.
/// \EndBrief
///
/// \Detailed
/// \EndDetailed

RequestAllMCTrees::RequestAllMCTrees(Core::BaseAnalysis *ba) : Analyzer(ba, "RequestAllMCTrees") {
  UserMethods::RequestAllMCTrees();
}
