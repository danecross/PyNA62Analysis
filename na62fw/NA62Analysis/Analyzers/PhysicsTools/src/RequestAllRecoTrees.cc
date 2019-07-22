#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "Event.hh"
#include "RequestAllRecoTrees.hh"
#include "Persistency.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class RequestAllRecoTrees
/// \Brief
/// Request all known Reco TTrees and branches.
/// \EndBrief
/// \Detailed
/// Can be used for filtering in order to export everything.
/// \EndDetailed

RequestAllRecoTrees::RequestAllRecoTrees(Core::BaseAnalysis *ba) : Analyzer(ba, "RequestAllRecoTrees") {
  UserMethods::RequestAllRecoTrees();
}
