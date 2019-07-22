#include <stdlib.h>
#include <iostream>
#include "SpectrometerSlimPersistencyTester.hh"
#include "Event.hh"
#include "Persistency.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

SpectrometerSlimPersistencyTester::SpectrometerSlimPersistencyTester(Core::BaseAnalysis *ba) :
    Analyzer(ba, "SpectrometerSlimPersistencyTester")
{
    fSpectrometerEvent = new TRecoSpectrometerEvent();
    RequestTree(new TRecoCedarEvent);
    RequestTree(fSpectrometerEvent);
}


void SpectrometerSlimPersistencyTester::InitOutput()
{
    OpenNewTree("Reco", "Reconstructed event tree");
    AddBranch<TRecoSpectrometerEvent>("Reco", "Spectrometer", fSpectrometerEvent);
}

void SpectrometerSlimPersistencyTester::Process(Int_t)
{
    //FilterAccept();
}

void SpectrometerSlimPersistencyTester::PostProcess()
{
}
