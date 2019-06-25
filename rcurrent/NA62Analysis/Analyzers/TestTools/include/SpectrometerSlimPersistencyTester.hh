#ifndef SPECTROMETER_PERSISTENCY_TESTER_HH
#define SPECTROMETER_PERSISTENCY_TESTER_HH

#include "Analyzer.hh"

class TRecoSpectrometerEvent;

class SpectrometerSlimPersistencyTester : public NA62Analysis::Analyzer {

public:
    explicit SpectrometerSlimPersistencyTester(NA62Analysis::Core::BaseAnalysis *ba);
    ~SpectrometerSlimPersistencyTester() {}
    void InitHist() {}
    void InitOutput();
    void DefineMCSimple() {}
    void Process(Int_t);
    void StartOfBurstUser() {}
    void EndOfBurstUser() {}
    void StartOfRunUser() {}
    void EndOfRunUser() {}
    void EndOfJobUser() {}
    void PostProcess();
    void DrawPlot() {}

private:
    TRecoSpectrometerEvent *fSpectrometerEvent;
};

#endif
