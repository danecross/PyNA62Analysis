// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-12-14
//
// ---------------------------------------------------------

#ifndef DOWNSTREAMTRACKENERGYCLUSTERASSOCIATION_HH
#define DOWNSTREAMTRACKENERGYCLUSTERASSOCIATION_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"

class TH1I;
class TH2F;
class TGraph;
class TTree;

class DownstreamTrackEnergyClusterAssociation : public NA62Analysis::Analyzer{
  public:
    explicit DownstreamTrackEnergyClusterAssociation(NA62Analysis::Core::BaseAnalysis *ba);
    ~DownstreamTrackEnergyClusterAssociation();
    void InitHist();
    void InitOutput();
    void DefineMCSimple();
    void Process(Int_t iEvent);
    void PostProcess();
    void StartOfBurstUser();
    void EndOfBurstUser();
    void StartOfRunUser();
    void EndOfRunUser();
    void EndOfJobUser();
    void DrawPlot();
  protected:


};
#endif
