// ---------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2018-05-03
//
// ---------------------------------------------------------

#ifndef BUILDNEWCHODPILEUPHISTO_HH
#define BUILDNEWCHODPILEUPHISTO_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"

class TH1I;
class TH2F;
class TGraph;
class TTree;

class BuildNewCHODPileupHisto : public NA62Analysis::Analyzer
{
	public:
		explicit BuildNewCHODPileupHisto(NA62Analysis::Core::BaseAnalysis *ba);
		~BuildNewCHODPileupHisto();
		void InitHist();
		void InitOutput();
		void DefineMCSimple();
		void ProcessSpecialTriggerUser(int iEvent, unsigned int triggerType);
		void Process(int iEvent);
		void PostProcess();
		void StartOfBurstUser();
		void EndOfBurstUser();
		void StartOfRunUser();
		void EndOfRunUser();
        void EndOfJobUser();
		void DrawPlot();
	protected:


  TH2F* fMyHist;
  TH1F* fMyHistA;
  TH1F* fMyHistB;
  TH1F* fMyHistC;
  
};
#endif
