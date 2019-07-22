// ---------------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson) 2017-05-08
// ---------------------------------------------------------------
#ifndef L0RATECOMPUTER_HH
#define L0RATECOMPUTER_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"

class TH1I;
class TH2F;
class TGraph;
class TTree;

#include "L0PrimitiveHandler.hh" 

class L0RateComputer : public NA62Analysis::Analyzer
{
	public:
		explicit L0RateComputer(NA62Analysis::Core::BaseAnalysis *ba);
		~L0RateComputer();
		void InitHist();
		void InitOutput();
		void DefineMCSimple();
		void ProcessSpecialTriggerUser(int iEvent, unsigned int triggerType);
		void Process(int iEvent);
  void ProcessEOBEvent();
		void PostProcess();
		void StartOfBurstUser();
		void EndOfBurstUser();
		void StartOfRunUser();
		void EndOfRunUser();
        void EndOfJobUser();
		void DrawPlot();
	protected:

  Int_t fNMasks;
  Double_t fArgonionCount;
  
  Int_t fControlDS;

  L0PrimitiveHandler* fL0PrimHandle;
  std::vector<TString> fMaskNames;
  std::vector<Int_t>   fDownScales;
  
  void PrimDumps(TString );
  L0TPData* CreateL0TPData();
};
#endif
