// ---------------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson) 2017-08-05
// ---------------------------------------------------------------
#ifndef L0TRIGGERVALIDATION_HH
#define L0TRIGGERVALIDATION_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"

class TH1I;
class TH2F;
class TGraph;
class TTree;

#include "L0PrimitiveHandler.hh" 

class L0TriggerValidation : public NA62Analysis::Analyzer
{
	public:
		explicit L0TriggerValidation(NA62Analysis::Core::BaseAnalysis *ba);
		~L0TriggerValidation();
		void InitHist();
		void InitOutput();
		void DefineMCSimple();
		void ProcessSpecialTriggerUser(int iEvent, unsigned int triggerType);
  void ProcessEOBEvent();
		void Process(int iEvent);
		void PostProcess();
		void StartOfBurstUser();
		void EndOfBurstUser();
		void StartOfRunUser();
		void EndOfRunUser();
        void EndOfJobUser();
		void DrawPlot();
	protected:

  L0PrimitiveHandler* fL0PrimHandle;
  Int_t fNMasks;
  std::vector< TString > fMaskNames;
  std::vector< Int_t >   fDownScale;

  Bool_t fBadBurst; 

};
#endif
