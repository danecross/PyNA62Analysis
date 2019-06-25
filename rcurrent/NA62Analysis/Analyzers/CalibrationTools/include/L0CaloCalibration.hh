// ---------------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson) 2017-05-08
// With help from Andrea Salamon and Nico De Simone
// ---------------------------------------------------------------
#ifndef L0CALOCALIBRATION_HH
#define L0CALOCALIBRATION_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"

class TH1I;
class TH2F;
class TGraph;
class TTree;

#include "L0PrimitiveHandler.hh"

class L0CaloCalibration : public NA62Analysis::Analyzer
{
	public:
		explicit L0CaloCalibration(NA62Analysis::Core::BaseAnalysis *ba);
		~L0CaloCalibration();
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

  L0PrimitiveHandler* fHandle;
  Bool_t fReadingData; 
  Bool_t fExtraOutput;
  std::vector<TH2F*> fHist1;
  std::vector<TH2F*> fHist2;
};
#endif
