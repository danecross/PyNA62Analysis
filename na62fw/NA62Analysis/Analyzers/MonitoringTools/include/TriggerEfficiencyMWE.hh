#ifndef TRIGGEREFFICIENCYMWE_HH
#define TRIGGEREFFICIENCYMWE_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"

class TH1I;
class TH2F;
class TGraph;
class TTree;

#include "L0PrimitiveHandler.hh"
#include "TRecoNewCHODEvent.hh"

class TriggerEfficiencyMWE : public NA62Analysis::Analyzer {
	public:
		explicit TriggerEfficiencyMWE(NA62Analysis::Core::BaseAnalysis *ba);
		~TriggerEfficiencyMWE();
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
  
  Bool_t IsQXEvent(TRecoNewCHODEvent* event, Int_t RICHTime);

  L0PrimitiveHandler* fPrimitiveHandler;

  std::vector<TH1*> fMyHists;

};
#endif
