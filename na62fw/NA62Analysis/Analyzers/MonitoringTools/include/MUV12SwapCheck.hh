#ifndef MUV12SWAPCHECK_HH
#define MUV12SWAPCHECK_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include "MUV12Corrections.hh"
#include "TDirectory.h"

class TH1I;
class TH2F;
class TGraph;
class TTree;

class MUV12SwapCheck : public NA62Analysis::Analyzer
{
	public:
		explicit MUV12SwapCheck(NA62Analysis::Core::BaseAnalysis *ba);
		~MUV12SwapCheck();
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

	private:
		MUV12Corrections *fMUV12Corr;
		TDirectory *fBurstFileDir;


};
#endif
