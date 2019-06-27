#ifndef TEMPLATEANALYZER_HH
#define TEMPLATEANALYZER_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include "MCSimple.hh"
#include <TCanvas.h>

class TH1I;
class TH2F;
class TGraph;
class TTree;

//Change everywhere templateAnalyzer by you Analyzer name
//Add the protected and private members and methods you want to add

class templateAnalyzer : public NA62Analysis::Analyzer
{
	public:
		explicit templateAnalyzer(NA62Analysis::Core::BaseAnalysis *ba);
		~templateAnalyzer();
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
		//Add the variables that should be registered as output

		//Add your parameters (allowed : bool, char, string, TString, int, long, float, double)

		//Add the variables to branch to the output trees
};
#endif
