#ifndef FILTERONETRACKTWOCLUSTERS_HH
#define FILTERONETRACKTWOCLUSTERS_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include "Persistency.hh"
#include "TriggerConditions.hh"

class TH1I;
class TH2F;
class TGraph;
class TTree;

class FilterOneTrackTwoClusters : public NA62Analysis::Analyzer
{
	public:
		explicit FilterOneTrackTwoClusters(NA62Analysis::Core::BaseAnalysis *ba);
		void InitHist();
		void InitOutput();
		void DefineMCSimple();
		void Process(int iEvent);
		void StartOfBurstUser();
		void EndOfBurstUser();
		void StartOfRunUser();
		void EndOfRunUser();
		void PostProcess();
		void DrawPlot();
	protected:

	private:
		Int_t fTrigger_nonMuon;
};
#endif
