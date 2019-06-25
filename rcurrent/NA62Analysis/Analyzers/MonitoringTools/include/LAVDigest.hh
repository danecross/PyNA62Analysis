#ifndef LAVDIGEST_HH
#define LAVDIGEST_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TCanvas.h>

using namespace std;

class TH1I;
class TH2F;
class TGraph;
class TTree;

class LAVDigest : public Analyzer
{
	public:
		explicit LAVDigest(BaseAnalysis *ba);
		void InitHist();
		void InitOutput();
		void DefineMCSimple();
		void Process(int iEvent);
		void StartOfBurstUser();
		void EndOfBurstUser();
		void StartOfRunUser();
		void EndOfRunUser();
		void PostProcess();
		void ExportPlot();
		void DrawPlot();
	private:
		void Publish(); ///< Deprecated

	protected:


};
#endif
