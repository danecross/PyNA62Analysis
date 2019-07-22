#ifndef MUV12DATAQUALITY_HH
#define MUV12DATAQUALITY_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TCanvas.h>
#include "DownstreamTrack.hh"
#include "TDatabasePDG.h"

class TH1I;
class TH2F;
class TGraph;
class TTree;

class MUV12DataQuality : public NA62Analysis::Analyzer
{
	public:
		explicit MUV12DataQuality(NA62Analysis::Core::BaseAnalysis *ba);
		~MUV12DataQuality();
		void InitHist();
		void InitOutput();
		void DefineMCSimple();
		void Process(int iEvent);
		void StartOfBurstUser();
		void EndOfBurstUser();
		void StartOfRunUser();
		void EndOfJobUser();
		void PostProcess();
		void DrawPlot();
		void GeneratePDFReport();
		void DefineBadBursts();

	protected:

    //timing parameters
    double fMUV1_dT = 25.; // [ ns ], time window half-width for track to MUV1 candidate matching
    double fMUV2_dT = 20.; // [ ns ], time window half-width for track to MUV2 candidate matching

    //space parameters
    double fMUV1_dR = 120.; // [ mm ], maximum distance between a track and matching MUV1 candidate
    double fMUV2_dR = 240.; // [ mm ], maximum distance between a track and matching MUV2 candidate

    //energy thresholds
    double fLKr_MIN_E = 250. ; //LKr noise bellow this threshold
    double fLKr_MIP_E = 1000.; // [ MeV ],  MIPs bellow this threshold
    double fMUV1_MIP_E = 1500.; // [ MeV ], MIPs bellow this threshold

    TString fPDFReportFileName, fBadBurstListFileName;
    double fMUV1EffLimit, fMUV2EffLimit;
};
#endif
