#ifndef CHANTIRATE_HH
#define CHANTIRATE_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include "CHANTIGeometry.hh"

class TH1I;
class TH2F;
class TGraph;
class TTree;

class CHANTIRate : public NA62Analysis::Analyzer
{
	public:
		explicit CHANTIRate(NA62Analysis::Core::BaseAnalysis *ba);
		~CHANTIRate();
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
		Bool_t   fReadingData;
		Double_t fArgonionCounts;
		CHANTIGeometry *fGeometry;		
		Double_t fStep;
  		Double_t  fGlobalShift;	
		Double_t fBurstLength;
		Int_t   fMaxNBursts;  ///< Number of bins in the histograms of counts vs burst ID, default = 5000
		Int_t fNSlots;
		Int_t fNCtrlTriggerInBurst;		
		TCanvas* fCanvas;
		TString  fOutPDFFileName;
		TString* fStationLabels; 
		TString* fTrigSuffix;
		TString* fThrType;
		TString* fChLabels;
		TH2F***** BarOccupancyX;
		TH2F***** BarOccupancyY;
		TGraph* fArg_BurstID;	

		TH2F**** fCHANTIStRateVSArgonion;
		TGraph**** fCHANTIStRateVSBurstID;
		TGraph**** fCHANTIStRateNormArgVSBurstID;
		TH2F**** fCHANTIStXRateVSChID;
		TH2F**** fCHANTIStXRateNormArgVSChID;
		TH2F**** fCHANTIStYRateVSChID;
		TH2F**** fCHANTIStYRateNormArgVSChID;
};
#endif
