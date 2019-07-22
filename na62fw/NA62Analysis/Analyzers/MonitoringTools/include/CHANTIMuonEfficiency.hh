#ifndef CHANTIMUONEFFICIENCY_HH
#define CHANTIMUONEFFICIENCY_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TCanvas.h>
#include "CHANTIGeometry.hh"
#include "TGraphErrors.h"
#include "TGraphAsymmErrors.h"
#include "TEfficiency.h"

class TH1I;
class TH2F;
class TGraph;
class TTree;
class TRecoCedarEvent;

class CHANTIMuonEfficiency : public NA62Analysis::Analyzer
{
	public:
		explicit CHANTIMuonEfficiency(NA62Analysis::Core::BaseAnalysis *ba);
		~CHANTIMuonEfficiency();
		void InitHist();
		void InitOutput();
		void DefineMCSimple();
		void Process(int iEvent);
		void StartOfBurstUser();
		void EndOfBurstUser();
		void StartOfRunUser();
		void EndOfRunUser();
		void EndOfJobUser();
		void PostProcess();
		void DrawPlot();
	protected:

		Int_t IsKaonEvent(Double_t RefTime,TRecoCedarEvent *CedarEvent);

		Double_t fStep;
  		Double_t  fGlobalShift;
		Double_t fLowBoundEff[6][2];
		Double_t fLayerEffCut[6];
		Int_t fMaxNBursts;
		Bool_t   fReadingData;
		CHANTIGeometry *fGeometry;
		EventHeader *fEventHeader;
		TString  fOutPDFFileName;
		TCanvas* fCanvas;

		Double_t*** fX;
  		Double_t*** fY;
  		Double_t*** fErrX;
  		Double_t*** fEfX;
  		Double_t*** fEfY;
  		Double_t*** fEfHighX;
  		Double_t*** fEfHighY;
  		Double_t*** fErrEfX;
  		Double_t*** fErrEfY;
  		Double_t*** fErrEfHighX;
  		Double_t*** fErrEfHighY;

		Double_t** fTotalEffX;
		Double_t** fTotalEffY;
		Double_t** fTotalEffHighX;
		Double_t** fTotalEffHighY;

		TEfficiency ***fEfficiencyEvalX;
		TEfficiency ***fEfficiencyEvalY;
		TEfficiency ***fEfficiencyEvalHighX;
		TEfficiency ***fEfficiencyEvalHighY;
		TGraphAsymmErrors*** fCumEffiX;
		TGraphAsymmErrors*** fCumEffiY;
		TGraphAsymmErrors*** fCumEffiHighX;
		TGraphAsymmErrors*** fCumEffiHighY;
		TH2F*** fEffiNumX_VS_BurstID;
		TH2F*** fEffiHighNumX_VS_BurstID;
		TH2F*** fEffiDenX_VS_BurstID;
		TH2F*** fEffiNumY_VS_BurstID;
		TH2F*** fEffiHighNumY_VS_BurstID;
		TH2F*** fEffiDenY_VS_BurstID;
		TH2F*** fEffiX_VS_BurstID;
		TH2F*** fEffiHighX_VS_BurstID;
		TH2F*** fEffiY_VS_BurstID;
		TH2F*** fEffiHighY_VS_BurstID;
		TH1F*** fInPlane_1Up;
  		TH1F*** fInPlane_1Down;
		TH1F*** fInPlane_1SideUp;
		TH1F*** fInPlane_2SideUp;
		TH1F*** fInPlane_1SideDown;
		TH1F*** fInPlane_2SideDown;
		TH1F*** fInPlane_High1Up;
		TH1F*** fInPlane_High1Down;
		TH1F*** fInPlane_High1SideUp;
		TH1F*** fInPlane_High2SideUp;
		TH1F*** fInPlane_High1SideDown;
		TH1F*** fInPlane_High2SideDown;
		TH1F*** fX_NumEffBurst;
		TH1F*** fX_NumHighEffBurst;
		TH1F*** fX_DenEffBurst;
		TH1F*** fY_NumEffBurst;
		TH1F*** fY_NumHighEffBurst;
		TH1F*** fY_DenEffBurst;
		TEfficiency ***fTotEfficiencyForPlaneX;
		TEfficiency ***fTotEfficiencyForPlaneHighX;
		TEfficiency ***fTotEfficiencyForPlaneY;
		TEfficiency ***fTotEfficiencyForPlaneHighY;
		TGraphAsymmErrors*** fX_EffBurst;
		TGraphAsymmErrors*** fX_HighEffBurst;
		TGraphAsymmErrors*** fY_EffBurst;
		TGraphAsymmErrors*** fY_HighEffBurst;
		TH2F *fhCHANTItiming;
		TH1F *fNEventsInBurstAnalyzed;

		Double_t *fLimitsSlice;

		ofstream fBurstFile;
		ofstream fBadLayersFile;
		ofstream fBadPositionsFile;

	private:
		Int_t fNPlanes;
		Int_t fNRings;
		Int_t fNBins;
		Int_t NTimeSlice;
};
#endif
