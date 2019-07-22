#ifndef CHANTIRANDOMVETO_HH
#define CHANTIRANDOMVETO_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include "TGraphAsymmErrors.h"
#include "TGraphErrors.h"
#include "TEfficiency.h"

class TH1I;
class TH2F;
class TGraph;
class TTree;
class TRecoCHANTICandidate;
class TRecoCHANTIEvent;

class CHANTIRandomVeto : public NA62Analysis::Analyzer
{
	public:
		explicit CHANTIRandomVeto(NA62Analysis::Core::BaseAnalysis *ba);
		~CHANTIRandomVeto();
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

		int FindBinHistoVetos(int MinBin, TString HistoName, double DeltaTimeCandi);
		void FillHistoVetos(int MinBin, TString HistoName);
		int GetStationFromCandidate(TRecoCHANTICandidate *RecoCHANTICandi,TRecoCHANTIEvent *CHANTIEvent);
		int FindNCHANTICandiInTime(Double_t RefTime, TRecoCHANTIEvent *CHANTIEvent);
		int FindNCHANTIHitInTime(Double_t RefTime, TRecoCHANTIEvent *CHANTIEvent);
		int Factorial(int n);
		void CreateLabelConfig(int offset, int k, TString &str, Int_t &Bin, Int_t NSt, Int_t Selection);
		TString  fOutPDFFileName;
		TCanvas* fCanvas;
		Bool_t   fReadingData;
		// Selection definitions for internal use only
    		enum Selection{ //bit 0: K2pi, bit 1: K3pi
      			K2pi=0,
      			K3pi,
      			NSELECTIONS //used to count the number of implemented selections
    		};

		Int_t    fBurstID;
    		Double_t fDecayTime;
    		Double_t fEventTimeStamp;
    		Double_t fArgonionCounts;
   	 	Int_t    fNTriggers;
    		Int_t    fNSelectedTriggers;
		Int_t 	 *fNSelectedKaonDecays;
		TString *fStationLabels;
		Double_t fM3pi;
		Double_t fZDecayVtx;
		Double_t fCHANTITimeWindow;
		Double_t *fCedarMaxTimeCut;
		Double_t *fCedarMinTimeCut;
		Double_t fCHANTIMinTimeCut;
		Double_t fCHANTIMaxTimeCut;
		Double_t fM3piMin;
		Double_t fM3piMax;
		Double_t fZDecayVtxMin;
		Double_t fZDecayVtxMax;
		Double_t fMinRMSCHANTIEval;
		Double_t fMaxRMSCHANTIEval;
		Int_t fMaxNBursts;

		TString* fSelectionLabels;
		TEfficiency **fFractionOfEventsVetoedCross;
		TEfficiency **fFractionOfEventsVetoedSingle;
		TGraphAsymmErrors **fFractionOfEventsVetoedCross_gr;
		TGraphAsymmErrors **fFractionOfEventsVetoedSingle_gr;
		TEfficiency **fFractionOfEventsVetoedVSTimeCross;
		TEfficiency **fFractionOfEventsVetoedVSTimeSingle;
		TGraphAsymmErrors **fFractionOfEventsVetoedVSTimeCross_gr;
		TGraphAsymmErrors **fFractionOfEventsVetoedVSTimeSingle_gr;
		TGraphErrors *fCHANTICandidateVsCedarKaonsTime_gr;
		TGraphErrors **fCHANTIStCandidateVsCedarKaonsTime_gr;
		TGraphErrors *fCHANTICandidateVsTriggerTime_gr;
		TGraphErrors **fCHANTIStCandidateVsTriggerTime_gr;
		TH2F *fCedarKaonsVsRefTime;
		TH2F *fCHANTICandidateVsCedarKaonsTime;
		TH2F **fCHANTIStCandidateVsCedarKaonsTime;
		TH2F *fCHANTICandidateVsTriggerTime;
		TH2F **fCHANTIStCandidateVsTriggerTime;
		TH2F *fBestCHANTICand_KTAGKaonTime_VS_NCHANTICandi;
		TH2F *fBestCHANTIHit_KTAGKaonTime_VS_NCHANTIHit;
		TH2F *fBestCHANTICand_TriggerTime_VS_NCHANTICandi;
		TH2F *fBestCHANTIHit_TriggerTime_VS_NCHANTIHit;
		TH1F *fhM3pi;
		TH2F *fChi2VertexVSK3pi_FourMomentum;
		TH2F **fCedarVsDecayTime;
		TH2F **fCedarVsRICHTime;
		TH2F **fCHANTICandidateVsCedarTime;
		TH1F **fCHANTIVetoSingle;
		TH1F **fCHANTIVetoCross;
		TH1F **fDenCHANTIVeto;
		TH1F **fNumberOfEventsVetoedSingle;
		TH1F **fNumberOfEventsVetoedCross;
		TH1F **fNumberOfEvents;
		TH1F **fZDecayVtx_VetoedSingle;
		TH1F **fZDecayVtx_VetoedCross;
		TH1F ***fEventVetoedConfigSingle_NSt;
		TH1F ***fEventVetoedConfigCross_NSt;
};
#endif
