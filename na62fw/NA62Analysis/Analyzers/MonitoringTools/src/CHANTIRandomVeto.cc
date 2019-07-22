#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "CHANTIRandomVeto.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "DownstreamTrack.hh"
#include "SpectrometerTrackVertex.hh"
#include <TLegend.h>
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class CHANTIRandomVeto
/// \Brief
/// Check the random veto of the CHANTI detector using the K2Pi and K3Pi official selections. 
/// \EndBrief
/// \Detailed
/// In this macro any physics trigger mask can be selected, but by default just the control trigger is used.
/// In the first part of the routine, plots for the evaluation of the CHANTI time resolution are produced (e.g. CHANTIcandi-KTAGcandi, CHANTIcandi-Trigger time distributions)
/// In the second part the K2Pi and K3Pi official selections are used. The kaon KTAG candidate that is closest in time to the kaon decay of K2Pi and K3Pi selections is chosen as reference time. Then CHANTI random veto as a function of the time window opened around the reference time is evaluated. Also CHANTI random veto in +-3ns time window around the reference time is evaluated as a function of the burst ID. Two different cuts are applied with the CHANTI to veto the event: 
/// - single config: at least one bar fired
/// - cross config: at least two bars crossing fired
/// Also the configuration of CHANTI stations fired is reported the proper histogram.
/// Usage:
/// The analyzer should be run in two modes:
/// 1) Read the reconstructed data and produce intermediate output;
/// 2) read its own output (using the --histo command line option) and produce the
/// graphs for random veto (as a root file and a PDF report) 
/// \author Marco Mirra (marco.mirra@cern.ch)
/// \EndDetailed

CHANTIRandomVeto::CHANTIRandomVeto(Core::BaseAnalysis *ba) : Analyzer(ba, "CHANTIRandomVeto")
{
	RequestTree("Cedar",new TRecoCedarEvent);
	RequestTree("CHANTI",new TRecoCHANTIEvent);

	fCedarMaxTimeCut = new Double_t[NSELECTIONS];
	fCedarMinTimeCut = new Double_t[NSELECTIONS];
	AddParam("fCedarMaxTimeCut", &fCedarMaxTimeCut[K2pi], 1.5);
	AddParam("fCedarMinTimeCut", &fCedarMinTimeCut[K2pi], -1.);
	AddParam("fCedarMaxTimeCut", &fCedarMaxTimeCut[K3pi], 1.5);
	AddParam("fCedarMinTimeCut", &fCedarMinTimeCut[K3pi], -1.);
	AddParam("fCHANTITimeWindow",&fCHANTITimeWindow, 25.);
	AddParam("fCHANTIMinTimeCut",&fCHANTIMinTimeCut, -3.);
	AddParam("fCHANTIMaxTimeCut",&fCHANTIMaxTimeCut, 3.);
	AddParam("fM3piMin", &fM3piMin, 492.); 
	AddParam("fM3piMax", &fM3piMax, 495.);
	AddParam("fZDecayVtxMin", &fZDecayVtxMin, 115000.); 
	AddParam("fZDecayVtxMax", &fZDecayVtxMax, 165000.);
	AddParam("fMaxNBursts", &fMaxNBursts, 3000); // max number of bins in histograms
	AddParam("fMinRMSCHANTIEval", &fMinRMSCHANTIEval, -2.5);
	AddParam("fMaxRMSCHANTIEval", &fMaxRMSCHANTIEval, 3.0);

	fOutPDFFileName = fAnalyzerName + ".pdf";
	fFractionOfEventsVetoedCross = 0;
	fFractionOfEventsVetoedSingle = 0;
	fFractionOfEventsVetoedCross_gr = 0;
	fFractionOfEventsVetoedSingle_gr = 0;
	fCedarKaonsVsRefTime = 0;
	fCHANTICandidateVsCedarKaonsTime = 0;
	fCHANTIStCandidateVsCedarKaonsTime = 0;
	fCHANTICandidateVsTriggerTime = 0;
	fCHANTIStCandidateVsTriggerTime = 0;			
	fBestCHANTICand_KTAGKaonTime_VS_NCHANTICandi = 0;
	fBestCHANTIHit_KTAGKaonTime_VS_NCHANTIHit = 0;
	fBestCHANTICand_TriggerTime_VS_NCHANTICandi = 0;
	fBestCHANTIHit_TriggerTime_VS_NCHANTIHit = 0;
	fhM3pi = 0;
	fChi2VertexVSK3pi_FourMomentum = 0;
	fCedarVsDecayTime = 0;
	fCedarVsRICHTime = 0;
	fCHANTICandidateVsCedarTime = 0;
	fCHANTIVetoSingle = 0;
	fCHANTIVetoCross = 0;
	fDenCHANTIVeto  = 0;
	fNumberOfEventsVetoedSingle = 0;
	fNumberOfEventsVetoedCross = 0;
	fNumberOfEvents = 0;
	fZDecayVtx_VetoedSingle  = 0;
	fZDecayVtx_VetoedCross = 0;
	fEventVetoedConfigSingle_NSt = 0;
	fEventVetoedConfigCross_NSt = 0;
	fCHANTICandidateVsCedarKaonsTime_gr = 0;
	fCHANTIStCandidateVsCedarKaonsTime_gr = 0;
	fCHANTICandidateVsTriggerTime_gr = 0;
	fCHANTIStCandidateVsTriggerTime_gr = 0;

	fSelectionLabels = new TString[NSELECTIONS];
  	fSelectionLabels[K2pi] = "K2pi";
  	fSelectionLabels[K3pi] = "K3pi";
	fNSelectedKaonDecays = new Int_t[NSELECTIONS];
	for(UInt_t iSelection=0;iSelection<NSELECTIONS;iSelection++){
    		fNSelectedKaonDecays[iSelection] = 0;
  	}

	fStationLabels = new TString[6];
  	fStationLabels[0] = "A";
  	fStationLabels[1] = "B";
	fStationLabels[2] = "C";
  	fStationLabels[3] = "D";
	fStationLabels[4] = "E";
  	fStationLabels[5] = "F";
}

CHANTIRandomVeto::~CHANTIRandomVeto() {
  	delete [] fSelectionLabels;
	delete [] fNSelectedKaonDecays;
	delete [] fCedarMaxTimeCut;
	delete [] fCedarMinTimeCut;
	delete [] fStationLabels;
}

void CHANTIRandomVeto::InitOutput(){
}

void CHANTIRandomVeto::InitHist(){
	
	fReadingData = GetIsTree();

	fFractionOfEventsVetoedCross = new TEfficiency*[NSELECTIONS];
	fFractionOfEventsVetoedSingle = new TEfficiency*[NSELECTIONS];
	fFractionOfEventsVetoedCross_gr = new TGraphAsymmErrors*[NSELECTIONS];
	fFractionOfEventsVetoedSingle_gr = new TGraphAsymmErrors*[NSELECTIONS];
	fFractionOfEventsVetoedVSTimeCross = new TEfficiency*[NSELECTIONS];
	fFractionOfEventsVetoedVSTimeSingle = new TEfficiency*[NSELECTIONS];
	fFractionOfEventsVetoedVSTimeCross_gr = new TGraphAsymmErrors*[NSELECTIONS];
	fFractionOfEventsVetoedVSTimeSingle_gr = new TGraphAsymmErrors*[NSELECTIONS];
	fCHANTICandidateVsCedarKaonsTime_gr = new TGraphErrors();
	fCHANTIStCandidateVsCedarKaonsTime_gr = new TGraphErrors*[6];
	fCHANTICandidateVsTriggerTime_gr = new TGraphErrors();
	fCHANTIStCandidateVsTriggerTime_gr = new TGraphErrors*[6];
	for (int iSt = 0; iSt < 6; iSt++){
		fCHANTIStCandidateVsCedarKaonsTime_gr[iSt] = new TGraphErrors();
		fCHANTIStCandidateVsTriggerTime_gr[iSt] = new TGraphErrors();	
	}

	if (fReadingData){
		std::cout << user_normal() << "Reading reconstructed data" << std::endl;
		BookHisto(new TH2F("CedarKaonsVsRefTime","CedarKaonsVsRefTime",fMaxNBursts,-0.5,fMaxNBursts-0.5,200,-20,20));
		BookHisto(new TH2F("CHANTICandidateVsCedarKaonsTime","CHANTICandidateVsCedarKaonsTime",fMaxNBursts,-0.5,fMaxNBursts-0.5,750,-50,100));
		BookHisto(new TH2F("CHANTICandidateVsTriggerTime","CHANTICandidateVsTriggerTime",fMaxNBursts,-0.5,fMaxNBursts-0.5,875,-50,125));	
		for (int iSt = 0; iSt < 6; iSt++){
			BookHisto(new TH2F(Form("CHANTI%dCandidateVsCedarKaonsTime",iSt+1),Form("CHANTI%dCandidateVsCedarKaonsTime",iSt+1),fMaxNBursts,-0.5,fMaxNBursts-0.5,750,-50,100));
			BookHisto(new TH2F(Form("CHANTI%dCandidateVsTriggerTime",iSt+1),Form("CHANTI%dCandidateVsTriggerTime",iSt+1),fMaxNBursts,-0.5,fMaxNBursts-0.5,875,-50,125));
		}
		BookHisto(new TH2F("BestCHANTICand_KTAGKaonTime_VS_NCHANTICandi","BestCHANTICand_KTAGKaonTime_VS_NCHANTICandi",250,-0.5,249.5,(int)20*(fCHANTITimeWindow+10),-fCHANTITimeWindow-10,fCHANTITimeWindow+10));
		BookHisto(new TH2F("BestCHANTIHit_KTAGKaonTime_VS_NCHANTIHit","BestCHANTIHit_KTAGKaonTime_VS_NCHANTIHit",350,-0.5,349.5,(int)20*(fCHANTITimeWindow+10),-fCHANTITimeWindow-10,fCHANTITimeWindow+10));
		BookHisto(new TH2F("BestCHANTICand_TriggerTime_VS_NCHANTICandi","BestCHANTICand_TriggerTime_VS_NCHANTICandi",250,-0.5,249.5,(int)20*(fCHANTITimeWindow+10),-fCHANTITimeWindow-10,fCHANTITimeWindow+10));
		BookHisto(new TH2F("BestCHANTIHit_TriggerTime_VS_NCHANTIHit","BestCHANTIHit_TriggerTime_VS_NCHANTIHit",350,-0.5,349.5,(int)20*(fCHANTITimeWindow+10),-fCHANTITimeWindow-10,fCHANTITimeWindow+10));
		BookHisto(new TH1F("M3pi","M(3#pi); M(3#pi) [MeV]", 120, 480, 510));
		BookHisto(new TH2F("Chi2VertexVSK3pi_FourMomentum","Chi2VertexVSK3pi_FourMomentum", 200, 50, 100, 100,  0.0, 30.0));
		for(Int_t iSelection = 0; iSelection < NSELECTIONS; iSelection++){
			BookHisto(new TH2F(Form("CedarVsDecayTime_%s",fSelectionLabels[iSelection].Data()),Form("CedarVsDecayTime_%s",fSelectionLabels[iSelection].Data()),fMaxNBursts,-0.5,fMaxNBursts-0.5,200,-20,20));
			BookHisto(new TH2F(Form("CedarVsRICHTime_%s",fSelectionLabels[iSelection].Data()),Form("CedarVsRICHTime_%s",fSelectionLabels[iSelection].Data()),fMaxNBursts,-0.5,fMaxNBursts-0.5,200,-20,20));
			BookHisto(new TH2F(Form("CHANTICandidateVsCedarTime_%s",fSelectionLabels[iSelection].Data()),Form("CHANTICandidateVsCedarTime_%s",fSelectionLabels[iSelection].Data()),fMaxNBursts,-0.5,fMaxNBursts-0.5,875,-50,125));
			BookHisto(new TH1F(Form("CHANTIVetoSingle_%s",fSelectionLabels[iSelection].Data()),Form("CHANTIVetoSingle_%s",fSelectionLabels[iSelection].Data()),21,-0.001,5.5));
			BookHisto(new TH1F(Form("CHANTIVetoCross_%s",fSelectionLabels[iSelection].Data()),Form("CHANTIVetoCross_%s",fSelectionLabels[iSelection].Data()),21,-0.001,5.5));
			BookHisto(new TH1F(Form("DenCHANTIVeto_%s",fSelectionLabels[iSelection].Data()),Form("DenCHANTIVeto_%s",fSelectionLabels[iSelection].Data()),21,-0.001,5.5));
			BookHisto(new TH1F(Form("NumberOfEventsVetoedSingle_%s",fSelectionLabels[iSelection].Data()),Form("NumberOfEventsVetoedSingle_%s",fSelectionLabels[iSelection].Data()),fMaxNBursts,-0.5,fMaxNBursts-0.5));
			BookHisto(new TH1F(Form("NumberOfEventsVetoedCross_%s",fSelectionLabels[iSelection].Data()),Form("NumberOfEventsVetoedCross_%s",fSelectionLabels[iSelection].Data()),fMaxNBursts,-0.5,fMaxNBursts-0.5));
			BookHisto(new TH1F(Form("NumberOfEvents_%s",fSelectionLabels[iSelection].Data()),Form("NumberOfEvents_%s",fSelectionLabels[iSelection].Data()),fMaxNBursts,-0.5,fMaxNBursts-0.5));

			BookHisto(new TH1F(Form("ZDecayVtx_VetoedSingle_%s",fSelectionLabels[iSelection].Data()),Form("ZDecayVtx_VetoedSingle_%s",fSelectionLabels[iSelection].Data()),200, 50, 250));
			BookHisto(new TH1F(Form("ZDecayVtx_VetoedCross_%s",fSelectionLabels[iSelection].Data()),Form("ZDecayVtx_VetoedCross_%s",fSelectionLabels[iSelection].Data()),200, 50, 250));
			for (int iSt=0; iSt<7; iSt++){
				int NBin = Factorial(6)/(Factorial(iSt)*Factorial(6-iSt));	
				BookHisto(new TH1F(Form("EventVetoedConfigSingle_NSt%d_%s",iSt,fSelectionLabels[iSelection].Data()),Form("EventVetoedConfigSingle_NSt%d_%s",iSt,fSelectionLabels[iSelection].Data()), NBin, -0.5, NBin-0.5));
				BookHisto(new TH1F(Form("EventVetoedConfigCross_NSt%d_%s",iSt,fSelectionLabels[iSelection].Data()),Form("EventVetoedConfigCross_NSt%d_%s",iSt,fSelectionLabels[iSelection].Data()), NBin, -0.5, NBin-0.5));
				TString DummyStr = "None";
				Int_t Bin = 1;
				CreateLabelConfig(0, iSt, DummyStr, Bin, iSt, iSelection);
			} 
			fFractionOfEventsVetoedSingle[iSelection] = new TEfficiency(Form("FractionOfEventsVetoedSingle_%s",fSelectionLabels[iSelection].Data()),Form("FractionOfEventsVetoedSingle_%s",fSelectionLabels[iSelection].Data()),fMaxNBursts,-0.5,fMaxNBursts-0.5);
			fFractionOfEventsVetoedCross[iSelection] = new TEfficiency(Form("FractionOfEventsVetoedCross_%s",fSelectionLabels[iSelection].Data()),Form("FractionOfEventsVetoedCross_%s",fSelectionLabels[iSelection].Data()),fMaxNBursts,-0.5,fMaxNBursts-0.5);
			fFractionOfEventsVetoedVSTimeSingle[iSelection] = new TEfficiency(Form("FractionOfEventsVetoedVSTimeSingle_%s",fSelectionLabels[iSelection].Data()),Form("FractionOfEventsVetoedVSTimeSingle_%s",fSelectionLabels[iSelection].Data()),21,-0.001,5.5);
			fFractionOfEventsVetoedVSTimeCross[iSelection] = new TEfficiency(Form("FractionOfEventsVetoedVSTimeCrossVS_%s",fSelectionLabels[iSelection].Data()),Form("FractionOfEventsVetoedVSTimeCross_%s",fSelectionLabels[iSelection].Data()),21,-0.001,5.5);
		}
	}  else {

    		std::cout << user_normal() << "Reading my own output" << std::endl;

		fCedarKaonsVsRefTime = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,"CedarKaonsVsRefTime",true));
		fCHANTICandidateVsCedarKaonsTime = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,"CHANTICandidateVsCedarKaonsTime",true));
		fCHANTICandidateVsTriggerTime = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,"CHANTICandidateVsTriggerTime",true));
		fCHANTIStCandidateVsCedarKaonsTime = new TH2F*[6];
		fCHANTIStCandidateVsTriggerTime = new TH2F*[6];
		for (int iSt = 0; iSt < 6; iSt++){
			fCHANTIStCandidateVsCedarKaonsTime[iSt] = new TH2F();			
			fCHANTIStCandidateVsCedarKaonsTime[iSt] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,Form("CHANTI%dCandidateVsCedarKaonsTime",iSt+1),true));
			fCHANTIStCandidateVsTriggerTime[iSt] = new TH2F();			
			fCHANTIStCandidateVsTriggerTime[iSt] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,Form("CHANTI%dCandidateVsTriggerTime",iSt+1),true));
		}
		fBestCHANTICand_KTAGKaonTime_VS_NCHANTICandi = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,"BestCHANTICand_KTAGKaonTime_VS_NCHANTICandi",true));
		fBestCHANTIHit_KTAGKaonTime_VS_NCHANTIHit = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,"BestCHANTIHit_KTAGKaonTime_VS_NCHANTIHit",true));
		fBestCHANTICand_TriggerTime_VS_NCHANTICandi = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,"BestCHANTICand_TriggerTime_VS_NCHANTICandi",true));
		fBestCHANTIHit_TriggerTime_VS_NCHANTIHit = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,"BestCHANTIHit_TriggerTime_VS_NCHANTIHit",true));
		fhM3pi = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,"M3pi",true));
		fChi2VertexVSK3pi_FourMomentum = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,"Chi2VertexVSK3pi_FourMomentum",true));
		
	
	
		fCedarVsDecayTime = new TH2F*[NSELECTIONS];
		fCedarVsRICHTime = new TH2F*[NSELECTIONS];
		fCHANTICandidateVsCedarTime = new TH2F*[NSELECTIONS];
		fCHANTIVetoSingle = new TH1F*[NSELECTIONS];
		fCHANTIVetoCross = new TH1F*[NSELECTIONS];
		fDenCHANTIVeto = new TH1F*[NSELECTIONS];
		fNumberOfEventsVetoedSingle = new TH1F*[NSELECTIONS];
		fNumberOfEventsVetoedCross = new TH1F*[NSELECTIONS];
		fNumberOfEvents = new TH1F*[NSELECTIONS];
		fZDecayVtx_VetoedSingle = new TH1F*[NSELECTIONS];
		fZDecayVtx_VetoedCross = new TH1F*[NSELECTIONS];
		fEventVetoedConfigSingle_NSt = new TH1F**[NSELECTIONS];
		fEventVetoedConfigCross_NSt = new TH1F**[NSELECTIONS];

		for(Int_t iSelection = 0; iSelection < NSELECTIONS; iSelection++){
			fFractionOfEventsVetoedVSTimeSingle_gr[iSelection] = new TGraphAsymmErrors();
			fFractionOfEventsVetoedVSTimeSingle_gr[iSelection] = (TGraphAsymmErrors*)RequestHistogram(fAnalyzerName,Form("FractionOfEventsVetoedVSTimeSingle_%s",fSelectionLabels[iSelection].Data()), true);
			fFractionOfEventsVetoedVSTimeCross_gr[iSelection] = new TGraphAsymmErrors();
			fFractionOfEventsVetoedVSTimeCross_gr[iSelection] = (TGraphAsymmErrors*)RequestHistogram(fAnalyzerName,Form("FractionOfEventsVetoedVSTimeCross_%s",fSelectionLabels[iSelection].Data()), true);
			fFractionOfEventsVetoedSingle_gr[iSelection] = new TGraphAsymmErrors();
			fFractionOfEventsVetoedSingle_gr[iSelection] = (TGraphAsymmErrors*)RequestHistogram(fAnalyzerName,Form("FractionOfEventsVetoedSingle_%s",fSelectionLabels[iSelection].Data()), true);
			fFractionOfEventsVetoedCross_gr[iSelection] = new TGraphAsymmErrors();
			fFractionOfEventsVetoedCross_gr[iSelection] = (TGraphAsymmErrors*)RequestHistogram(fAnalyzerName,Form("FractionOfEventsVetoedCross_%s",fSelectionLabels[iSelection].Data()), true);



			fCedarVsDecayTime[iSelection] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,Form("CedarVsDecayTime_%s",fSelectionLabels[iSelection].Data()),true));
			fCedarVsRICHTime[iSelection] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,Form("CedarVsRICHTime_%s",fSelectionLabels[iSelection].Data()),true));
			fCHANTICandidateVsCedarTime[iSelection] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,Form("CHANTICandidateVsCedarTime_%s",fSelectionLabels[iSelection].Data()),true));
			fCHANTIVetoSingle[iSelection] = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,Form("CHANTIVetoSingle_%s",fSelectionLabels[iSelection].Data()),true));
			fCHANTIVetoCross[iSelection] = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,Form("CHANTIVetoCross_%s",fSelectionLabels[iSelection].Data()),true));
			fDenCHANTIVeto[iSelection] = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,Form("DenCHANTIVeto_%s",fSelectionLabels[iSelection].Data()),true));
			fNumberOfEventsVetoedSingle[iSelection] = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,Form("NumberOfEventsVetoedSingle_%s",fSelectionLabels[iSelection].Data()),true));
			fNumberOfEventsVetoedCross[iSelection] = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,Form("NumberOfEventsVetoedCross_%s",fSelectionLabels[iSelection].Data()),true));
			fNumberOfEvents[iSelection] = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,Form("NumberOfEvents_%s",fSelectionLabels[iSelection].Data()),true));
			fZDecayVtx_VetoedSingle[iSelection] = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,Form("ZDecayVtx_VetoedSingle_%s",fSelectionLabels[iSelection].Data()),true));
			fZDecayVtx_VetoedCross[iSelection] = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,Form("ZDecayVtx_VetoedCross_%s",fSelectionLabels[iSelection].Data()),true));
			fEventVetoedConfigSingle_NSt[iSelection] = new TH1F*[7];
			fEventVetoedConfigCross_NSt[iSelection] = new TH1F*[7];
			for (int iSt=0; iSt<7; iSt++){
				fEventVetoedConfigSingle_NSt[iSelection][iSt] = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,Form("EventVetoedConfigSingle_NSt%d_%s",iSt,fSelectionLabels[iSelection].Data()),true));
				fEventVetoedConfigCross_NSt[iSelection][iSt] = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,Form("EventVetoedConfigCross_NSt%d_%s",iSt,fSelectionLabels[iSelection].Data()),true));
			} 
	
		}

		fCHANTICandidateVsCedarKaonsTime_gr = (TGraphErrors*)RequestHistogram(fAnalyzerName,"RMS_CHANTITimewrtKTAGTime", true);
		fCHANTICandidateVsTriggerTime_gr = (TGraphErrors*)RequestHistogram(fAnalyzerName,"RMS_CHANTITimewrtTriggerTime", true);
		for (int iSt = 0; iSt < 6; iSt++){
			fCHANTIStCandidateVsCedarKaonsTime_gr[iSt] = (TGraphErrors*)RequestHistogram(fAnalyzerName,Form("RMS_CHANTI%dTimewrtKTAGTime",iSt+1), true);
			fCHANTIStCandidateVsTriggerTime_gr[iSt] = (TGraphErrors*)RequestHistogram(fAnalyzerName,Form("RMS_CHANTI%dTimewrtTriggerTime",iSt+1), true);
		}

	}

}

void CHANTIRandomVeto::CreateLabelConfig(int offset, int k, TString &str, Int_t &Bin, Int_t NSt, Int_t Sel){
	if (k == 0){
		fHisto.GetTH1(Form("EventVetoedConfigSingle_NSt%d_%s",NSt,fSelectionLabels[Sel].Data()))->GetXaxis()->SetBinLabel(Bin,str);
		fHisto.GetTH1(Form("EventVetoedConfigCross_NSt%d_%s",NSt,fSelectionLabels[Sel].Data()))->GetXaxis()->SetBinLabel(Bin,str);
		Bin++;
		return;
	}
        if(k == NSt) str = "";
	for (int i = offset; i <= 6-k; ++i) {
		str.Append(fStationLabels[i]);
		CreateLabelConfig(i+1, k-1, str, Bin, NSt, Sel);
		str.Remove(str.Length()-1);
	}
}

int CHANTIRandomVeto::Factorial(int n){
	int factorial = 1;	
	for (int i=1; i<=n; i++){
		factorial = factorial * i;
	}
	return factorial;
}

void CHANTIRandomVeto::DefineMCSimple(){
}

void CHANTIRandomVeto::StartOfRunUser(){
}

void CHANTIRandomVeto::StartOfBurstUser(){
	fBurstID = GetBurstID(); 
  	fArgonionCounts = 0.; 
  	fNTriggers = 0.; 
  	fNSelectedTriggers = 0.; 
}

void CHANTIRandomVeto::ProcessSpecialTriggerUser(int, unsigned int){
}

void CHANTIRandomVeto::Process(int){
	
	TRecoCedarEvent *CedarEvent = GetEvent<TRecoCedarEvent>();
	TRecoCHANTIEvent *CHANTIEvent = GetEvent<TRecoCHANTIEvent>();

	if (!fReadingData) return;

  	fBurstID        = GetEventHeader()->GetBurstID();
  	fDecayTime      = GetEventHeader()->GetFineTime()*ClockPeriod/256.;
  	fEventTimeStamp = GetEventHeader()->GetTimeStamp()*ClockPeriod/1.e9;
	fM3pi = 0.;
	fZDecayVtx = 0.;	
  	fNTriggers++;

  	Int_t  L0DataType    = GetL0Data()->GetDataType();
  	//Int_t  L0TriggerWord = GetL0Data()->GetTriggerFlags(); //To use only when some physics masks are enabled
  	//Bool_t PhysicsData   = L0DataType & 0x1; //To use only when some physics masks are enabled
  	Bool_t CTRLTrigger   = L0DataType & 0x10;
  	//Bool_t TriggerOK     = (PhysicsData && (L0TriggerWord&0x1)) || CTRLTrigger; //MASK0 only + CTRL
  	//Bool_t TriggerOK     = (PhysicsData && (L0TriggerWord&0xFF)) || CTRLTrigger; //ALL MASKS + CTRL
  	Bool_t TriggerOK     = CTRLTrigger; //CTRL only
	if (!TriggerOK) return; // process control triggers and selected MASKS only

	fNSelectedTriggers++;


	//Time resolution study
  	for(Int_t iCedarCand=0; iCedarCand<CedarEvent->GetNCandidates(); iCedarCand++){
    		TRecoCedarCandidate *CedarCandidate = static_cast<TRecoCedarCandidate*>(CedarEvent->GetCandidate(iCedarCand));
		Double_t KaonTime = CedarCandidate->GetTime(); 
		Double_t dt = KaonTime-fDecayTime; 
		if (CedarCandidate->GetNSectors() > 4){
			FillHisto("CedarKaonsVsRefTime",fBurstID,dt);
			Double_t dtCHANTI_KTAGMin = 999.;
			Double_t AbsdtCHANTI_KTAGMin = 999.;			
			for (int iCHANTICandidate=0; iCHANTICandidate<CHANTIEvent->GetNCandidates(); iCHANTICandidate++ ){
				TRecoCHANTICandidate *CHANTICandidate = static_cast<TRecoCHANTICandidate*>(CHANTIEvent->GetCandidate(iCHANTICandidate));
				Double_t dtCHANTI = CHANTICandidate->GetTime()-KaonTime;
				if (TMath::Abs(dtCHANTI)<AbsdtCHANTI_KTAGMin){
					AbsdtCHANTI_KTAGMin = TMath::Abs(dtCHANTI);
					dtCHANTI_KTAGMin = dtCHANTI;
				}		
				FillHisto("CHANTICandidateVsCedarKaonsTime",fBurstID,dtCHANTI);
				FillHisto(Form("CHANTI%dCandidateVsCedarKaonsTime",GetStationFromCandidate(CHANTICandidate,CHANTIEvent)+1),fBurstID,dtCHANTI);	  
			}
			Int_t NCHANTICandiInTime = FindNCHANTICandiInTime(KaonTime,CHANTIEvent);
			FillHisto("BestCHANTICand_KTAGKaonTime_VS_NCHANTICandi",NCHANTICandiInTime,dtCHANTI_KTAGMin);
			
			AbsdtCHANTI_KTAGMin = 999.;
			dtCHANTI_KTAGMin = 999.;
			for (int iCHANTIHit=0; iCHANTIHit<CHANTIEvent->GetNHits(); iCHANTIHit++ ){
				TRecoCHANTIHit *CHANTIHit = static_cast<TRecoCHANTIHit*>(CHANTIEvent->GetHit(iCHANTIHit));
				Double_t dtCHANTI = CHANTIHit->GetTime()-KaonTime;
				if (CHANTIHit ->GetQualityFlag() == 5) continue;
  				if (CHANTIHit ->GetThresholdFlag() == 1) continue;
				if (TMath::Abs(dtCHANTI)<AbsdtCHANTI_KTAGMin){
					AbsdtCHANTI_KTAGMin = TMath::Abs(dtCHANTI);
					dtCHANTI_KTAGMin = dtCHANTI;
				}
			}
			Int_t NCHANTIHitInTime = FindNCHANTIHitInTime(KaonTime,CHANTIEvent);			
			FillHisto("BestCHANTIHit_KTAGKaonTime_VS_NCHANTIHit",NCHANTIHitInTime,dtCHANTI_KTAGMin);  	
  		}
  	}	

	Double_t AbsdtCHANTI_TriggerMin = 999.;
	Double_t dtCHANTI_TriggerMin = 999.;
	for (int iCHANTICandidate=0; iCHANTICandidate<CHANTIEvent->GetNCandidates(); iCHANTICandidate++ ){
		TRecoCHANTICandidate *CHANTICandidate = static_cast<TRecoCHANTICandidate*>(CHANTIEvent->GetCandidate(iCHANTICandidate));
		Double_t dtCHANTI = CHANTICandidate->GetTime()-fDecayTime;
		FillHisto("CHANTICandidateVsTriggerTime",fBurstID,dtCHANTI);
		FillHisto(Form("CHANTI%dCandidateVsTriggerTime",GetStationFromCandidate(CHANTICandidate,CHANTIEvent)+1),fBurstID,dtCHANTI);	  
		if (TMath::Abs(dtCHANTI)<AbsdtCHANTI_TriggerMin){
			AbsdtCHANTI_TriggerMin = TMath::Abs(dtCHANTI);
			dtCHANTI_TriggerMin = dtCHANTI;
		}
	}
	Int_t NCHANTICandiInTime = FindNCHANTICandiInTime(fDecayTime,CHANTIEvent);
	FillHisto("BestCHANTICand_TriggerTime_VS_NCHANTICandi",NCHANTICandiInTime,dtCHANTI_TriggerMin);


	AbsdtCHANTI_TriggerMin = 999.;
	dtCHANTI_TriggerMin = 999.;
	for (int iCHANTIHit=0; iCHANTIHit<CHANTIEvent->GetNHits(); iCHANTIHit++ ){
		TRecoCHANTIHit *CHANTIHit = static_cast<TRecoCHANTIHit*>(CHANTIEvent->GetHit(iCHANTIHit));
		Double_t dtCHANTI = CHANTIHit->GetTime()-fDecayTime;
		if (CHANTIHit ->GetQualityFlag() == 5) continue;
  		if (CHANTIHit ->GetThresholdFlag() == 1) continue;
		if (TMath::Abs(dtCHANTI)<AbsdtCHANTI_TriggerMin){
			AbsdtCHANTI_TriggerMin = TMath::Abs(dtCHANTI);
			dtCHANTI_TriggerMin = dtCHANTI;
		}
	}	
	Int_t NCHANTIHitInTime = FindNCHANTIHitInTime(fDecayTime,CHANTIEvent);
	FillHisto("BestCHANTIHit_TriggerTime_VS_NCHANTIHit",NCHANTIHitInTime,dtCHANTI_TriggerMin); 


	//Random veto study
  	Double_t CedarTime      = -999.999;
  	UInt_t   SelectionMask  = 0;
	Double_t RICHTime = -999.999;
	Bool_t K2piSelected = *(Bool_t*)GetOutput("K2piSelection.EventSelected");
  	Bool_t K3piSelected = *(Bool_t*)GetOutput("K3piSelection.EventSelected");
  	if(K2piSelected) {
    		SelectionMask = (SelectionMask|(1<<K2pi));
    		fDecayTime = *(Double_t*)GetOutput("K2piSelection.K2piTime");
		Int_t K2piTrackID = *(Int_t*)GetOutput("K2piSelection.K2piTrackID");
		std::vector<DownstreamTrack> Tracks =
    			*(std::vector<DownstreamTrack>*)GetOutput("DownstreamTrackBuilder.Output");
		fZDecayVtx = Tracks[K2piTrackID].GetBeamAxisVertex().Z();
		if (Tracks[K2piTrackID].RICHAssociationSuccessful()) RICHTime = Tracks[K2piTrackID].GetRICHRingTime(3); 
  	}
  	if(K3piSelected) {
    		SelectionMask = (SelectionMask|(1<<K3pi));
    		fDecayTime = *(Double_t*)GetOutput("K3piSelection.K3piTime");
		fM3pi = *(Double_t*)GetOutput("K3piSelection.M3pi");
		FillHisto("M3pi",fM3pi);
		Int_t VtxIndex = *(Int_t*)GetOutput("K3piSelection.VertexID");
		std::vector<SpectrometerTrackVertex> Vertices =
    				*(std::vector<SpectrometerTrackVertex>*)GetOutput("SpectrometerVertexBuilder.Output");
		fZDecayVtx = Vertices[VtxIndex].GetPosition().z();	
		TLorentzVector v[3];
  		for (Int_t i=0; i<3; i++) v[i].SetVectM(Vertices[VtxIndex].GetTrackThreeMomentum(i), MPI);
    		TLorentzVector K3pi_FourMomentum  = v[0] + v[1] + v[2];
		FillHisto("Chi2VertexVSK3pi_FourMomentum",K3pi_FourMomentum.P()*0.001,Vertices[VtxIndex].GetChi2());
  	}

  	if(!SelectionMask) return;
	if (fZDecayVtx < fZDecayVtxMin || fZDecayVtx > fZDecayVtxMax) return;

	Int_t MinBinDtSingle[NSELECTIONS] = {100,100};
	Int_t MinBinDtCross[NSELECTIONS] = {100,100};	
	Bool_t EventsVetoedSingle[NSELECTIONS] = {false,false};
	Bool_t EventsVetoedCross[NSELECTIONS] = {false,false};
	for(UInt_t iSelection=0;iSelection<NSELECTIONS;iSelection++){
    		if(!(SelectionMask&(1<<iSelection))) continue;
		if(K3piSelected)  if (fM3pi < fM3piMin || fM3pi > fM3piMax ) continue;

		// Find best-match within Cedar Candidates wrt DecayTime
		TRecoCedarCandidate *BestCedarCandidate=0;
  		Double_t CedarDecayDeltaT = 1.e19;
  		for(Int_t iCedarCand=0; iCedarCand<CedarEvent->GetNCandidates(); iCedarCand++){
    			TRecoCedarCandidate *CedarCandidate = static_cast<TRecoCedarCandidate*>(CedarEvent->GetCandidate(iCedarCand));
			if (! (CedarCandidate->GetNSectors() > 4 ) ) continue;

			if (K2piSelected){
				Double_t dtKTAG_RICH = CedarCandidate->GetTime()-RICHTime; 
				FillHisto(Form("CedarVsRICHTime_%s",fSelectionLabels[iSelection].Data()),fBurstID,dtKTAG_RICH);
			}
			Double_t dt = CedarCandidate->GetTime()-fDecayTime; 
			FillHisto(Form("CedarVsDecayTime_%s",fSelectionLabels[iSelection].Data()),fBurstID,dt);
    			if(fabs(dt)<fabs(CedarDecayDeltaT)) {
      				CedarDecayDeltaT = dt;
      				BestCedarCandidate = CedarCandidate;
    			}
  		}
		Bool_t KaonCandidate = false;
  		if(BestCedarCandidate) {
    			CedarTime     = BestCedarCandidate->GetTime();
			KaonCandidate  = BestCedarCandidate->GetTime()-fDecayTime < fCedarMaxTimeCut[iSelection] && BestCedarCandidate->GetTime()-fDecayTime > fCedarMinTimeCut[iSelection];
  		}
		
		if (!KaonCandidate) continue;
		fNSelectedKaonDecays[iSelection]++;
		FillHisto(Form("NumberOfEvents_%s",fSelectionLabels[iSelection].Data()),fBurstID);
		bool SingleConfig[6] = {false,false,false,false,false,false};
		bool CrossConfig[6] = {false,false,false,false,false,false};
		for (int iCHANTICandidate=0; iCHANTICandidate<CHANTIEvent->GetNCandidates(); iCHANTICandidate++ ){
			TRecoCHANTICandidate *CHANTICandidate = static_cast<TRecoCHANTICandidate*>(CHANTIEvent->GetCandidate(iCHANTICandidate));
			Double_t dtCHANTI = CHANTICandidate->GetTime()-CedarTime;
			FillHisto(Form("CHANTICandidateVsCedarTime_%s",fSelectionLabels[iSelection].Data()),fBurstID,dtCHANTI);		
			MinBinDtSingle[iSelection] = FindBinHistoVetos(MinBinDtSingle[iSelection],Form("CHANTIVetoSingle_%s",fSelectionLabels[iSelection].Data()),dtCHANTI);
			if (dtCHANTI > fCHANTIMinTimeCut && dtCHANTI < fCHANTIMaxTimeCut) {
				EventsVetoedSingle[iSelection] = true;
				SingleConfig[GetStationFromCandidate(CHANTICandidate,CHANTIEvent)]=true;		
			}		

			if (CHANTICandidate->GetXYMult()>0) {
				MinBinDtCross[iSelection] = FindBinHistoVetos(MinBinDtCross[iSelection],Form("CHANTIVetoCross_%s",fSelectionLabels[iSelection].Data()),dtCHANTI);		
				if (dtCHANTI > fCHANTIMinTimeCut && dtCHANTI < fCHANTIMaxTimeCut){
					EventsVetoedCross[iSelection] = true;
					CrossConfig[GetStationFromCandidate(CHANTICandidate,CHANTIEvent)]=true;
				}
			}
		}
		
		if (EventsVetoedSingle[iSelection]) {
			FillHisto(Form("NumberOfEventsVetoedSingle_%s",fSelectionLabels[iSelection].Data()),fBurstID);
			FillHisto(Form("ZDecayVtx_VetoedSingle_%s",fSelectionLabels[iSelection].Data()),fZDecayVtx*0.001);
		}		
		if (EventsVetoedCross[iSelection]) {
			FillHisto(Form("NumberOfEventsVetoedCross_%s",fSelectionLabels[iSelection].Data()),fBurstID);
			FillHisto(Form("ZDecayVtx_VetoedCross_%s",fSelectionLabels[iSelection].Data()),fZDecayVtx*0.001);		
		}
		FillHistoVetos(MinBinDtSingle[iSelection],Form("CHANTIVetoSingle_%s",fSelectionLabels[iSelection].Data()));
		FillHistoVetos(MinBinDtCross[iSelection],Form("CHANTIVetoCross_%s",fSelectionLabels[iSelection].Data()));
		
		TString SingleConfigFired = "None";
		TString CrossConfigFired = "None";
		Int_t NSt_Single = 0;
		Int_t NSt_Cross = 0;
		for (int iSt=0; iSt<6; iSt++){
			if (SingleConfig[iSt]) {
				if(!NSt_Single) SingleConfigFired = "";
				SingleConfigFired.Append(fStationLabels[iSt].Data());
				NSt_Single++;
			}			
			if (CrossConfig[iSt]){
				if(!NSt_Cross) CrossConfigFired = "";
	 			CrossConfigFired.Append(fStationLabels[iSt].Data());
				NSt_Cross++;
			}
		}
		fHisto.GetTH1(Form("EventVetoedConfigSingle_NSt%d_%s",NSt_Single,fSelectionLabels[iSelection].Data()))->Fill(SingleConfigFired.Data(),1);
		fHisto.GetTH1(Form("EventVetoedConfigCross_NSt%d_%s",NSt_Cross,fSelectionLabels[iSelection].Data()))->Fill(CrossConfigFired.Data(),1);
	}	
}


int CHANTIRandomVeto::FindNCHANTICandiInTime(Double_t RefTime, TRecoCHANTIEvent *CHANTIEvent){
	int NCHANTICandi = 0;	
	for (int iCHANTICandidate=0; iCHANTICandidate<CHANTIEvent->GetNCandidates(); iCHANTICandidate++ ){
		TRecoCHANTICandidate *CHANTICandidate = static_cast<TRecoCHANTICandidate*>(CHANTIEvent->GetCandidate(iCHANTICandidate));
		Double_t dtCHANTI = CHANTICandidate->GetTime()-RefTime;
		if (TMath::Abs(dtCHANTI) < fCHANTITimeWindow) NCHANTICandi++;
	}
	return NCHANTICandi;
}

int CHANTIRandomVeto::FindNCHANTIHitInTime(Double_t RefTime, TRecoCHANTIEvent *CHANTIEvent){
	int NCHANTIHit = 0;	
	for (int iCHANTIHit=0; iCHANTIHit<CHANTIEvent->GetNHits(); iCHANTIHit++ ){
		TRecoCHANTIHit *CHANTIHit = static_cast<TRecoCHANTIHit*>(CHANTIEvent->GetHit(iCHANTIHit));
		if (CHANTIHit ->GetQualityFlag() == 5) continue;
  		if (CHANTIHit ->GetThresholdFlag() == 1) continue;
		Double_t dtCHANTI = CHANTIHit->GetTime()-RefTime;
		if (TMath::Abs(dtCHANTI) < fCHANTITimeWindow) NCHANTIHit++;
	}
	return NCHANTIHit;
}

int CHANTIRandomVeto::GetStationFromCandidate(TRecoCHANTICandidate *RecoCHANTICandi, TRecoCHANTIEvent *CHANTIEvent){
	RecoCHANTICandi->SetEvent(CHANTIEvent);
	TRecoCHANTIHit* CHANTIHitInCandidate = static_cast<TRecoCHANTIHit*>(RecoCHANTICandi->GetHit(0));
	return (CHANTIHitInCandidate->GetPlaneID());
}

int CHANTIRandomVeto::FindBinHistoVetos(int MinBin, TString HistoName, double DeltaTimeCandi){
	Int_t DeltaTimeBin  = fHisto.GetTH1(HistoName)->FindBin( TMath::Abs(DeltaTimeCandi) );
	MinBin = (MinBin<DeltaTimeBin) ? MinBin:DeltaTimeBin;
	return MinBin;
}

void CHANTIRandomVeto::FillHistoVetos(int MinBin, TString HistoName){
	if (MinBin <= fHisto.GetTH1(HistoName)->GetXaxis()->GetNbins() ){
		for (int ibin = MinBin; ibin <= fHisto.GetTH1(HistoName)->GetXaxis()->GetNbins(); ibin++ ){
			Double_t Value = fHisto.GetTH1(HistoName)->GetBinCenter(ibin);
			fHisto.GetTH1(HistoName)->Fill(Value);
		}
	} 
}

void CHANTIRandomVeto::PostProcess(){

}

void CHANTIRandomVeto::EndOfBurstUser(){
}

void CHANTIRandomVeto::EndOfRunUser(){

}

void CHANTIRandomVeto::EndOfJobUser(){

	gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed

	////////////
  	// DATA mode
	if (fReadingData) {
		for(UInt_t iSelection=0;iSelection<NSELECTIONS;iSelection++){
			fFractionOfEventsVetoedSingle[iSelection]->SetPassedHistogram(*fHisto.GetTH1(Form("NumberOfEventsVetoedSingle_%s",fSelectionLabels[iSelection].Data())),"f");
			fFractionOfEventsVetoedSingle[iSelection]->SetTotalHistogram(*fHisto.GetTH1(Form("NumberOfEvents_%s",fSelectionLabels[iSelection].Data())),"f");
			fFractionOfEventsVetoedSingle_gr[iSelection]=fFractionOfEventsVetoedSingle[iSelection]->CreateGraph();
			fFractionOfEventsVetoedSingle_gr[iSelection]->SetNameTitle(Form("FractionOfEventsVetoedSingle_%s",fSelectionLabels[iSelection].Data()),Form("FractionOfEventsVetoedSingle_%s",fSelectionLabels[iSelection].Data()));	
			fFractionOfEventsVetoedCross[iSelection]->SetPassedHistogram(*fHisto.GetTH1(Form("NumberOfEventsVetoedCross_%s",fSelectionLabels[iSelection].Data())),"f");
			fFractionOfEventsVetoedCross[iSelection]->SetTotalHistogram(*fHisto.GetTH1(Form("NumberOfEvents_%s",fSelectionLabels[iSelection].Data())),"f");
			fFractionOfEventsVetoedCross_gr[iSelection]=fFractionOfEventsVetoedCross[iSelection]->CreateGraph();
			fFractionOfEventsVetoedCross_gr[iSelection]->SetNameTitle(Form("FractionOfEventsVetoedCross_%s",fSelectionLabels[iSelection].Data()),Form("FractionOfEventsVetoedCross_%s",fSelectionLabels[iSelection].Data()));

			Int_t NBins = fHisto.GetTH1(Form("DenCHANTIVeto_%s",fSelectionLabels[iSelection].Data()))->GetNbinsX();
			for (Int_t iBin = 1; iBin <= NBins; iBin++) fHisto.GetTH1(Form("DenCHANTIVeto_%s",fSelectionLabels[iSelection].Data()))->SetBinContent(iBin,fNSelectedKaonDecays[iSelection]);
			fFractionOfEventsVetoedVSTimeCross[iSelection]->SetPassedHistogram(*fHisto.GetTH1(Form("CHANTIVetoCross_%s",fSelectionLabels[iSelection].Data())),"f");
			fFractionOfEventsVetoedVSTimeCross[iSelection]->SetTotalHistogram(*fHisto.GetTH1(Form("DenCHANTIVeto_%s",fSelectionLabels[iSelection].Data())),"f");
			fFractionOfEventsVetoedVSTimeCross_gr[iSelection]=fFractionOfEventsVetoedVSTimeCross[iSelection]->CreateGraph();
			fFractionOfEventsVetoedVSTimeCross_gr[iSelection]->SetNameTitle(Form("FractionOfEventsVetoedVSTimeCross_%s",fSelectionLabels[iSelection].Data()),Form("FractionOfEventsVetoedVSTimeCross_%s",fSelectionLabels[iSelection].Data()));
			fFractionOfEventsVetoedVSTimeSingle[iSelection]->SetPassedHistogram(*fHisto.GetTH1(Form("CHANTIVetoSingle_%s",fSelectionLabels[iSelection].Data())),"f");
			fFractionOfEventsVetoedVSTimeSingle[iSelection]->SetTotalHistogram(*fHisto.GetTH1(Form("DenCHANTIVeto_%s",fSelectionLabels[iSelection].Data())),"f");
			fFractionOfEventsVetoedVSTimeSingle_gr[iSelection]=fFractionOfEventsVetoedVSTimeSingle[iSelection]->CreateGraph();
			fFractionOfEventsVetoedVSTimeSingle_gr[iSelection]->SetNameTitle(Form("FractionOfEventsVetoedVSTimeSingle_%s",fSelectionLabels[iSelection].Data()),Form("FractionOfEventsVetoedVSTimeSingle_%s",fSelectionLabels[iSelection].Data()));			

			
		}
		
		fCHANTICandidateVsCedarKaonsTime_gr->SetNameTitle("RMS_CHANTITimewrtKTAGTime","RMS_CHANTITimewrtKTAGTime");
		fCHANTICandidateVsCedarKaonsTime_gr->Set(fMaxNBursts);
		fCHANTICandidateVsTriggerTime_gr->SetNameTitle("RMS_CHANTITimewrtTriggerTime","RMS_CHANTITimewrtTriggerTime");
		fCHANTICandidateVsTriggerTime_gr->Set(fMaxNBursts);
		for (int iSt = 0; iSt < 6; iSt++){
			fCHANTIStCandidateVsCedarKaonsTime_gr[iSt]->SetNameTitle(Form("RMS_CHANTI%dTimewrtKTAGTime",iSt+1),Form("RMS_CHANTI%dTimewrtKTAGTime",iSt+1));
			fCHANTIStCandidateVsCedarKaonsTime_gr[iSt]->Set(fMaxNBursts);
			fCHANTIStCandidateVsTriggerTime_gr[iSt]->SetNameTitle(Form("RMS_CHANTI%dTimewrtTriggerTime",iSt+1),Form("RMS_CHANTI%dTimewrtTriggerTime",iSt+1));
			fCHANTIStCandidateVsTriggerTime_gr[iSt]->Set(fMaxNBursts);
		}

		for (int iBurst = 0; iBurst < fMaxNBursts; iBurst++){
			TH1D *tempProjection = fHisto.GetTH2("CHANTICandidateVsCedarKaonsTime")->ProjectionY("CHANTITimeKTAG",iBurst+1,iBurst+1);
			if ( tempProjection->GetEntries() ){
				tempProjection->GetXaxis()->SetRangeUser(fMinRMSCHANTIEval,fMaxRMSCHANTIEval);
				fCHANTICandidateVsCedarKaonsTime_gr->SetPoint(iBurst,iBurst,tempProjection->GetRMS());
				fCHANTICandidateVsCedarKaonsTime_gr->SetPointError(iBurst,0.,tempProjection->GetRMSError());			
			}
			delete tempProjection;

			tempProjection = fHisto.GetTH2("CHANTICandidateVsTriggerTime")->ProjectionY("CHANTITimeTrigger",iBurst+1,iBurst+1);
			if ( tempProjection->GetEntries() ){
				tempProjection->GetXaxis()->SetRangeUser(fMinRMSCHANTIEval,fMaxRMSCHANTIEval);
				fCHANTICandidateVsTriggerTime_gr->SetPoint(iBurst,iBurst,tempProjection->GetRMS());
				fCHANTICandidateVsTriggerTime_gr->SetPointError(iBurst,0.,tempProjection->GetRMSError());			
			}
			delete tempProjection;
			
			for (int iSt = 0; iSt < 6; iSt++){
				tempProjection = fHisto.GetTH2(Form("CHANTI%dCandidateVsCedarKaonsTime",iSt+1))->ProjectionY(Form("CHANTI%dTimeKTAG",iSt+1),iBurst+1,iBurst+1);
				if ( tempProjection->GetEntries() ){
					tempProjection->GetXaxis()->SetRangeUser(fMinRMSCHANTIEval,fMaxRMSCHANTIEval);
					fCHANTIStCandidateVsCedarKaonsTime_gr[iSt]->SetPoint(iBurst,iBurst,tempProjection->GetRMS());
					fCHANTIStCandidateVsCedarKaonsTime_gr[iSt]->SetPointError(iBurst,0.,tempProjection->GetRMSError());			
				}
				delete tempProjection;

				tempProjection = fHisto.GetTH2(Form("CHANTI%dCandidateVsTriggerTime",iSt+1))->ProjectionY(Form("CHANTI%dTimeTrigger",iSt+1),iBurst+1,iBurst+1);
				if ( tempProjection->GetEntries() ){
					tempProjection->GetXaxis()->SetRangeUser(fMinRMSCHANTIEval,fMaxRMSCHANTIEval);
					fCHANTIStCandidateVsTriggerTime_gr[iSt]->SetPoint(iBurst,iBurst,tempProjection->GetRMS());
					fCHANTIStCandidateVsTriggerTime_gr[iSt]->SetPointError(iBurst,0.,tempProjection->GetRMSError());			
				}
				delete tempProjection;
			}
		}
		fCHANTICandidateVsCedarKaonsTime_gr->Write();
		fCHANTICandidateVsTriggerTime_gr->Write();
		for (int iSt = 0; iSt < 6; iSt++){
			fCHANTIStCandidateVsCedarKaonsTime_gr[iSt]->Write();
			fCHANTIStCandidateVsTriggerTime_gr[iSt]->Write();
		}
		for(UInt_t iSelection=0;iSelection<NSELECTIONS;iSelection++){
			fFractionOfEventsVetoedSingle_gr[iSelection]->Write();
			fFractionOfEventsVetoedCross_gr[iSelection]->Write();
			fFractionOfEventsVetoedVSTimeSingle_gr[iSelection]->Write();
			fFractionOfEventsVetoedVSTimeCross_gr[iSelection]->Write();	
		}
	}

	/////////////
  	// HISTO mode

  	if (!fReadingData) {
		if (!fCedarKaonsVsRefTime) {
      			std::cout << user_normal() << "Asked to read my own output but cannot found it" << std::endl;
      			return;
    		}
		/////////////////////////
   		// Produce the PDF output

		TString Planes[6];
		Planes[0]="A";Planes[1]="B";Planes[2]="C";Planes[3]="D";Planes[4]="E";Planes[5]="F";

    		fCanvas = new TCanvas("Canvas");

		// Random Veto in +-3ns time window around kaon KTAG time candidate
    		TString DrawOption = "";
    		TLegend* Legend = new TLegend(0.2,0.55,0.32,0.83);
    		Legend->SetFillColor(kWhite);
		for(UInt_t iSelection=0;iSelection<NSELECTIONS;iSelection++){
			if(iSelection) DrawOption = "P&same";
			else {
				fFractionOfEventsVetoedSingle_gr[iSelection]->GetYaxis()->SetRangeUser(0.,0.4);	
				DrawOption = "AP";	
			}
			fFractionOfEventsVetoedSingle_gr[iSelection]->SetTitle(Form("Fraction of events vetoed in +-3ns window (single hit config)"));    			
			fFractionOfEventsVetoedSingle_gr[iSelection]->SetLineColor(iSelection+1);
      			fFractionOfEventsVetoedSingle_gr[iSelection]->SetMarkerColor(iSelection+1);
      			fFractionOfEventsVetoedSingle_gr[iSelection]->SetMarkerStyle(20+iSelection*4);
      			fFractionOfEventsVetoedSingle_gr[iSelection]->SetMarkerSize(0.8);
      			fFractionOfEventsVetoedSingle_gr[iSelection]->Draw(DrawOption);
      			fFractionOfEventsVetoedSingle_gr[iSelection]->GetXaxis()->SetTitle("BurstID");
			fFractionOfEventsVetoedSingle_gr[iSelection]->GetYaxis()->SetTitle("Random Veto");
			Legend->AddEntry(fFractionOfEventsVetoedSingle_gr[iSelection],fSelectionLabels[iSelection].Data());
		}
		Legend->Draw();
		fCanvas->Print(Form(fOutPDFFileName + "("), "pdf"); // open and print the canvas	
		delete Legend;		
		
		fCanvas->Clear();
   	 	Legend = new TLegend(0.2,0.55,0.32,0.83);
    		Legend->SetFillColor(kWhite);
		for(UInt_t iSelection=0;iSelection<NSELECTIONS;iSelection++){
			if(iSelection) DrawOption = "P&same";
			else {
				fFractionOfEventsVetoedCross_gr[iSelection]->GetYaxis()->SetRangeUser(0.,0.25);	
				DrawOption = "AP";	
			}
			fFractionOfEventsVetoedCross_gr[iSelection]->SetTitle(Form("Fraction of events vetoed in +-3ns window (cross hit config)"));    			
			fFractionOfEventsVetoedCross_gr[iSelection]->SetLineColor(iSelection+1);
      			fFractionOfEventsVetoedCross_gr[iSelection]->SetMarkerColor(iSelection+1);
      			fFractionOfEventsVetoedCross_gr[iSelection]->SetMarkerStyle(20+iSelection*4);
      			fFractionOfEventsVetoedCross_gr[iSelection]->SetMarkerSize(0.8);
      			fFractionOfEventsVetoedCross_gr[iSelection]->Draw(DrawOption);
      			fFractionOfEventsVetoedCross_gr[iSelection]->GetXaxis()->SetTitle("BurstID");
			fFractionOfEventsVetoedCross_gr[iSelection]->GetYaxis()->SetTitle("Random Veto");
			Legend->AddEntry(fFractionOfEventsVetoedCross_gr[iSelection],fSelectionLabels[iSelection].Data());
		}
		Legend->Draw();
		fCanvas->Print(fOutPDFFileName, "pdf");
		delete Legend;		

		fCanvas->SetLogy(1);
		for (int iSt = 1; iSt < 6; iSt++){
			fCanvas->Clear();
			DrawOption = "";
   	 		Legend = new TLegend(0.2,0.55,0.25,0.63);
    			Legend->SetFillColor(kWhite);
			for(UInt_t iSelection=0;iSelection<NSELECTIONS;iSelection++){
				if (iSelection) DrawOption = "same"; 
				//else DrawOption = "A";			
				fEventVetoedConfigSingle_NSt[iSelection][iSt]->SetStats(0);
				fEventVetoedConfigSingle_NSt[iSelection][iSt]->SetLineColor(iSelection+1);
      				fEventVetoedConfigSingle_NSt[iSelection][iSt]->Draw(DrawOption);
      				fEventVetoedConfigSingle_NSt[iSelection][iSt]->GetXaxis()->SetTitle("Station configuration fired");
				TString NStString = (iSt>1) ? "stations":"station";
				fEventVetoedConfigSingle_NSt[iSelection][iSt]->SetTitle(Form("Events with %d %s fired in single config candidate", iSt, NStString.Data()));	
				Legend->AddEntry(fEventVetoedConfigSingle_NSt[iSelection][iSt],fSelectionLabels[iSelection].Data());						
			}
			Legend->Draw();
			fCanvas->Print(fOutPDFFileName, "pdf");
			delete Legend;	

			fCanvas->Clear();
			DrawOption = "";
   	 		Legend = new TLegend(0.2,0.55,0.25,0.63);
    			Legend->SetFillColor(kWhite);
			for(UInt_t iSelection=0;iSelection<NSELECTIONS;iSelection++){
				if (iSelection) DrawOption = "same"; 
				//else DrawOption = "A";			
				fEventVetoedConfigCross_NSt[iSelection][iSt]->SetStats(0);
				fEventVetoedConfigCross_NSt[iSelection][iSt]->SetLineColor(iSelection+1);
      				fEventVetoedConfigCross_NSt[iSelection][iSt]->Draw(DrawOption);
      				fEventVetoedConfigCross_NSt[iSelection][iSt]->GetXaxis()->SetTitle("Station configuration fired");
				TString NStString = (iSt>1) ? "stations":"station";
				fEventVetoedConfigCross_NSt[iSelection][iSt]->SetTitle(Form("Events with %d %s fired in cross config candidate", iSt,NStString.Data()));	
				Legend->AddEntry(fEventVetoedConfigCross_NSt[iSelection][iSt],fSelectionLabels[iSelection].Data());						
			}
			Legend->Draw();		
			fCanvas->Print(fOutPDFFileName, "pdf");
			delete Legend;	
		}

		fCanvas->SetLogy(0);
		fCanvas->Clear();
   	 	Legend = new TLegend(0.83,0.13,0.95,0.4);
    		Legend->SetFillColor(kWhite);
		for(UInt_t iSt=0;iSt<6;iSt++){
			if(iSt) DrawOption = "P&same";
			else {
				fCHANTIStCandidateVsCedarKaonsTime_gr[iSt]->GetYaxis()->SetRangeUser(0.8,1.6);	
				DrawOption = "AP";	
			}
			fCHANTIStCandidateVsCedarKaonsTime_gr[iSt]->SetTitle(Form("RMS_TimeWindow_%1.1fns_%1.1fns_CHANTI_TimewrtKTAGTime",fMinRMSCHANTIEval,fMaxRMSCHANTIEval));    			
			fCHANTIStCandidateVsCedarKaonsTime_gr[iSt]->SetLineColor(iSt+1);
      			fCHANTIStCandidateVsCedarKaonsTime_gr[iSt]->SetMarkerColor(iSt+1);
      			fCHANTIStCandidateVsCedarKaonsTime_gr[iSt]->SetMarkerStyle(20+iSt*4);
      			fCHANTIStCandidateVsCedarKaonsTime_gr[iSt]->SetMarkerSize(0.8);
      			fCHANTIStCandidateVsCedarKaonsTime_gr[iSt]->Draw(DrawOption);
      			fCHANTIStCandidateVsCedarKaonsTime_gr[iSt]->GetXaxis()->SetTitle("BurstID");
			fCHANTIStCandidateVsCedarKaonsTime_gr[iSt]->GetYaxis()->SetTitle("RMS value");
			Legend->AddEntry(fCHANTIStCandidateVsCedarKaonsTime_gr[iSt],Form("Station %s",Planes[iSt].Data()));
		}
		Legend->Draw();
		fCanvas->Print(fOutPDFFileName, "pdf");
		delete Legend;

		fCanvas->Clear();
   	 	Legend = new TLegend(0.83,0.13,0.95,0.4);
    		Legend->SetFillColor(kWhite);
		for(UInt_t iSt=0;iSt<6;iSt++){
			if(iSt) DrawOption = "P&same";
			else {
				fCHANTIStCandidateVsTriggerTime_gr[iSt]->GetYaxis()->SetRangeUser(0.8,1.6);	
				DrawOption = "AP";	
			}
			fCHANTIStCandidateVsTriggerTime_gr[iSt]->SetTitle(Form("RMS_TimeWindow_%1.1fns_%1.1fns_CHANTI_TimewrtTriggerTime",fMinRMSCHANTIEval,fMaxRMSCHANTIEval));    			
			fCHANTIStCandidateVsTriggerTime_gr[iSt]->SetLineColor(iSt+1);
      			fCHANTIStCandidateVsTriggerTime_gr[iSt]->SetMarkerColor(iSt+1);
      			fCHANTIStCandidateVsTriggerTime_gr[iSt]->SetMarkerStyle(20+iSt*4);
      			fCHANTIStCandidateVsTriggerTime_gr[iSt]->SetMarkerSize(0.8);
      			fCHANTIStCandidateVsTriggerTime_gr[iSt]->Draw(DrawOption);
      			fCHANTIStCandidateVsTriggerTime_gr[iSt]->GetXaxis()->SetTitle("BurstID");
			fCHANTIStCandidateVsTriggerTime_gr[iSt]->GetYaxis()->SetTitle("RMS value");
			Legend->AddEntry(fCHANTIStCandidateVsTriggerTime_gr[iSt],Form("Station %s",Planes[iSt].Data()));
		}
		Legend->Draw();
		fCanvas->Print(fOutPDFFileName, "pdf");
		delete Legend;

		fCanvas->Clear();
		DrawOption = "AP";
		fCHANTICandidateVsCedarKaonsTime_gr->GetYaxis()->SetRangeUser(0.8,1.6);	
		fCHANTICandidateVsCedarKaonsTime_gr->SetTitle(Form("RMS_TimeWindow_%1.1fns_%1.1fns_CHANTITimewrtKTAGTime",fMinRMSCHANTIEval,fMaxRMSCHANTIEval));    			
		fCHANTICandidateVsCedarKaonsTime_gr->SetLineColor(1);
      		fCHANTICandidateVsCedarKaonsTime_gr->SetMarkerColor(1);
      		fCHANTICandidateVsCedarKaonsTime_gr->SetMarkerStyle(20);
      		fCHANTICandidateVsCedarKaonsTime_gr->SetMarkerSize(0.8);
      		fCHANTICandidateVsCedarKaonsTime_gr->Draw(DrawOption);
      		fCHANTICandidateVsCedarKaonsTime_gr->GetXaxis()->SetTitle("BurstID");
		fCHANTICandidateVsCedarKaonsTime_gr->GetYaxis()->SetTitle("RMS value");
		fCanvas->Print(fOutPDFFileName, "pdf");

		fCanvas->Clear();
		fCHANTICandidateVsTriggerTime_gr->GetYaxis()->SetRangeUser(0.8,1.6);	
		fCHANTICandidateVsTriggerTime_gr->SetTitle(Form("RMS_TimeWindow_%1.1fns_%1.1fns_CHANTITimewrtTriggerTime",fMinRMSCHANTIEval,fMaxRMSCHANTIEval));   
		fCHANTICandidateVsTriggerTime_gr->SetLineColor(1);
      		fCHANTICandidateVsTriggerTime_gr->SetMarkerColor(1);
      		fCHANTICandidateVsTriggerTime_gr->SetMarkerStyle(20);
      		fCHANTICandidateVsTriggerTime_gr->SetMarkerSize(0.8);
      		fCHANTICandidateVsTriggerTime_gr->Draw(DrawOption);
      		fCHANTICandidateVsTriggerTime_gr->GetXaxis()->SetTitle("BurstID");
		fCHANTICandidateVsTriggerTime_gr->GetYaxis()->SetTitle("RMS value");
		fCanvas->Print(Form(fOutPDFFileName + ")"), "pdf");
	}
	SaveAllPlots();	

	
	gErrorIgnoreLevel = -1; // restore the default
}

void CHANTIRandomVeto::DrawPlot(){

}


