#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "CHANTITimeSlewing.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "TProfile.h"
#include "TF1.h"
#include "TString.h"
#include "TRegexp.h"
#include "NA62ConditionsService.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class CHANTITimeSlewing
/// \Brief
/// Macro to evaluate CHANTI time slewing correction channel by channel.
/// \EndBrief
///
/// \Detailed
/// This macro can be used by CHANTI experts to evaluate the time slewing corrections channel by channel. 
/// The corrections as a function of time-over-threshold (ToT) are evaluated using hits passing both thresholds 
/// with a linear extrapolation and these are reported in a TH2. Fits on the profile of these TH2 give the
/// parameters of the function used to evaluate the corrections that are applied to hits passing just single threshold.
/// Usage:
/// The analyzer should be run in two modes.
/// 1) Read the reconstructed data and produce intermediate output. It needs as input the threshold values, the CHANTI
///    config file and the old CHANTI time slewing correction file.
/// 2) Read its own output (using the --histo command line option) and produce the
///    graphs for the time slewing correction (as a root file and a PDF report)
/// In the .pdf file there is also the effect of the time slewing correction on the CHANTI channels time resolution. 
/// The histo mode produces also .dat file containing the parameters of the time-slewing correction VS ToT function 
/// that is in the same format used by CHANTIReconstruction.   
/// \author Marco Mirra (marco.mirra@cern.ch)
/// \EndDetailed

CHANTITimeSlewing::CHANTITimeSlewing(Core::BaseAnalysis *ba) : Analyzer(ba, "CHANTITimeSlewing")
{
  fReadingData = kTRUE; // reading reconstructed data, by default
  fOutPDFFileName = fAnalyzerName + ".pdf";

  Int_t NSTATION = 6;
  fStationLabels = new TString[NSTATION];
  fStationLabels[0] = "A";
  fStationLabels[1] = "B";
  fStationLabels[2] = "C";
  fStationLabels[3] = "D";
  fStationLabels[4] = "E";
  fStationLabels[5] = "F";

  Int_t NLayers = 2;
  fLayers = new TString[NLayers];
  fLayers[0] = "X";	
  fLayers[1] = "Y";

  Int_t NCHForStation = 24;
  fChLabels = new TString[NCHForStation];
  fChLabels[0] = "0";
  fChLabels[1] = "1";
  fChLabels[2] = "2";
  fChLabels[3] = "3";
  fChLabels[4] = "4";
  fChLabels[5] = "5";
  fChLabels[6] = "-5"; 
  fChLabels[7] = "6";
  fChLabels[8] = "-6";
  fChLabels[9] = "7";
  fChLabels[10] = "-7";
  fChLabels[11] = "8";
  fChLabels[12] = "-8";
  fChLabels[13] = "9";
  fChLabels[14] = "-9";
  fChLabels[15] = "10";
  fChLabels[16] = "-10";
  fChLabels[17] = "11";
  fChLabels[18] = "-11";
  fChLabels[19] = "12";
  fChLabels[20] = "13";
  fChLabels[21] = "14";
  fChLabels[22] = "15";
  fChLabels[23] = "16";

  fCorrL_ToT = 0;
  fNChannels = 1024;
  fChannelID = new Int_t[fNChannels];
  for (Int_t i=0; i<fNChannels; i++) {
    fChannelID[i]       = -99;
  }
  fOldTSCorrection = new Double_t*[fNChannels];
  for (Int_t j=0; j<fNChannels; j++){
    fOldTSCorrection[j] = new Double_t[3];
    for (Int_t i=0; i<3; i++) {
      fOldTSCorrection[j][i]       = -99;
    }
  }
  fNewTSCorrection = new Double_t*[fNChannels];
  for (Int_t j=0; j<fNChannels; j++){
    fNewTSCorrection[j] = new Double_t[3];
    for (Int_t i=0; i<3; i++) {
      fNewTSCorrection[j][i]       = -99;
    }
  }	

  RequestL0Data();
  RequestTree("CHANTI",new TRecoCHANTIEvent);
  RequestTree("Cedar",new TRecoCedarEvent);
  AddParam("MaxNBursts", &fMaxNBursts, 1800); // max number of bins in histograms
  AddParam("ThrH", &fThrH, 70.); //High threshold value in mV
  AddParam("ThrL", &fThrL, 30.); //Low threshold value in mV
  AddParam("MinFitL", &fMinFitL, 30);
  AddParam("MaxFitL", &fMaxFitL, 100);
  AddParam("MinFitH", &fMinFitH, 10);
  AddParam("MaxFitH", &fMaxFitH, 80);
  AddParam("UseKTAGRef", &fUseKTAGRef, false); //If true use KTAG kaon candidates (#sector>4) as reference time, otherwise trigger time is used
  AddParam("RawDecoderSettingsFileName",&fRawDecoderSettingsFileName,"CHANTI-RawDecoderSettings.dat");
  AddParam("OldTimeSlewing",&fOldTimeSlewing,Form("../NA62Reconstruction/Conditions/CHANTI-SlewingCorr.run7441_0005-run7441_0012.dat"));
  AddParam("NewTimeSlewing",&fNewTimeSlewing,Form("CHANTITimeSlewing_Run8007_original.dat"));
  SetOldTimeSlewing();	
}


void CHANTITimeSlewing::SetOldTimeSlewing(){
  TString Line;
  std::ifstream SlewingFile(fOldTimeSlewing.Data());
  if (!SlewingFile.is_open()){
    cout<< user_normal() << "Old time slewing correction file not found: corrections will not be set"<<endl;
    return;
  }	
  while (Line.ReadLine(SlewingFile)){
    if (Line.BeginsWith("#")) continue;
    else{    
      TObjArray *l = Line.Tokenize(" ");     
      Int_t Ch = ((TObjString*)(l->At(0)))->GetString().Atoi();      
      Double_t p0 = ((TObjString*)(l->At(1)))->GetString().Atof();
      Double_t p1 = ((TObjString*)(l->At(2)))->GetString().Atof();
      Double_t p2 = ((TObjString*)(l->At(3)))->GetString().Atof();    			
      if (Ch < fNChannels) {
        fOldTSCorrection[Ch][0] = p0;
        fOldTSCorrection[Ch][1] = p1;
        fOldTSCorrection[Ch][2] = p2;
      }      			
      delete l;
      continue;
    }
  }

  std::ifstream NewSlewingFile(fNewTimeSlewing.Data());
  if (NewSlewingFile.is_open()) {
    while (Line.ReadLine(NewSlewingFile)){
      if (Line.BeginsWith("#")) continue;
      else{    
        TObjArray *l = Line.Tokenize(" ");     
        Int_t Ch = ((TObjString*)(l->At(0)))->GetString().Atoi();     
        Double_t p0 = ((TObjString*)(l->At(1)))->GetString().Atof();
        Double_t p1 = ((TObjString*)(l->At(2)))->GetString().Atof();
        Double_t p2 = ((TObjString*)(l->At(3)))->GetString().Atof();    			
        if (Ch < fNChannels) {
          fNewTSCorrection[Ch][0] = p0;
          fNewTSCorrection[Ch][1] = p1;
          fNewTSCorrection[Ch][2] = p2;
        }      			
        delete l;
        continue;
      }
    }
  }
}


void CHANTITimeSlewing::InitOutput(){

}

void CHANTITimeSlewing::InitHist(){

  fReadingData = GetIsTree();
  if (fReadingData){
    std::cout << user_normal() << "Reading reconstructed data" << std::endl;
    BookHisto(new TH2F("CedarKaonsVsRefTime","CedarKaonsVsRefTime",fMaxNBursts,-0.5,fMaxNBursts-0.5,1500,-100,200));
    BookHisto(new TH2F("CHANTIHitVsRefTime","CHANTIHitVsRefTime",fMaxNBursts,-0.5,fMaxNBursts-0.5,1500,-100,200));
    BookHisto(new TH2F("CHANTIHitNotCorrVsRefTime","CHANTIHitNotCorrVsRefTime",fMaxNBursts,-0.5,fMaxNBursts-0.5,1500,-100,200));
    BookHisto(new TH2F("CHANTIHitNewCorrVsRefTime","CHANTIHitNewCorrVsRefTime",fMaxNBursts,-0.5,fMaxNBursts-0.5,1500,-100,200));
    BookHisto(new TH2F("CHANTIHitBothTHRVsRefTime","CHANTIHitBothTHRVsRefTime",fMaxNBursts/2,-0.5,fMaxNBursts-0.5,1500,-100,200));
    BookHisto(new TH2F("CHANTIHitSingleTHRVsRefTime","CHANTIHitSingleTHRVsRefTime",fMaxNBursts/2,-0.5,fMaxNBursts-0.5,1500,-100,200));
    BookHisto(new TH2F("CHANTIHitSingleNotCorrTHRVsRefTime","CHANTIHitSingleNotCorrTHRVsRefTime",fMaxNBursts/2,-0.5,fMaxNBursts-0.5,1500,-100,200));
    BookHisto(new TH2F("CHANTIHitSingleNewCorrTHRVsRefTime","CHANTIHitSingleNewCorrTHRVsRefTime",fMaxNBursts/2,-0.5,fMaxNBursts-0.5,1500,-100,200));	
    for (int iSt=0; iSt<6; iSt++){
      for (int iCh=0; iCh<24; iCh++){
        for (int iLayer=0; iLayer < 2; iLayer++){
          BookHisto(new TH2F(Form("CHANTIHit%s_St%s_BarID%s_BothTHRVsRefTime",fLayers[iLayer].Data(),fStationLabels[iSt].Data(),fChLabels[iCh].Data()),Form("CHANTIHit%s_St%s_BarID%s_BothTHRVsRefTime",fLayers[iLayer].Data(),fStationLabels[iSt].Data(),fChLabels[iCh].Data()),fMaxNBursts/2,-0.5,fMaxNBursts-0.5,1500,-100,200));
          BookHisto(new TH2F(Form("CHANTIHit%s_St%s_BarID%s_SingleTHRVsRefTime",fLayers[iLayer].Data(),fStationLabels[iSt].Data(),fChLabels[iCh].Data()),Form("CHANTIHit%s_St%s_BarID%s_SingleTHRVsRefTime",fLayers[iLayer].Data(),fStationLabels[iSt].Data(),fChLabels[iCh].Data()),fMaxNBursts/2,-0.5,fMaxNBursts-0.5,1500,-100,200));
          BookHisto(new TH2F(Form("CHANTIHitNotCorr%s_St%s_BarID%s_SingleTHRVsRefTime",fLayers[iLayer].Data(),fStationLabels[iSt].Data(),fChLabels[iCh].Data()),Form("CHANTIHitNotCorr%s_St%s_BarID%s_SingleTHRVsRefTime",fLayers[iLayer].Data(),fStationLabels[iSt].Data(),fChLabels[iCh].Data()),fMaxNBursts/2,-0.5,fMaxNBursts-0.5,1500,-100,200));
          BookHisto(new TH2F(Form("CHANTIHitNewCorr%s_St%s_BarID%s_SingleTHRVsRefTime",fLayers[iLayer].Data(),fStationLabels[iSt].Data(),fChLabels[iCh].Data()),Form("CHANTIHitNewCorr%s_St%s_BarID%s_SingleTHRVsRefTime",fLayers[iLayer].Data(),fStationLabels[iSt].Data(),fChLabels[iCh].Data()),fMaxNBursts/2,-0.5,fMaxNBursts-0.5,1500,-100,200));
          BookHisto(new TH2F(Form("LeadingLCorrVSTimeWidth_St%s_%s_BarID%s",fStationLabels[iSt].Data(),fLayers[iLayer].Data(),fChLabels[iCh].Data()),Form("LeadingLCorrVSTimeWidth_St%s_%s_BarID%s",fStationLabels[iSt].Data(),fLayers[iLayer].Data(),fChLabels[iCh].Data()),300,0,150,100,0,20)); 
          BookHisto(new TH2F(Form("LeadingHCorrVSTimeWidth_St%s_%s_BarID%s",fStationLabels[iSt].Data(),fLayers[iLayer].Data(),fChLabels[iCh].Data()),Form("LeadingHCorrVSTimeWidth_St%s_%s_BarID%s",fStationLabels[iSt].Data(),fLayers[iLayer].Data(),fChLabels[iCh].Data()),300,0,150,100,0,20)); 
        }
      }
    }
  } else {
    std::cout << user_normal() << "Reading my own output" << std::endl;
    fCHANTIHitTimeReso = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,"CHANTIHitVsRefTime",true));
    fCHANTIHitNotCorrTimeReso = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,"CHANTIHitNotCorrVsRefTime",true));
    fCHANTIHitNewCorrTimeReso = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,"CHANTIHitNewCorrVsRefTime",true));
    fCHANTIHitTimeResoBothTHR = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,"CHANTIHitBothTHRVsRefTime",true));
    fCHANTIHitTimeResoSingleTHR = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,"CHANTIHitSingleTHRVsRefTime",true));
    fCHANTIHitTimeResoSingleTHRNotCorr = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,"CHANTIHitSingleNotCorrTHRVsRefTime",true));
    fCHANTIHitTimeResoSingleTHRNewCorr = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,"CHANTIHitSingleNewCorrTHRVsRefTime",true));
    fPar0L = new TGraph**[6];
    fPar1L = new TGraph**[6];
    fPar2L = new TGraph**[6];
    fPar0H = new TGraph**[6];
    fPar1H = new TGraph**[6];
    fPar2H = new TGraph**[6];
    fSigmaSingle = new TGraph**[6];
    fSigmaSingleNotCorr = new TGraph**[6];
    fSigmaSingleNewCorr = new TGraph**[6];	
    fSigmaDouble = new TGraph**[6];
    fCorrL_ToT = new TH2F***[6];
    fCorrH_ToT = new TH2F***[6];
    fHitTimeSingle_BurstID = new TH2F***[6];
    fHitTimeNotCorrSingle_BurstID = new TH2F***[6];
    fHitTimeNewCorrSingle_BurstID = new TH2F***[6];	
    fHitTimeDouble_BurstID = new TH2F***[6];	
    for (int iSt=0; iSt<6; iSt++){
      fPar0L[iSt] = new TGraph*[2];
      fPar1L[iSt] = new TGraph*[2];
      fPar2L[iSt] = new TGraph*[2];
      fPar0H[iSt] = new TGraph*[2];
      fPar1H[iSt] = new TGraph*[2];
      fPar2H[iSt] = new TGraph*[2];
      fSigmaSingle[iSt]  = new TGraph*[2];
      fSigmaSingleNotCorr[iSt]  = new TGraph*[2];
      fSigmaSingleNewCorr[iSt]  = new TGraph*[2];
      fSigmaDouble[iSt]  = new TGraph*[2];
      fCorrL_ToT[iSt] = new TH2F**[2];
      fCorrH_ToT[iSt] = new TH2F**[2];
      fHitTimeSingle_BurstID[iSt] = new TH2F**[2];
      fHitTimeNotCorrSingle_BurstID[iSt] = new TH2F**[2];
      fHitTimeNewCorrSingle_BurstID[iSt] = new TH2F**[2];
      fHitTimeDouble_BurstID[iSt] = new TH2F**[2];
      for (int iLayer=0; iLayer < 2; iLayer++){
        fPar0L[iSt][iLayer] = new TGraph();
        fPar0L[iSt][iLayer]->SetNameTitle(Form("St%s_%s_LowThrPar0Constant_VS_Channel",fStationLabels[iSt].Data(),fLayers[iLayer].Data()),Form("St%s_%s_LowThrPar0Constant_VS_Channel",fStationLabels[iSt].Data(),fLayers[iLayer].Data()));
        fPar1L[iSt][iLayer] = new TGraph();	
        fPar1L[iSt][iLayer]->SetNameTitle(Form("St%s_%s_LowThrPar1ExpNormalization_VS_Channel",fStationLabels[iSt].Data(),fLayers[iLayer].Data()),Form("St%s_%s_LowThrPar1ExpNormalization_VS_Channel",fStationLabels[iSt].Data(),fLayers[iLayer].Data()));
        fPar2L[iSt][iLayer] = new TGraph();
        fPar2L[iSt][iLayer]->SetNameTitle(Form("St%s_%s_LowThrPar2ExpScale_VS_Channel",fStationLabels[iSt].Data(),fLayers[iLayer].Data()),Form("St%s_%s_LowThrPar2ExpScale_VS_Channel",fStationLabels[iSt].Data(),fLayers[iLayer].Data()));
        fPar0H[iSt][iLayer] = new TGraph();
        fPar0H[iSt][iLayer]->SetNameTitle(Form("St%s_%s_HighThrPar0Constant_VS_Channel",fStationLabels[iSt].Data(),fLayers[iLayer].Data()),Form("St%s_%s_HighThrPar0Constant_VS_Channel",fStationLabels[iSt].Data(),fLayers[iLayer].Data()));
        fPar1H[iSt][iLayer] = new TGraph();	
        fPar1H[iSt][iLayer]->SetNameTitle(Form("St%s_%s_HighThrPar1ExpNormalization_VS_Channel",fStationLabels[iSt].Data(),fLayers[iLayer].Data()),Form("St%s_%s_HighThrPar1ExpNormalization_VS_Channel",fStationLabels[iSt].Data(),fLayers[iLayer].Data()));
        fPar2H[iSt][iLayer] = new TGraph();	
        fPar2H[iSt][iLayer]->SetNameTitle(Form("St%s_%s_HighThrPar2ExpScale_VS_Channel",fStationLabels[iSt].Data(),fLayers[iLayer].Data()),Form("St%s_%s_HighThrPar2ExpScale_VS_Channel",fStationLabels[iSt].Data(),fLayers[iLayer].Data()));
        fSigmaSingle[iSt][iLayer] = new TGraph();
        fSigmaSingle[iSt][iLayer]->SetNameTitle(Form("St%s_%s_TimeResoSingle_VS_Channel",fStationLabels[iSt].Data(),fLayers[iLayer].Data()),Form("St%s_%s_TimeResoSingle_VS_Channel",fStationLabels[iSt].Data(),fLayers[iLayer].Data()));
        fSigmaSingleNotCorr[iSt][iLayer] = new TGraph();
        fSigmaSingleNotCorr[iSt][iLayer]->SetNameTitle(Form("St%s_%s_TimeResoSingleNotCorr_VS_Channel",fStationLabels[iSt].Data(),fLayers[iLayer].Data()),Form("St%s_%s_TimeResoSingleNotCorr_VS_Channel",fStationLabels[iSt].Data(),fLayers[iLayer].Data()));
        fSigmaSingleNewCorr[iSt][iLayer] = new TGraph();
        fSigmaSingleNewCorr[iSt][iLayer]->SetNameTitle(Form("St%s_%s_TimeResoSingleNewCorr_VS_Channel",fStationLabels[iSt].Data(),fLayers[iLayer].Data()),Form("St%s_%s_TimeResoSingleNewCorr_VS_Channel",fStationLabels[iSt].Data(),fLayers[iLayer].Data()));

        fSigmaDouble[iSt][iLayer] = new TGraph();
        fSigmaDouble[iSt][iLayer]->SetNameTitle(Form("St%s_%s_TimeResoDouble_VS_Channel",fStationLabels[iSt].Data(),fLayers[iLayer].Data()),Form("St%s_%s_TimeResoDouble_VS_Channel",fStationLabels[iSt].Data(),fLayers[iLayer].Data()));
        fCorrL_ToT[iSt][iLayer] = new TH2F*[24];
        fCorrH_ToT[iSt][iLayer] = new TH2F*[24];
        fHitTimeSingle_BurstID[iSt][iLayer] = new TH2F*[24];
        fHitTimeNotCorrSingle_BurstID[iSt][iLayer] = new TH2F*[24];
        fHitTimeNewCorrSingle_BurstID[iSt][iLayer] = new TH2F*[24];
        fHitTimeDouble_BurstID[iSt][iLayer] = new TH2F*[24];
        for (int iCh=0; iCh<24; iCh++){			
          fCorrL_ToT[iSt][iLayer][iCh] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,Form("LeadingLCorrVSTimeWidth_St%s_%s_BarID%s",fStationLabels[iSt].Data(),fLayers[iLayer].Data(),fChLabels[iCh].Data()),true));
          fCorrH_ToT[iSt][iLayer][iCh] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,Form("LeadingHCorrVSTimeWidth_St%s_%s_BarID%s",fStationLabels[iSt].Data(),fLayers[iLayer].Data(),fChLabels[iCh].Data()),true));
          fHitTimeSingle_BurstID[iSt][iLayer][iCh] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,Form("CHANTIHit%s_St%s_BarID%s_SingleTHRVsRefTime",fLayers[iLayer].Data(),fStationLabels[iSt].Data(),fChLabels[iCh].Data()),true));
          fHitTimeNotCorrSingle_BurstID[iSt][iLayer][iCh]  = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,Form("CHANTIHitNotCorr%s_St%s_BarID%s_SingleTHRVsRefTime",fLayers[iLayer].Data(),fStationLabels[iSt].Data(),fChLabels[iCh].Data()),true));
          fHitTimeNewCorrSingle_BurstID[iSt][iLayer][iCh]  = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,Form("CHANTIHitNewCorr%s_St%s_BarID%s_SingleTHRVsRefTime",fLayers[iLayer].Data(),fStationLabels[iSt].Data(),fChLabels[iCh].Data()),true));
          fHitTimeDouble_BurstID[iSt][iLayer][iCh]  = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,Form("CHANTIHit%s_St%s_BarID%s_BothTHRVsRefTime",fLayers[iLayer].Data(),fStationLabels[iSt].Data(),fChLabels[iCh].Data()),true));
        }			
      }
    }
  }
}

void CHANTITimeSlewing::DefineMCSimple(){

}

void CHANTITimeSlewing::StartOfRunUser(){
  ParseRawDecoderSettingsFile();
}

void CHANTITimeSlewing::StartOfBurstUser(){

}

void CHANTITimeSlewing::ProcessSpecialTriggerUser(int, unsigned int){

}

void CHANTITimeSlewing::Process(int){


  TRecoCHANTIEvent *CHANTIEvent = GetEvent<TRecoCHANTIEvent>();
  TRecoCedarEvent *CedarEvent = GetEvent<TRecoCedarEvent>();

  if (!fReadingData) return; // no action if reading its own output in --histo mode 

  Int_t  L0DataType    = GetL0Data()->GetDataType();
  Int_t  L0TriggerWord = GetL0Data()->GetTriggerFlags(); //To use only when some physics masks are enabled
  Bool_t PhysicsData   = L0DataType & 0x1; //To use only when some physics masks are enabled
  //Bool_t CTRLTrigger   = L0DataType & 0x10;
  //Bool_t TriggerOK     = (PhysicsData && (L0TriggerWord&0x1)) || CTRLTrigger; //MASK0 only + CTRL
  //Bool_t TriggerOK     = (PhysicsData && (L0TriggerWord&0xFF)) || CTRLTrigger; //ALL MASKS + CTRL
  //Bool_t TriggerOK     = CTRLTrigger; //CTRL only
  Bool_t TriggerOK     = (PhysicsData && ( (L0TriggerWord&0x1) || (L0TriggerWord&0x2) ) ); //MASK0 && 1 only
  if (!TriggerOK) return; // process control triggers and selected MASKS only

  Double_t RefTime = GetEventHeader()->GetFineTime()*ClockPeriod/256.;
  Int_t nHits = CHANTIEvent->GetNHits();
  TClonesArray & RecoHits = (* (CHANTIEvent->GetHits()));
  for(Int_t iCedarCand=0; iCedarCand<CedarEvent->GetNCandidates(); iCedarCand++){
    TRecoCedarCandidate *CedarCandidate = static_cast<TRecoCedarCandidate*>(CedarEvent->GetCandidate(iCedarCand));
    Double_t KaonTime = CedarCandidate->GetTime(); 
    Double_t dt = KaonTime-RefTime; 
    if (CedarCandidate->GetNSectors() > 4){
      FillHisto("CedarKaonsVsRefTime",GetBurstID(),dt);		
      if (fUseKTAGRef) FillHistoTimeReso(nHits, RecoHits, KaonTime);	
    }
  }

  if (!fUseKTAGRef) FillHistoTimeReso(nHits, RecoHits, RefTime);



  for(Int_t iHit = 0; iHit<nHits; iHit++){
    TRecoCHANTIHit *RecoHit = static_cast<TRecoCHANTIHit*>(RecoHits[iHit]);
    int iSt = RecoHit->GetPlaneID();
    int BarID = RecoHit->GetBarID();
    int ThresholdFlag = RecoHit->GetThresholdFlag();
    int QualityFlag = RecoHit->GetQualityFlag();
    Int_t RingType = RecoHit->GetRingType(); 
    if (ThresholdFlag!=2 || QualityFlag!=0) continue;
    Double_t LeadingCorrection = RecoHit->GetDeltaTime()*fThrL/(fThrH-fThrL);
    FillHisto(Form("LeadingLCorrVSTimeWidth_St%s_%s_BarID%d",fStationLabels[iSt].Data(),fLayers[RingType].Data(),BarID),RecoHit->GetTimeWidth(),LeadingCorrection);
    FillHisto(Form("LeadingHCorrVSTimeWidth_St%s_%s_BarID%d",fStationLabels[iSt].Data(),fLayers[RingType].Data(),BarID),RecoHit->GetTimeWidth()+RecoHit->GetDeltaWidth(),LeadingCorrection+RecoHit->GetDeltaTime());
  }
}



void CHANTITimeSlewing::FillHistoTimeReso(Int_t nHits, TClonesArray & RecoHits, Double_t RefTime){
  for (int iCHANTIHit=0; iCHANTIHit<nHits; iCHANTIHit++ ){
    TRecoCHANTIHit *CHANTIHit = static_cast<TRecoCHANTIHit*>(RecoHits[iCHANTIHit]);
    int iSt = CHANTIHit->GetPlaneID();
    int BarID = CHANTIHit->GetBarID();
    Int_t SideID = CHANTIHit->GetSideID();
    Int_t RingType = CHANTIHit->GetRingType(); 
    int IndexGeneralBar = -1;
    Int_t AbsfBarID = (BarID>0)? BarID : -BarID;
    Int_t ChannelID = (iSt + 1)*100000 + RingType*10000 + SideID*1000 + AbsfBarID*10;				
    for (int iCh = 0; iCh < fNChannels; iCh++){
      if (ChannelID==fChannelID[iCh]){
        IndexGeneralBar = iCh;
        break;	
      }
    }
    if (IndexGeneralBar==-1) continue;
    int ThresholdFlag = CHANTIHit->GetThresholdFlag();
    int QualityFlag = CHANTIHit->GetQualityFlag();
    Double_t dtCHANTI = CHANTIHit->GetTime()-RefTime;
    Double_t Corr = fOldTSCorrection[IndexGeneralBar][2]+fOldTSCorrection[IndexGeneralBar][1]*TMath::Exp(fOldTSCorrection[IndexGeneralBar][0]*CHANTIHit->GetTimeWidth()); 
    Double_t NewCorr;
    if (fNewTSCorrection[IndexGeneralBar][0] == -99) NewCorr=-99;
    else NewCorr = fNewTSCorrection[IndexGeneralBar][2]+fNewTSCorrection[IndexGeneralBar][1]*TMath::Exp(fNewTSCorrection[IndexGeneralBar][0]*CHANTIHit->GetTimeWidth());	
    Double_t dtCHANTINotCorrected = CHANTIHit->GetTime()-RefTime+Corr;
    Double_t dtCHANTINewCorr = -99.;
    if (NewCorr!=-99.) dtCHANTINewCorr = CHANTIHit->GetTime()-RefTime+Corr-NewCorr;
    if (QualityFlag == 5) continue;
    if (ThresholdFlag == 1) continue;
    FillHisto("CHANTIHitVsRefTime",GetBurstID(),dtCHANTI);
    if (ThresholdFlag == 0) { //Only low thr crossed
      FillHisto("CHANTIHitNotCorrVsRefTime",GetBurstID(),dtCHANTINotCorrected);				
      FillHisto(Form("CHANTIHit%s_St%s_BarID%d_SingleTHRVsRefTime",fLayers[RingType].Data(),fStationLabels[iSt].Data(),BarID),GetBurstID(),dtCHANTI);	
      FillHisto(Form("CHANTIHitNotCorr%s_St%s_BarID%d_SingleTHRVsRefTime",fLayers[RingType].Data(),fStationLabels[iSt].Data(),BarID),GetBurstID(),dtCHANTINotCorrected);
      FillHisto("CHANTIHitSingleTHRVsRefTime",GetBurstID(),dtCHANTI);
      FillHisto("CHANTIHitSingleNotCorrTHRVsRefTime",GetBurstID(),dtCHANTINotCorrected);
      if (NewCorr!=-99.){
        FillHisto("CHANTIHitNewCorrVsRefTime",GetBurstID(),dtCHANTINewCorr);	
        FillHisto("CHANTIHitSingleNewCorrTHRVsRefTime",GetBurstID(),dtCHANTINewCorr);
        FillHisto(Form("CHANTIHitNewCorr%s_St%s_BarID%d_SingleTHRVsRefTime",fLayers[RingType].Data(),fStationLabels[iSt].Data(),BarID),GetBurstID(),dtCHANTINewCorr);
      }		
    }
    if (ThresholdFlag == 2) { //Both thr crossed
      FillHisto("CHANTIHitNotCorrVsRefTime",GetBurstID(),dtCHANTI);
      if (NewCorr!=-99.) FillHisto("CHANTIHitNewCorrVsRefTime",GetBurstID(),dtCHANTI);
      FillHisto(Form("CHANTIHit%s_St%s_BarID%d_BothTHRVsRefTime",fLayers[RingType].Data(),fStationLabels[iSt].Data(),BarID),GetBurstID(),dtCHANTI);
      FillHisto("CHANTIHitBothTHRVsRefTime",GetBurstID(),dtCHANTI);		
    }
  }
}


void CHANTITimeSlewing::PostProcess(){


}

void CHANTITimeSlewing::EndOfBurstUser(){

}

void CHANTITimeSlewing::EndOfRunUser(){


}

void CHANTITimeSlewing::EndOfJobUser(){

  gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed

  /////////////
  // HISTO mode

  if (!fReadingData) {
    if (!fCorrL_ToT) {
      std::cout << user_normal() << "Asked to read my own output but cannot found it" << std::endl;
      return;
    }

    std::ifstream NewSlewingFile(fNewTimeSlewing.Data());
    Bool_t NewTSFileAvailable = NewSlewingFile.is_open(); 

    fCanvas = new TCanvas("Canvas");
    TH1* TimeResoSingle[6][2][24];
    TH1* TimeResoSingleNotCorr[6][2][24];
    TH1* TimeResoSingleNewCorr[6][2][24];
    TH1* TimeResoDouble[6][2][24];
    //TF1 *ResoFunc = new TF1("ResoFunc","[0]*TMath::Exp(-0.5*(TMath::Power((x-[1])/[2],2)))",-10.,10.);
    TF1 *ResoFunc = new TF1("ResoFunc","gaus",-10.,10.);
    for (int iSt=0; iSt<6; iSt++){
      for (int iLayer=0; iLayer<2; iLayer++){
        for (int iCh=0; iCh<24; iCh++){
          //cout<<"START"<<"	iSt = "<<iSt<<"		iCh = "<<iCh<<endl;
          TimeResoSingle[iSt][iLayer][iCh] = fHitTimeSingle_BurstID[iSt][iLayer][iCh]->ProjectionY();
          Double_t XMax =  TimeResoSingle[iSt][iLayer][iCh]->GetBinCenter(TimeResoSingle[iSt][iLayer][iCh]->GetMaximumBin());					
          ResoFunc->SetRange(XMax-3.5,XMax+3.5);
          ResoFunc->SetParLimits(0,0,10000);					
          //ResoFunc->SetParameter(0,10);
          ResoFunc->SetParameter(1,0.);
          ResoFunc->SetParameter(2,1.);
          ResoFunc->SetParLimits(2,0,8);
          TimeResoSingle[iSt][iLayer][iCh]->Fit("ResoFunc","RBQ");
          fSigmaSingle[iSt][iLayer]->SetPoint(fSigmaSingle[iSt][iLayer]->GetN(),fChLabels[iCh].Atoi(),ResoFunc->GetParameter(2));
          TimeResoSingle[iSt][iLayer][iCh]->Write();

          if (NewTSFileAvailable){
            TimeResoSingleNewCorr[iSt][iLayer][iCh] = fHitTimeNewCorrSingle_BurstID[iSt][iLayer][iCh]->ProjectionY();
            XMax =  TimeResoSingleNewCorr[iSt][iLayer][iCh]->GetBinCenter(TimeResoSingleNewCorr[iSt][iLayer][iCh]->GetMaximumBin());					
            ResoFunc->SetRange(XMax-3.5,XMax+3.5);
            ResoFunc->SetParLimits(0,0,10000);					
            //ResoFunc->SetParameter(0,10);
            ResoFunc->SetParameter(1,0.);
            ResoFunc->SetParameter(2,1.);
            ResoFunc->SetParLimits(2,0,8);
            TimeResoSingleNewCorr[iSt][iLayer][iCh]->Fit("ResoFunc","RBQ");
            fSigmaSingleNewCorr[iSt][iLayer]->SetPoint(fSigmaSingleNewCorr[iSt][iLayer]->GetN(),fChLabels[iCh].Atoi(),ResoFunc->GetParameter(2));
            TimeResoSingleNewCorr[iSt][iLayer][iCh]->Write();
          }

          TimeResoSingleNotCorr[iSt][iLayer][iCh] = fHitTimeNotCorrSingle_BurstID[iSt][iLayer][iCh]->ProjectionY();
          XMax =  TimeResoSingle[iSt][iLayer][iCh]->GetBinCenter(TimeResoSingleNotCorr[iSt][iLayer][iCh]->GetMaximumBin());					
          ResoFunc->SetRange(XMax-3.5,XMax+3.5);
          ResoFunc->SetParLimits(0,0,10000);					
          //ResoFunc->SetParameter(0,10);
          ResoFunc->SetParameter(1,0.);
          ResoFunc->SetParameter(2,1.);
          ResoFunc->SetParLimits(2,0,8);	
          TimeResoSingleNotCorr[iSt][iLayer][iCh]->Fit("ResoFunc","RBQ");
          fSigmaSingleNotCorr[iSt][iLayer]->SetPoint(fSigmaSingleNotCorr[iSt][iLayer]->GetN(),fChLabels[iCh].Atoi(),ResoFunc->GetParameter(2));
          TimeResoSingleNotCorr[iSt][iLayer][iCh]->Write();

          TimeResoDouble[iSt][iLayer][iCh] = fHitTimeDouble_BurstID[iSt][iLayer][iCh]->ProjectionY();
          XMax =  TimeResoSingle[iSt][iLayer][iCh]->GetBinCenter(TimeResoDouble[iSt][iLayer][iCh]->GetMaximumBin());			
          ResoFunc->SetRange(XMax-1.8,XMax+1.8);
          ResoFunc->SetParLimits(0,0,10000);					
          //ResoFunc->SetParameter(0,10);
          ResoFunc->SetParameter(1,0.);
          ResoFunc->SetParameter(2,1.1);
          ResoFunc->SetParLimits(2,0,8);
          TimeResoDouble[iSt][iLayer][iCh]->Fit("ResoFunc","RBQ");
          fSigmaDouble[iSt][iLayer]->SetPoint(fSigmaDouble[iSt][iLayer]->GetN(),fChLabels[iCh].Atoi(),ResoFunc->GetParameter(2));
          TimeResoDouble[iSt][iLayer][iCh]->Write();
          //cout<<"END"<<"	iSt = "<<iSt<<"		iCh = "<<iCh<<endl<<endl;
        }
      }
    }
    fCanvas->Clear();
    fCanvas->Divide(3,4);
    for (Int_t i=1; i<=12; i++) {
      fCanvas->GetPad(i)->SetLeftMargin(0.07);
      fCanvas->GetPad(i)->SetRightMargin(0.01);
      fCanvas->GetPad(i)->SetTopMargin(0.01);
      fCanvas->GetPad(i)->SetBottomMargin(0.06);
    }
    fCanvas->GetPad(1)->Clear();		
    for (int iSt=0; iSt<6; iSt++){
      for (int iLayer=0; iLayer<2; iLayer++){
        fCanvas->GetPad(2*iSt+iLayer+1)->Clear();
        fCanvas->cd(2*iSt+iLayer+1);
        fSigmaDouble[iSt][iLayer]->GetXaxis()->SetLabelSize(0.07);
        fSigmaDouble[iSt][iLayer]->GetYaxis()->SetLabelSize(0.055);
        fSigmaDouble[iSt][iLayer]->GetXaxis()->SetRangeUser(-20.,20.);
        fSigmaDouble[iSt][iLayer]->GetYaxis()->SetRangeUser(0., 4.);
        fSigmaDouble[iSt][iLayer]->SetMarkerColor(1);
        fSigmaDouble[iSt][iLayer]->SetMarkerStyle(20);
        fSigmaDouble[iSt][iLayer]->SetMarkerSize(0.8);
        fSigmaDouble[iSt][iLayer]->GetXaxis()->SetTitle("");
        fSigmaDouble[iSt][iLayer]->GetYaxis()->SetTitle("");
        fSigmaDouble[iSt][iLayer]->Draw("AP");
      }
    }
    fCanvas->Print(Form(fOutPDFFileName + "("), "pdf");


    if (NewTSFileAvailable){
      fCanvas->Clear();
      fCanvas->Divide(3,4);
      for (Int_t i=1; i<=12; i++) {
        fCanvas->GetPad(i)->SetLeftMargin(0.07);
        fCanvas->GetPad(i)->SetRightMargin(0.01);
        fCanvas->GetPad(i)->SetTopMargin(0.01);
        fCanvas->GetPad(i)->SetBottomMargin(0.06);
      }
      fCanvas->GetPad(1)->Clear();		
      for (int iSt=0; iSt<6; iSt++){
        for (int iLayer=0; iLayer<2; iLayer++){
          fCanvas->GetPad(2*iSt+iLayer+1)->Clear();
          fCanvas->cd(2*iSt+iLayer+1);
          fSigmaSingleNewCorr[iSt][iLayer]->GetXaxis()->SetLabelSize(0.07);
          fSigmaSingleNewCorr[iSt][iLayer]->GetYaxis()->SetLabelSize(0.055);
          fSigmaSingleNewCorr[iSt][iLayer]->GetXaxis()->SetRangeUser(-20.,20.);
          fSigmaSingleNewCorr[iSt][iLayer]->GetYaxis()->SetRangeUser(0., 4.);
          fSigmaSingleNewCorr[iSt][iLayer]->SetMarkerColor(1);
          fSigmaSingleNewCorr[iSt][iLayer]->SetMarkerStyle(20);
          fSigmaSingleNewCorr[iSt][iLayer]->SetMarkerSize(0.8);
          fSigmaSingleNewCorr[iSt][iLayer]->GetXaxis()->SetTitle("");
          fSigmaSingleNewCorr[iSt][iLayer]->GetYaxis()->SetTitle("");
          fSigmaSingleNewCorr[iSt][iLayer]->Draw("AP");
        }
      }
      fCanvas->Print(fOutPDFFileName, "pdf");
    }


    fCanvas->Clear();
    fCanvas->Divide(3,4);
    for (Int_t i=1; i<=12; i++) {
      fCanvas->GetPad(i)->SetLeftMargin(0.07);
      fCanvas->GetPad(i)->SetRightMargin(0.01);
      fCanvas->GetPad(i)->SetTopMargin(0.01);
      fCanvas->GetPad(i)->SetBottomMargin(0.06);
    }
    fCanvas->GetPad(1)->Clear();		
    for (int iSt=0; iSt<6; iSt++){
      for (int iLayer=0; iLayer<2; iLayer++){
        fCanvas->GetPad(2*iSt+iLayer+1)->Clear();
        fCanvas->cd(2*iSt+iLayer+1);
        fSigmaSingleNotCorr[iSt][iLayer]->GetXaxis()->SetLabelSize(0.07);
        fSigmaSingleNotCorr[iSt][iLayer]->GetYaxis()->SetLabelSize(0.055);
        fSigmaSingleNotCorr[iSt][iLayer]->GetXaxis()->SetRangeUser(-20.,20.);
        fSigmaSingleNotCorr[iSt][iLayer]->GetYaxis()->SetRangeUser(0., 4.);
        fSigmaSingleNotCorr[iSt][iLayer]->SetMarkerColor(1);
        fSigmaSingleNotCorr[iSt][iLayer]->SetMarkerStyle(20);
        fSigmaSingleNotCorr[iSt][iLayer]->SetMarkerSize(0.8);	
        fSigmaSingleNotCorr[iSt][iLayer]->GetXaxis()->SetTitle("");
        fSigmaSingleNotCorr[iSt][iLayer]->GetYaxis()->SetTitle("");
        fSigmaSingleNotCorr[iSt][iLayer]->Draw("AP");
      }
    }
    fCanvas->Print(fOutPDFFileName, "pdf");

    fCanvas->Clear();
    fCanvas->Divide(3,4);
    for (Int_t i=1; i<=12; i++) {
      fCanvas->GetPad(i)->SetLeftMargin(0.07);
      fCanvas->GetPad(i)->SetRightMargin(0.01);
      fCanvas->GetPad(i)->SetTopMargin(0.01);
      fCanvas->GetPad(i)->SetBottomMargin(0.06);
    }
    fCanvas->GetPad(1)->Clear();		
    for (int iSt=0; iSt<6; iSt++){
      for (int iLayer=0; iLayer<2; iLayer++){
        fCanvas->GetPad(2*iSt+iLayer+1)->Clear();
        fCanvas->cd(2*iSt+iLayer+1);
        fSigmaSingle[iSt][iLayer]->GetXaxis()->SetLabelSize(0.07);
        fSigmaSingle[iSt][iLayer]->GetYaxis()->SetLabelSize(0.055);
        fSigmaSingle[iSt][iLayer]->GetXaxis()->SetRangeUser(-20.,20.);
        fSigmaSingle[iSt][iLayer]->GetYaxis()->SetRangeUser(0., 4.);
        fSigmaSingle[iSt][iLayer]->SetMarkerColor(1);
        fSigmaSingle[iSt][iLayer]->SetMarkerStyle(20);
        fSigmaSingle[iSt][iLayer]->SetMarkerSize(0.8);
        fSigmaSingle[iSt][iLayer]->GetXaxis()->SetTitle("");
        fSigmaSingle[iSt][iLayer]->GetYaxis()->SetTitle("");
        fSigmaSingle[iSt][iLayer]->Draw("AP");
      }
    }
    fCanvas->Print(fOutPDFFileName, "pdf");

    fCanvas->Clear();
    fCanvas->Divide(4,4);
    for (Int_t i=1; i<=16; i++) {
      fCanvas->GetPad(i)->SetLeftMargin(0.07);
      fCanvas->GetPad(i)->SetRightMargin(0.01);
      fCanvas->GetPad(i)->SetTopMargin(0.01);
      fCanvas->GetPad(i)->SetBottomMargin(0.06);
    }
    fCanvas->GetPad(1)->Clear();
    Int_t NGraphs = 0;
    TF1  *FitFunc = new TF1("FitFunc","[2]+[1]*TMath::Exp([0]*x)",fMinFitL,fMaxFitL);
    TProfile* CorrVSToT[6][2][24];
    for (int iSt=0; iSt<6; iSt++){
      for (int iLayer=0; iLayer<2; iLayer++){
        for (int iCh=0; iCh<24; iCh++){
          CorrVSToT[iSt][iLayer][iCh] = fCorrL_ToT[iSt][iLayer][iCh]->ProfileX();
          CorrVSToT[iSt][iLayer][iCh]->SetNameTitle(Form("LowThr_CorrVSToT_St%s_%s_BarID%s",fStationLabels[iSt].Data(),fLayers[iLayer].Data(),fChLabels[iCh].Data()),Form("LowThr_CorrVSToT_St%s_%s_BarID%s",fStationLabels[iSt].Data(),fLayers[iLayer].Data(),fChLabels[iCh].Data()));
          FitFunc->SetParameter(0,-8e-2);
          FitFunc->SetParLimits(0,-0.09,0.);
          FitFunc->SetParameter(1,16);
          FitFunc->SetParLimits(1,5.,20.);
          FitFunc->SetParameter(2,0.9);
          FitFunc->SetParLimits(2,0.,5.);

          CorrVSToT[iSt][iLayer][iCh]->Fit("FitFunc","RBQ");

          fPar0L[iSt][iLayer]->SetPoint(fPar0L[iSt][iLayer]->GetN(),fChLabels[iCh].Atoi(),FitFunc->GetParameter(0));
          fPar1L[iSt][iLayer]->SetPoint(fPar1L[iSt][iLayer]->GetN(),fChLabels[iCh].Atoi(),FitFunc->GetParameter(1));
          fPar2L[iSt][iLayer]->SetPoint(fPar2L[iSt][iLayer]->GetN(),fChLabels[iCh].Atoi(),FitFunc->GetParameter(2));	

          if (NGraphs==16) NGraphs = 0;
          fCanvas->GetPad(NGraphs+1)->Clear();
          fCanvas->cd(NGraphs+1);
          CorrVSToT[iSt][iLayer][iCh]->GetXaxis()->SetLabelSize(0.07);
          CorrVSToT[iSt][iLayer][iCh]->GetYaxis()->SetLabelSize(0.055);
          CorrVSToT[iSt][iLayer][iCh]->GetXaxis()->SetRangeUser(0.,150.);
          CorrVSToT[iSt][iLayer][iCh]->GetYaxis()->SetRangeUser(0., 10.);
          CorrVSToT[iSt][iLayer][iCh]->GetXaxis()->SetTitle("");
          CorrVSToT[iSt][iLayer][iCh]->GetYaxis()->SetTitle("");
          CorrVSToT[iSt][iLayer][iCh]->Draw();
          if (NGraphs==15) {
            fCanvas->Print(fOutPDFFileName, "pdf");
          }
          NGraphs++;
          CorrVSToT[iSt][iLayer][iCh]->Write();
        }
      }
    }

    Int_t NPages = 0;
    FitFunc->SetRange(fMinFitH,fMaxFitH);
    NGraphs = 0;
    for (int iSt=0; iSt<6; iSt++){
      for (int iLayer=0; iLayer<2; iLayer++){
        for (int iCh=0; iCh<24; iCh++){
          CorrVSToT[iSt][iLayer][iCh] = fCorrH_ToT[iSt][iLayer][iCh]->ProfileX();
          CorrVSToT[iSt][iLayer][iCh]->SetNameTitle(Form("HighThr_CorrVSToT_St%s_%s_BarID%s",fStationLabels[iSt].Data(),fLayers[iLayer].Data(),fChLabels[iCh].Data()),Form("LowThr_CorrVSToT_St%s_BarID%s",fStationLabels[iSt].Data(),fChLabels[iCh].Data()));
          FitFunc->SetParameter(0,-8e-2);
          FitFunc->SetParLimits(0,-0.09,0.);
          FitFunc->SetParameter(1,16);
          FitFunc->SetParLimits(1,5.,20.);
          FitFunc->SetParameter(2,0.9);
          FitFunc->SetParLimits(2,0.,5.);

          CorrVSToT[iSt][iLayer][iCh]->Fit("FitFunc","RBQ");

          fPar0H[iSt][iLayer]->SetPoint(fPar0H[iSt][iLayer]->GetN(),fChLabels[iCh].Atoi(),FitFunc->GetParameter(0));
          fPar1H[iSt][iLayer]->SetPoint(fPar1H[iSt][iLayer]->GetN(),fChLabels[iCh].Atoi(),FitFunc->GetParameter(1));
          fPar2H[iSt][iLayer]->SetPoint(fPar2H[iSt][iLayer]->GetN(),fChLabels[iCh].Atoi(),FitFunc->GetParameter(2));	

          if (NGraphs==16) NGraphs = 0;
          fCanvas->GetPad(NGraphs+1)->Clear();
          fCanvas->cd(NGraphs+1);
          CorrVSToT[iSt][iLayer][iCh]->GetXaxis()->SetLabelSize(0.07);
          CorrVSToT[iSt][iLayer][iCh]->GetYaxis()->SetLabelSize(0.055);
          CorrVSToT[iSt][iLayer][iCh]->GetXaxis()->SetRangeUser(0.,150.);
          CorrVSToT[iSt][iLayer][iCh]->GetYaxis()->SetRangeUser(0., 10.);
          CorrVSToT[iSt][iLayer][iCh]->GetXaxis()->SetTitle("");
          CorrVSToT[iSt][iLayer][iCh]->GetYaxis()->SetTitle("");
          CorrVSToT[iSt][iLayer][iCh]->Draw();
          if (NGraphs==15) {
            if (NPages==16) fCanvas->Print(Form(fOutPDFFileName + ")"), "pdf");
            else if (NPages>=0 && NPages<16){
              fCanvas->Print(fOutPDFFileName, "pdf");
            }
            NPages++;
          }
          NGraphs++;
          CorrVSToT[iSt][iLayer][iCh]->Write();
        }
      }
    }


    Int_t RunID = GetRunID();
    fTimeSlewingFile.open(Form("CHANTITimeSlewing_Run%d.dat",RunID), ios::trunc);
    fTimeSlewingFile << "# Slewing corrections obtained with run "<<RunID<<"\n";
    fTimeSlewingFile << "#-----------------------------------------------------------------------------------"<<"\n";
    fTimeSlewingFile << "#SeqID 	p0       	p1		p2"<<"\n";
    fTimeSlewingFile << "#-----------------------------------------------------------------------------------"<<"\n";
    for (int iCh=0; iCh<fNChannels; iCh++){		
      fTimeSlewingFile<<iCh<<"     ";
      Bool_t IsActive = (fChannelID[iCh]>=0);
      if (!IsActive){
        fTimeSlewingFile<<"-1"<<"     ";
        fTimeSlewingFile<<"-1"<<"     ";
        fTimeSlewingFile<<"-1"<<"\n";
      } else {
        Int_t PlaneID = fChannelID[iCh]/100000 - 1;
        Int_t SideID = (fChannelID[iCh]%10000)/1000;
        Int_t BarID  = (1-2*SideID)*(fChannelID[iCh]%1000)/10;
        Int_t RingType = (fChannelID[iCh]%100000)/10000 ;
        Bool_t LowThr = (fChannelID[iCh]%2==0) ? true:false;
        Double_t Parameter = (LowThr) ? fPar0L[PlaneID][RingType]->Eval(BarID):fPar0H[PlaneID][RingType]->Eval(BarID);
        fTimeSlewingFile<<Parameter<<"     ";
        Parameter = (LowThr) ? fPar1L[PlaneID][RingType]->Eval(BarID):fPar1H[PlaneID][RingType]->Eval(BarID);
        fTimeSlewingFile<<Parameter<<"     ";
        Parameter = (LowThr) ? fPar2L[PlaneID][RingType]->Eval(BarID):fPar2H[PlaneID][RingType]->Eval(BarID);
        fTimeSlewingFile<<Parameter<<"\n";
      }
    }

    fTimeSlewingFile.close();
    for (int iSt=0; iSt<6; iSt++){	
      for (int iLayer=0; iLayer<2; iLayer++){	
        fPar0L[iSt][iLayer]->Write();
        fPar1L[iSt][iLayer]->Write();
        fPar2L[iSt][iLayer]->Write();
      }
    }	
  }

  SaveAllPlots();  
  gErrorIgnoreLevel = -1; // restore the default  
}

void CHANTITimeSlewing::ParseRawDecoderSettingsFile() {
  NA62ConditionsService::GetInstance()->Open(fRawDecoderSettingsFileName);
  TString Line;
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fRawDecoderSettingsFileName))) {
    if (!Line.BeginsWith("ChRemap_")) continue;
    TString chNumber = TString(Line("ChRemap_[0-9]+"))(8, 4);	
    if (chNumber.IsNull()) continue;
    Int_t iCh = chNumber.Atoi();
    TObjArray *l = Line.Tokenize(" ");
    for (Int_t jCh = 0; jCh < 16; jCh++) {
      fChannelID[16*iCh+jCh] = ((TObjString*)(l->At(jCh+1)))->GetString().Atoi();
    }
    delete l;
  }
  NA62ConditionsService::GetInstance()->Close(fRawDecoderSettingsFileName);
}

void CHANTITimeSlewing::DrawPlot(){

}

CHANTITimeSlewing::~CHANTITimeSlewing(){
  delete [] fStationLabels;
  delete [] fLayers;
  delete [] fChLabels;
  delete [] fChannelID;
  for (Int_t j=0; j<fNChannels; j++){
    if (fOldTSCorrection[j]) delete [] fOldTSCorrection[j];
  }
  delete [] fOldTSCorrection;
  for (Int_t j=0; j<fNChannels; j++){
    if (fNewTSCorrection[j]) delete [] fNewTSCorrection[j];
  }
  delete [] fNewTSCorrection;
}
