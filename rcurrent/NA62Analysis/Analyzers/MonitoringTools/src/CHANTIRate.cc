#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "CHANTIRate.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "TTDCBSpecialTrigger.hh"
#include <TLegend.h>
#include <TProfile.h>
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;
using Array3D_D = Array3D<Double_t>;

/// \class CHANTIRate
/// \Brief
/// Check the rate of the CHANTI detector. 
/// \EndBrief
/// \Detailed
/// In this macro the rates of each CHANTI station and channel is evaluated using both the end-of-burst (EOB) counters and the control trigger.
/// Several plots are reported showing the rates (in MHz) and the rates normalized to the argonion counts.
/// Usage:
/// The analyzer should be run in two modes:
/// 1) Read the reconstructed data and produce intermediate output;
/// 2) read its own output (using the --histo command line option) and produce the
/// graphs for the rates (as a root file and a PDF report) 
/// \author Marco Mirra (marco.mirra@cern.ch)
/// \EndDetailed

CHANTIRate::CHANTIRate(Core::BaseAnalysis *ba) : Analyzer(ba, "CHANTIRate")
{
	fReadingData = kTRUE; // reading reconstructed data, by default		 	
	fArgonionCounts = 0;
	RequestL0SpecialTrigger();
	RequestL0Data();
  	RequestBeamSpecialTrigger();
	RequestTree("CHANTI",new TRecoCHANTIEvent);	

	fGeometry = CHANTIGeometry::GetInstance();
  	fStep           = fGeometry->GetTriangleBase()/2;
  	fGlobalShift    = fGeometry->GetSquareLength()/2 - fStep;
	fOutPDFFileName = fAnalyzerName + ".pdf";

	Int_t NSTATION = 6;
	fStationLabels = new TString[NSTATION];
  	fStationLabels[0] = "A";
  	fStationLabels[1] = "B";
	fStationLabels[2] = "C";
  	fStationLabels[3] = "D";
	fStationLabels[4] = "E";
  	fStationLabels[5] = "F";

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


	Int_t NTrigs = 2;
  	fTrigSuffix = new TString[NTrigs];
	fTrigSuffix[0]="";
	fTrigSuffix[1]="EOB";

	fThrType = new TString[2];
	fThrType[0]="Low";
	fThrType[1]="High";

	AddParam("MaxNBursts", &fMaxNBursts, 3000); // max number of bins in histograms
	AddParam("BurstLength", &fBurstLength, 3.5);
	AddParam("NSlots", &fNSlots, 7);		
}

void CHANTIRate::InitOutput(){
}

void CHANTIRate::InitHist(){
	fReadingData = GetIsTree();
	fCHANTIStRateNormArgVSBurstID = new TGraph***[6];
	fCHANTIStRateVSBurstID = new TGraph***[6];
	for (int iSt = 0; iSt < 6; iSt++){
		fCHANTIStRateNormArgVSBurstID[iSt] = new TGraph**[2];
		fCHANTIStRateVSBurstID[iSt] = new TGraph**[2]; 	
		for (int iThr = 0; iThr < 2; iThr++){
			fCHANTIStRateNormArgVSBurstID[iSt][iThr] = new TGraph*[2];
			fCHANTIStRateVSBurstID[iSt][iThr] = new TGraph*[2];
		}
	}
	fArg_BurstID = new TGraph();
	if (fReadingData){
		std::cout << user_normal() << "Reading reconstructed data" << std::endl;	
		fArg_BurstID->SetNameTitle("Arg_BurstID","Arg_BurstID"); 
		BookHisto(fArg_BurstID);		
		BarOccupancyX = new TH2F****[6];	
		BarOccupancyY = new TH2F****[6];		
		Double_t xbins[5];
  		Double_t ybins[4];   
  		xbins[0]=-150;
  		xbins[1]=-47.5;
  		xbins[2]=0;
  		xbins[3]=47.5;
  		xbins[4]=150;   
  		ybins[0]=-150;
  		ybins[1]=-32.5;
  		ybins[2]=32.5;
  		ybins[3]=150;	
		for (int iSt = 0; iSt < 6; iSt++){
			for (int iThr = 0; iThr < 2; iThr++){
				for (int iTrig = 0; iTrig < 2; iTrig++){	
					BookHisto(new TH2F(Form("CHANTI_St%s_Thr%s_Rate%s_VS_Argonion",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()),Form("CHANTI_St%s_Thr%s_Rate%s_VS_Argonion",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()),200,0.,2.5,150,0.,30.));
					fCHANTIStRateVSBurstID[iSt][iThr][iTrig] = new TGraph();
					fCHANTIStRateVSBurstID[iSt][iThr][iTrig]->SetNameTitle(Form("CHANTI_St%s_Thr%s_Rate%s_VS_BurstID",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()),Form("CHANTI_St%s_Thr%s_Rate%s_VS_BurstID",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()));
					BookHisto(fCHANTIStRateVSBurstID[iSt][iThr][iTrig]); 
					fCHANTIStRateNormArgVSBurstID[iSt][iThr][iTrig] = new TGraph();
					fCHANTIStRateNormArgVSBurstID[iSt][iThr][iTrig]->SetNameTitle(Form("CHANTI_St%s_Thr%s_Rate%sNormArg_VS_BurstID",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()),Form("CHANTI_St%s_Thr%s_Rate%sNormArg_VS_BurstID",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()));
					BookHisto(fCHANTIStRateNormArgVSBurstID[iSt][iThr][iTrig]);
					BookHisto(new TH2F(Form("CHANTI_St%s_Xview_Thr%s_Rate%s_VS_ChID",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()),Form("CHANTI_St%s_Xview_Thr%s_Rate%s_VS_ChID",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()),24,-0.5,23.5,200,0.,2.));
					BookHisto(new TH2F(Form("CHANTI_St%s_Xview_Thr%s_Rate%sNormArg_VS_ChID",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()),Form("CHANTI_St%s_Xview_Thr%s_Rate%sNormArg_VS_ChID",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()),24,-0.5,23.5,200,0.,2.));
					BookHisto(new TH2F(Form("CHANTI_St%s_Yview_Thr%s_Rate%s_VS_ChID",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()),Form("CHANTI_St%s_Yview_Thr%s_Rate%s_VS_ChID",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()),24,-0.5,23.5,200,0.,2.));
					BookHisto(new TH2F(Form("CHANTI_St%s_Yview_Thr%s_Rate%sNormArg_VS_ChID",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()),Form("CHANTI_St%s_Yview_Thr%s_Rate%sNormArg_VS_ChID",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()),24,-0.5,23.5,200,0.,2.));
					for (int iCh=1; iCh<=24; iCh++){
						fHisto.GetTH2(Form("CHANTI_St%s_Xview_Thr%s_Rate%s_VS_ChID",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()))->GetXaxis()->SetBinLabel(iCh,fChLabels[iCh-1]);
						fHisto.GetTH2(Form("CHANTI_St%s_Xview_Thr%s_Rate%sNormArg_VS_ChID",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()))->GetXaxis()->SetBinLabel(iCh,fChLabels[iCh-1]);
						fHisto.GetTH2(Form("CHANTI_St%s_Yview_Thr%s_Rate%s_VS_ChID",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()))->GetXaxis()->SetBinLabel(iCh,fChLabels[iCh-1]);
						fHisto.GetTH2(Form("CHANTI_St%s_Yview_Thr%s_Rate%sNormArg_VS_ChID",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()))->GetXaxis()->SetBinLabel(iCh,fChLabels[iCh-1]);
					} 
				}
			}
			BarOccupancyX[iSt] = new TH2F***[2];	
			BarOccupancyY[iSt] = new TH2F***[2];
			for (int iLayer=0; iLayer < 2; iLayer++){	
				BarOccupancyX[iSt][iLayer] = new TH2F**[2];	
				BarOccupancyY[iSt][iLayer] = new TH2F**[2];
				for (int iThr = 0; iThr < 2; iThr++){
					BarOccupancyX[iSt][iLayer][iThr] = new TH2F*[2];	
					BarOccupancyY[iSt][iLayer][iThr] = new TH2F*[2];
					for (int iTrig = 0; iTrig < 2; iTrig++){
						if (iTrig==0){
							if (iLayer==0){
								BarOccupancyX[iSt][iLayer][iThr][iTrig] = new TH2F(Form("BarOccupancyForX%dviewAnd%sTHRInPlane%d%s",iLayer+1,fThrType[iThr].Data(),iSt+1,fTrigSuffix[iTrig].Data()), Form("BarOccupancyForX%dviewAnd%sTHRInPlane%d%s",iLayer+1,fThrType[iThr].Data(),iSt+1,fTrigSuffix[iTrig].Data()),8 ,-132, 132, 3, ybins);
								BookHisto(BarOccupancyX[iSt][iLayer][iThr][iTrig]);
								BarOccupancyY[iSt][iLayer][iThr][iTrig] = new TH2F(Form("BarOccupancyForY%dviewAnd%sTHRInPlane%d%s",iLayer+1,fThrType[iThr].Data(),iSt+1,fTrigSuffix[iTrig].Data()),Form("BarOccupancyForY%dviewAnd%sTHRInPlane%d%s",iLayer+1,fThrType[iThr].Data(),iSt+1,fTrigSuffix[iTrig].Data()), 4, xbins, 9,-148.5, 148.5);
								BookHisto(BarOccupancyY[iSt][iLayer][iThr][iTrig]);
							}else if (iLayer==1){

								BarOccupancyX[iSt][iLayer][iThr][iTrig] = new TH2F(Form("BarOccupancyForX%dviewAnd%sTHRInPlane%d%s",iLayer+1,fThrType[iThr].Data(),iSt+1,fTrigSuffix[iTrig].Data()), Form("BarOccupancyForX%dviewAnd%sTHRInPlane%d%s",iLayer+1,fThrType[iThr].Data(),iSt+1,fTrigSuffix[iTrig].Data()),9 ,-148.5, 148.5, 3, ybins);
								BookHisto(BarOccupancyX[iSt][iLayer][iThr][iTrig]);
								BarOccupancyY[iSt][iLayer][iThr][iTrig] = new TH2F(Form("BarOccupancyForY%dviewAnd%sTHRInPlane%d%s",iLayer+1,fThrType[iThr].Data(),iSt+1,fTrigSuffix[iTrig].Data()),Form("BarOccupancyForY%dviewAnd%sTHRInPlane%d%s",iLayer+1,fThrType[iThr].Data(),iSt+1,fTrigSuffix[iTrig].Data()), 4, xbins, 8 ,-132, 132);
								BookHisto(BarOccupancyY[iSt][iLayer][iThr][iTrig]);
							}
						}else if (iTrig==1){
							BarOccupancyX[iSt][iLayer][iThr][iTrig] = static_cast<TH2F*>(RequestHistogram("CHANTIMonitor",Form("BarOccupancyForX%dviewAnd%sTHRInPlane%d%s",iLayer+1,fThrType[iThr].Data(),iSt+1,fTrigSuffix[iTrig].Data()), false));
							BarOccupancyY[iSt][iLayer][iThr][iTrig] = static_cast<TH2F*>(RequestHistogram("CHANTIMonitor",Form("BarOccupancyForY%dviewAnd%sTHRInPlane%d%s",iLayer+1,fThrType[iThr].Data(),iSt+1,fTrigSuffix[iTrig].Data()), false));
						}
					}	
				}
			}
		}		
	} else {
		std::cout << user_normal() << "Reading my own output" << std::endl;
		fArg_BurstID = reinterpret_cast<TGraph*>(RequestHistogram(fAnalyzerName,"Arg_BurstID",true));
		fCHANTIStXRateVSChID = new TH2F***[6];
		fCHANTIStXRateNormArgVSChID = new TH2F***[6];
		fCHANTIStYRateVSChID = new TH2F***[6];
		fCHANTIStYRateNormArgVSChID = new TH2F***[6];
		fCHANTIStRateVSArgonion = new TH2F***[6];
		for (int iSt = 0; iSt < 6; iSt++){
			fCHANTIStXRateVSChID[iSt] = new TH2F**[2];
			fCHANTIStXRateNormArgVSChID[iSt] = new TH2F**[2];
			fCHANTIStYRateVSChID[iSt] = new TH2F**[2];
			fCHANTIStYRateNormArgVSChID[iSt] = new TH2F**[2];
			fCHANTIStRateVSArgonion[iSt] = new TH2F**[2];	
			for (int iThr = 0; iThr < 2; iThr++){
				fCHANTIStXRateVSChID[iSt][iThr] = new TH2F*[2];
				fCHANTIStXRateNormArgVSChID[iSt][iThr] = new TH2F*[2];
				fCHANTIStYRateVSChID[iSt][iThr] = new TH2F*[2];
				fCHANTIStYRateNormArgVSChID[iSt][iThr] = new TH2F*[2];
				fCHANTIStRateVSArgonion[iSt][iThr] = new TH2F*[2];
				for (int iTrig = 0; iTrig < 2; iTrig++){
					fCHANTIStXRateVSChID[iSt][iThr][iTrig] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,Form("CHANTI_St%s_Xview_Thr%s_Rate%s_VS_ChID",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()),true));
					fCHANTIStXRateNormArgVSChID[iSt][iThr][iTrig] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,Form("CHANTI_St%s_Xview_Thr%s_Rate%sNormArg_VS_ChID",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()),true));
					fCHANTIStYRateVSChID[iSt][iThr][iTrig] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,Form("CHANTI_St%s_Yview_Thr%s_Rate%s_VS_ChID",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()),true));
					fCHANTIStYRateNormArgVSChID[iSt][iThr][iTrig] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,Form("CHANTI_St%s_Yview_Thr%s_Rate%sNormArg_VS_ChID",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()),true));
					fCHANTIStRateVSArgonion[iSt][iThr][iTrig] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,Form("CHANTI_St%s_Thr%s_Rate%s_VS_Argonion",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()),true));
					fCHANTIStRateNormArgVSBurstID[iSt][iThr][iTrig] = reinterpret_cast<TGraph*>(RequestHistogram(fAnalyzerName,Form("CHANTI_St%s_Thr%s_Rate%sNormArg_VS_BurstID",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()),true));
					fCHANTIStRateVSBurstID[iSt][iThr][iTrig] = reinterpret_cast<TGraph*>(RequestHistogram(fAnalyzerName,Form("CHANTI_St%s_Thr%s_Rate%s_VS_BurstID",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()),true));
				}
			}
		}
	}
}

void CHANTIRate::DefineMCSimple(){
}

void CHANTIRate::StartOfRunUser(){
}

void CHANTIRate::StartOfBurstUser(){
	//Reset counter and histograms filled in the processing of the events
	if (!fReadingData) return; // no action if reading its own output in --histo mode 
	fNCtrlTriggerInBurst = 0;
	for (int iSt = 0; iSt < 6; iSt++){
		for (int iLayer=0; iLayer < 2; iLayer++){
			for (int iThr = 0; iThr < 2; iThr++){
				BarOccupancyX[iSt][iLayer][iThr][0]->Reset();				
				BarOccupancyY[iSt][iLayer][iThr][0]->Reset();
			}
		}
	} 
}

void CHANTIRate::ProcessSpecialTriggerUser(int , unsigned int triggerType){
	if(triggerType!=0x23) return; // only EOB
  	fArgonionCounts = GetBeamSpecialTrigger()->GetCountsARGONION()/1.e9;
}

void CHANTIRate::Process(int){
	
	if (!fReadingData) return; // no action if reading its own output in --histo mode 
	
	TRecoCHANTIEvent *CHANTIEvent = GetEvent<TRecoCHANTIEvent>();
	Int_t  L0DataType    = GetL0Data()->GetDataType();
  	Bool_t CTRLTrigger   = L0DataType & 0x10;
  	Bool_t TriggerOK     = CTRLTrigger; //CTRL
  	if (!TriggerOK) return; // process control triggers only	
	double HalfHoleXmin = 47.5;
    	double HalfHoleY1 = 33+fStep;
    	double HalfHoleXmax = 47.5+fStep;
    	double HalfHoleY2max = 2*33;
    	double HalfHoleY2min = 33;
	Int_t nHits = CHANTIEvent->GetNHits();
	TClonesArray & RecoHits = (* (CHANTIEvent->GetHits()));
	for(Int_t iHit = 0; iHit<nHits; iHit++){
      		TRecoCHANTIHit *RecoHit = static_cast<TRecoCHANTIHit*>(RecoHits[iHit]);
      		int iSt = RecoHit->GetPlaneID();
      		int BarID = RecoHit->GetBarID();
      		int ThresholdFlag = RecoHit->GetThresholdFlag();
		int QualityFlag = RecoHit->GetQualityFlag();
      		double X = RecoHit->GetX();
      		double Y = RecoHit->GetY();
		if(ThresholdFlag%2 == 0 && QualityFlag!=5){	
        		if (RecoHit->GetRingType() == kX){ 
          			if (BarID%2!=0){     
            				if (TMath::Abs(X)>HalfHoleXmax){
              					BarOccupancyX[iSt][0][0][0]->Fill(X,-91.25);
              					BarOccupancyX[iSt][0][0][0]->Fill(X,0);
              					BarOccupancyX[iSt][0][0][0]->Fill(X,91.25); 
            				} else {
              					if(RecoHit->GetSideID() == 0) BarOccupancyX[iSt][0][0][0]->Fill(X,91.25);
              					else BarOccupancyX[iSt][0][0][0]->Fill(X,-91.25);
            				}  
          			} else {
            				if (TMath::Abs(X)>HalfHoleXmin){
              					BarOccupancyX[iSt][1][0][0]->Fill(X,-91.25);
              					BarOccupancyX[iSt][1][0][0]->Fill(X,0);
              					BarOccupancyX[iSt][1][0][0]->Fill(X,91.25); 
            				} else {
              					if(RecoHit->GetSideID() == kPositive) BarOccupancyX[iSt][1][0][0]->Fill(X,91.25);
              					else BarOccupancyX[iSt][1][0][0]->Fill(X,-91.25);
            				}
          			}
        		} else {  
          			if (BarID%2==0){     
            				if (TMath::Abs(Y)>HalfHoleY1){   
              					BarOccupancyY[iSt][0][0][0]->Fill(98.75,Y);
              					BarOccupancyY[iSt][0][0][0]->Fill(23.75,Y);
              					BarOccupancyY[iSt][0][0][0]->Fill(-23.75,Y);
              					BarOccupancyY[iSt][0][0][0]->Fill(-98.75,Y); 
            				} else {
              					if(RecoHit->GetSideID() == kPositive) BarOccupancyY[iSt][0][0][0]->Fill(98.75,Y);
              					else BarOccupancyY[iSt][0][0][0]->Fill(-98.75,Y);
            				}  
          			} else {
            				if (TMath::Abs(Y)>HalfHoleY2max){
              					BarOccupancyY[iSt][1][0][0]->Fill(98.75,Y);
              					BarOccupancyY[iSt][1][0][0]->Fill(23.75,Y);
              					BarOccupancyY[iSt][1][0][0]->Fill(-23.75,Y);
              					BarOccupancyY[iSt][1][0][0]->Fill(-98.75,Y); 
            				} else if (TMath::Abs(Y)<HalfHoleY2min){
              					if(RecoHit->GetSideID() == kPositive) BarOccupancyY[iSt][1][0][0]->Fill(98.75,Y);
              					else BarOccupancyY[iSt][1][0][0]->Fill(-98.75,Y); 
            				} else {
              					if(RecoHit->GetSideID() == kPositive){
                					BarOccupancyY[iSt][1][0][0]->Fill(98.75,Y);
                					BarOccupancyY[iSt][1][0][0]->Fill(23.75,Y);
              					} else {
                					BarOccupancyY[iSt][1][0][0]->Fill(-98.75,Y);
                					BarOccupancyY[iSt][1][0][0]->Fill(-23.75,Y);
              					}     
            				}
          			}
        		}          
      		}				
      		if(ThresholdFlag > 0 && QualityFlag!=5){
        		if (RecoHit->GetRingType() == kX){ 
          			if (BarID%2!=0){     
            				if (TMath::Abs(X)>HalfHoleXmax){
              					BarOccupancyX[iSt][0][1][0]->Fill(X,-91.25);
              					BarOccupancyX[iSt][0][1][0]->Fill(X,0);
              					BarOccupancyX[iSt][0][1][0]->Fill(X,91.25); 
            				} else {
              					if(RecoHit->GetSideID() == kPositive) BarOccupancyX[iSt][0][1][0]->Fill(X,91.25);
              					else BarOccupancyX[iSt][0][1][0]->Fill(X,-91.25);
            				}  
          			} else {
            				if (TMath::Abs(X)>HalfHoleXmin){
              					BarOccupancyX[iSt][1][1][0]->Fill(X,-91.25);
              					BarOccupancyX[iSt][1][1][0]->Fill(X,0);
              					BarOccupancyX[iSt][1][1][0]->Fill(X,91.25); 
            				} else {
              					if(RecoHit->GetSideID() == kPositive) BarOccupancyX[iSt][1][1][0]->Fill(X,91.25);
              					else BarOccupancyX[iSt][1][1][0]->Fill(X,-91.25);
            				}
          			}
        		} else {  
          			if (BarID%2==0){     
            				if (TMath::Abs(Y)>HalfHoleY1){
              					BarOccupancyY[iSt][0][1][0]->Fill(98.75,Y);
              					BarOccupancyY[iSt][0][1][0]->Fill(23.75,Y);
              					BarOccupancyY[iSt][0][1][0]->Fill(-23.75,Y);
              					BarOccupancyY[iSt][0][1][0]->Fill(-98.75,Y); 
            				} else {
              					if(RecoHit->GetSideID() == kPositive) BarOccupancyY[iSt][0][1][0]->Fill(98.75,Y);
              					else BarOccupancyY[iSt][0][1][0]->Fill(-98.75,Y);
            				}  
          			} else {
            				if (TMath::Abs(Y)>HalfHoleY2max){
              					BarOccupancyY[iSt][1][1][0]->Fill(98.75,Y);
              					BarOccupancyY[iSt][1][1][0]->Fill(23.75,Y);
              					BarOccupancyY[iSt][1][1][0]->Fill(-23.75,Y);
              					BarOccupancyY[iSt][1][1][0]->Fill(-98.75,Y); 
            				} else if (TMath::Abs(Y)<HalfHoleY2min){
              					if(RecoHit->GetSideID() == kPositive) BarOccupancyY[iSt][1][1][0]->Fill(98.75,Y);
              					else BarOccupancyY[iSt][1][1][0]->Fill(-98.75,Y); 
            				} else {
              					if(RecoHit->GetSideID() == kPositive) {
                					BarOccupancyY[iSt][1][1][0]->Fill(98.75,Y);
                					BarOccupancyY[iSt][1][1][0]->Fill(23.75,Y);
              					} else {
                					BarOccupancyY[iSt][1][1][0]->Fill(-98.75,Y);
                					BarOccupancyY[iSt][1][1][0]->Fill(-23.75,Y);
              					}     
            				}
          			}
        		}
      		}	
	}
	fNCtrlTriggerInBurst++;	
}


void CHANTIRate::PostProcess(){
}

void CHANTIRate::EndOfBurstUser(){	
	if (!fReadingData) return;
	fArg_BurstID->SetPoint(fArg_BurstID->GetN(),GetBurstID(),fArgonionCounts);
	Int_t NBinX_X1 = BarOccupancyX[0][0][0][0]->GetNbinsX();
	Int_t NBinY_X1 = BarOccupancyX[0][0][0][0]->GetNbinsY();
	Int_t NBinX_X2 = BarOccupancyX[0][1][0][0]->GetNbinsX();
	Int_t NBinY_X2 = BarOccupancyX[0][1][0][0]->GetNbinsY();
	Int_t NBinX_Y1 = BarOccupancyY[0][0][0][0]->GetNbinsX();
	Int_t NBinY_Y1 = BarOccupancyY[0][0][0][0]->GetNbinsY();
	Int_t NBinX_Y2 = BarOccupancyY[0][1][0][0]->GetNbinsX();
	Int_t NBinY_Y2 = BarOccupancyY[0][1][0][0]->GetNbinsY();
	
	Array3D_D StationRates = CreateArray3D(6, 2, 2, 0.);
	for (int iSt = 0; iSt < 6; iSt++){
		for (int iThr = 0; iThr < 2; iThr++){
			for (int iTrig = 0; iTrig < 2; iTrig++){
				for (int iLayer = 1; iLayer < 3; iLayer++){
					Int_t NBinX, NBinY;					
					if (iLayer==1){
						NBinX = NBinX_X1; 
						NBinY = NBinY_X1;
					} else if (iLayer==2){
						NBinX = NBinX_X2; 
						NBinY = NBinY_X2;
					}  	
					for (int iBinX = 1; iBinX <= NBinX; iBinX++){
						for (int iBinY = 1; iBinY <= NBinY; iBinY++){
							if (iLayer==1){	
								if ( (iBinX<=2 || iBinX>=7) && (iBinY<3) ) continue;
								if ( (iBinX>2 && iBinX<7) && iBinY==2 ) continue;
							}else if (iLayer==2){
								if ( (iBinX<=3 || iBinX>=7) && (iBinY<3) ) continue;
								if ( (iBinX>3 && iBinX<7) && iBinY==2 ) continue;
							}
							if (BarOccupancyX[iSt][iLayer-1][iThr][iTrig]) {
								Int_t GeneralBin = BarOccupancyX[iSt][iLayer-1][iThr][iTrig]->GetBin(iBinX,iBinY);
								Double_t BarOccupancy = BarOccupancyX[iSt][iLayer-1][iThr][iTrig]->GetBinContent(GeneralBin);
								BarOccupancy = (iTrig==1) ? BarOccupancy/fBurstLength:1.e9*BarOccupancy/(fNSlots*fNCtrlTriggerInBurst*ClockPeriod);
								StationRates[iSt][iThr][iTrig] += BarOccupancy; 
								Int_t ChannelID = -1;
								Double_t XBinCenter = BarOccupancyX[iSt][iLayer-1][iThr][iTrig]->GetXaxis()->GetBinCenter(iBinX);
								ChannelID = round( (XBinCenter + fGlobalShift)/fStep );
								if (iBinY==1) ChannelID=-ChannelID;
								Int_t BinToFill = -1;							
								for (int i=0; i<24; i++){
									if (fChLabels[i].Atoi() == ChannelID){
										BinToFill = i;
										break;
									}
								} 
								FillHisto(Form("CHANTI_St%s_Xview_Thr%s_Rate%s_VS_ChID",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()),BinToFill,BarOccupancy/1.e6);   
								FillHisto(Form("CHANTI_St%s_Xview_Thr%s_Rate%sNormArg_VS_ChID",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()),BinToFill,BarOccupancy/(1.e6*fArgonionCounts));
							}
						}
					}
				}

				for (int iLayer = 1; iLayer < 3; iLayer++){
					Int_t NBinX, NBinY;					
					if (iLayer==1){
						NBinX = NBinX_Y1; 
						NBinY = NBinY_Y1;
					} else if (iLayer==2){
						NBinX = NBinX_Y2; 
						NBinY = NBinY_Y2;
					}  	
					for (int iBinX = 1; iBinX <= NBinX; iBinX++){
						for (int iBinY = 1; iBinY <= NBinY; iBinY++){
							if (iLayer==1){	
								if ( (iBinY<=3 || iBinY>=7) && (iBinX<4) ) continue;
								if ( (iBinY>3 && iBinY<7) && (iBinX==2 || iBinX==3) ) continue;
							}else if (iLayer==2){
								if ( (iBinY<=2 || iBinY>=7) && (iBinX<4) ) continue;
								if ( (iBinY>=3 && iBinY<=6) && (iBinX==2 || iBinX==3) ) continue;
							}
							if (BarOccupancyY[iSt][iLayer-1][iThr][iTrig]) {
								Int_t GeneralBin = BarOccupancyY[iSt][iLayer-1][iThr][iTrig]->GetBin(iBinX,iBinY);
								Double_t BarOccupancy = BarOccupancyY[iSt][iLayer-1][iThr][iTrig]->GetBinContent(GeneralBin);
								BarOccupancy = (iTrig==1) ? BarOccupancy/fBurstLength:1.e9*BarOccupancy/(fNSlots*fNCtrlTriggerInBurst*ClockPeriod);		
								StationRates[iSt][iThr][iTrig] += BarOccupancy; 
								Int_t ChannelID = -1;
								Double_t YBinCenter = BarOccupancyY[iSt][iLayer-1][iThr][iTrig]->GetYaxis()->GetBinCenter(iBinY);
								ChannelID = round( (YBinCenter + fGlobalShift)/fStep );
								if (iBinX==1) ChannelID=-ChannelID; 
								Int_t BinToFill = -1;							
								for (int i=0; i<24; i++){
									if (fChLabels[i].Atoi() == ChannelID){
										BinToFill = i;
										break;
									}
								} 
								FillHisto(Form("CHANTI_St%s_Yview_Thr%s_Rate%s_VS_ChID",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()),BinToFill,BarOccupancy/1.e6);
								FillHisto(Form("CHANTI_St%s_Yview_Thr%s_Rate%sNormArg_VS_ChID",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()),BinToFill,BarOccupancy/(1.e6*fArgonionCounts));
							}
						}
					}
				}
				FillHisto(Form("CHANTI_St%s_Thr%s_Rate%s_VS_Argonion",fStationLabels[iSt].Data(),fThrType[iThr].Data(),fTrigSuffix[iTrig].Data()),fArgonionCounts,StationRates[iSt][iThr][iTrig]/1.e6); 
				fCHANTIStRateVSBurstID[iSt][iThr][iTrig]->SetPoint(fCHANTIStRateVSBurstID[iSt][iThr][iTrig]->GetN(),GetBurstID(),StationRates[iSt][iThr][iTrig]/1.e6);
				fCHANTIStRateNormArgVSBurstID[iSt][iThr][iTrig]->SetPoint(fCHANTIStRateNormArgVSBurstID[iSt][iThr][iTrig]->GetN(),GetBurstID(),StationRates[iSt][iThr][iTrig]/(1.e6*fArgonionCounts));
			}
		}
	}
}

void CHANTIRate::EndOfRunUser(){
	
}

void CHANTIRate::EndOfJobUser(){
	gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed

	/////////////
  	// HISTO mode

  	if (!fReadingData) {

		if (!fCHANTIStXRateVSChID) {
      			std::cout << user_normal() << "Asked to read my own output but cannot found it" << std::endl;
      			return;
    		}
		/////////////////////////
   		// Produce the PDF output		

    		fCanvas = new TCanvas("Canvas");
    		TString DrawOption = "";
    		TLegend* Legend = new TLegend(0.83,0.13,0.95,0.4);
    		Legend->SetFillColor(kWhite);
		for (int iSt = 0; iSt < 6; iSt++){
			if(iSt) DrawOption = "P&same";
			else {
				fCHANTIStRateVSBurstID[iSt][0][1]->GetYaxis()->SetRangeUser(0.,30);	
				DrawOption = "AP";	
			}
			fCHANTIStRateVSBurstID[iSt][0][1]->SetTitle(Form("Stations rate (low thr and EOB) VS burst ID"));    			
			fCHANTIStRateVSBurstID[iSt][0][1]->SetLineColor(iSt+1);
      			fCHANTIStRateVSBurstID[iSt][0][1]->SetMarkerColor(iSt+1);
      			fCHANTIStRateVSBurstID[iSt][0][1]->SetMarkerStyle(20+iSt);
      			fCHANTIStRateVSBurstID[iSt][0][1]->SetMarkerSize(0.8);
      			fCHANTIStRateVSBurstID[iSt][0][1]->Draw(DrawOption);
      			fCHANTIStRateVSBurstID[iSt][0][1]->GetXaxis()->SetTitle("BurstID");
			fCHANTIStRateVSBurstID[iSt][0][1]->GetYaxis()->SetTitle("Station rate (MHz)");
			Legend->AddEntry(fCHANTIStRateVSBurstID[iSt][0][1],Form("Station %s",fStationLabels[iSt].Data()));
		}
		Legend->Draw();
		fCanvas->Print(Form(fOutPDFFileName + "("), "pdf"); // open and print the canvas	
		delete Legend;

		
		fCanvas->Clear();
   	 	Legend = new TLegend(0.83,0.13,0.95,0.4);
    		Legend->SetFillColor(kWhite);
		for (int iSt = 0; iSt < 6; iSt++){
			if(iSt) DrawOption = "P&same";
			else {
				fCHANTIStRateNormArgVSBurstID[iSt][0][1]->GetYaxis()->SetRangeUser(0.,30);		
				DrawOption = "AP";	
			}
			fCHANTIStRateNormArgVSBurstID[iSt][0][1]->SetTitle(Form("Stations rate (low thr and EOB) normalized to the argonion count VS burst ID"));    			        fCHANTIStRateNormArgVSBurstID[iSt][0][1]->SetLineColor(iSt+1);
      			fCHANTIStRateNormArgVSBurstID[iSt][0][1]->SetMarkerColor(iSt+1);
      			fCHANTIStRateNormArgVSBurstID[iSt][0][1]->SetMarkerStyle(20+iSt);
      			fCHANTIStRateNormArgVSBurstID[iSt][0][1]->SetMarkerSize(0.8);
      			fCHANTIStRateNormArgVSBurstID[iSt][0][1]->Draw(DrawOption);
      			fCHANTIStRateNormArgVSBurstID[iSt][0][1]->GetXaxis()->SetTitle("BurstID");
			fCHANTIStRateNormArgVSBurstID[iSt][0][1]->GetYaxis()->SetTitle("Station rate (MHz/Arg count)");
			Legend->AddEntry(fCHANTIStRateNormArgVSBurstID[iSt][0][1],Form("Station %s",fStationLabels[iSt].Data()));
		}
		Legend->Draw();
		fCanvas->Print(fOutPDFFileName, "pdf");
		delete Legend;	

		DrawOption = "colz";

		TProfile* AverageBarRateX[6];
		TProfile* AverageBarRateY[6];	
		for (int iSt = 0; iSt < 6; iSt++){
			fCanvas->Clear();
			fCHANTIStRateVSArgonion[iSt][0][1]->SetStats(0);       			
      			fCHANTIStRateVSArgonion[iSt][0][1]->Draw(DrawOption);
      			fCHANTIStRateVSArgonion[iSt][0][1]->GetXaxis()->SetTitle("Argonion count (1e9 unit)");
			fCHANTIStRateVSArgonion[iSt][0][1]->GetYaxis()->SetTitle("Station rate (MHz)");				
			fCanvas->Print(fOutPDFFileName, "pdf");

			fCanvas->Clear();
			fCHANTIStXRateVSChID[iSt][0][1]->SetStats(0);       			
      			fCHANTIStXRateVSChID[iSt][0][1]->Draw(DrawOption);
      			fCHANTIStXRateVSChID[iSt][0][1]->GetXaxis()->SetTitle("Channel ID");
			fCHANTIStXRateVSChID[iSt][0][1]->GetYaxis()->SetTitle("Bar rate (MHz)");				
			fCanvas->Print(fOutPDFFileName, "pdf");
			AverageBarRateX[iSt] = fCHANTIStXRateVSChID[iSt][0][1]->ProfileX(Form("RateXProfile_St%s",fStationLabels[iSt].Data()),1,-1,"s");

			fCanvas->Clear();
			fCHANTIStYRateVSChID[iSt][0][1]->SetStats(0);       			
      			fCHANTIStYRateVSChID[iSt][0][1]->Draw(DrawOption);
      			fCHANTIStYRateVSChID[iSt][0][1]->GetXaxis()->SetTitle("Channel ID");
			fCHANTIStYRateVSChID[iSt][0][1]->GetYaxis()->SetTitle("Bar rate (MHz)");				
			fCanvas->Print(fOutPDFFileName, "pdf");
			AverageBarRateY[iSt] = fCHANTIStYRateVSChID[iSt][0][1]->ProfileX(Form("RateYProfile_St%s",fStationLabels[iSt].Data()),1,-1,"s");
		}

		fCanvas->Clear();
		DrawOption = "";
   	 	Legend = new TLegend(0.2,0.55,0.32,0.83);
    		Legend->SetFillColor(kWhite);
		for (int iSt = 0; iSt < 6; iSt++){
			if(iSt) DrawOption = "same";
			else {
				AverageBarRateX[iSt]->GetYaxis()->SetRangeUser(0.,2);		
				DrawOption = "";	
				for (int iCh=1; iCh<=24; iCh++){
						AverageBarRateX[iSt]->GetXaxis()->SetBinLabel(iCh,fChLabels[iCh-1]);
				} 
			}
			AverageBarRateX[iSt]->SetTitle(Form("Average bar rate X-layer (low thr and EOB) VS burst ID"));    	
			AverageBarRateX[iSt]->SetLineColor(iSt+1);
      			AverageBarRateX[iSt]->SetMarkerColor(iSt+1);
      			AverageBarRateX[iSt]->SetMarkerStyle(20+iSt);
      			AverageBarRateX[iSt]->SetMarkerSize(0.8);
      			AverageBarRateX[iSt]->Draw(DrawOption);
      			AverageBarRateX[iSt]->GetXaxis()->SetTitle("Channel ID");
			AverageBarRateX[iSt]->GetYaxis()->SetTitle("Average bar rate (MHz)");
			Legend->AddEntry(AverageBarRateX[iSt],Form("Station %s",fStationLabels[iSt].Data()));
		}
		Legend->Draw();
		fCanvas->Print(fOutPDFFileName, "pdf");
		delete Legend;		

		fCanvas->Clear();
   	 	Legend = new TLegend(0.2,0.55,0.32,0.83);
    		Legend->SetFillColor(kWhite);
		for (int iSt = 0; iSt < 6; iSt++){
			if(iSt) DrawOption = "same";
			else {
				AverageBarRateY[iSt]->GetYaxis()->SetRangeUser(0.,2);		
				DrawOption = "";	
				for (int iCh=1; iCh<=24; iCh++){
						AverageBarRateY[iSt]->GetXaxis()->SetBinLabel(iCh,fChLabels[iCh-1]);
				} 
			}
			AverageBarRateY[iSt]->SetTitle(Form("Average bar rate Y-layer (low thr and EOB) VS burst ID"));    	
			AverageBarRateY[iSt]->SetLineColor(iSt+1);
      			AverageBarRateY[iSt]->SetMarkerColor(iSt+1);
      			AverageBarRateY[iSt]->SetMarkerStyle(20+iSt);
      			AverageBarRateY[iSt]->SetMarkerSize(0.8);
      			AverageBarRateY[iSt]->Draw(DrawOption);
      			AverageBarRateY[iSt]->GetXaxis()->SetTitle("Channel ID");
			AverageBarRateY[iSt]->GetYaxis()->SetTitle("Average bar rate (MHz)");
			Legend->AddEntry(AverageBarRateY[iSt],Form("Station %s",fStationLabels[iSt].Data()));
		}
		Legend->Draw();
		fCanvas->Print(Form(fOutPDFFileName + ")"), "pdf"); 
		delete Legend;	
	}
	SaveAllPlots();
	gErrorIgnoreLevel = -1; // restore the default
}

void CHANTIRate::DrawPlot(){

}

CHANTIRate::~CHANTIRate(){
	delete [] fStationLabels;
	delete [] fTrigSuffix;
	delete [] fThrType;
	delete [] fChLabels;
}
