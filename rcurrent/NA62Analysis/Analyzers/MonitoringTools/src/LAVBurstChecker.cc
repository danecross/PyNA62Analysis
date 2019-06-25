#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <TChain.h>
#include "LAVBurstChecker.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include <TTDCBSpecialTrigger.hh>
#include "BaseAnalysis.hh"
#include "ConfigSettings.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class LAVBurstChecker
/// \Brief
/// This analyser evaluate the LAV detector data quality burst by burst, and produce two output .dat files:
/// one with the list of bad burst and one with the list of bad LAV channels for the good bursts.
/// ALL THE OPERATIONS ARE DONE IN THE FIRST STEP OF THE TWO PASS PROCEDURE, reading reconstructed data.
/// \EndBrief
/// \Detailed
/// This Analyser uses the EOB information to evaluate the LAV data quality. 
/// From the LAV Occupancy plots of the EOB counts the analyser finds out the dead and noisy channels. 
/// The data quality decision depends on the number of dead and noisy channels found 
/// and on the Total_LAV_EOB_counts/Argonion_Counts ratio 
/// that, given the beam intensity, has a stable value for each LAV.
/// THE OUTPUT LAVBadChannels FILE FORMAT IS:
/// RunNumber BurstNumber 0/1 (dead channels/noisy channels respectively) channelID (in GeographicMap notation)
/// \EndDetailed

LAVBurstChecker::LAVBurstChecker(Core::BaseAnalysis *ba) : Analyzer(ba, "LAVBurstChecker"){
  fReadingData = kTRUE; // reading reconstructed data, by default
  RequestL0SpecialTrigger();
  RequestBeamSpecialTrigger();
  RequestTree("LAV", new TSpecialTriggerEvent);
  Configuration::ConfigSettings::SetNoSkipBadBurst(true);// do not skip bad bursts
  fArgonionCounts = 0;
  fRunID = -1;
  fBurstID = -1;
  fBurstTime = -1;
}

void LAVBurstChecker::InitOutput(){
}

void LAVBurstChecker::InitHist(){

  fReadingData = GetIsTree();

  BookHisto(new TH1F("LAV_EOBTotCounts", "LAVs EOB Counts", 12, 0.5, 12.5));
  BookHisto(new TH2F("CompactEOBCountsVsLAV", "Counts Per Channel Vs LAV", 512, 0., 512., 12, 1., 13.));
  BookHisto("ArgonionCountsVsBurstID", new TGraph());
  fHisto.GetTGraph("ArgonionCountsVsBurstID")->SetName("ArgonionCountsVsBurstID");

  Int_t TotLAVCh[12] = {320, 320, 320, 320, 320, 480, 480, 480, 480, 480, 480, 512};  
  for(Int_t i=1; i<13; i++){
    BookHisto(new TH1F(Form("EOBChannelOccupancy_LAV%d",i), Form("EOB Occupancy on LAV %d",i), TotLAVCh[i-1],0.,TotLAVCh[i-1]));
    BookHisto(Form("EOBTotCountsVsArgonion_LAV%d",i), new TGraph());  
    fHisto.GetTGraph(Form("EOBTotCountsVsArgonion_LAV%d",i))->SetName(Form("EOBTotCountsVsArgonion_LAV%d",i));
    BookHisto(Form("EOBArgonionRatio_VsBurstT_LAV%d",i), new TGraph());  
    fHisto.GetTGraph(Form("EOBArgonionRatio_VsBurstT_LAV%d",i))->SetName(Form("EOBArgonionRatio_VsBurstT_LAV%d",i));
    BookHisto(Form("EOBArgonionRatio_VsBurstID_LAV%d",i), new TGraph());  
    fHisto.GetTGraph(Form("EOBArgonionRatio_VsBurstID_LAV%d",i))->SetName(Form("EOBArgonionRatio_VsBurstID_LAV%d",i));
  }

}

void LAVBurstChecker::DefineMCSimple(){}

void LAVBurstChecker::StartOfRunUser(){}

void LAVBurstChecker::StartOfBurstUser(){}

void LAVBurstChecker::ProcessSOBEvent(){

  EventHeader *EventHeader = GetEventHeader("SpecialTrigger");
  fBurstTime = EventHeader->GetBurstTime();
  fRunID     = EventHeader->GetRunID();
  fBurstID   = EventHeader->GetBurstID();
  //cout << "Run " << fRunID << " Analyzed Burst " << fBurstID << endl;
  fEOBProcessed = kFALSE;

  fBurstFile.open ("LAVBadBursts.dat", ios::app);
  fChannelFile.open ("LAVBadChannels.dat", ios::app);

}

void LAVBurstChecker::ProcessEOBEvent(){

  //if(!fReadingData) return;
  fEOBProcessed = kTRUE;
  BeamSpecialTrigger *BeamInfo = GetBeamSpecialTrigger();
  TSpecialTriggerEvent *LAVEvent = static_cast<TSpecialTriggerEvent*>(GetEvent("LAV", "SpecialTrigger"));

  Int_t TotLAVHit[12] = {-1};
  Double_t MeanLAVHit[12] = {0};
  vector <Int_t> noisyChannel;
  vector <Int_t> deadChannel;
  Int_t nNoisyCh[12] = {0};
  Int_t nDeadCh[12] = {0};
  Int_t TotLAVCh[12] = {320, 320, 320, 320, 320, 480, 480, 480, 480, 480, 480, 512};

  fArgonionCounts = BeamInfo->GetCountsARGONION();

  Int_t NCHxTDC[12][4];
  for(Int_t lav=1;lav<6;lav++){
    NCHxTDC[lav-1][0] = 128;
    NCHxTDC[lav-1][1] = 128;
    NCHxTDC[lav-1][2] = 64;
    NCHxTDC[lav-1][3] = 0;
  }
  for(Int_t lav=6;lav<9;lav++){
    NCHxTDC[lav-1][0] = 128;
    NCHxTDC[lav-1][1] = 128;
    NCHxTDC[lav-1][2] = 128;
    NCHxTDC[lav-1][3] = 96;
  }
  for(Int_t lav=9;lav<12;lav++){
    NCHxTDC[lav-1][0] = 120;
    NCHxTDC[lav-1][1] = 120;
    NCHxTDC[lav-1][2] = 120;
    NCHxTDC[lav-1][3] = 120;
  }
  NCHxTDC[11][0] = 128;
  NCHxTDC[11][1] = 128;
  NCHxTDC[11][2] = 128;
  NCHxTDC[11][3] = 128;
  Int_t GeoCh[12][512];
  for(Int_t j=0;j<12;j++){
    for(Int_t i=0;i<512;i++){
      GeoCh[j][i] = -1;
    }
  }

  if(fArgonionCounts<100000) {
    fBurstFile << fRunID << " " << fBurstID << " ARGONION = " << fArgonionCounts <<"\n";
  } else {
      
    fHisto.GetTGraph("ArgonionCountsVsBurstID")->Set(fHisto.GetTGraph("ArgonionCountsVsBurstID")->GetN()+1);
    fHisto.GetTGraph("ArgonionCountsVsBurstID")->SetPoint(fHisto.GetTGraph("ArgonionCountsVsBurstID")->GetN()-1,fBurstID,fArgonionCounts);
    
    for(Int_t i=LAVEvent->GetNHits()-1; i>=0; i--){
      TTDCBSpecialTrigger * SpecTrig = reinterpret_cast<TTDCBSpecialTrigger *>(LAVEvent->GetHit(i));
      PrimCounter* ChannelCounts = SpecTrig->GetCounter("CHANNEL_COUNT_L");
      if(ChannelCounts){
	Int_t TEL62ID = SpecTrig->GetROBoardID();
	if(TEL62ID < 0 || TEL62ID > 11) continue;
	Int_t FPGAID  = SpecTrig->GetFPGAID();
	if(FPGAID < 0 || FPGAID > 3) {
	  cout << user_normal() << "*** WARNING: FPGA ID IN TTDCBSpecialTrigger HIT = " << FPGAID << endl;
	  continue;
	}
	std::vector<Int_t>  ChIDs  = ChannelCounts->GetChannelIDs();
	std::vector<UInt_t> Values = ChannelCounts->GetValues();
	for(Int_t iCh=0;iCh<NCHxTDC[TEL62ID][FPGAID];iCh++){	    
	  Int_t ChannelInPlot = iCh;
	  for(Int_t tdc=0;tdc<FPGAID;tdc++){
	    ChannelInPlot += NCHxTDC[TEL62ID][tdc];
	  }
	  fHisto.GetTH1(Form("EOBChannelOccupancy_LAV%d",TEL62ID+1))->SetBinContent(ChannelInPlot+1, Values[iCh]);
	  fHisto.GetTH2("CompactEOBCountsVsLAV")->SetBinContent(iCh+FPGAID*128+1, TEL62ID+1, Values[iCh]);
	  GeoCh[TEL62ID][ChannelInPlot] = ChIDs[iCh];
	  TotLAVHit[TEL62ID] += Values[iCh];
	}
      }
    }

    Int_t Min = -1, Max = -1;
    Int_t CountsXCh;
    Int_t FullyDeadLAV[12] = {0,0,0,0,0,0,0,0,0,0,0,0};
    
    for(Int_t lav=0; lav<12; lav++){
      
      if(TotLAVHit[lav] == 0){
	FullyDeadLAV[lav] = 1;
	continue;
      }
      //Mean Counts. Maximum and minimum count removed
      Min = fHisto.GetTH1(Form("EOBChannelOccupancy_LAV%d",lav+1))->GetBinContent(1);
      Max = fHisto.GetTH1(Form("EOBChannelOccupancy_LAV%d",lav+1))->GetBinContent(1);
      for(Int_t n=1; n<TotLAVCh[lav]+1; n++){
	CountsXCh = fHisto.GetTH1(Form("EOBChannelOccupancy_LAV%d",lav+1))->GetBinContent(n+1);
	if(CountsXCh<Min) Min = CountsXCh;
	if(CountsXCh>Max) Max = CountsXCh;
      }
      MeanLAVHit[lav] = (TotLAVHit[lav]-Min-Max)/((double)TotLAVCh[lav]-2);
      //Noisy Channels subtraction
      for(Int_t n=0; n<TotLAVCh[lav]; n++){
	CountsXCh = fHisto.GetTH1(Form("EOBChannelOccupancy_LAV%d",lav+1))->GetBinContent(n+1);
	if(CountsXCh == 0){
	  nDeadCh[lav]++;
	  deadChannel.push_back(GeoCh[lav][n]);
	} 
	if(CountsXCh > (MeanLAVHit[lav]*10 +1)){
	  noisyChannel.push_back(GeoCh[lav][n]);
	  nNoisyCh[lav]++;
	  TotLAVHit[lav] -= CountsXCh;
	}
      }
      //Filling histograms
      fHisto.GetTH1("LAV_EOBTotCounts")->SetBinContent(lav+1,TotLAVHit[lav]);
      fHisto.GetTGraph(Form("EOBTotCountsVsArgonion_LAV%d",lav+1))
	->Set(fHisto.GetTGraph(Form("EOBTotCountsVsArgonion_LAV%d", lav+1))->GetN()+1);  
      fHisto.GetTGraph(Form("EOBTotCountsVsArgonion_LAV%d", lav+1))
	->SetPoint(fHisto.GetTGraph(Form("EOBTotCountsVsArgonion_LAV%d", lav+1))->GetN()-1,TotLAVHit[lav],fArgonionCounts);
      fHisto.GetTGraph(Form("EOBArgonionRatio_VsBurstT_LAV%d",lav+1))
	->Set(fHisto.GetTGraph(Form("EOBArgonionRatio_VsBurstT_LAV%d", lav+1))->GetN()+1);  
      fHisto.GetTGraph(Form("EOBArgonionRatio_VsBurstT_LAV%d", lav+1))
	  ->SetPoint(fHisto.GetTGraph(Form("EOBArgonionRatio_VsBurstT_LAV%d",lav+1))->GetN()-1,fBurstTime,TotLAVHit[lav]/fArgonionCounts);
      fHisto.GetTGraph(Form("EOBArgonionRatio_VsBurstID_LAV%d",lav+1))
	->Set(fHisto.GetTGraph(Form("EOBArgonionRatio_VsBurstID_LAV%d", lav+1))->GetN()+1);  
      fHisto.GetTGraph(Form("EOBArgonionRatio_VsBurstID_LAV%d", lav+1))
	->SetPoint(fHisto.GetTGraph(Form("EOBArgonionRatio_VsBurstID_LAV%d",lav+1))->GetN()-1,fBurstID,TotLAVHit[lav]/fArgonionCounts);
    }

    vector <Int_t> DeadLAVs;
    vector <Int_t> NoisyLAVs;
    vector <Int_t> WrongRateLAVs;

    Double_t ExpEOBArgRatio[12] = {0.118, 0.080, 0.060, 0.054, 0.051, 0.046, 0.045, 0.042, 0.027, 0.026, 0.034, 0.067};
    Double_t EOBArgonionRatio[12];
    for(Int_t i=0;i<12;i++){
      EOBArgonionRatio[i] = TotLAVHit[i]/fArgonionCounts;
    }
    
    for(Int_t lav=0;lav<12;lav++){
      if(FullyDeadLAV[lav] == 1){
	DeadLAVs.push_back(lav+1);
	continue;
      }
      if(nNoisyCh[lav] > 15) NoisyLAVs.push_back(lav+1);
      if(nDeadCh[lav] > 15) DeadLAVs.push_back(lav+1);
      if(EOBArgonionRatio[lav] > ExpEOBArgRatio[lav]*1.5 || EOBArgonionRatio[lav] < ExpEOBArgRatio[lav]*0.5){
	WrongRateLAVs.push_back(lav+1);
      }
    }

    if(NoisyLAVs.size()>0 || DeadLAVs.size()>0 || WrongRateLAVs.size()>0){
      fBurstFile << fRunID << " " << fBurstID;
      for(Int_t i=0;i<(Int_t)NoisyLAVs.size();i++){
	if(i==0)fBurstFile << " NOISY LAV: ";
	fBurstFile << NoisyLAVs.at(i) << " "; 
      }
      for(Int_t i=0;i<(Int_t)DeadLAVs.size();i++){
	if(i==0)fBurstFile << " DEAD LAV: ";
	fBurstFile << DeadLAVs.at(i) << " "; 
      }
      for(Int_t i=0;i<(Int_t)WrongRateLAVs.size();i++){
	if(i==0)fBurstFile << " WRONG RATE LAV: ";
	fBurstFile << WrongRateLAVs.at(i) << " "; 
      }
      fBurstFile << "\n";
      
    } else {

      fChannelFile << fRunID << " " << fBurstID << " 1 ";
      for(Int_t i=0; i<(Int_t)noisyChannel.size();i++){
	fChannelFile << noisyChannel.at(i) << " ";
      }
      fChannelFile << "\n"<< fRunID << " " << fBurstID << " 0 ";
      for(Int_t i=0; i<(Int_t)deadChannel.size();i++){
	fChannelFile << deadChannel.at(i) << " ";
      }
      fChannelFile << "\n";
      
    }

    DeadLAVs.clear();
    NoisyLAVs.clear();
    WrongRateLAVs.clear();

  }

}

void LAVBurstChecker::Process(Int_t){}

void LAVBurstChecker::PostProcess(){}

void LAVBurstChecker::EndOfBurstUser(){

  if(!fEOBProcessed){
    fBurstFile << fRunID << " " << fBurstID << " NO EOB \n";
  }
  fBurstFile.close();
  fChannelFile.close();

}

void LAVBurstChecker::EndOfRunUser(){}

void LAVBurstChecker::EndOfJobUser(){
  SaveAllPlots();
}

void LAVBurstChecker::DrawPlot(){}

LAVBurstChecker::~LAVBurstChecker(){}
