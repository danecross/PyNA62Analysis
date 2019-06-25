#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "MUV12SwapCheck.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "DownstreamTrack.hh"
#include "GeometricAcceptance.hh"
#include "TMath.h"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;
using namespace TMath;

/// \class MUV12SwapCheck
/// \Brief
/// Analyzer to check MUV1&2 CREAM swap
/// \EndBrief
///
/// \Detailed
/// "Good" tracks matching in CHOD, NewCHOD and LKr (with less than 1GeV energy deposit) are extrapolated to MUV1,2.
/// It searches hits in the channels corresponding to the geometric extrapolation of the track, for both standard and swapped case
/// and fills histograms if the found configuration is compatible with only one of the hypotheses.
/// In case only one half of the CREAM slot is populated a check on the efficiency to be >50% is performed.\n
/// At the end of each burst on histo Burst ID vs Slot ID is filled with the response for each slot.\n
/// When running in histo-mode a text file named RunXXXX_MUV12DetectedCREAMSwaps.txt is created and filled if any swap is found.
///
/// \EndDetailed

MUV12SwapCheck::MUV12SwapCheck(Core::BaseAnalysis *ba) : Analyzer(ba, "MUV12SwapCheck")
{

	fMUV12Corr = nullptr;

}

void MUV12SwapCheck::InitOutput(){

}

void MUV12SwapCheck::InitHist(){

	if (GetIsTree()){
		gFile->mkdir(TString(fAnalyzerName).Append("/Bursts"));
		fBurstFileDir = gFile->GetDirectory(TString(fAnalyzerName).Append("/Bursts"),true);

		BookHisto(new TH2F("MUV1CorrectChanneldT","; dT [ns]; Slot ID",100,-50,50,31,1,32),0,"Matching");
		BookHisto(new TH2F("MUV2CorrectChanneldT","; dT [ns]; Slot ID",100,-50,50,31,1,32),0,"Matching");

		BookHisto(new TH2F("MUV1SwapChanneldT","; dT [ns]; Slot ID",100,-50,50,31,1,32),0,"Matching");
		BookHisto(new TH2F("MUV2SwapChanneldT","; dT [ns]; Slot ID",100,-50,50,31,1,32),0,"Matching");

		BookHisto(new TH2F("CREAM_SLOTS_SWAP","Swap check; CREAM Slot; Swapped?",31,1,32,2,0,2));
		BookHisto(new TH2F("CREAM_SLOT_SWAPPED","; Burst ID; Slot",3000,0,3000,31,1,32));
		BookHisto(new TH2F("CREAM_SLOT_SWAPPED_TIME","; Burst ID; Slot",3000,0,3000,31,1,32));

		BookHisto(new TH1F("MUV1ChannelFound","",200,100,300),0,"Efficiencies");
		BookHisto(new TH1F("MUV1ChannelLooked","",200,100,300),0,"Efficiencies");
		BookHisto(new TH1F("SlotFound","",31,1,32),0,"Efficiencies");
		BookHisto(new TH1F("SlotLooked","",31,1,32),0,"Efficiencies");

		BookHisto(new TH1F("MUV2ChannelFound","",200,100,300),0,"Efficiencies");
		BookHisto(new TH1F("MUV2ChannelLooked","",200,100,300),0,"Efficiencies");

		for (int i=1; i<=31; i++){
			BookHisto(Form("EfficiencySlot%d",i), new TGraph(),0,"SlotEfficiencies");
			TGraph *e = fHisto.GetTGraph(Form("EfficiencySlot%d",i));
			e->SetName(Form("EfficiencySlot%d",i));
			e->SetTitle("; Burst ID; Efficiency");
		}

	}
	else if (GetIsHisto()){
		RequestHistogram("MUV12SwapCheck/","CREAM_SLOT_SWAPPED",1);
		RequestHistogram("MUV12SwapCheck/","CREAM_SLOT_SWAPPED_TIME",1);
		RequestHistogram("MUV12SwapCheck/Efficiencies/","MUV1ChannelFound",1);
		RequestHistogram("MUV12SwapCheck/Efficiencies/","MUV2ChannelFound",1);
		RequestHistogram("MUV12SwapCheck/Efficiencies/","MUV1ChannelLooked",1);
		RequestHistogram("MUV12SwapCheck/Efficiencies/","MUV2ChannelLooked",1);
		for (int i=1; i<=31; i++)
			RequestHistogram("MUV12SwapCheck/SlotEfficiencies",Form("EfficiencySlot%d",i),1);
	}
}

void MUV12SwapCheck::DefineMCSimple(){}

void MUV12SwapCheck::StartOfRunUser(){}

void MUV12SwapCheck::StartOfBurstUser(){

	if (!GetIsTree()) return;
	if (!fMUV12Corr) fMUV12Corr = MUV12Corrections::GetInstance(GetRunID(),GetRevision(),GetWithMC());

	fHisto.GetTH2("CREAM_SLOTS_SWAP")->Reset("ICESM");
	fHisto.GetTH1("SlotFound")->Reset("ICESM");
	fHisto.GetTH1("SlotLooked")->Reset("ICESM");
	fHisto.GetTH2("MUV1CorrectChanneldT")->Reset("ICESM");
	fHisto.GetTH2("MUV2CorrectChanneldT")->Reset("ICESM");
	fHisto.GetTH2("MUV1SwapChanneldT")->Reset("ICESM");
	fHisto.GetTH2("MUV2SwapChanneldT")->Reset("ICESM");
	fHisto.GetTH1("MUV1ChannelFound")->Reset("ICESM");
	fHisto.GetTH1("MUV2ChannelFound")->Reset("ICESM");
	fHisto.GetTH1("MUV1ChannelLooked")->Reset("ICESM");
	fHisto.GetTH1("MUV2ChannelLooked")->Reset("ICESM");

}

void MUV12SwapCheck::ProcessSpecialTriggerUser(int , unsigned int ){

}

void MUV12SwapCheck::Process(int ){

	if (!GetIsTree()) return;

	const vector<DownstreamTrack> Tracks = *GetOutput<vector<DownstreamTrack>>("DownstreamTrackBuilder.Output");

	TRecoMUV1Event *MUV1Ev = GetEvent<TRecoMUV1Event>();
	TRecoMUV2Event *MUV2Ev = GetEvent<TRecoMUV2Event>();

	for (auto track : Tracks ){
		if (track.GetChi2()>30) continue;
		if (!track.CHODAssociationExists() || !track.NewCHODAssociationExists()) continue;
		if (!track.MUV3AssociationExists()) continue;
		if (Abs(track.GetMomentum() - track.GetMomentumBeforeFit())>5e+3) continue;
		if (!track.LKrAssociationExists()) continue;

		if (track.GetLKrEnergy()>1.e+3 || track.GetLKrEnergy()<200) continue;

		double TrackTime = track.GetCHODTime();

		double XM1 = track.xAt(GeometricAcceptance::GetInstance()->GetZMUV1());
		double YM1 = track.yAt(GeometricAcceptance::GetInstance()->GetZMUV1());
		double XM2 = track.xAt(GeometricAcceptance::GetInstance()->GetZMUV2());
		double YM2 = track.yAt(GeometricAcceptance::GetInstance()->GetZMUV2());

		pair<int,int> M1Channels = fMUV12Corr->GetMUV1Channels(XM1,YM1);
		pair<int,int> M2Channels = fMUV12Corr->GetMUV2Channels(XM2,YM2);

		pair<int,int> M1CREAMChannelX = fMUV12Corr->GetMUV1CREAMID(M1Channels.first);
		pair<int,int> M1CREAMChannelY = fMUV12Corr->GetMUV1CREAMID(M1Channels.second);

		const pair<int,int> M2CREAMChannelX = fMUV12Corr->GetMUV2CREAMID(M1Channels.first);
		const pair<int,int> M2CREAMChannelY = fMUV12Corr->GetMUV2CREAMID(M1Channels.second);

		pair<int,int> M1SwapChannels = make_pair(fMUV12Corr->GetMUV1ChannelID(M1CREAMChannelX.first, (M1CREAMChannelX.second+16)%32),fMUV12Corr->GetMUV1ChannelID(M1CREAMChannelY.first, (M1CREAMChannelY.second+16)%32));
		pair<int,int> M2SwapChannels = make_pair(fMUV12Corr->GetMUV2ChannelID(M2CREAMChannelX.first, (M2CREAMChannelX.second+16)%32),fMUV12Corr->GetMUV2ChannelID(M2CREAMChannelY.first, (M2CREAMChannelY.second+16)%32));

		bool FoundM1[2] = {false}, FoundM1Swap[2] = {false},FoundM2[2] = {false}, FoundM2Swap[2] = {false};

		for (int i=0; i<MUV1Ev->GetNHits(); i++){
			TRecoMUV1Hit *Hit = static_cast<TRecoMUV1Hit*>(MUV1Ev->GetHit(i));
			int ChID = Hit->GetChannelID();
			double Time = Hit->GetTime();
			int match = 0;

			if (ChID==M1Channels.first) {
					match = 1;
					Time += fMUV12Corr->GetMUV1TimeCorrection(ChID, YM1);
			}
			else if (ChID==M1Channels.second){
					match = 2;
					Time += fMUV12Corr->GetMUV1TimeCorrection(ChID, XM1);
			}
			else if (ChID == M1SwapChannels.first){
					match = 3;
					Time += fMUV12Corr->GetMUV1TimeCorrection(M1Channels.first, YM1);
			}
			else if (ChID == M1SwapChannels.second){
					match = 4;
					Time += fMUV12Corr->GetMUV1TimeCorrection(M1Channels.second, XM1);
			}
			else 	continue;

			if (match<=0) continue;

			int Slot = ( (match%2) ? M1CREAMChannelX.first : M1CREAMChannelY.first);
			if (match<3) FillHisto("MUV1CorrectChanneldT",Time - TrackTime,Slot);
			else{
				FillHisto("MUV1SwapChanneldT",Time - TrackTime,Slot);
			}

			if (Abs(Time - TrackTime)>10) continue;

			if (match<3) FoundM1[1-match%2] = true;
			else FoundM1Swap[1-match%2-1] = true;


		}

		FillHisto("MUV1ChannelLooked",M1Channels.first);
		if (FoundM1[0]) FillHisto("MUV1ChannelFound",M1Channels.first);

		FillHisto("MUV1ChannelLooked",M1Channels.second);
		if (FoundM1[1]) FillHisto("MUV1ChannelFound",M1Channels.second);

		FillHisto("SlotLooked",M1CREAMChannelX.first);
		if (FoundM1[0])	FillHisto("SlotFound",M1CREAMChannelX.first);

		FillHisto("SlotLooked",M1CREAMChannelY.first);
		if (FoundM1[1])	FillHisto("SlotFound",M1CREAMChannelY.first);

		if (FoundM1[0] && !FoundM1Swap[0]){
			FillHisto("CREAM_SLOTS_SWAP",M1CREAMChannelX.first,0);
		}
		else if (!FoundM1[0] && FoundM1Swap[0]){

			FillHisto("CREAM_SLOTS_SWAP",M1CREAMChannelX.first,1);
		}
		else if (!FoundM1[0] && fMUV12Corr->IsMUV1CREAMHalfEmpty(M1CREAMChannelX.first,1-static_cast<int>(M1CREAMChannelX.second/16.))){

			FillHisto("CREAM_SLOTS_SWAP",M1CREAMChannelX.first,1);

		}

		if (FoundM1[1] && !FoundM1Swap[1]){

			FillHisto("CREAM_SLOTS_SWAP",M1CREAMChannelY.first,0);
		}
		else if (!FoundM1[1] && FoundM1Swap[1]){

			FillHisto("CREAM_SLOTS_SWAP",M1CREAMChannelY.first,1);
		}
		else if (!FoundM1[1] && fMUV12Corr->IsMUV1CREAMHalfEmpty(M1CREAMChannelY.first,1-static_cast<int>(M1CREAMChannelY.second/16.))){

			FillHisto("CREAM_SLOTS_SWAP",M1CREAMChannelY.first,1);

		}

		for (int i=0; i<MUV2Ev->GetNHits(); i++){
			TRecoMUV2Hit *Hit = static_cast<TRecoMUV2Hit*>(MUV2Ev->GetHit(i));
			int ChID = Hit->GetChannelID();
			double Time = Hit->GetTime();
			int match = 0;

			if (ChID==M2Channels.first) {
				match = 1;
				Time += fMUV12Corr->GetMUV2TimeCorrection(ChID, YM2);
			}
			else if (ChID==M2Channels.second){
				match = 2;
				Time += fMUV12Corr->GetMUV2TimeCorrection(ChID, XM2);
			}
			else if (ChID == M2SwapChannels.first){
				match = 3;
				Time += fMUV12Corr->GetMUV2TimeCorrection(M2Channels.first, YM2);
			}
			else if (ChID == M2SwapChannels.second){
				match = 4;
				Time += fMUV12Corr->GetMUV2TimeCorrection(M2Channels.second, XM2);
			}
			else 	continue;

			if (match<=0) continue;
			int Slot = ( (match%2) ? M2CREAMChannelX.first : M2CREAMChannelY.first);

			if (match<3) FillHisto("MUV2CorrectChanneldT",Time - TrackTime,Slot);
			else FillHisto("MUV2SwapChanneldT",Time - TrackTime,Slot);

			if (Abs(Time - TrackTime)>10) continue;

			if (match<3) FoundM2[1-match%2] = true;
			else FoundM2Swap[1-match%2-1] = true;


		}

		FillHisto("MUV2ChannelLooked",M2Channels.first);
		if (FoundM2[0]) FillHisto("MUV2ChannelFound",M2Channels.first);

		FillHisto("MUV2ChannelLooked",M2Channels.second);
		if (FoundM2[1]) FillHisto("MUV2ChannelFound",M2Channels.second);

		FillHisto("SlotLooked",M2CREAMChannelX.first);
		if (FoundM2[0])	FillHisto("SlotFound",M2CREAMChannelX.first);

		FillHisto("SlotLooked",M2CREAMChannelY.first);
		if (FoundM2[1])	FillHisto("SlotFound",M2CREAMChannelY.first);


		if (FoundM2[0] && !FoundM2Swap[0]){
			FillHisto("CREAM_SLOTS_SWAP",M2CREAMChannelX.first,0);
		}
		else if (!FoundM2[0] && FoundM2Swap[0]){

			FillHisto("CREAM_SLOTS_SWAP",M2CREAMChannelX.first,1);
		}
		else if (!FoundM2[0] && fMUV12Corr->IsMUV2CREAMHalfEmpty(M2CREAMChannelX.first,1-static_cast<int>(M2CREAMChannelX.second/16.))){

			FillHisto("CREAM_SLOTS_SWAP",M2CREAMChannelX.first,1);
		}

		if (FoundM2[1] && !FoundM2Swap[1]){

			FillHisto("CREAM_SLOTS_SWAP",M2CREAMChannelY.first,0);
		}
		else if (!FoundM2[1] && FoundM2Swap[1]){

			FillHisto("CREAM_SLOTS_SWAP",M2CREAMChannelY.first,1);
		}
		else if (!FoundM2[1] && fMUV12Corr->IsMUV2CREAMHalfEmpty(M2CREAMChannelY.first,1-static_cast<int>(M2CREAMChannelY.second/16.))){

			FillHisto("CREAM_SLOTS_SWAP",M2CREAMChannelY.first,1);
		}

	}




}

void MUV12SwapCheck::PostProcess(){

}

void MUV12SwapCheck::EndOfBurstUser(){

	if (!GetIsTree()) return;
	fBurstFileDir->cd();

	TH1F *MUV1ChannelEfficiency = new TH1F(*((TH1F*)fHisto.GetTH1("MUV1ChannelFound")));
	MUV1ChannelEfficiency->SetName(Form("MUV2ChannelEfficiency_Burst%d",GetBurstID()));

	MUV1ChannelEfficiency->SetDirectory(fBurstFileDir);
	MUV1ChannelEfficiency->Write();
	MUV1ChannelEfficiency->Delete();
	MUV1ChannelEfficiency = nullptr;

	TH1F *MUV2ChannelEfficiency = new TH1F(*((TH1F*)fHisto.GetTH1("MUV2ChannelFound")));
	MUV2ChannelEfficiency->SetName(Form("MUV2ChannelEfficiency_Burst%d",GetBurstID()));
	MUV2ChannelEfficiency->Divide(fHisto.GetTH1("MUV2ChannelLooked"));

	MUV2ChannelEfficiency->SetDirectory(fBurstFileDir);
	MUV2ChannelEfficiency->Write();
	MUV2ChannelEfficiency->Delete();
	MUV2ChannelEfficiency = nullptr;


	TH2 *h = fHisto.GetTH2("CREAM_SLOTS_SWAP");
	TH2 *hfinal = fHisto.GetTH2("CREAM_SLOT_SWAPPED_TIME");

	TH2 *hM12DTimeCorr = fHisto.GetTH2("MUV1CorrectChanneldT");
	TH2 *hM12DTimeSwap = fHisto.GetTH2("MUV1SwapChanneldT");
	TH2 *hM22DTimeCorr = fHisto.GetTH2("MUV2CorrectChanneldT");
	TH2 *hM22DTimeSwap = fHisto.GetTH2("MUV2SwapChanneldT");
	/*
	hM12DTimeCorr->SetDirectory(fBurstFileDir);
	hM12DTimeSwap->SetDirectory(fBurstFileDir);
	hM22DTimeCorr->SetDirectory(fBurstFileDir);
	hM22DTimeSwap->SetDirectory(fBurstFileDir);
*/
	hM12DTimeCorr->Write(Form("MUV1CorrectChanneldT_%d",GetBurstID()));
	hM12DTimeSwap->Write(Form("MUV1SwapChanneldT_%d",GetBurstID()));
	hM22DTimeCorr->Write(Form("MUV2CorrectChanneldT_%d",GetBurstID()));
	hM22DTimeSwap->Write(Form("MUV2SwapChanneldT_%d",GetBurstID()));

	TH1 *hnum = fHisto.GetTH1("SlotFound");
	TH1 *hdeno = fHisto.GetTH1("SlotLooked");
	for (int i=1; i<=31; i++){
		float Swap = h->GetBinContent(h->FindBin(i, 1));
		float NoSwap = h->GetBinContent(h->FindBin(i, 0));
		if (Swap+NoSwap<20) continue;


		if (Swap>2*NoSwap) FillHisto("CREAM_SLOT_SWAPPED",GetBurstID(),i,2);
		else FillHisto("CREAM_SLOT_SWAPPED",GetBurstID(),i,1);

		if (hdeno->GetBinContent(i))
			FillHisto(Form("EfficiencySlot%d",i),GetBurstID(),hnum->GetBinContent(i)/hdeno->GetBinContent(i));


		TH1D *hM1TimeCorr = hM12DTimeCorr->ProjectionX(Form("CorrSlot%d",i),i,i);
		TH1D *hM1TimeSwap = hM12DTimeSwap->ProjectionX(Form("SwapSlot%d",i),i,i);

		if (hM1TimeCorr->Integral()>100 || hM1TimeSwap->Integral()>100){

			int MaxBinCorr = hM1TimeCorr->GetMaximumBin();
			int MaxBinSwap = hM1TimeSwap->GetMaximumBin();

			double bkg_corr = (hM1TimeCorr->Integral(MaxBinCorr-20,MaxBinSwap-10) + hM1TimeCorr->Integral(MaxBinCorr+10,MaxBinCorr+20))/22.;
			double bkg_swap = (hM1TimeSwap->Integral(MaxBinSwap-20,MaxBinSwap-10) + hM1TimeSwap->Integral(MaxBinSwap+10,MaxBinSwap+20))/22.;

			double TotalCorr = hM1TimeCorr->Integral(MaxBinCorr-3,MaxBinCorr+3) - 7.*bkg_corr;
			double TotalSwap = hM1TimeSwap->Integral(MaxBinSwap-3,MaxBinSwap+3) - 7.*bkg_swap;

			if (TotalCorr > TotalSwap) hfinal->SetBinContent(hfinal->FindBin(GetBurstID(),i),1);
			else hfinal->SetBinContent(hfinal->FindBin(GetBurstID(),i),2);

		}

		hM1TimeCorr->Delete();
		hM1TimeCorr = nullptr;
		hM1TimeSwap->Delete();
		hM1TimeSwap = nullptr;

		TH1D *hM2TimeCorr = hM22DTimeCorr->ProjectionX(Form("CorrSlot%d",i),i,i);
		TH1D *hM2TimeSwap = hM22DTimeSwap->ProjectionX(Form("SwapSlot%d",i),i,i);

		if (hM2TimeCorr->Integral()>100 || hM2TimeSwap->Integral()>100){

			int MaxBinCorr = hM2TimeCorr->GetMaximumBin();
			int MaxBinSwap = hM2TimeSwap->GetMaximumBin();

			double bkg_corr = (hM2TimeCorr->Integral(MaxBinCorr-20,MaxBinSwap-10) + hM2TimeCorr->Integral(MaxBinCorr+10,MaxBinCorr+20))/22.;
			double bkg_swap = (hM2TimeSwap->Integral(MaxBinSwap-20,MaxBinSwap-10) + hM2TimeSwap->Integral(MaxBinSwap+10,MaxBinSwap+20))/22.;

			double TotalCorr = hM2TimeCorr->Integral(MaxBinCorr-3,MaxBinCorr+3) - 7.*bkg_corr;
			double TotalSwap = hM2TimeSwap->Integral(MaxBinSwap-3,MaxBinSwap+3) - 7.*bkg_swap;

			if (TotalCorr>0 && TotalCorr > TotalSwap) hfinal->SetBinContent(hfinal->FindBin(GetBurstID(),i),1);
			else if (TotalSwap>0) hfinal->SetBinContent(hfinal->FindBin(GetBurstID(),i),2);

		}

		hM2TimeCorr->Delete();
		hM2TimeCorr = nullptr;
		hM2TimeSwap->Delete();
		hM2TimeSwap = nullptr;
	}

}

void MUV12SwapCheck::EndOfRunUser(){

}

void MUV12SwapCheck::EndOfJobUser(){

	if (GetIsTree()) SaveAllPlots();

	if (!GetIsTree() && GetIsHisto()){
		TH2 *h1 = static_cast<TH2*>(fHisto.GetHisto("CREAM_SLOT_SWAPPED"));
		if (!h1){
			cerr <<"[MUV12SwapCheck] Cannot find CREAM_SLOT_SWAPPED histogram"<<endl;
			return;
		}
		h1->Write();

		TH2 *h2 = static_cast<TH2*>(fHisto.GetHisto("CREAM_SLOT_SWAPPED_TIME"));
		if (!h2){
			cerr <<"[MUV12SwapCheck] Cannot find CREAM_SLOT_SWAPPED_TIME histogram"<<endl;
			return;
		}
		h2->Write();

		TString OutFileName=Form("Run%d_MUV12DetectedCREAMSwaps.txt",GetRunID());
		ofstream OutTextFile;

		for (int i=1; i<=h1->GetNbinsX(); i++){
			int burst = h1->GetXaxis()->GetBinCenter(i);
			TH1D *b1 = h1->ProjectionY(Form("Check1_Burst%d",burst),i,i);
			TH1D *b2 = h2->ProjectionY(Form("Check2_Burst%d",burst),i,i);
			if (b1->GetMaximum()>1 || b2->GetMaximum()>1){
				if (!OutTextFile.is_open()) OutTextFile.open(OutFileName.Data());
				OutTextFile <<GetRunID()<<" "<<burst;
				for (int slot=1; slot<b1->GetNbinsX(); slot++){
					if (b1->GetBinContent(slot)>1 || b2->GetBinContent(slot)>1) OutTextFile<<" "<<slot;
				}
				OutTextFile<<endl;
			}
			b1->Delete();
			b2->Delete();
		}

		OutTextFile.close();

		for (int s=1; s<32; s++){
			TGraph *gr = fHisto.GetTGraph(Form("EfficiencySlot%d",s));
			if (!gr) break;
			if (gr->GetN()<1) continue;
			gr -> Write();
		}
	}
}

void MUV12SwapCheck::DrawPlot(){
	/// \MemberDescr
	/// This method is called at the end of processing to draw plots when the -g option is used.\n
	/// If you want to draw all the plots, just call\n
	/// \code
	/// 	DrawAllPlots();
	/// \endcode
	/// Or get the pointer to the histogram with\n
	/// \code
	/// 	fHisto.GetTH1("histoName");// for TH1
	/// 	fHisto.GetTH2("histoName");// for TH2
	/// 	fHisto.GetGraph("graphName");// for TGraph and TGraphAsymmErrors
	///     fHisto.GetHisto("histoName");// for TH1 or TH2 (returns a TH1 pointer)
	/// \endcode
	/// and manipulate it as usual (TCanvas, Draw, ...)\n
	/// \EndMemberDescr
}

MUV12SwapCheck::~MUV12SwapCheck(){
	/// \MemberDescr
	/// Destructor of the Analyzer. If you allocated any memory for class
	/// members, delete them here.
	/// \EndMemberDescr
}
