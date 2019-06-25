#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "TMath.h"
#include "TGraphErrors.h"
#include "TGraphAsymmErrors.h"
#include "TEfficiency.h"
#include "TF1.h"

#include "MCSimple.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "GeometricAcceptance.hh"
#include "CalorimeterCluster.hh"
#include "MUV12Corrections.hh"

#include "MUV12DataQuality.hh"
#include "TLegend.h"
#include "TLine.h"
#include "TMarker.h"
#include "TProfile.h"


using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;
using namespace TMath;

/// \class MUV12DataQuality
/// \Brief
/// MUV1 and MUV2 data quality monitor
/// \EndBrief
///
/// \Detailed
/// Check MUV1&2 performance (efficiency, ... )
/// with a basic one-track-selection (K2pi and Kmu2).
/// SpectrometerTrackCorrections and LKrClusterCorrections used as pre-analyzer.
/// DownstreamTrackBuilder used to associated the track with CHOD, LKr and MUV3.
/// \EndDetailed

MUV12DataQuality::MUV12DataQuality(Core::BaseAnalysis *ba) : Analyzer(ba, "MUV12DataQuality")
{

    RequestTree("Spectrometer", new TRecoSpectrometerEvent);
    RequestTree("CHOD", new TRecoCHODEvent);
    RequestTree("LKr", new TRecoLKrEvent);
    RequestTree("MUV3", new TRecoMUV3Event);
    RequestTree("Cedar", new TRecoCedarEvent);
    RequestTree("MUV1", new TRecoMUV1Event);
    RequestTree("MUV2", new TRecoMUV2Event);

    RequestL0Data();

    //PDF Report and Bad Burst List File Names
    AddParam("ReportFileName", &fPDFReportFileName, "MUV12DataQualityReport.pdf");
    AddParam("BadBurstListFileName", &fBadBurstListFileName, "MUV12BadBursts.dat");

    //Bad Burst Limits
    AddParam("MUV1EffLimit", &fMUV1EffLimit, 0.85); //
    AddParam("MUV2EffLimit", &fMUV2EffLimit, 0.85); //


    //track CHOD matching time window half-widths
    AddParam("MUV1_dT", &fMUV1_dT, 25); // [ ns ]
    AddParam("MUV2_dT", &fMUV2_dT, 20); // [ ns ]

    //track to candidate matching distance cut
    AddParam("MUV1_dR", &fMUV1_dR, 120); // [ mm ]
    AddParam("MUV2_dR", &fMUV2_dR, 240); // [ mm ]

    //MIP energy thresholds
    AddParam("LKr_MIN_E", &fLKr_MIN_E, 100.); // [ MeV ]
    AddParam("LKr_MIP_E", &fLKr_MIP_E, 1000.); // [ MeV ]
    AddParam("MUV1_MIP_E", &fMUV1_MIP_E, 2000.); // [ MeV ]
}

void MUV12DataQuality::InitOutput(){}

void MUV12DataQuality::InitHist(){

	if (GetIsTree()){

		BookHisto(new TH1F("CHODdT","Track CHOD Time; #Deltat [ ns ]",100,-25,25),false,"Match");
		BookHisto(new TH1F("CHODdist","Track CHOD Distance; Distance [ mm ]",200,0,2000),false,"Match");
		BookHisto(new TH1F("MUV3dT","Track MUV3 Time; #Deltat [ ns ]",1000,-50,50),false,"Match");
		BookHisto(new TH1F("MUV3dist","Track MUV3 Distance; Distance [ mm ]",200,0,2000),false,"Match");


		BookHisto(new TH1F("MUV1_LKr_timeresolution",";Tmuv1 - Tlkr [ns]",200,-50.,50.),false,"TimingCheck");
		BookHisto(new TH1F("MUV1_CHOD_timeresolution",";Tmuv1 - Tchod [ns]",200,-50.,50.),false,"TimingCheck");

		BookHisto(new TH1F("MUV2_LKr_timeresolution",";Tmuv2 - Tlkr [ns]",200,-50.,50.),false,"TimingCheck");
		BookHisto(new TH1F("MUV2_CHOD_timeresolution",";Tmuv2 - Tchod [ns]",200,-50.,50.),false,"TimingCheck");


		BookHisto(new TH2F("MUV1_Xmapping","channel ID ; x [mm]",200,100,300,400,-2000.,2000.),false,"MappingCheck");
		BookHisto(new TH2F("MUV1_Ymapping","channel ID ; y [mm]",200,100,300,400,-2000.,2000.),false,"MappingCheck");

		BookHisto(new TH2F("MUV2_Xmapping","channel ID ; x [mm]",200,100,300,400,-2000.,2000.),false,"MappingCheck");
		BookHisto(new TH2F("MUV2_Ymapping","channel ID ; y [mm]",200,100,300,400,-2000.,2000.),false,"MappingCheck");


		// Histos for efficiency studies
		BookHisto(new TH2F("MuAtMUV1","",120,-1200,1200,120,-1200,1200),false,"EfficiencyMap");
		BookHisto(new TH2F("MuAtMUV2","",120,-1200,1200,120,-1200,1200),false,"EfficiencyMap");
		BookHisto(new TH2F("MuAtMUV1Matched","",120,-1200,1200,120,-1200,1200),false,"EfficiencyMap");
		BookHisto(new TH2F("MuAtMUV2Matched","",120,-1200,1200,120,-1200,1200),false,"EfficiencyMap");

		BookHisto(new TH1F("Track_Occ_MIPatMUV1","Track occupancy @MUV1; Channel",  200, 100, 300),false,"ChannelEfficiency");
		BookHisto(new TH1F("MUV1_Occ_MIPatMUV1","MUV1 matching cluster occupancy; Channel",  200, 100, 300),false,"ChannelEfficiency");
		BookHisto(new TH1F("Track_Occ_MIPatMUV2","Track occupancy @MUV2; Channel",  200, 100, 300),false,"ChannelEfficiency");
		BookHisto(new TH1F("MUV2_Occ_MIPatMUV2","MUV2 matching cluster occupancy; Channel",  200, 100, 300),false,"ChannelEfficiency");

		BookHisto(new TH1F("MUV1NSelectedKmu2","",3000,1,3001),false,"Efficiency");
		BookHisto(new TH1F("MUV1NDetectedKmu2","",3000,1,3001),false,"Efficiency");
		BookHisto(new TH1F("MUV2NSelectedKmu2","",3000,1,3001),false,"Efficiency");
		BookHisto(new TH1F("MUV2NDetectedKmu2","",3000,1,3001),false,"Efficiency");

		BookHisto(new TH2F("MUV1SingleChEffBurst","",3000,1,3001,10,0.5,1.),false,"Results");
		BookHisto(new TH2F("MUV2SingleChEffBurst","",3000,1,3001,10,0.5,1.),false,"Results");

		BookHisto(new TH2F("EoPBurst","",3000,1,3000,150,0,1.5),false,"PionChecks");
		BookHisto(new TH2F("HAC_FrontBack_Ratio_BurstID","",3000,1,3001,120,0,1.2),false,"PionChecks");

		auto BookGraph = [&](TString Name, TString Dir){
			TGraphErrors *gr = new TGraphErrors();
			gr->SetName(Name);
			return this->BookHisto(gr,false,Dir);
		};

		TString grnames[] = {"MUV1CHODOffset","MUV1LKrOffset","MUV1CHODSigma","MUV1LKrSigma","MUV2CHODOffset","MUV2LKrOffset","MUV2CHODSigma","MUV2LKrSigma"};

		for (const TString &name : grnames) BookGraph(name,"Results");

	}
	else if (GetIsHisto()){

		RequestHistogram(fAnalyzerName+"/MappingCheck", "MUV1_Xmapping", true, "MappingCheck");
		RequestHistogram(fAnalyzerName+"/MappingCheck", "MUV1_Ymapping", true, "MappingCheck");
		RequestHistogram(fAnalyzerName+"/MappingCheck", "MUV2_Xmapping", true, "MappingCheck");
		RequestHistogram(fAnalyzerName+"/MappingCheck", "MUV2_Ymapping", true, "MappingCheck");

		RequestHistogram(fAnalyzerName+"/EfficiencyMap", "MuAtMUV1", true, "EfficiencyMap");
		RequestHistogram(fAnalyzerName+"/EfficiencyMap", "MuAtMUV2", true, "EfficiencyMap");
		RequestHistogram(fAnalyzerName+"/EfficiencyMap", "MuAtMUV1Matched", true, "EfficiencyMap");
		RequestHistogram(fAnalyzerName+"/EfficiencyMap", "MuAtMUV2Matched", true, "EfficiencyMap");

		RequestHistogram(fAnalyzerName+"/Efficiency", "MUV1NSelectedKmu2", true, "Efficiency");
		RequestHistogram(fAnalyzerName+"/Efficiency", "MUV1NDetectedKmu2", true, "Efficiency");
		RequestHistogram(fAnalyzerName+"/Efficiency", "MUV2NSelectedKmu2", true, "Efficiency");
		RequestHistogram(fAnalyzerName+"/Efficiency", "MUV2NDetectedKmu2", true, "Efficiency");

		TString grnames[] = {"MUV1CHODOffset","MUV1LKrOffset","MUV1CHODSigma","MUV1LKrSigma","MUV2CHODOffset","MUV2LKrOffset","MUV2CHODSigma","MUV2LKrSigma","MUV1SingleChEffBurst","MUV2SingleChEffBurst"};
		for (const TString &name : grnames) RequestHistogram(fAnalyzerName+"/Results", name, true, "Results");

		RequestHistogram(fAnalyzerName+"/PionChecks", "EoPBurst", true, "PionChecks");
		RequestHistogram(fAnalyzerName+"/PionChecks", "HAC_FrontBack_Ratio_BurstID", true, "PionChecks");

	}
}

void MUV12DataQuality::DefineMCSimple(){}

void MUV12DataQuality::StartOfRunUser(){}

void MUV12DataQuality::StartOfBurstUser(){}

void MUV12DataQuality::Process(int){

	if (!GetIsTree()) return;
    //Data type
    bool  IsL0ControlTrigger = GetL0Data()->GetDataType() & 0x10;
    if(!IsL0ControlTrigger) return;

    TRecoMUV1Event *MUV1Event = GetEvent<TRecoMUV1Event>();
    TRecoMUV2Event *MUV2Event = GetEvent<TRecoMUV2Event>();

    // Output for DownstreamTrackBuilder (makes the association with CHOD,LKr cluster, RICH and MUV3).
    const std::vector<DownstreamTrack> DownstreamTracksArray = *GetOutput<std::vector<DownstreamTrack>>("DownstreamTrackBuilder.Output");
    const TClonesArray * CalorimetersCandidates = GetOutput<TClonesArray>("SpectrometerCalorimetersAssociation.MatchedClusters");
    //vector<string> &DebugOutput = *((vector<string>*) GetOutput("SpectrometerCalorimetersAssociation.DebugStream"));

    Bool_t IsKmu2 = *GetOutput<Bool_t>("Kmu2Selection.EventSelected");
    Bool_t IsK2pi = *GetOutput<Bool_t>("K2piSelection.EventSelected");
    Int_t K2piTrackID = *GetOutput<Int_t>("K2piSelection.K2piTrackID");

    if (!(IsK2pi xor IsKmu2) ) return;

    const int iTrack = IsKmu2 ? 0 : K2piTrackID;
    DownstreamTrack Track = DownstreamTracksArray[iTrack];

    TVector2 track_atCHOD(Track.xAt(GeometricAcceptance::GetInstance()->GetZCHODHPlane()),Track.yAt(GeometricAcceptance::GetInstance()->GetZCHODVPlane()));
    TVector2 track_atLKr(Track.xAt(GeometricAcceptance::GetInstance()->GetZLKr()),Track.yAt(GeometricAcceptance::GetInstance()->GetZLKr()));
    TVector2 track_atMUV1 (Track.xAt(GeometricAcceptance::GetInstance()->GetZMUV1()),Track.yAt(GeometricAcceptance::GetInstance()->GetZMUV1()));
    TVector2 track_atMUV2 (Track.xAt(GeometricAcceptance::GetInstance()->GetZMUV2()),Track.yAt(GeometricAcceptance::GetInstance()->GetZMUV2()));
    TVector2 track_atMUV3 (Track.xAt(GeometricAcceptance::GetInstance()->GetZMUV3()),Track.yAt(GeometricAcceptance::GetInstance()->GetZMUV3()));
    TVector2 track_atLAV12 (Track.xAt(GeometricAcceptance::GetInstance()->GetZLAVFront(11)),Track.yAt(GeometricAcceptance::GetInstance()->GetZLAVFront(11)));

    if (IsK2pi && track_atLAV12.Mod()>GeometricAcceptance::GetInstance()->GetLAVRinner(11)) return;

    //Old CHOD Matching
    double TrackTime = Track.GetTrackTime();

    if (!Track.CHODAssociationExists()) return;
    else {
        int CHODIndex = -1;

        double mindiscr = 1.e+10;
        for (int j=0; j<Track.GetNCHODAssociationRecords(); j++){

            double time = Track.GetCHODCandidateTime(j);
            if (GetWithMC()) time += 5.;
            TVector2 position = Track.GetCHODCandidatePosition(j).XYvector();

            double discr = Sqrt(((time-TrackTime)*(time-TrackTime))/(5.*5.) + (position - track_atCHOD).Mod2()/(100.*100.));

            if (discr>mindiscr) continue;

            mindiscr=discr;
            CHODIndex = j;

        }

        double CHODTime = Track.GetCHODCandidateTime(CHODIndex);

        FillHisto("CHODdT",TrackTime - CHODTime);
        FillHisto("CHODdist",(track_atCHOD - Track.GetCHODCandidatePosition(CHODIndex).XYvector()).Mod());

        if (Abs(TrackTime - CHODTime)>20. || (track_atCHOD - Track.GetCHODCandidatePosition(CHODIndex).XYvector()).Mod()>100.) return;

        TrackTime = CHODTime;
    }


    CalorimeterCluster *Shower = static_cast<CalorimeterCluster*>(CalorimetersCandidates->At(0));

    if (Shower->GetLKrEnergy()<fLKr_MIN_E) return;

    TRecoMUV1Candidate *MUV1Candidate = Shower->GetMUV1Candidate();
    TRecoMUV2Candidate *MUV2Candidate = Shower->GetMUV2Candidate();

    Bool_t IsLKrMIP = Shower->GetLKrEnergy()< fLKr_MIP_E;//1.e+3;
    Bool_t IsMUV1MIP = Shower->GetMUV1Energy()<fMUV1_MIP_E;//1.5e+3;

    Bool_t MUV1_present = Shower->GetMUV1Energy()>0;
    Bool_t MUV2_present = Shower->GetMUV2Energy()>0;
    Bool_t MUV3PositionMatched = false;

    if (Track.MUV3AssociationExists()){

        double mindiscr = 9999.;
        int MUV3Index = -1;

        for (int j=0; j<Track.GetNMUV3AssociationRecords(); j++){

            double time = Track.GetMUV3Time(j);
            TVector2 position = Track.GetMUV3CandidatePosition(j).XYvector();

            if (GetWithMC()) time += 7;

            double discr = Sqrt((time-TrackTime)*(time-TrackTime)/(5.*5.) + (position - track_atMUV3).Mod2()/(100*100));

            if (discr>mindiscr) continue;

            mindiscr=discr;
            MUV3Index = j;

        }
        double MUV3Time = Track.GetMUV3Time(MUV3Index);
        FillHisto("MUV3dT",TrackTime - MUV3Time);

        if (Abs(TrackTime - MUV3Time)<5.){
            FillHisto("MUV3dist",(track_atMUV3 - Track.GetMUV3CandidatePosition(MUV3Index).XYvector()).Mod());
            if ((track_atMUV3 - Track.GetMUV3CandidatePosition(MUV3Index).XYvector()).Mod()<200.) MUV3PositionMatched = true;
        }

    }

    if (IsKmu2){

    	if (MUV3PositionMatched && IsLKrMIP){

    		auto track_muv1channels = MUV12Corrections::GetInstance()->GetMUV1Channels(track_atMUV1.X(),track_atMUV1.Y());
    		auto track_muv2channels = MUV12Corrections::GetInstance()->GetMUV2Channels(track_atMUV2.X(),track_atMUV2.Y());


    		FillHisto("MuAtMUV1",track_atMUV1.X(),track_atMUV1.Y());
    		FillHisto("MUV1NSelectedKmu2",GetBurstID());
    		FillHisto("Track_Occ_MIPatMUV1",track_muv1channels.first);
    		FillHisto("Track_Occ_MIPatMUV1",track_muv1channels.second);

    		if (MUV1_present) {


    			double muv1_lKr_dt = MUV1Candidate->GetTime() - Shower->GetLKrTime();
    			double muv1_chod_dt = MUV1Candidate->GetTime() - TrackTime;
    			FillHisto("MUV1_LKr_timeresolution",muv1_lKr_dt);
    			FillHisto("MUV1_CHOD_timeresolution",muv1_chod_dt);


    			int *hitIndexes = MUV1Candidate->GetHitsIndexes();
    			for (int jhit= 0; jhit < MUV1Candidate->GetNHits(); jhit++) {
    				TRecoMUV1Hit * hit = static_cast<TRecoMUV1Hit*>(MUV1Event->GetHits()->At(hitIndexes[jhit]));
    				int channel = hit->GetChannelID();
    				if (channel == track_muv1channels.first || channel == track_muv1channels.second) {
    					//MUV1 efficiency study, only LKr MIP muon candidates
    					FillHisto("MUV1_Occ_MIPatMUV1",channel);
    				}


    				if (hit->GetScintillatorOrientation() == 0) FillHisto("MUV1_Xmapping",channel,track_atMUV1.X());
    				else FillHisto("MUV1_Ymapping",channel,track_atMUV1.Y());
    			}


    			//MUV1 efficiency study, only LKr MIP muon candidates
    			if( (track_atMUV1-MUV1Candidate->GetPosition()).Mod()<fMUV1_dR ) {
    				FillHisto("MUV1NDetectedKmu2",GetBurstID());
    				FillHisto("MuAtMUV1Matched",track_atMUV1.X(),track_atMUV1.Y());
    			}

    		}

    		if (IsMUV1MIP){
    			FillHisto("MuAtMUV2",track_atMUV2.X(),track_atMUV2.Y());
    			FillHisto("MUV2NSelectedKmu2",GetBurstID());
    			FillHisto("Track_Occ_MIPatMUV2",track_muv2channels.first);
    			FillHisto("Track_Occ_MIPatMUV2",track_muv2channels.second);

    			if (MUV2_present) {


    				double muv2_lKr_dt = MUV2Candidate->GetTime() - Shower->GetLKrTime();
    				double muv2_chod_dt = MUV2Candidate->GetTime() - TrackTime;
    				FillHisto("MUV2_LKr_timeresolution",muv2_lKr_dt);
    				FillHisto("MUV2_CHOD_timeresolution",muv2_chod_dt);


    				int *hitIndexes = MUV2Candidate->GetHitsIndexes();
    				for (int jhit= 0; jhit < MUV2Candidate->GetNHits(); jhit++) {
    					TRecoMUV2Hit * hit = static_cast<TRecoMUV2Hit*>(MUV2Event->GetHits()->At(hitIndexes[jhit]));
    					int channel = hit->GetChannelID();
    					if (channel == track_muv2channels.first || channel == track_muv2channels.second) {

    						//MUV2 efficiency study, only LKr and MUV1 MIP muon candidates
    						FillHisto("MUV2_Occ_MIPatMUV2",channel);

    					}


    					//Mapping Check
    					if (hit->GetScintillatorOrientation() == 0) FillHisto("MUV2_Xmapping",channel,track_atMUV2.X());
    					else FillHisto("MUV2_Ymapping",channel,track_atMUV2.Y());

    				}


    				//MUV2 efficiency study, only LKr and MUV1 MIP muon candidates
    				if( (track_atMUV2-MUV2Candidate->GetPosition()).Mod()<fMUV2_dR ) {
    					FillHisto("MUV2NDetectedKmu2",GetBurstID());
    					FillHisto("MuAtMUV2Matched",track_atMUV2.X(),track_atMUV2.Y());
    				}
    			}
    		}

    	}
    }

    if (IsK2pi){

        FillHisto("EoPBurst",GetBurstID(),Shower->GetHadronicEnergy()/Track.GetMomentum());
        if (MUV1_present || MUV2_present) FillHisto("HAC_FrontBack_Ratio_BurstID",GetBurstID(),Shower->GetMUV1Energy()/(Shower->GetMUV1Energy()+Shower->GetMUV2Energy()));

    }

}

void MUV12DataQuality::PostProcess(){


}

void MUV12DataQuality::EndOfBurstUser(){

	if (!GetIsTree()) return;

	auto ResFits = [&] (TString hname, TString hmname, TString hsname){

		TH1 *h = fHisto.GetHisto(hname);
		TGraphErrors *hmean = (TGraphErrors*) fHisto.GetTGraph(hmname);
		TGraphErrors *hsig = (TGraphErrors*) fHisto.GetTGraph(hsname);

		if (!h || !hmean || !hsig){
			if (!h) cout << user_normal() << "Cannot get histo "<<hname<<endl;
			if (!hmean) cout << user_normal() << "Cannot get graph "<<hmname<<endl;
			if (!hsig) cout << user_normal() << "Cannot get graph "<<hsname<<endl;
			return 1;
		}
		else {
			float mean = h->GetMean(), rms = h->GetRMS();
			TF1 fgaus ("fgaus","gaus",mean-2*rms,mean+2*rms);
			h->Fit(&fgaus,"RQ0");
			hmean->SetPoint(hmean->GetN(),GetBurstID(),fgaus.GetParameter(1));
			hmean->SetPointError(hmean->GetN()-1,0.01,fgaus.GetParError(1));
			hsig->SetPoint(hsig->GetN(),GetBurstID(),fgaus.GetParameter(2));
			hsig->SetPointError(hsig->GetN()-1,0.01,fgaus.GetParError(2));
			h->Reset("ICESM");
		}

		return 0;
	};

	TString hnames[] = {"MUV1_CHOD_timeresolution","MUV1_LKr_timeresolution","MUV2_CHOD_timeresolution","MUV2_LKr_timeresolution"};
	TString meannames[] = {"MUV1CHODOffset","MUV1LKrOffset","MUV2CHODOffset","MUV2LKrOffset"};
	TString signames[] = {"MUV1CHODSigma","MUV1LKrSigma","MUV2CHODSigma","MUV2LKrSigma"};

	for (int i=0; i<4; i++) ResFits(hnames[i],meannames[i],signames[i]);

	TH1 *h1n = fHisto.GetHisto("MUV1_Occ_MIPatMUV1"); TH1 *h2n = fHisto.GetHisto("MUV2_Occ_MIPatMUV2");
	TH1 *h1d = fHisto.GetHisto("Track_Occ_MIPatMUV1"); TH1 *h2d = fHisto.GetHisto("Track_Occ_MIPatMUV2");

	bool GoodMUV1 = true, GoodMUV2 = true;
	if (!h1n || !h1d){
		cout << user_normal() << "Cannot evaluate MUV1 single channel efficiency"<<endl;
		GoodMUV1=false;
	}
	if (!h2n || !h2d){
		cout << user_normal() << "Cannot evaluate MUV2 single channel efficiency"<<endl;
		GoodMUV2=false;
	}

	for (int i=1; i<=200; i++){
		if (GoodMUV1 && (i-1)%50<44){
			double num = h1n->GetBinContent(i);
			double den = h1d->GetBinContent(i);
			if (den>0){
				float eff = num/den;
				FillHisto("MUV1SingleChEffBurst",GetBurstID(),eff);
			}
		}

		if (GoodMUV2 && (i-1)%50<22){
			double num = h2n->GetBinContent(i);
			double den = h2d->GetBinContent(i);
			if (den>0){
				float eff = num/den;
				FillHisto("MUV2SingleChEffBurst",GetBurstID(),eff);
			}
		}
	}

	TH2F *hm1bid = static_cast<TH2F*>(fHisto.GetHisto("MUV1SingleChEffBurst"));
	TH2F *hm2bid = static_cast<TH2F*>(fHisto.GetHisto("MUV2SingleChEffBurst"));

	int burst_bin = hm1bid->GetXaxis()->FindBin(GetBurstID());
	TH1D *hproj1 = hm1bid->ProjectionY("m1px",burst_bin,burst_bin);
	TH1D *hproj2 = hm2bid->ProjectionY("m2px",burst_bin,burst_bin);
	TH1 *hcum1 = hproj1->GetCumulative(0);
	TH1 *hcum2 = hproj2->GetCumulative(0);

	for (int i=1; i<=hcum1->GetNbinsX(); i++){
		hm1bid->SetBinContent(burst_bin,i,hcum1->GetBinContent(i));
		hm2bid->SetBinContent(burst_bin,i,hcum2->GetBinContent(i));
	}

	hproj1->Delete(); hproj2->Delete();
	hcum1->Delete(); hcum2->Delete();

	h1n -> Reset("ICESM"); h2n -> Reset("ICESM");
	h1d -> Reset("ICESM"); h2d -> Reset("ICESM");

}

void MUV12DataQuality::EndOfJobUser(){

	if (!GetIsTree() && GetIsHisto()){

		TH2* M1SingleCh = static_cast<TH2*>(fHisto.GetHisto("MUV1SingleChEffBurst"));
		if (!M1SingleCh){
			cout << user_normal() << "Failed to fetch histo "<<"MUV1SingleChEffBurst"<<endl;
		}
		else{
			int eff_bin = M1SingleCh->GetYaxis()->FindBin(0.9);
			TH1D *M1SingleAbove90 = M1SingleCh->ProjectionX("MUV1SingleAbove90",eff_bin,eff_bin);
			BookHisto(M1SingleAbove90,false,"Results");
		}

		TH2* M2SingleCh = static_cast<TH2*>(fHisto.GetHisto("MUV2SingleChEffBurst"));
		if (!M2SingleCh){
			cout << user_normal() << "Failed to fetch histo "<<"MUV2SingleChEffBurst"<<endl;
		}
		else{
			int eff_bin = M2SingleCh->GetYaxis()->FindBin(0.8);
			TH1D *M2SingleAbove90 = M2SingleCh->ProjectionX("MUV2SingleAbove90",eff_bin,eff_bin);
			BookHisto(M2SingleAbove90,false,"Results");
		}

		TEfficiency *MUV1Efficiency = new TEfficiency (*fHisto.GetHisto("MUV1NDetectedKmu2"),*fHisto.GetHisto("MUV1NSelectedKmu2"));
		TEfficiency *MUV2Efficiency = new TEfficiency (*fHisto.GetHisto("MUV2NDetectedKmu2"),*fHisto.GetHisto("MUV2NSelectedKmu2"));
		MUV1Efficiency->SetName("MUV1EfficiencyvsBurst");
		MUV2Efficiency->SetName("MUV2EfficiencyvsBurst");

		TEfficiency *MUV1MuEfficiency = new TEfficiency (*fHisto.GetHisto("MuAtMUV1Matched"),*fHisto.GetHisto("MuAtMUV1"));
		TEfficiency *MUV2MuEfficiency = new TEfficiency (*fHisto.GetHisto("MuAtMUV2Matched"),*fHisto.GetHisto("MuAtMUV2"));
		MUV1MuEfficiency->SetName("MUV1EfficiencyMap");
		MUV2MuEfficiency->SetName("MUV2EfficiencyMap");

		BookHisto(MUV1Efficiency,false,"Results");
		BookHisto(MUV2Efficiency,false,"Results");

		BookHisto(MUV1MuEfficiency,false,"Results");
		BookHisto(MUV2MuEfficiency,false,"Results");

		if (fBadBurstListFileName.Contains("%rid")) fBadBurstListFileName.ReplaceAll("%rid",Form("%d",GetRunID()));
		if (fPDFReportFileName.Contains("%rid")) fPDFReportFileName.ReplaceAll("%rid",Form("%d",GetRunID()));

		GeneratePDFReport();

		ofstream BadBurstFile;
                BadBurstFile.open(fBadBurstListFileName); 
		int RunID = GetRunID();
		BadBurstFile <<"RunID \tBurstID \tReasons\n";
		for (int i=1; i<=3000; i++){
			int bin = fHisto.GetHisto("MUV2NSelectedKmu2")->FindBin(double(i));
			double M1Eff = MUV1Efficiency->GetEfficiency(bin);
			double M2Eff = MUV2Efficiency->GetEfficiency(bin);
			int NSelected = MUV2Efficiency->GetTotalHistogram()->GetBinContent(bin);
			if (NSelected<1) continue;
			if (NSelected<30){
				BadBurstFile <<Form("BadBurst %06d %04d",RunID,i)<<" \t"<<" \tstat"<<endl;
			}
			else if (M1Eff < fMUV1EffLimit || M2Eff < fMUV2EffLimit){
				BadBurstFile <<Form("BadBurst %06d %04d",RunID,i)<<" \t"<<(M1Eff < fMUV1EffLimit ? "MUV1 " : "")<<(M2Eff < fMUV2EffLimit ? "MUV2 " : "")<<endl;
			}
		}
		BadBurstFile.close();
	}

    SaveAllPlots();

}

void MUV12DataQuality::DrawPlot(){}

MUV12DataQuality::~MUV12DataQuality(){}

void MUV12DataQuality::GeneratePDFReport(){

	auto prev_err_ignore_lev = gErrorIgnoreLevel;
	gErrorIgnoreLevel = 5000;
	TCanvas c0("c0");
	if (!fPDFReportFileName.EndsWith(".pdf")) fPDFReportFileName.Append(".pdf");
	c0.Print(fPDFReportFileName+"[","pdf");

	TEfficiency *M1EffBurst = fHisto.GetTEfficiency("MUV1EfficiencyvsBurst");
	TEfficiency *M2EffBurst = fHisto.GetTEfficiency("MUV2EfficiencyvsBurst");

	TEfficiency *M1EffMap = fHisto.GetTEfficiency("MUV1EfficiencyMap");
	TEfficiency *M2EffMap = fHisto.GetTEfficiency("MUV2EfficiencyMap");

	int firstburst = M1EffBurst->GetTotalHistogram()->FindFirstBinAbove(0);
	int lastburst  = M1EffBurst->GetTotalHistogram()->FindLastBinAbove(0);

	TLine M1Lim (firstburst-1,fMUV1EffLimit,lastburst+1,fMUV1EffLimit);
	TLine M2Lim (firstburst-1,fMUV2EffLimit,lastburst+1,fMUV2EffLimit);
	c0.Divide(1,2);

	TGraph *g = M1EffBurst->CreateGraph();
	g->GetXaxis()->SetRangeUser(firstburst-1,lastburst+1);
	g->GetYaxis()->SetRangeUser(0.6,1.01);

	c0.cd(1)->SetGrid();
	g->Draw("ape");
	M1Lim.Draw("same");

	c0.cd(2)->Divide(2);
	c0.cd(2)->cd(1);
	M1EffMap->Draw("colz");
	c0.cd(2)->cd(2);
	TH1* h1 = fHisto.GetTH1("MUV1SingleAbove90");
	if (!h1){
		cout << user_normal() << "MUV1SingleAbove90 is missing!"<<endl;
	}
	else{
		h1->GetXaxis()->SetRangeUser(firstburst-1,lastburst+1);
		h1->GetYaxis()->SetRangeUser(0,200);
		h1->Draw();
	}
	c0.Print(fPDFReportFileName,"pdf");



	TCanvas c1("c1");
	c1.Divide(1,2);
	c1.cd(1);
	TGraph *g1 = M2EffBurst->CreateGraph();
	g1->GetXaxis()->SetRangeUser(firstburst-1,lastburst+1);
	g1->GetYaxis()->SetRangeUser(0.6,1.01);
	c1.cd(1)->SetGrid();

	g1->Draw("ape");
	M2Lim.Draw("same");

	c1.cd(2)->Divide(2);
	c1.cd(2)->cd(1);
	M2EffMap->Draw("colz");
	c1.cd(2)->cd(2)->SetGrid();
	h1 = fHisto.GetTH1("MUV2SingleAbove90");
	if (!h1){
		cout << user_normal() << "MUV2SingleAbove90 is missing!"<<endl;
	}
	else{
		h1->GetXaxis()->SetRangeUser(firstburst-1,lastburst+1);
		h1->GetYaxis()->SetRangeUser(0,100);
		h1->Draw();
	}
	c1.Print(fPDFReportFileName,"pdf");



	TCanvas c3("c3");
	c3.Divide(1,2);

	TH2F *hMapM1X = static_cast<TH2F*>(fHisto.GetHisto("MUV1_Xmapping")), *hMapM1Y = static_cast<TH2F*>(fHisto.GetHisto("MUV1_Ymapping"));
	TH2F *hMapM2X = static_cast<TH2F*>(fHisto.GetHisto("MUV2_Xmapping")), *hMapM2Y = static_cast<TH2F*>(fHisto.GetHisto("MUV2_Ymapping"));
	TH2F *hMapM1 =  static_cast<TH2F*>(hMapM1X->Clone("MUV1MapCheck")); hMapM1->Add(hMapM1Y);
	TH2F *hMapM2 =  static_cast<TH2F*>(hMapM2X->Clone("MUV2MapCheck")); hMapM2->Add(hMapM2Y);
	BookHisto(hMapM1,false,"MappingCheck");
	BookHisto(hMapM2,false,"MappingCheck");

	c3.cd(1);
	TProfile *pfx1 = hMapM1->ProfileX();
	pfx1->SetMarkerStyle(22); pfx1->SetMarkerSize(0.5); pfx1->SetMarkerColor(kRed);
	pfx1->SetTitle("MUV1 Mapping Check; Channel ID; Position [mm]");
	hMapM1->GetYaxis()->SetRange(-1400,1400);
	hMapM1->Draw();
	TMarker m(0,0,4);
	m.SetMarkerSize(0.5); m.SetMarkerColor(kBlue);
	for (int i=0; i<44; i++){
		m.SetY(MUV12Corrections::GetInstance()->GetMUV1ScintillatorCenter(101+i));
		for (int k=0; k<4; k++){
			m.SetX(101.5+i+k*50);
			m.DrawClone("same");
		}
	}
	pfx1->Draw("same");

	c3.cd(2);
	TProfile *pfx2 = hMapM2->ProfileX();
	pfx2->SetMarkerStyle(22); pfx2->SetMarkerSize(0.5); pfx2->SetMarkerColor(kRed);
	pfx1->SetTitle("MUV2 Mapping Check; Channel ID; Position [mm]");
	hMapM2->GetYaxis()->SetRange(-1400,1400);
	hMapM2->Draw();
	for (int i=0; i<22; i++){
		m.SetY(MUV12Corrections::GetInstance()->GetMUV2ScintillatorCenter(101+i));
		for (int k=0; k<4; k++){
			m.SetX(101.5+i+k*50);
			m.DrawClone("same");
		}
	}
	pfx2->Draw("same");

	c3.Print(fPDFReportFileName,"pdf");



	TGraph *grLKr, *grCHOD;

	TCanvas c2("c2");
	c2.Divide(2,2);

	c2.cd(1)->SetGrid();

	grLKr = fHisto.GetTGraph("MUV1LKrOffset");
	grCHOD = fHisto.GetTGraph("MUV1CHODOffset");

	grLKr->SetTitle("MUV1 Time: Offset vs Burst ID; Burst ID; Offset [ns]");
	grLKr->GetXaxis()->SetRangeUser(firstburst-1,lastburst+1);
	grLKr->GetYaxis()->SetRangeUser(-10.,10.);
	grLKr->SetLineColor(kRed); grLKr->SetMarkerColor(kRed); grLKr->SetMarkerStyle(20);
	grCHOD->SetLineColor(kBlue); grCHOD->SetMarkerColor(kBlue); grCHOD->SetMarkerStyle(22);

	TLegend leg1 (0.75,0.75,0.95,0.95);
	leg1.AddEntry(grLKr, "MUV1-LKr","PEE");
	leg1.AddEntry(grCHOD,"MUV1-CHOD","PEE");

	grLKr->Draw("APE");
	grCHOD->Draw("PEsame");
	leg1.Draw("same");

	c2.cd(2)->SetGrid();

	grLKr = fHisto.GetTGraph("MUV2LKrOffset");
	grCHOD = fHisto.GetTGraph("MUV2CHODOffset");
	grLKr->SetTitle("MUV2 Time: Offset vs Burst ID; Burst ID; Offset [ns]");
	grLKr->GetXaxis()->SetRangeUser(firstburst-1,lastburst+1);
	grLKr->GetYaxis()->SetRangeUser(-10.,10.);

	grLKr->SetLineColor(kRed); grLKr->SetMarkerColor(kRed); grLKr->SetMarkerStyle(20);
	grCHOD->SetLineColor(kBlue); grCHOD->SetMarkerColor(kBlue); grCHOD->SetMarkerStyle(22);

	TLegend leg2 (0.75,0.75,0.95,0.95);
	leg2.AddEntry(grLKr, "MUV2-LKr","PEE");
	leg2.AddEntry(grCHOD,"MUV2-CHOD","PEE");

	grLKr->Draw("APE");
	grCHOD->Draw("PEsame");
	leg2.Draw("same");

	c2.cd(3)->SetGrid();

	grLKr = fHisto.GetTGraph("MUV1LKrSigma");
	grCHOD = fHisto.GetTGraph("MUV1CHODSigma");

	grLKr->SetTitle("MUV1 Time: Sigma vs Burst ID; Burst ID; Offset [ns]");
	grLKr->GetXaxis()->SetRangeUser(firstburst-1,lastburst+1);
	grLKr->GetYaxis()->SetRangeUser(0.,5.);
	grLKr->SetLineColor(kRed); grLKr->SetMarkerColor(kRed); grLKr->SetMarkerStyle(20);
	grCHOD->SetLineColor(kBlue); grCHOD->SetMarkerColor(kBlue); grCHOD->SetMarkerStyle(22);

	TLegend leg3 (0.75,0.75,0.95,0.95);
	leg3.AddEntry(grLKr, "MUV1-LKr","PEE");
	leg3.AddEntry(grCHOD,"MUV1-CHOD","PEE");

	grLKr->Draw("APE");
	grCHOD->Draw("PEsame");
	leg3.Draw("same");


	c2.cd(4)->SetGrid();

	grLKr = fHisto.GetTGraph("MUV2LKrSigma");
	grCHOD = fHisto.GetTGraph("MUV2CHODSigma");

	grLKr->SetTitle("MUV2 Time: Sigma vs Burst ID; Burst ID; Offset [ns]");
	grLKr->GetXaxis()->SetRangeUser(firstburst-1,lastburst+1);
	grLKr->GetYaxis()->SetRangeUser(0.,5.);
	grLKr->SetLineColor(kRed); grLKr->SetMarkerColor(kRed); grLKr->SetMarkerStyle(20);
	grCHOD->SetLineColor(kBlue); grCHOD->SetMarkerColor(kBlue); grCHOD->SetMarkerStyle(22);

	TLegend leg4 (0.75,0.75,0.95,0.95);
	leg4.AddEntry(grLKr, "MUV2-LKr","PEE");
	leg4.AddEntry(grCHOD,"MUV2-CHOD","PEE");

	grLKr->Draw("APE");
	grCHOD->Draw("PEsame");
	leg4.Draw("same");

	c2.Print(fPDFReportFileName,"pdf");

	c0.Print(fPDFReportFileName+"]","pdf");
	gErrorIgnoreLevel = prev_err_ignore_lev;

}
