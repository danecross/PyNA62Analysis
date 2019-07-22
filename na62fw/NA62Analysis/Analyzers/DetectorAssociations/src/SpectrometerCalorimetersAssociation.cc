#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "SpectrometerCalorimetersAssociation.hh"
#include "LKrClusterCorrectionFunctions.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "TMath.h"
#include "TVector2.h"
#include "NA62Exceptions.hh"
#include "TSystem.h"
#include "GeometricAcceptance.hh"
#include "TRegexp.h"
#include "NA62ConditionsService.hh"

using namespace TMath;
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class SpectrometerCalorimetersAssociation
/// \Brief
/// Association between tracks and energy deposits in the calorimeters (LKr,MUV1,MUV2)
/// with integrated PID
/// \EndBrief
/// \Detailed
/// Builds a TClonesArray containing CalorimeterCluster. The number of output objects is
/// equal to the number of tracks and follows the track numbering.
/// Each CalorimeterCluster contains the information provided by LKr, MUV1 and MUV2 and the result
/// of a Multivariate analysis to determine the particle ID of the track between pions, muons and electrons.\n
/// Cluster building in LKr can start from cells (default) or can try to associate with a standard cluster setting the parameter UseStdLKr=1.
/// The reference time for cluster construction in MUV1&2 can be defined to be the LKr (when present) with the parameter UseLKrRef=1, otherwise the STRAW time is used. \n\n
/// The output of the analyzer can be retrieved in the following way:
/// \code
/// TClonesArray *ShowerCandidates = (TClonesArray*)GetOutput("SpectrometerCalorimetersAssociation.MatchedClusters");
/// \endcode
/// and then
/// \code
/// CalorimeterCluster *Cluster = (CalorimeterCluster*)ShowerCandidates[i];
/// \endcode
/// The CalorimeterCluster contains the pointers to the LKr/MUV1/MUV2 reconstructed clusters, if found.
/// The pointers can be retrieved by the function GetLKr(MUV1,MUV2)Candidate() as TRecoLKr(MUV1,MUV2)Candidate,
/// and the presence of the association checked by IsLKr(MUV1,MUV2)Associated() functions.\n\n
/// The access of the main information for each detector and to the global shower is provided by the Candididate
/// class with the functions: \n
/// GetTime(): global time of the shower \n
/// GetEnergy(): total energy of the shower \n
/// GetHadronicEnergy(): like get energy, but with a rough calibration of the LKr energy for pions \n
/// GetLKr(MUV1,MUV2)Energy(): energy collected by the detector \n
/// GetLKr(MUV1,MUV2)Time(): time measured by the detector \n
/// GetLKr(MUV1,MUV2)Weight(): variable expressing the shower shape in the detector (->0 hadronic, ->1 EM, not normalized!) \n
/// GetLKr(MUV1,MUV2)OuterEnergy(): provides an estimate of the energy not associated to the track in the detector \n\n
/// The PID system gives the probability this cluster is an electron, a muon or a pion. The sum of the 3 probabilities must
/// give always 1. The probabilities are accessed by
///  GetIsElectronProbability(), GetIsMuonProbability() and GetIsPionProbability()\n
/// \n
/// As an example setting the pion probability above 0.8 and vetoing on the MUV3 the final muon efficiency was estimated as
/// 5 \times 10^{-5} with a pion efficiency of more than 95%.
/// \EndDetailed

SpectrometerCalorimetersAssociation::SpectrometerCalorimetersAssociation(Core::BaseAnalysis *ba) : Analyzer(ba, "SpectrometerCalorimetersAssociation") {


	RequestTree("Spectrometer",new TRecoSpectrometerEvent);
	RequestTree("LKr",new TRecoLKrEvent);
	RequestTree("MUV1",new TRecoMUV1Event);
	RequestTree("MUV2",new TRecoMUV2Event);

	AddParam("UseStdLKr", &fUseLKrStandard, 1);
	AddParam("UseLKrRef", &fUseLKrRefTime, 1);
	AddParam("DebugMode", &fDebug, false);

	const char* inputVars[] = {"Momentum","Easym","(LKrEnergy+MUV1Energy+MUV2Energy)__D__Momentum","LKrEnergy__D__Momentum","(MUV2Energy)__D__(LKrEnergy+MUV1Energy+MUV2Energy)","(MUV1Energy)__D__(LKrEnergy+MUV1Energy+MUV2Energy)","LKrEnergy__D__(LKrEnergy+MUV1Energy+MUV2Energy)","(MUV1Energy*MUV1Weight+MUV2Energy*MUV2Weight)__D__(MUV1Energy+MUV2Energy)","LKrRMS","(MUV1SeedEnergy+MUV2SeedEnergy)__D__(MUV1Energy+MUV2Energy)","(MUV1NHits)__D__(MUV1Energy+1)+2*(MUV2NHits)__D__(MUV2Energy+1)","(LKrSeedEnergy)__D__(LKrEnergy)","LKrNCells/LKrEnergy","LKrDist","MUV1Dist","MUV2Dist"};
	const char* newinputVars[] = {"EoP","LKrEoP","MUV1Fr","HACFr","MUV1Weight","HACWeight","LKrRMS","HACSW","LKrSeedFr","LKrNCellsFr","LKrDist","MUV1Dist"};
	fTMVAReader = new TMVA::Reader("Silent",0);
	fTMVAReaderNew = new TMVA::Reader("Silent",0);

	for (int i=0; i<16; i++){
		if (i<2) fTMVAReader->AddSpectator(TString(inputVars[i]),&fTMVAVariables[i]);
		else fTMVAReader->AddVariable(TString(inputVars[i]),&fTMVAVariables[i]);
	}

	for (int i=0; i<12; i++) fTMVAReaderNew->AddVariable(TString(newinputVars[i]),&fNewTMVAVariables[i]);

	fTMVAReader->BookMVA("BDT",NA62ConditionsService::GetInstance()->GetFullPath("SpectrometerCalorimetersAssociation_TMVA_weight_file.xml"));
	fTMVAReaderNew->BookMVA("NewBDT",NA62ConditionsService::GetInstance()->GetFullPath("SpectrometerCalorimetersAssociation_TMVA_weight_file_new.xml"));
	LKrProfInit();


	for (int i=0; i<44; i++){
		MUV1ScintillatorPosition[i]=-1278;

		if (i<20){ MUV1ScintillatorPosition[i] += i*60.; }
		else if (i>23){ MUV1ScintillatorPosition[i] += (43 - i)*60.; }
		else if (i==20 || i==23) { MUV1ScintillatorPosition[i] = -81;  }
		else { MUV1ScintillatorPosition[i] = -27;  }

		if (i>21) MUV1ScintillatorPosition[i] *= -1.;

	}


	MUV2ScintillatorPosition[0] = -1238.5;
	for (int i=1; i<22; i++){
		if (i<10 || i>11){ MUV2ScintillatorPosition[i] = MUV2ScintillatorPosition[i-1] + 119.; }
		else{ MUV2ScintillatorPosition[i] = 54.*(i<12 ? -1. : 1.); }
	}

}

void SpectrometerCalorimetersAssociation::InitOutput(){
	fLKrCandidates  = new TClonesArray("TRecoLKrCandidate", 20);
	fMUV1Candidates = new TClonesArray("TRecoMUV1Candidate",20);
	fMUV2Candidates = new TClonesArray("TRecoMUV2Candidate",20);
	fReconstructedCluster = new TClonesArray("CalorimeterCluster",20);
	RegisterOutput("MatchedClusters",fReconstructedCluster);
	RegisterOutput("DebugStream", &fDebugStream);
	CalorimeterCluster c;
	LKrProfInit();
}

void SpectrometerCalorimetersAssociation::InitHist(){

	BookHisto(new TH2F("LKrdtAtExtrapPos","dT of LKr Hit at extrap position vs SC ID",1024,0,1024,50,-100,100));
	for (int i=1; i<=1024; i++) fHisto.GetTH2("LKrdtAtExtrapPos")->GetXaxis()->SetBinLabel(i,Form("%d,%d",(i-1)%32+1,(i-1)/32));

}

void SpectrometerCalorimetersAssociation::DefineMCSimple(){}

void SpectrometerCalorimetersAssociation::StartOfRunUser(){}

void SpectrometerCalorimetersAssociation::StartOfBurstUser(){
	/// \MemberDescr
	/// This method reads the corrections for this run, at the moment only for MUV1&2 and with differentiation between 2015 and 2016
	/// and other reference files
	/// \EndMemberDescr
	if (!GetIsTree()) return;
	fMUV12CorrHandler = MUV12Corrections::GetInstance
			(GetRunID(),GetStreamInfo()->GetRecoInfo().GetRevision(),GetWithMC());
}

void SpectrometerCalorimetersAssociation::PreProcess(){}

void SpectrometerCalorimetersAssociation::Process(Int_t){
	if (!GetIsTree()) return;
	fLKrCandidates->Clear("C");
	fMUV1Candidates->Clear("C");
	fMUV2Candidates->Clear("C");
	fReconstructedCluster->Clear("C");

	const double LKr_Z  = GeometricAcceptance::GetInstance()->GetZLKr();
	const double MUV1_Z = GeometricAcceptance::GetInstance()->GetZMUV1();
	const double MUV2_Z = GeometricAcceptance::GetInstance()->GetZMUV2();

	TRecoSpectrometerEvent *STRAWEv = GetEvent<TRecoSpectrometerEvent>();
	CalorimeterCluster *RecoCluster = nullptr;

	fDebugStream.clear();
	for (int itrack=0; itrack<STRAWEv->GetNCandidates(); itrack++){
		if (fDebug) thisstream.str(std::string());
		RecoCluster = static_cast<CalorimeterCluster*>(fReconstructedCluster->ConstructedAt(fReconstructedCluster->GetEntries()));
		RecoCluster->SetTrackID(itrack);

		TRecoSpectrometerCandidate* Track = static_cast<TRecoSpectrometerCandidate*>(STRAWEv->GetCandidate(itrack));

		double TrackTime = Track->GetTime();
		TVector2 PosAtLKr (Track->xAt(LKr_Z),Track->yAt(LKr_Z));
		TVector2 PosAtMUV1 (Track->xAt(MUV1_Z),Track->yAt(MUV1_Z));
		TVector2 PosAtMUV2 (Track->xAt(MUV2_Z),Track->yAt(MUV2_Z));

		//---------------------------------------------------------------------------------------------
		//----------------------------------- LKr Matching --------------------------------------------
		//---------------------------------------------------------------------------------------------

		if (fUseLKrStandard) MatchLKrCandidate  (TrackTime, PosAtLKr, RecoCluster);
		else ReconstructLKrCandidate  (TrackTime, PosAtLKr, RecoCluster);


		//---------------------------------------------------------------------------------------------
		//----------------------------------- MUV1 Matching --------------------------------------------
		//---------------------------------------------------------------------------------------------
		ReconstructMUV1Candidate  (TrackTime, PosAtMUV1, RecoCluster);

		//---------------------------------------------------------------------------------------------
		//----------------------------------- MUV2 Matching --------------------------------------------
		//---------------------------------------------------------------------------------------------
		ReconstructMUV2Candidate  (TrackTime, PosAtMUV2, RecoCluster);
		if ((IsNaN(RecoCluster->GetHACShowerWidth()) || IsNaN(RecoCluster->GetHACWeight())) && !fDebug){
			fDebug= true;
			thisstream << "Running MUV2 reco again beacuse SW "<<RecoCluster->GetHACShowerWidth()<<" and weight "<<RecoCluster->GetHACWeight()<<endl;
			ReconstructMUV2Candidate  (TrackTime, PosAtMUV2, RecoCluster);
			cout << user_normal() << thisstream.str().c_str();
			thisstream.str(std::string());
			fDebug= false;
		}

		fDebugStream.push_back(thisstream.str());
		//---------------------------------------------------------------------------------------------
		//----------------------------------- Calibration ---------------------------------------------
		//---------------------------------------------------------------------------------------------

		double LKrEnergy = 1.e-3*RecoCluster->GetLKrEnergy();
		double LKrHadronicEnergy = 1.e-3*LKrEnergy;

		double MUV1Energy = 1.e-3*RecoCluster->GetMUV1Energy();
		double MUV1EnergyOld = 1.e-3*RecoCluster->GetMUV1Energy();
		double MUV1Uncorr = 1.e-3*MUV1Energy;

		double MUV2Energy = 1.e-3*RecoCluster->GetMUV2Energy();
		double MUV2EnergyOld = 1.e-3*RecoCluster->GetMUV2Energy();
		double MUV2Uncorr = 1.e-3*MUV2Energy;

		double HACEnergy = MUV1Energy + MUV2Energy;
		double TotalEnergy = HACEnergy + LKrEnergy;

		if (HACEnergy>0){

			if (MUV1Energy>0){
				double R = RecoCluster->GetMUV1Candidate()->GetPosition().Mod();
				double RCorrection = 0.923 + 6.9e-5*R;
				if (!GetWithMC()) MUV1Energy /= RCorrection;
			}

			auto M1VisibleFraction = [] (double ETotal){
				double res = 0.8846*((1-exp(-((ETotal+13.78)/10.12))));
				return res;
			};
			auto M2VisibleFraction = [] (double ETotal){
				double res = 0.813*((1-exp(-((ETotal+10.9)/8.15))));
				return res;
			};

			double M1InvFr = M1VisibleFraction(TotalEnergy);
			double M1Inv = MUV1Energy * (1./M1InvFr - 1.);
			double M2InvFr = M2VisibleFraction(TotalEnergy);
			double M2Inv = MUV2Energy * (1./M2InvFr-1.);

			for (int i=0; i<3; i++){
				M1InvFr = M1VisibleFraction(TotalEnergy+M1Inv+M2Inv);
				M2InvFr = M2VisibleFraction(TotalEnergy+M1Inv+M2Inv);
				M1Inv = MUV1Energy * (1./M1InvFr - 1.);
				M2Inv = MUV2Energy * (1./M2InvFr-1.);
			}

			if (MUV1Energy>0){
				double W1scale = 1./(0.592 + 0.206/(1+exp(-(RecoCluster->GetMUV1Weight()-0.227)/1.27e-2)) + 0.588*RecoCluster->GetMUV1Weight());
				MUV1Energy = (MUV1Energy+M1Inv)*W1scale;

				RecoCluster->SetMUV1Energy(1.e+3*MUV1Energy);
				RecoCluster->GetMUV1Candidate()->SetEnergy(1.e+3*MUV1Energy);
				RecoCluster->SetMUV1EnergyCorrections(1.e+3*MUV1Energy - 1.e+3*MUV1Uncorr);

				double MUV1WeightCorr = (-0.94 + 1.42/(exp((RecoCluster->GetOldMUV1Weight() - 0.255)/0.0545)+1));
				double MUV1InvisibleOld = MUV1EnergyOld*( 1/ ( 0.85 * (1 - Exp(-(TotalEnergy + 30.)/18.))) - 1.);
				MUV1EnergyOld += (1+MUV1WeightCorr)*MUV1InvisibleOld;
				RecoCluster->SetOldMUV1Energy(1.e+3*MUV1EnergyOld);

			}


			if (MUV2Energy>0){
				//cout << "HAC Weight "<<RecoCluster->GetHACWeight()<<" MUV1 Weight "<<RecoCluster->GetMUV1Weight()<<endl;
				//cout <<"HAC Energy"<<HACEnergy<<" MUV1 Energy "<<MUV1Energy<<endl;
				double W2scale = 1./(0.979 + 0.19/(1+exp(-(RecoCluster->GetHACWeight()-0.525)/1.94e-2)) + 0.033*RecoCluster->GetHACWeight());

				MUV2Energy = (MUV2Energy + M2Inv)*W2scale;

				RecoCluster->SetMUV2Energy(1.e+3*MUV2Energy);
				RecoCluster->GetMUV2Candidate()->SetEnergy(1.e+3*MUV2Energy);
				RecoCluster->SetMUV2EnergyCorrections(1.e+3*MUV2Energy-1.e+3*MUV2Uncorr);

				double MUV2WeightCorr = (-0.99 + 1.217/(exp((RecoCluster->GetOldMUV2Weight() - 0.34)/0.049)+1));
				double MUV2InvisibleOld = MUV2Energy*( 1./(0.7969 * (1 - Exp(-(TotalEnergy + 10.66)/8.39))) - 1.);
				MUV2EnergyOld += (1+MUV2WeightCorr)*MUV2InvisibleOld;

				RecoCluster->SetOldMUV2Energy(1.e+3*MUV2EnergyOld);

			}

			HACEnergy = MUV1Energy + MUV2Energy;
			TotalEnergy = HACEnergy + LKrEnergy;

		}

		if (LKrEnergy>0){
			double HACFraction = HACEnergy/TotalEnergy;
			float RMSx = RecoCluster->GetLKrCandidate()->GetClusterRMSX();
			float RMSy = RecoCluster->GetLKrCandidate()->GetClusterRMSY();
			float LKrRMS = Sqrt(RMSx*RMSx + RMSy*RMSy)/2.;
			LKrRMS /= 1. - exp(-(1.27+4.5e-2*LKrEnergy));

			auto LKrInvisibleFraction = [] (float ETot,float HACFr){
				float val = 0.496*(1-exp(-(ETot + 15)/24));
				val += (-0.307 + 1.39e-3*ETot)*HACFr;
				return val;
			};

			float LKrInvFr = LKrInvisibleFraction(TotalEnergy,HACFraction);
			float LKrInv = LKrEnergy * (1./LKrInvFr - 1.);
			for (int i=0; i<3; i++){
				LKrInvFr = LKrInvisibleFraction(TotalEnergy+LKrInv,HACFraction);
				LKrInv = LKrEnergy * (1./LKrInvFr - 1.);
			}

			float c1 = 3.15 - 0.144*LKrRMS, c2 = 1.466 - 0.034*LKrRMS;
			float LKrFinalCorr = 1. / (LKrRMS < 15 ? c1 : (LKrRMS > 16 ? c2 : c1*(1. + 15. - LKrRMS) + c2*(LKrRMS - 15)));
			float LKrFinal = LKrFinalCorr*(LKrEnergy + LKrInv);

			float LKrNLCorrection = 1.155 - 3.91e-3*(LKrFinal+HACEnergy);
			LKrFinal /= LKrNLCorrection;

			LKrHadronicEnergy = LKrFinal;

			RecoCluster->SetLKrHadronicEnergy(1.e+3*LKrHadronicEnergy);
		}


		if (TotalEnergy > 0) {

			double ShowerTime = 0;
			if (LKrEnergy >0) ShowerTime += RecoCluster->GetLKrTime()*LKrEnergy/1.;
			if (MUV1Energy>0) ShowerTime += RecoCluster->GetMUV1Time()*MUV1Energy/1.5;
			if (MUV2Energy>0) ShowerTime += RecoCluster->GetMUV2Time()*MUV2Energy/1.2;

			ShowerTime /= LKrEnergy/1. + MUV1Energy/1.5 + MUV2Energy/1.3;
			RecoCluster->SetTime(ShowerTime);
			RecoCluster->SetEnergy(1.e+3*TotalEnergy);
			RecoCluster->SetHadronicEnergy(1.e+3*(LKrHadronicEnergy+HACEnergy));

			if (LKrEnergy/RecoCluster->GetEnergy()>1.) cout << user_normal() << "Error: LKrEnergy > Cluster Energy "<<LKrEnergy/RecoCluster->GetEnergy()<<endl;

			fTMVAVariables[0] = Track->GetMomentum();
			fTMVAVariables[1] = (RecoCluster->GetLKrEnergy() - (RecoCluster->GetMUV1Energy()+RecoCluster->GetMUV2Energy()))/RecoCluster->GetEnergy();

			float OldTotalEnergy = (RecoCluster->GetLKrEnergy()+RecoCluster->GetOldMUV1Energy()+RecoCluster->GetOldMUV2Energy());
			fTMVAVariables[2] = OldTotalEnergy/Track->GetMomentum();
			fTMVAVariables[3] = RecoCluster->GetLKrEnergy()/Track->GetMomentum();
			fTMVAVariables[4] = RecoCluster->GetOldMUV2Energy()/OldTotalEnergy;
			fTMVAVariables[5] = RecoCluster->GetOldMUV1Energy()/OldTotalEnergy;
			fTMVAVariables[6] = RecoCluster->GetLKrEnergy()/OldTotalEnergy;

			double w1 = (fTMVAVariables[5]>0 ? 1./(exp((RecoCluster->GetOldMUV1Weight() - 0.255)/0.0545)+1) : -1);
			double w2 = (fTMVAVariables[4]>0 ?1./(exp((RecoCluster->GetOldMUV2Weight() - 0.34)/0.049)+1) : -1);

			fTMVAVariables[7] = ((RecoCluster->GetOldMUV1Energy()*w1+RecoCluster->GetOldMUV2Energy()*w2)/(RecoCluster->GetOldMUV1Energy()+RecoCluster->GetOldMUV2Energy()));

			double RMSx=0., RMSy=0.;
			if (RecoCluster->GetLKrEnergy()>0){

				RMSx = RecoCluster->GetLKrCandidate()->GetClusterRMSX();
				RMSy = RecoCluster->GetLKrCandidate()->GetClusterRMSY();
				fTMVAVariables[8] = Sqrt(RMSx*RMSx + RMSy*RMSy)/2.;
			}
			else fTMVAVariables[8] = 0;

			double MUV1Seed = (fTMVAVariables[5]>0 ? RecoCluster->GetMUV1Candidate()->GetSeedEnergy() : -1);
			double MUV2Seed = (fTMVAVariables[4]>0 ? RecoCluster->GetMUV2Candidate()->GetSeedEnergy() : -1);

			fTMVAVariables[9] = (MUV1Seed + MUV2Seed)/(RecoCluster->GetOldMUV1Energy()+RecoCluster->GetOldMUV2Energy());

			double MUV1NHits = (fTMVAVariables[5]>0 ? RecoCluster->GetMUV1Candidate()->GetNHits() : -1);
			double MUV2NHits = (fTMVAVariables[4]>0 ? RecoCluster->GetMUV2Candidate()->GetNHits() : -1);

			fTMVAVariables[10] = (MUV1NHits)/(RecoCluster->GetOldMUV1Energy()+1) + 2.*(MUV2NHits)/(RecoCluster->GetOldMUV2Energy()+1);

			double LKrSeed = (fTMVAVariables[3]>0 ? RecoCluster->GetLKrCandidate()->GetClusterSeedEnergy() : -1);
			double LKrNCells = (fTMVAVariables[3]>0 ? RecoCluster->GetLKrCandidate()->GetNCells() : -1);

			fTMVAVariables[11] = LKrSeed / RecoCluster->GetLKrEnergy();
			fTMVAVariables[12] = LKrNCells / RecoCluster->GetLKrEnergy();

			double Distances[3];

			TVector2 LKrPosition = (RecoCluster->GetLKrEnergy() > 0 ? TVector2(RecoCluster->GetLKrCandidate()->GetClusterX(),RecoCluster->GetLKrCandidate()->GetClusterY()) : TVector2(0,0));
			TVector2 MUV1Position = (RecoCluster->GetMUV1Energy()>0 ? RecoCluster->GetMUV1Candidate()->GetPosition() : TVector2(0,0));
			TVector2 MUV2Position = (RecoCluster->GetMUV2Energy()>0 ? RecoCluster->GetMUV2Candidate()->GetPosition() : TVector2(0,0));

			Distances[0] = (LKrPosition - PosAtLKr).Mod();
			Distances[1] = (MUV1Position - PosAtMUV1).Mod();
			Distances[2] = (MUV2Position - PosAtMUV2).Mod();

			fTMVAVariables[13] = (fTMVAVariables[3]>0 ? Distances[0] : -1);
			fTMVAVariables[14] = (fTMVAVariables[5]>0 ? Distances[1] : -1);
			fTMVAVariables[15] = (fTMVAVariables[4]>0 ? Distances[2] : -1);

			for (int i=0; i<16; i++)
				if (IsNaN(fTMVAVariables[i])) fTMVAVariables[i]=-1;



			vector<float> Probabilities = fTMVAReader->EvaluateMulticlass("BDT");

			RecoCluster->SetIsPionOldProbability(Probabilities[0]);
			RecoCluster->SetIsMuonOldProbability(Probabilities[1]);
			RecoCluster->SetIsElectronOldProbability(Probabilities[2]);

			//"EoP","LKrEoP","MUV1Fr","HACFr","MUV1Weight","HACWeight","LKrRMS","HACSW","LKrSeedFr","LKrNCellsFr","LKrDist","MUV1Dist"
			fNewTMVAVariables[0] = RecoCluster->GetHadronicEnergy()/Track->GetMomentum();
			fNewTMVAVariables[1] = RecoCluster->GetLKrEnergy()/Track->GetMomentum();
			fNewTMVAVariables[2] = (HACEnergy>0) ? MUV1Energy/HACEnergy : 0;
			fNewTMVAVariables[3] = HACEnergy/TotalEnergy;
			fNewTMVAVariables[4] = (MUV1Energy>0) ? RecoCluster->GetMUV1Weight() : -1;
			fNewTMVAVariables[5] = (HACEnergy>0) ? RecoCluster->GetHACWeight() : -1;
			fNewTMVAVariables[6] = Sqrt(RMSx*RMSx + RMSy*RMSy)/2.;
			fNewTMVAVariables[7] = (HACEnergy>0) ? RecoCluster->GetHACShowerWidth() : -1;
			fNewTMVAVariables[8] = (LKrEnergy>0) ? RecoCluster->GetLKrCandidate()->GetClusterSeedEnergy()/RecoCluster->GetLKrEnergy() : -1;
			fNewTMVAVariables[9] = (LKrEnergy>0) ? RecoCluster->GetLKrCandidate()->GetNCells()/RecoCluster->GetLKrEnergy() : -1;
			fNewTMVAVariables[10] = Distances[0];
			fNewTMVAVariables[11] = (HACEnergy>0) ? Distances[1] : -1;

			Probabilities = fTMVAReaderNew->EvaluateMulticlass("NewBDT");

			RecoCluster->SetIsPionProbability(Probabilities[1]);
			RecoCluster->SetIsMuonProbability(Probabilities[0]);
			RecoCluster->SetIsElectronProbability(Probabilities[2]);
		}
	}
}

void SpectrometerCalorimetersAssociation::PostProcess(){}

void SpectrometerCalorimetersAssociation::EndOfBurstUser(){}

void SpectrometerCalorimetersAssociation::EndOfJobUser(){
	SaveAllPlots();
}

void SpectrometerCalorimetersAssociation::DrawPlot(){}

SpectrometerCalorimetersAssociation::~SpectrometerCalorimetersAssociation(){
	fLKrCandidates  -> Delete();
	fMUV1Candidates -> Delete();
	fMUV2Candidates -> Delete();
	fReconstructedCluster -> Delete();

	fTMVAReader->Delete();

	delete fLKrCandidates;
	delete fMUV1Candidates;
	delete fMUV2Candidates;
	delete fReconstructedCluster;
}

void SpectrometerCalorimetersAssociation::MatchLKrCandidate (double TrackTime, TVector2 PosAtLKr, CalorimeterCluster *RecoCluster){

	TRecoLKrEvent *LKrEv = GetEvent<TRecoLKrEvent>();
	TClonesArray *LKrHits  = LKrEv->GetHits();

	double mindiscr = 9.e+5;
	int minindex = -1;

	for (int i=0; i<LKrEv->GetNCandidates(); i++){
		TRecoLKrCandidate *cand = static_cast<TRecoLKrCandidate*>(LKrEv->GetCandidate(i));

		TVector2 CandPos (cand->GetClusterX(),cand->GetClusterY());
		double CandTime = cand->GetTime();

		double discr = Sqrt((CandTime-TrackTime)*(CandTime-TrackTime)/(5.*5.) + (PosAtLKr-CandPos).Mod2()/(20.*20.));

		if (discr>mindiscr) continue;

		mindiscr = discr;
		minindex = i;
	}

	if (minindex >= 0){
		TRecoLKrCandidate *cand = static_cast<TRecoLKrCandidate*>(LKrEv->GetCandidate(minindex));
		TVector2 CandPos (cand->GetClusterX(),cand->GetClusterY());
		double CandTime = cand->GetTime();

		if (Abs(CandTime-TrackTime)>15.) minindex=-1;
		if ((CandPos - PosAtLKr).Mod()>60.) minindex=-1;
	}

	if (minindex<0){ ReconstructLKrCandidate(TrackTime,PosAtLKr,RecoCluster); }
	else{

		TRecoLKrCandidate *cand = static_cast<TRecoLKrCandidate*>(LKrEv->GetCandidate(minindex));
		RecoCluster->SetLKrCandidate(cand);
		RecoCluster->SetLKrTime(cand->GetTime());
		RecoCluster->SetLKrEnergy(cand->GetClusterEnergy());

		int seedID = cand->GetIdSeed();
		int LKrSeedIndex[2] = {seedID/1000,seedID%1000};
		double LKrOuterEnergy = 0;
		double LKrSeedTime = cand->GetTime();

		TVector2 ThisClusterPosition (cand->GetClusterX(),cand->GetClusterY());

		double LKrWeight = 0;
		double LKrRawEnergy = 0;

		for (int i=0; i<LKrEv->GetNHits(); i++){
			TRecoLKrHit *Hit = static_cast<TRecoLKrHit*>(LKrHits->At(i));

			if (Hit->GetEnergy()<0) continue;
			if (Abs(Hit->GetTime() - LKrSeedTime)>25.) continue;
			int x_dist = Abs(Hit->GetXCellID() - LKrSeedIndex[0]);
			int y_dist = Abs(Hit->GetYCellID() - LKrSeedIndex[1]);

			if (x_dist<5 && y_dist<5){

				double CellWeight = cand->GetClusterEnergy()*LKrCellEnergyFraction(cand->GetClusterEnergy(),(Hit->GetPosition().XYvector()-ThisClusterPosition).Mod());
				double CellWeightSum = CellWeight;

				for (int icand=0; icand<LKrEv->GetNCandidates(); icand++){
					if (icand==minindex) continue;

					TRecoLKrCandidate *TestCand = static_cast<TRecoLKrCandidate*>(LKrEv->GetCandidate(icand));

					if (Abs(TestCand->GetTime() - LKrSeedTime) > 25) continue;

					TVector2 ClusterPosition (TestCand->GetClusterX(),TestCand->GetClusterY());
					TVector2 HitPosition = Hit->GetPosition().XYvector();
					double dist = (HitPosition - ClusterPosition).Mod();

					if (dist>250.) continue;

					CellWeightSum += TestCand->GetClusterEnergy()*LKrCellEnergyFraction(TestCand->GetClusterEnergy(),(HitPosition-ClusterPosition).Mod());
				}

				LKrWeight += Hit->GetEnergy()*Hit->GetEnergy() * CellWeight * CellWeight / (CellWeightSum * CellWeightSum);
				LKrRawEnergy += Hit->GetEnergy() * CellWeight / CellWeightSum;

				continue;
			}
			LKrOuterEnergy+=Hit->GetEnergy();
		}

		LKrWeight /= LKrRawEnergy * LKrRawEnergy;
		LKrWeight /= 1.07309997 * 1.07309997;
		RecoCluster->SetLKrWeight(LKrWeight);
		RecoCluster->SetLKrOuterEnergy(LKrOuterEnergy);

		//cout <<"\nWith Matched cluster: Energy "<<RecoCluster->GetLKrEnergy()<<" Weight "<<RecoCluster->GetLKrWeight()<<" RMSX "<<cand->GetClusterRMSX()<<" RMSY "<<cand->GetClusterRMSY()<<endl;

		//ReconstructLKrCandidate(TrackTime,PosAtLKr,RecoCluster);

		//cout <<"With Reconstructed cluster: Energy "<<RecoCluster->GetLKrEnergy()<<" Weight "<<RecoCluster->GetLKrWeight()<<" RMSX "<<RecoCluster->GetLKrCandidate()->GetClusterRMSX()<<" RMSY "<<RecoCluster->GetLKrCandidate()->GetClusterRMSY()<<endl;
	}
}

void SpectrometerCalorimetersAssociation::ReconstructLKrCandidate  (double TrackTime, TVector2 PosAtLKr, CalorimeterCluster *RecoCluster){

	if (fDebug) thisstream<<"\n\nBuilding LKr cluster for track at "<<PosAtLKr.X()<<" "<<PosAtLKr.Y()<<" time "<<TrackTime<<endl;

	TRecoLKrEvent *LKrEv = GetEvent<TRecoLKrEvent>();
	TClonesArray *LKrHits  = LKrEv->GetHits();
	int NLKrHits  = LKrEv->GetNHits();
	LKrMatchingHits.clear();

	double EnergyLKr = 0.;
	double LKrSeedEnergy = 0;
	TVector2 LKrPosition (0,0);

	int LKrSeedIndex[2] = {0,0};
	TVector2 LKrSeedPosition;
	double LKrSeedTime = 0;
	double LKrTotalEnergy = 0;
	double LKrOuterEnergy = 0;


	TRecoLKrCandidate *LKrRecoCandidate = nullptr;

	for (int iCell=0; iCell<NLKrHits; iCell++){
		TRecoLKrHit *Hit = static_cast<TRecoLKrHit*>(LKrHits->At(iCell));

		if (Hit->GetEnergy()>40.e+3 || Hit->GetEnergy()<=0.) continue;

		TVector2 HitPosition = Hit->GetPosition().XYvector();
		double dT = Hit->GetTime() - TrackTime;

		if ((HitPosition - PosAtLKr).Mod()<30.){
			FillHisto("LKrdtAtExtrapPos",Hit->GetXCellID()/4+32*Hit->GetYCellID()/4,dT);
		}

		if (Abs(dT) > 25.) continue;

		LKrTotalEnergy += Hit->GetEnergy();

		if ((HitPosition - PosAtLKr).Mod()>200.){
			LKrOuterEnergy+=Hit->GetEnergy();
			continue;
		}

		if (fDebug) thisstream<<"Found Hit at "<<Form("%.1f,%.1f",HitPosition.X(),HitPosition.Y())<<" cell "<<Form("%d,%d",Hit->GetXCellID(),Hit->GetYCellID())<<" dT "<<dT<<" energy "<<Hit->GetEnergy()<<endl;

		LKrMatchingHits.push_back(Hit);

		if (Hit->GetEnergy()<40) continue;

		if (Hit->GetEnergy()>LKrSeedEnergy && (HitPosition - PosAtLKr).Mod()<50.) {
			LKrSeedEnergy = Hit->GetEnergy();
			LKrSeedIndex[0] = Hit->GetXCellID();
			LKrSeedIndex[1] = Hit->GetYCellID();
			LKrSeedPosition = HitPosition;
			LKrSeedTime = Hit->GetTime();
			if (fDebug) thisstream<<"Seed candidate found!"<<endl;
		}
	}

	if (LKrSeedEnergy>0){

		LKrRecoCandidate = static_cast<TRecoLKrCandidate*>(fLKrCandidates->ConstructedAt(fLKrCandidates->GetEntries()));
		RecoCluster->SetLKrCandidate(LKrRecoCandidate);

		double EnergyLKrNN[15]={0};
		double TimeLKrNN[15]={0};
		TVector2 LKrPositionNN[15];
		int LKrNCellsNN[15]={0};
		double LKrWeightNN[15]={0};
		double LKrOuterEnergyNN[15]={0};
		vector <int> LKrHitIndexesNN[15];
		double RMSX=0, RMSY=0, SUMX=0, SUMY=0, SUME=0;
		double ClusterEnergyAssumption = 0;
		TVector2 ClusterPosAssumption(0,0);

		for (unsigned int i=0; i<LKrMatchingHits.size(); i++){

			if (Abs(LKrMatchingHits[i]->GetTime() - LKrSeedTime)>15) continue;

			int xindex = abs(LKrMatchingHits[i]->GetXCellID() - LKrSeedIndex[0]);
			int yindex = abs(LKrMatchingHits[i]->GetYCellID() - LKrSeedIndex[1]);

			int index = Max(xindex,yindex);

			if (index>1) continue;

			ClusterEnergyAssumption += LKrMatchingHits[i]->GetEnergy();
			ClusterPosAssumption += LKrMatchingHits[i]->GetEnergy()*LKrMatchingHits[i]->GetPosition().XYvector();
		}

		ClusterPosAssumption *= 1./ClusterEnergyAssumption;
		ClusterEnergyAssumption /= 0.67;

		if (fDebug)	thisstream<<"LKr Cluster Seed at "<<LKrSeedIndex[0]<<","<<LKrSeedIndex[1]<<" Time "<<LKrSeedTime<<" Energy "<<LKrSeedEnergy*1.e-3<<" GeV "<<" Pos Assumption "<<ClusterPosAssumption.X()<<","<<ClusterPosAssumption.Y()<<endl;

		for (unsigned int i=0; i<LKrMatchingHits.size(); i++){

			if (Abs(LKrMatchingHits[i]->GetTime() - LKrSeedTime)>25) continue;

			int xindex = Abs(LKrMatchingHits[i]->GetXCellID() - LKrSeedIndex[0]);
			int yindex = Abs(LKrMatchingHits[i]->GetYCellID() - LKrSeedIndex[1]);

			int index = Max(xindex,yindex);

			if (index>=15) continue;

			TVector2 HitPosition = LKrMatchingHits[i]->GetPosition().XYvector();
			double CellWeight = ClusterEnergyAssumption*LKrCellEnergyFraction(ClusterEnergyAssumption,(HitPosition-ClusterPosAssumption).Mod());
			double CellWeightSum = CellWeight;

			int Nnear = 0;
			double Wnearest = 0;
			double Dnearest = 9999;

			for (int icand=0; icand<LKrEv->GetNCandidates(); icand++){
				TRecoLKrCandidate *TestCand = static_cast<TRecoLKrCandidate*>(LKrEv->GetCandidate(icand));
				if (Abs(TestCand->GetTime() - LKrSeedTime) > 25) continue;

				TVector2 ClusterPosition (TestCand->GetClusterX(),TestCand->GetClusterY());

				double dist = (ClusterPosition - ClusterPosAssumption).Mod();
				if ((ClusterPosition-HitPosition).Mod()>500.) continue;

				double WeightVal = TestCand->GetClusterEnergy()*LKrCellEnergyFraction(TestCand->GetClusterEnergy(),(ClusterPosition-HitPosition).Mod());
				if (dist<50.){
					Nnear++;
					if (dist<Dnearest){
						Dnearest = dist;
						Wnearest = WeightVal;
					}
				}
				CellWeightSum += WeightVal;
			}

			if (Nnear>0) CellWeightSum -= Wnearest;

			EnergyLKrNN[index] += LKrMatchingHits[i]->GetEnergy() * CellWeight/CellWeightSum;
			TimeLKrNN[index] += LKrMatchingHits[i]->GetTime() * LKrMatchingHits[i]->GetEnergy() * CellWeight/CellWeightSum;
			LKrPositionNN[index] += HitPosition * LKrMatchingHits[i]->GetEnergy()  * CellWeight/CellWeightSum;
			LKrNCellsNN[index]++;
			LKrWeightNN[index] += LKrMatchingHits[i]->GetEnergy()*LKrMatchingHits[i]->GetEnergy() * CellWeight/CellWeightSum * CellWeight/CellWeightSum;
			LKrHitIndexesNN[index].push_back(LKrHits->IndexOf(LKrMatchingHits[i]));
			LKrOuterEnergyNN[index] += LKrMatchingHits[i]->GetEnergy() * (1.-CellWeight/CellWeightSum);

			if (index<=3){
				RMSX += xindex*xindex * LKrMatchingHits[i]->GetEnergy()  * CellWeight/CellWeightSum;
				RMSY += yindex*yindex * LKrMatchingHits[i]->GetEnergy()  * CellWeight/CellWeightSum;
				SUMX += xindex*LKrMatchingHits[i]->GetEnergy() * CellWeight/CellWeightSum;;
				SUMY += yindex*LKrMatchingHits[i]->GetEnergy() * CellWeight/CellWeightSum;;
				SUME += LKrMatchingHits[i]->GetEnergy() * CellWeight/CellWeightSum;
			}
		}

		int EnWeight33 = 0;
		int stop=6;

		double TimeLKr = 0.;
		double LKr77Energy = 0;
		double LKrWeight = 0.;
		int LKrNCells = 0;

		for (int j=0; j<10; j++){

			if (j>=stop){
				LKrOuterEnergyNN[j]+=EnergyLKrNN[j];
				continue;
			}

			if (j==0) LKrSeedEnergy = EnergyLKrNN[j];

			EnergyLKr += EnergyLKrNN[j];
			if (j<3) {
				TimeLKr += TimeLKrNN[j];
				LKrPosition += LKrPositionNN[j];
				EnWeight33+=EnergyLKrNN[j];
			}
			LKrNCells += LKrNCellsNN[j];
			LKrWeight += LKrWeightNN[j];
			if (j<=3) LKr77Energy+=EnergyLKrNN[j];

			for (unsigned int k=0; k<LKrHitIndexesNN[j].size(); k++) LKrRecoCandidate->AddHit(LKrHitIndexesNN[j][k]);
		}

		if (LKrSeedEnergy/EnergyLKr > 1.) {
			cout<< user_normal() << "LKr Seed Energy / Energy > 1 ("<<LKrSeedEnergy/EnergyLKr<<") " <<endl;
			cout << user_normal() << thisstream.str().c_str()<<endl;
		}

		TimeLKr /= EnWeight33;
		LKrPosition /= EnWeight33;

		EnergyLKr *= 1.07309997;
		LKrWeight /= EnergyLKr*EnergyLKr;

		RMSX /= SUME;
		RMSY /= SUME;

		SUMX /= SUME;
		SUMY /= SUME;

		RMSX -= SUMX*SUMX;
		RMSY -= SUMY*SUMY;

		RMSX = Sqrt(Max(0.,RMSX));
		RMSY = Sqrt(Max(0.,RMSY));

		const double xcell = 19.7383881;
		double ixc = int(64. + LKrPosition.X()/xcell);
		double xc = LKrPosition.X() - xcell * (ixc - 0.5*127.);
		xc *= 0.1;
		RMSX /= 1. + 0.111699998*xc*xc;

		const double ycell = 19.7383881;
		double iyc = int(64. + LKrPosition.Y()/ycell);
		double yc = LKrPosition.Y() - ycell * (iyc - 0.5*127.);
		yc *= 0.1;
		RMSY /= 1. + 0.0895999968 * yc*yc;

		LKrRecoCandidate->SetTime(TimeLKr);
		LKrRecoCandidate->SetNCells(LKrNCells);
		LKrRecoCandidate->SetClusterEnergy(EnergyLKr);
		LKrRecoCandidate->SetCluster77Energy(LKr77Energy);
		LKrRecoCandidate->SetClusterSeedEnergy(LKrSeedEnergy);
		LKrRecoCandidate->SetClusterX(LKrPosition.X());
		LKrRecoCandidate->SetClusterY(LKrPosition.Y());
		LKrRecoCandidate->SetClusterRMSX(20.*RMSX);
		LKrRecoCandidate->SetClusterRMSY(20.*RMSY);
		LKrRecoCandidate->SetIdSeed(LKrSeedIndex[0]*1000+LKrSeedIndex[1]);
		//Apply Standard Cluster Corrections
		EnergyLKr = LKrClusterCorrectionFunctions::CorrectedEnergy(LKrRecoCandidate,GetWithMC());
		LKrRecoCandidate->SetClusterEnergy(EnergyLKr);
		LKrPosition.Set(LKrRecoCandidate->GetClusterX(),LKrRecoCandidate->GetClusterY());

		RecoCluster->SetLKrTime(TimeLKr);
		RecoCluster->SetLKrEnergy(EnergyLKr);
		RecoCluster->SetLKrWeight(LKrWeight);

		for (int i=0; i<15; i++){
			LKrOuterEnergy += LKrOuterEnergyNN[i];
		}

		for (int iCell=0; iCell<NLKrHits; iCell++){

			TRecoLKrHit *Hit = static_cast<TRecoLKrHit*>(LKrHits->At(iCell));
			if (Hit->GetEnergy()<0) continue;
			if (Abs(Hit->GetTime() - LKrSeedTime)>25.) continue;

			int x_dist = Hit->GetXCellID() - LKrSeedIndex[0];
			int y_dist = Hit->GetYCellID() - LKrSeedIndex[1];
			if (x_dist<15 && y_dist<15) continue;

			LKrOuterEnergy+=Hit->GetEnergy();
		}
		RecoCluster->SetLKrOuterEnergy(LKrOuterEnergy);
	}
	else RecoCluster->SetLKrOuterEnergy(LKrTotalEnergy);
}

void SpectrometerCalorimetersAssociation::ReconstructMUV1Candidate  (double TrackTime, TVector2 PosAtMUV1, CalorimeterCluster *RecoCluster, bool dry){

	if (!dry){
		memset(HACChannelEnergy[0],0,22*sizeof(double));
		memset(HACChannelEnergy[1],0,22*sizeof(double));
	}

	if (fDebug){
		thisstream<<"\n\nLooking in MUV1 for track at "<<PosAtMUV1.X()<<" "<<PosAtMUV1.Y()<<endl;
		auto channels = fMUV12CorrHandler->GetMUV1Channels(PosAtMUV1.X(), PosAtMUV1.Y());
		thisstream<<"Seed expected at "<<channels.first<<" "<<channels.second<<endl;
		thisstream<<"LKr Energy "<<RecoCluster->GetLKrEnergy()<<endl;
	}

	TRecoMUV1Event *MUV1Ev = GetEvent<TRecoMUV1Event>();
	TClonesArray *MUV1Hits = MUV1Ev->GetHits();
	int NMUV1Hits = MUV1Ev->GetNHits();
	MUV1MatchingHits_H.clear();
	MUV1MatchingHits_V.clear();

	double SeedTimeCut[3] = {20.,20,20};
	if (fUseLKrRefTime && RecoCluster->GetLKrEnergy()>250.){
		SeedTimeCut[0] = 20.; SeedTimeCut[1] = 15.; SeedTimeCut[2] = 10.;
		TrackTime = RecoCluster->GetLKrTime()-2.;
		if (fDebug) thisstream<<"MUV1 Using LKr Time "<<TrackTime<<endl;
	}
	else if (fDebug) thisstream<<"Usign spectrometer time "<<TrackTime<<endl;


	double MUV1SeedTime_H = 0;
	double MUV1SeedTime_V = 0;
	double MUV1SeedTime_H_nc = 0;
	double MUV1SeedTime_V_nc = 0;

	double MUV1SeedEnergy_H = 0;
	double MUV1SeedEnergy_V = 0;

	//  double MUV1SeedPosition_H = 0;
	//  double MUV1SeedPosition_V = 0;

	int MUV1SeedChannel_H = 0;
	int MUV1SeedChannel_V = 0;

	int MUV1SeedSide_H = -1;
	int MUV1SeedSide_V = -1;

	int MUV1InnerEnergy_H = 0;
	int MUV1InnerEnergy_V = 0;

	double MUV1OuterEnergy = 0, MUV1OuterEnergyX=0, MUV1OuterEnergyY=0;
	double MUV1OuterX=0, MUV1OuterY=0;
	double MUV1TotalEnergy = 0;

	TRecoMUV1Candidate *MUV1RecoCandidate = nullptr;

	for (int iHit=0; iHit<NMUV1Hits; iHit++){
		TRecoMUV1Hit *Hit = static_cast<TRecoMUV1Hit*>(MUV1Hits->At(iHit));

		double x = ( (Hit->GetSide()%2) ? PosAtMUV1.Y() : PosAtMUV1.X() );
		double y = ( (Hit->GetSide()%2) ? PosAtMUV1.X() : PosAtMUV1.Y() );
		double Time = Hit->GetTime() + fMUV12CorrHandler->GetMUV1TimeCorrection(Hit->GetChannelID(),y);
		double HitSeedEnergy = Hit->GetCharge() * fMUV12CorrHandler->GetMUV1ChargeCorrection(Hit->GetChannelID(),60.*(Hit->GetSide()>=2 ? +1. : -1.));

		if (!Hit->IsLongScintillator()){
			if (Hit->GetSide()<2 && y>0) y = -60.;
			else if (Hit->GetSide()>=2 && y<0) y = 60.;
		}

		int idx = static_cast<int>(Abs(Hit->GetScintillatorPosition() - x)/35.);
		if (idx>2) idx=2;

		if (Abs(Time - TrackTime) > 3*SeedTimeCut[idx]) continue;

		if (fDebug){
			thisstream<<" Found hit on channel "<<Hit->GetChannelID()<<" time "<<Time<<" energy "<<HitSeedEnergy/1.575<<endl;
		}

		if (Abs(Time - TrackTime) > SeedTimeCut[idx]) continue;

		MUV1TotalEnergy += HitSeedEnergy/(1.575 * (Hit->IsLongScintillator() ? 2 : 1));
		if (Hit->GetSide()%2==0){
			MUV1OuterEnergyX += HitSeedEnergy/(1.575 * (Hit->IsLongScintillator() ? 2 : 1));
			MUV1OuterX += Hit->GetPosition().X() * HitSeedEnergy/(1.575 * (Hit->IsLongScintillator() ? 2 : 1));
		}
		else{
			MUV1OuterEnergyY += HitSeedEnergy/(1.575 * (Hit->IsLongScintillator() ? 2 : 1));
			MUV1OuterY += Hit->GetPosition().Y() * HitSeedEnergy/(1.575 * (Hit->IsLongScintillator() ? 2 : 1));
		}

		if (Abs(Hit->GetScintillatorPosition() - x) > 300) continue;

		if (Hit->GetSide()%2==0){

			if (HitSeedEnergy > MUV1SeedEnergy_H){

				MUV1SeedEnergy_H = HitSeedEnergy;
				//MUV1SeedPosition_H = Hit->GetScintillatorPosition();
				MUV1SeedChannel_H = Hit->GetScintillatorNumber();
				MUV1SeedSide_H = Hit->GetSide();
				MUV1SeedTime_H = Time;
				MUV1SeedTime_H_nc = Hit->GetTime() + fMUV12CorrHandler->GetMUV1TimeCorrection(Hit->GetChannelID(),0);
			}
		}

		else{

			if (HitSeedEnergy > MUV1SeedEnergy_V){

				MUV1SeedEnergy_V = HitSeedEnergy;
				//MUV1SeedPosition_V = Hit->GetScintillatorPosition();
				MUV1SeedChannel_V = Hit->GetScintillatorNumber();
				MUV1SeedSide_V = Hit->GetSide();
				MUV1SeedTime_V = Time;
				MUV1SeedTime_V_nc = Hit->GetTime() + fMUV12CorrHandler->GetMUV1TimeCorrection(Hit->GetChannelID(),0);
			}
		}
	}

	if (fDebug) thisstream<<"Seeds found at "<< MUV1SeedChannel_H <<" and "<<MUV1SeedChannel_V<<" with "<<MUV1SeedEnergy_H<<" and "<<MUV1SeedEnergy_V<<endl;

	if (MUV1SeedEnergy_H>0 && MUV1SeedEnergy_V>0){
		MUV1OuterEnergyX=0; MUV1OuterEnergyY=0;
		MUV1OuterX=0; MUV1OuterY=0;

		int MUV1OuterNhits = 0;

		if (!dry) MUV1RecoCandidate = static_cast<TRecoMUV1Candidate*>(fMUV1Candidates->ConstructedAt(fMUV1Candidates->GetEntries()));

		MUV1SeedEnergy_H /= 1.575;
		MUV1SeedEnergy_V /= 1.575;

		double MUV1EnergyChannels[2][15];
		double MUV1PositionChannels[2][15];
		double MUV1TimeChannels[2][15];
		int    MUV1NHitsChannels[2][15];
		int    MUV1HitIndexes[2][2][15];

		for (int i=0; i<15; i++){
			MUV1EnergyChannels[0][i] = MUV1EnergyChannels[1][i] = 0.;
			MUV1PositionChannels[0][i] = MUV1PositionChannels[1][i] = 0.;
			MUV1TimeChannels[0][i] = MUV1TimeChannels[1][i] = 0.;
			MUV1NHitsChannels[0][i] = MUV1NHitsChannels[1][i] = 0.;
			MUV1HitIndexes[0][0][i] = MUV1HitIndexes[0][1][i] = -1;
			MUV1HitIndexes[1][0][i] = MUV1HitIndexes[1][1][i] = -1;
		}

		for (int iHit=0; iHit<NMUV1Hits; iHit++){

			TRecoMUV1Hit *Hit = static_cast<TRecoMUV1Hit*>(MUV1Hits->At(iHit));

			int SeedSide = ( (Hit->GetSide()%2) ? MUV1SeedSide_V : MUV1SeedSide_H );

			double y = ( (Hit->GetSide()%2) ? PosAtMUV1.X() : PosAtMUV1.Y() );
			if (!Hit->IsLongScintillator()){
				if (Hit->GetSide()<2 && y>0) y = -60.;
				else if (Hit->GetSide()>=2 && y<0) y = 60.;
			}
			double HitTime = Hit->GetTime() + fMUV12CorrHandler->GetMUV1TimeCorrection(Hit->GetChannelID(),y);
			double HitEnergy = Hit->GetCharge() * fMUV12CorrHandler->GetMUV1ChargeCorrection(Hit->GetChannelID(),y) /1.575;

			if (HitEnergy<0) {
				cout << user_normal() << "MUV1 Hit energy < 0! Channel "<<Hit->GetChannelID()<<" y "<<y<<" Hit Charge "<<Hit->GetCharge()<<" correction "<<fMUV12CorrHandler->GetMUV1ChargeCorrection(Hit->GetChannelID(),y)<<endl;
				//continue;
			}
			if (fDebug) thisstream<<Form("Hit on channel %d at %.1f with %.2f\n", Hit->GetChannelID(), HitTime, 1.e-3*HitEnergy);

			if (Hit->GetSide()%2==0 && MUV1SeedEnergy_H>0){
				if (Abs(Hit->GetScintillatorNumber() - MUV1SeedChannel_H) > 7){

					if (fDebug) thisstream <<"Out of Limit"<<endl;
					HitTime = Hit->GetTime() + fMUV12CorrHandler->GetMUV1TimeCorrection(Hit->GetChannelID(),0);

					if (fDebug) thisstream <<"uncorrected hit time (y=0) "<<HitTime<<" versus seed uncorrected "<<MUV1SeedTime_H_nc<<endl;
					if (Abs(HitTime-MUV1SeedTime_H_nc)>10.) continue;

					HitEnergy = Hit->GetCharge() / 1.575;

					if (Hit->IsLongScintillator()) HitEnergy *= 0.5*fMUV12CorrHandler->GetMUV1ChargeCorrection(Hit->GetChannelID(),0);
					else HitEnergy *= fMUV12CorrHandler->GetMUV1ChargeCorrection(Hit->GetChannelID(),(Hit->GetSide()<2 ? -60 : 60));

					if (fDebug) thisstream <<"adding "<<1.e-3*HitEnergy<<" Gev outer energy"<<endl;
					MUV1OuterEnergy += HitEnergy;
					MUV1OuterEnergyX += HitEnergy;
					MUV1OuterX += HitEnergy*Hit->GetPosition().X();
					MUV1OuterNhits ++;

					continue;
				}

				if (Abs(HitTime - MUV1SeedTime_H) > 10.){
					if (fDebug) thisstream <<"Out of time!"<<endl;
					continue;
				}

				MUV1PositionChannels[0][Hit->GetScintillatorNumber() - MUV1SeedChannel_H + 7] = Hit->GetScintillatorPosition();

				if (Hit->IsLongScintillator()){

					if (!GetWithMC()){
						HitEnergy *= 1.;
						HitEnergy *= 1.002;
					}
					if (fDebug) thisstream <<"Added"<<endl;
					MUV1EnergyChannels[0][Hit->GetScintillatorNumber() - MUV1SeedChannel_H + 7]+= HitEnergy;
					MUV1TimeChannels[0][Hit->GetScintillatorNumber() - MUV1SeedChannel_H + 7]+= HitTime * HitEnergy;
					MUV1NHitsChannels[0][Hit->GetScintillatorNumber() - MUV1SeedChannel_H + 7]++;
					MUV1MatchingHits_H.push_back(Hit);
					MUV1HitIndexes[0][Hit->GetSide()/2][Hit->GetScintillatorNumber() - MUV1SeedChannel_H + 7] = iHit;

				}
				else
				{
					if (SeedSide!=Hit->GetSide() && Abs(MUV1SeedChannel_V-21.5)>5){ if (fDebug) thisstream<<"Skipping, wrong side"<<endl; continue; }
					HitEnergy = Hit->GetCharge() * fMUV12CorrHandler->GetMUV1ChargeCorrection(Hit->GetChannelID(),y) /1.575;
					if (!GetWithMC()){
						HitEnergy *= 1.01;
						HitEnergy *= 1.002;
					}
					if (fDebug) thisstream <<"Added"<<endl;
					MUV1EnergyChannels[0][Hit->GetScintillatorNumber() - MUV1SeedChannel_H + 7]+= HitEnergy;
					MUV1TimeChannels[0][Hit->GetScintillatorNumber() - MUV1SeedChannel_H + 7]+= HitTime * HitEnergy;
					MUV1NHitsChannels[0][Hit->GetScintillatorNumber() - MUV1SeedChannel_H + 7] = 1;
					MUV1MatchingHits_H.push_back(Hit);
					MUV1HitIndexes[0][Hit->GetSide()/2][Hit->GetScintillatorNumber() - MUV1SeedChannel_H + 7] = iHit;

				}
			}

			else if (Hit->GetSide()%2==1 && MUV1SeedEnergy_V>0) {

				if (Abs(Hit->GetScintillatorNumber() - MUV1SeedChannel_V) > 7){

					if (fDebug) thisstream <<"Out of Limit"<<endl;

					HitTime = Hit->GetTime() + fMUV12CorrHandler->GetMUV1TimeCorrection(Hit->GetChannelID(),0);

					if (fDebug) thisstream <<"uncorrected hit time (y=0) "<<HitTime<<" versus seed uncorrected "<<MUV1SeedTime_V_nc<<endl;

					if (Abs(HitTime-MUV1SeedTime_V_nc)>10.) continue;

					HitEnergy = Hit->GetCharge() / 1.575;

					if (Hit->IsLongScintillator()) HitEnergy *= 0.5*fMUV12CorrHandler->GetMUV1ChargeCorrection(Hit->GetChannelID(),0);
					else HitEnergy *= fMUV12CorrHandler->GetMUV1ChargeCorrection(Hit->GetChannelID(),(Hit->GetSide()<2 ? -60 : 60));

					if (fDebug) thisstream <<"adding "<<1.e-3*HitEnergy<<" Gev outer energy"<<endl;

					MUV1OuterEnergy += HitEnergy;
					MUV1OuterEnergyY += HitEnergy;
					MUV1OuterY += HitEnergy*Hit->GetPosition().Y();
					MUV1OuterNhits ++;
					continue;
				}

				if (Abs(Hit->GetTime() - MUV1SeedTime_V + fMUV12CorrHandler->GetMUV1TimeCorrection(Hit->GetChannelID(),y) ) > 10.) continue;

				MUV1PositionChannels[1][Hit->GetScintillatorNumber() - MUV1SeedChannel_V + 7] = Hit->GetScintillatorPosition();

				if (Hit->IsLongScintillator()){
					HitEnergy = Hit->GetCharge() * fMUV12CorrHandler->GetMUV1ChargeCorrection(Hit->GetChannelID(),y) /1.575;
					if (!GetWithMC()){
						HitEnergy *= 1.10;
						HitEnergy *= 1.002;
					}
					if (fDebug) thisstream <<"Added"<<endl;
					MUV1EnergyChannels[1][Hit->GetScintillatorNumber() - MUV1SeedChannel_V + 7]+= HitEnergy;
					MUV1TimeChannels[1][Hit->GetScintillatorNumber() - MUV1SeedChannel_V + 7]+= HitTime * HitEnergy;
					MUV1NHitsChannels[1][Hit->GetScintillatorNumber() - MUV1SeedChannel_V + 7]++;
					MUV1MatchingHits_V.push_back(Hit);
					MUV1HitIndexes[1][Hit->GetSide()/2][Hit->GetScintillatorNumber() - MUV1SeedChannel_V + 7] = iHit;

				}
				else
				{
					if (SeedSide!=Hit->GetSide() && Abs(MUV1SeedChannel_H-21.5)>5) { if (fDebug) thisstream<<"Skipping, wrong side"<<endl; continue; }
					HitEnergy = Hit->GetCharge() * fMUV12CorrHandler->GetMUV1ChargeCorrection(Hit->GetChannelID(),y) /1.575;
					if (!GetWithMC()){
						HitEnergy *= 1.13;
						HitEnergy *= 1.002;
					}
					if (fDebug) thisstream <<"Added"<<endl;
					MUV1EnergyChannels[1][Hit->GetScintillatorNumber() - MUV1SeedChannel_V + 7]+= HitEnergy;
					MUV1TimeChannels[1][Hit->GetScintillatorNumber() - MUV1SeedChannel_V + 7]+= HitTime * HitEnergy;
					MUV1NHitsChannels[1][Hit->GetScintillatorNumber() - MUV1SeedChannel_V + 7] = 1;
					MUV1MatchingHits_V.push_back(Hit);
					MUV1HitIndexes[1][Hit->GetSide()/2][Hit->GetScintillatorNumber() - MUV1SeedChannel_V + 7] = iHit;
				}
			}
		}

		double MUV1Energy_H = 0;
		double MUV1Energy_V = 0;

		double MUV1Time_H = 0;
		double MUV1Time_V = 0;
		double MUV1Time = 0;

		double MUV1Position_H = 0;
		double MUV1Position_V = 0;

		double MUV1Weight = 0;
		double MUV1Weight_H = 0;
		double MUV1Weight_V = 0;
		double MUV1WeightOld = 0;
		double MUV1WeightOld_H = 0;
		double MUV1WeightOld_V = 0;

		double MUV1ShowerWidth = 0;
		double MUV1ShowerWidth_H = 0;
		double MUV1ShowerWidth_V = 0;

		if (!dry){

			for (int i=0; i<15; i++){

				int ChID_H = MUV1SeedChannel_H-7+i;
				ChID_H = static_cast<int>((ChID_H+1.)/2.);
				HACChannelEnergy[0][ChID_H-1] += MUV1EnergyChannels[0][i];

				int ChID_V = MUV1SeedChannel_V-7+i;
				ChID_V = static_cast<int>((ChID_V+1.)/2.);
				HACChannelEnergy[1][ChID_V-1] += MUV1EnergyChannels[1][i];

			}
		}

		if (fDebug){
			thisstream <<"Looking for cluster limits"<<endl;
			thisstream <<"Energy H: ";

			for_each(MUV1EnergyChannels[0], MUV1EnergyChannels[0]+15, [&] (float x){ thisstream <<" "<<1.e-3*x;} );
			thisstream <<endl;
			thisstream <<"Energy V: ";
			for_each(MUV1EnergyChannels[1], MUV1EnergyChannels[1]+15, [&] (float x){ thisstream <<" "<<1.e-3*x;} );
			thisstream <<endl;

		}

		int startH=0, startV=0;
		for (int i=6; i>=0; i--) {
			if (startH<i && MUV1EnergyChannels[0][i]<=0 && MUV1EnergyChannels[0][i+1]<=0) startH=i;
			if (startV<i && MUV1EnergyChannels[1][i]<=0 && MUV1EnergyChannels[1][i+1]<=0) startV=i;
		}

		int stopH=15, stopV=15;
		for (int i=14; i>=8; i--) {
			if (stopH>i && MUV1EnergyChannels[0][i]<=0 && MUV1EnergyChannels[0][i-1]<=0) stopH=i;
			if (stopV>i && MUV1EnergyChannels[1][i]<=0 && MUV1EnergyChannels[1][i-1]<=0) stopV=i;
		}


		if (fDebug) thisstream <<Form("cluster H [ %d %d ] V [ %d %d ] \n",startH,stopH,startV,stopV);


		for (int i=0; i<15; i++){
			if (MUV1EnergyChannels[0][i]>0){
				if (i>=startH && i<=stopH){
					MUV1TimeChannels[0][i] /= MUV1EnergyChannels[0][i];
					MUV1EnergyChannels[0][i] /= MUV1NHitsChannels[0][i];
					MUV1Energy_H += MUV1EnergyChannels[0][i];
					MUV1Position_H += MUV1PositionChannels[0][i] * MUV1EnergyChannels[0][i];
					MUV1Time_H += MUV1TimeChannels[0][i] * MUV1EnergyChannels[0][i];
					MUV1Weight += Power(MUV1EnergyChannels[0][i],4.);
					MUV1Weight_H += Power(MUV1EnergyChannels[0][i],4.);
					MUV1WeightOld += Power(MUV1EnergyChannels[0][i],2.);
					MUV1WeightOld_H += Power(MUV1EnergyChannels[0][i],2.);
					if (abs(i-7)<2) MUV1InnerEnergy_H += MUV1EnergyChannels[0][i];
					if (MUV1HitIndexes[0][0][i]>=0 && !dry) MUV1RecoCandidate->AddHit(MUV1HitIndexes[0][0][i]);
					if (MUV1HitIndexes[0][1][i]>=0 && !dry) MUV1RecoCandidate->AddHit(MUV1HitIndexes[0][1][i]);
				}
				else if (MUV1EnergyChannels[0][i]>0) {
					MUV1OuterEnergy += MUV1EnergyChannels[0][i];
					MUV1OuterEnergyX += MUV1EnergyChannels[0][i];
					MUV1OuterX += MUV1EnergyChannels[0][i]*MUV1PositionChannels[0][i];
					MUV1OuterNhits ++;
				}
			}

			if (MUV1EnergyChannels[1][i]>0){

				if (i>=startV && i<=stopV){
					MUV1TimeChannels[1][i] /= MUV1EnergyChannels[1][i];
					MUV1EnergyChannels[1][i] /= MUV1NHitsChannels[1][i];
					MUV1Energy_V += MUV1EnergyChannels[1][i];
					MUV1Position_V += MUV1PositionChannels[1][i] * MUV1EnergyChannels[1][i];
					MUV1Time_V += MUV1TimeChannels[1][i] * MUV1EnergyChannels[1][i];
					MUV1Weight += Power(MUV1EnergyChannels[1][i],4.);
					MUV1Weight_V += Power(MUV1EnergyChannels[1][i],4.);
					MUV1WeightOld += Power(MUV1EnergyChannels[1][i],2.);
					MUV1WeightOld_V += Power(MUV1EnergyChannels[1][i],2.);
					if (abs(i-7)<2) MUV1InnerEnergy_V += MUV1EnergyChannels[1][i];
					if (MUV1HitIndexes[1][0][i]>=0  && !dry) MUV1RecoCandidate->AddHit(MUV1HitIndexes[1][0][i]);
					if (MUV1HitIndexes[1][1][i]>=0  && !dry) MUV1RecoCandidate->AddHit(MUV1HitIndexes[1][1][i]);
				}
				else if (MUV1EnergyChannels[1][i]>0) {
					MUV1OuterEnergy += MUV1EnergyChannels[1][i];
					MUV1OuterEnergyY += MUV1EnergyChannels[1][i];
					MUV1OuterY += MUV1EnergyChannels[1][i]*MUV1PositionChannels[1][i];
					MUV1OuterNhits ++;
				}

			}
		}

		if (MUV1Energy_H>0){
			MUV1Position_H /= MUV1Energy_H;
			MUV1Time_H /= MUV1Energy_H;
			MUV1Weight_H /= Power(MUV1Energy_H,4.);
			MUV1Weight_H = Power(MUV1Weight_H,0.25);
			MUV1WeightOld_H /= Power(MUV1Energy_H,2.);
			MUV1ShowerWidth_H = TMath::RMS(stopH-startH,&MUV1PositionChannels[0][startH],&MUV1EnergyChannels[0][startH]);
		}

		if (MUV1Energy_V>0){
			MUV1Position_V /= MUV1Energy_V;
			MUV1Time_V /= MUV1Energy_V;
			MUV1Weight_V /= Power(MUV1Energy_V,4.);
			MUV1Weight_V = Power(MUV1Weight_V,0.25);
			MUV1WeightOld_V /= Power(MUV1Energy_V,2.);
			MUV1ShowerWidth_V = TMath::RMS(stopV-startV,&MUV1PositionChannels[1][startV],&MUV1EnergyChannels[1][startV]);
		}

		if (MUV1Energy_H>0 && MUV1Energy_V>0){
			MUV1Time = 0.5*(MUV1Time_H + MUV1Time_V);
			MUV1Weight /= Power(MUV1Energy_H + MUV1Energy_V,4.);
			MUV1Weight = Power(MUV1Weight,0.25);
			MUV1WeightOld /= Power(MUV1Energy_H + MUV1Energy_V,2.);
			MUV1ShowerWidth = Sqrt(MUV1ShowerWidth_H*MUV1ShowerWidth_H + MUV1ShowerWidth_V*MUV1ShowerWidth_V);
		}
		else if (MUV1Energy_H>0){
			MUV1Time = MUV1Time_H;
			MUV1Weight = MUV1Weight_H;
			MUV1WeightOld = MUV1WeightOld_H;
			MUV1ShowerWidth = MUV1ShowerWidth_H;
		}
		else{
			MUV1Time = MUV1Time_V;
			MUV1Weight = MUV1Weight_V;
			MUV1WeightOld = MUV1WeightOld_V;
			MUV1ShowerWidth = MUV1ShowerWidth_V;
		}

		if (!dry){
			MUV1RecoCandidate->SetTime(MUV1Time);
			MUV1RecoCandidate->SetPosition(MUV1Position_H,MUV1Position_V);
			MUV1RecoCandidate->SetEnergy(0.5*(MUV1Energy_H+MUV1Energy_V));
			MUV1RecoCandidate->SetCharge(3.15*0.5*(MUV1Energy_H+MUV1Energy_V));
			MUV1RecoCandidate->SetShowerWidth(MUV1ShowerWidth);

			MUV1RecoCandidate->SetTimeHorizontal(MUV1Time_H);
			MUV1RecoCandidate->SetTimeVertical(MUV1Time_V);

			MUV1RecoCandidate->SetEnergyHorizontal(0.5*MUV1Energy_H);
			MUV1RecoCandidate->SetEnergyVertical(0.5*MUV1Energy_V);

			MUV1RecoCandidate->SetChargeHorizontal(3.15*0.5*MUV1Energy_H);
			MUV1RecoCandidate->SetChargeVertical(3.15*0.5*MUV1Energy_V);

			MUV1RecoCandidate->SetShowerWidthHorizontal(MUV1ShowerWidth_H);
			MUV1RecoCandidate->SetShowerWidthVertical(MUV1ShowerWidth_V);

			MUV1RecoCandidate->SetSeedEnergyHorizontal(0.5*MUV1SeedEnergy_H);
			MUV1RecoCandidate->SetSeedEnergyVertical(0.5*MUV1SeedEnergy_V);

			MUV1RecoCandidate->SetInnerEnergyHorizontal(0.5*MUV1InnerEnergy_H);
			MUV1RecoCandidate->SetInnerEnergyVertical(0.5*MUV1InnerEnergy_V);

			MUV1RecoCandidate->SetSeedIndexHorizontal(MUV1SeedChannel_H);
			MUV1RecoCandidate->SetSeedIndexVertical(MUV1SeedChannel_V);

			RecoCluster->SetMUV1Candidate(MUV1RecoCandidate);
			RecoCluster->SetMUV1Energy(0.5*(MUV1Energy_H+MUV1Energy_V));
			RecoCluster->SetMUV1Time(MUV1Time);
			RecoCluster->SetMUV1Weight(MUV1Weight);
			RecoCluster->SetOldMUV1Weight(MUV1WeightOld);
			RecoCluster->SetMUV1OuterEnergy(0.5*MUV1OuterEnergy);
			RecoCluster->SetMUV1OuterNhits(MUV1OuterNhits);
			RecoCluster->SetMUV1OuterPosition((MUV1OuterEnergyX>0?MUV1OuterX/MUV1OuterEnergyX:-9999),(MUV1OuterEnergyY>0?MUV1OuterY/MUV1OuterEnergyY:-9999));
		}
	}
	else if (!dry){
		RecoCluster->SetMUV1OuterEnergy(MUV1TotalEnergy);
		RecoCluster->SetMUV1OuterPosition((MUV1OuterEnergyX>0?MUV1OuterX/MUV1OuterEnergyX:-9999),(MUV1OuterEnergyY>0?MUV1OuterY/MUV1OuterEnergyY:-9999));
	}

	if (!dry){
		RecoCluster->SetMUV1InTimeEnergy(MUV1TotalEnergy);
	}

}

void SpectrometerCalorimetersAssociation::ReconstructMUV2Candidate  (double TrackTime, TVector2 PosAtMUV2, CalorimeterCluster *RecoCluster){
	if (fDebug) thisstream<<"Looking in MUV2 for track at "<<PosAtMUV2.X()<<" "<<PosAtMUV2.Y()<<endl;

	double SeedTimeCut[3] = {20.,20,20};
	if (fUseLKrRefTime && RecoCluster->GetLKrEnergy()>250.){
		SeedTimeCut[0] = 20.; SeedTimeCut[1] = 15.; SeedTimeCut[2] = 10.;
		TrackTime = RecoCluster->GetLKrTime()-2;
		if (fDebug) thisstream <<"Using LKr Time Reference "<<TrackTime<<endl;
	}
	else if (RecoCluster->GetMUV1Energy()>500){
		SeedTimeCut[0] = 20.; SeedTimeCut[1] = 15.; SeedTimeCut[2] = 10.;
		TrackTime = RecoCluster->GetMUV1Time();
		if (fDebug) thisstream <<"Using MUV1 Time Reference "<<TrackTime<<endl;
	}

	auto print = [&] (float x){ thisstream <<" "<<1.e-3*x; };

	float MUV2ChPositions[22];
	for (int i=0; i<22; i++) MUV2ChPositions[i] = fMUV12CorrHandler->GetMUV2ScintillatorCenter(101+i);

	if (fDebug) {
		thisstream <<"Initial HAC map "<<endl;
		for_each(MUV2ChPositions,MUV2ChPositions+22,print);
		thisstream <<endl;
		for_each(HACChannelEnergy[0],HACChannelEnergy[0]+22,print);
		thisstream <<endl;
		for_each(HACChannelEnergy[1],HACChannelEnergy[1]+22,print);
		thisstream <<endl;
		auto channels = fMUV12CorrHandler->GetMUV2Channels(PosAtMUV2.X(), PosAtMUV2.Y());
		thisstream <<"Seed expected at "<<channels.first<<" "<<channels.second<<endl;
	}


	TRecoMUV2Event *MUV2Ev = GetEvent<TRecoMUV2Event>();

	TClonesArray *MUV2Hits = MUV2Ev->GetHits();

	int NMUV2Hits = MUV2Ev->GetNHits();

	MUV2MatchingHits_H.clear();
	MUV2MatchingHits_V.clear();

	double MUV2Energy_H = 0;
	double MUV2Energy_V = 0;

	double MUV2Time_H = 0;
	double MUV2Time_V = 0;
	double MUV2Time = 0;

	double MUV2Position_H = 0;
	double MUV2Position_V = 0;

	double MUV2Weight = 0;
	double MUV2Weight_H = 0;
	double MUV2Weight_V = 0;
	double MUV2WeightOld = 0;
	double MUV2WeightOld_H = 0;
	double MUV2WeightOld_V = 0;

	double MUV2ShowerWidth_H = 0;
	double MUV2ShowerWidth_V = 0;
	double MUV2ShowerWidth = 0;

	double MUV2SeedTime_H = 0;
	double MUV2SeedTime_V = 0;

	double MUV2SeedEnergy_H = 0;
	double MUV2SeedEnergy_V = 0;

	//  double MUV2SeedPosition_H = 0;
	//  double MUV2SeedPosition_V = 0;

	int MUV2SeedChannel_H = 0;
	int MUV2SeedChannel_V = 0;

	int MUV2SeedSide_H = -1;
	int MUV2SeedSide_V = -1;

	double MUV2InnerEnergy_H = 0;
	double MUV2InnerEnergy_V = 0;

	double MUV2OuterEnergy = 0;
	double MUV2TotalEnergy = 0;
	int MUV2OuterNhits = 0;
	int MUV2TotalNhits = 0;
	TRecoMUV2Candidate *MUV2RecoCandidate = nullptr;

	for (int i=0; i<4; i++){
		memset(MUV2ChannelEnergy[i],0,22*sizeof(double));
		memset(MUV2ChannelTime[i],0,22*sizeof(double));
	}

	for (int iHit=0; iHit<NMUV2Hits; iHit++){

		TRecoMUV2Hit *Hit = static_cast<TRecoMUV2Hit*>(MUV2Hits->At(iHit));

		double x = ( (Hit->GetSide()%2) ? PosAtMUV2.Y() : PosAtMUV2.X() );
		double y = ( (Hit->GetSide()%2) ? PosAtMUV2.X() : PosAtMUV2.Y() );
		if (((Hit->GetSide()<2 && y>0) || ((Hit->GetSide()>=2 && y<0)))) y = 0;

		double Time = Hit->GetTime() + fMUV12CorrHandler->GetMUV2TimeCorrection(Hit->GetChannelID(),Abs(y));
		double Energy = Hit->GetCharge() * fMUV12CorrHandler->GetMUV2ChargeCorrection(Hit->GetChannelID(), abs(y));

		if (!Finite(Energy)){
			cout << user_normal() << "ReconstructMUV2Candidate: Found hit with not finite energy!!! ChannelID "<<Hit->GetChannelID()<<" raw charge "<<Hit->GetCharge() <<" correction "<<fMUV12CorrHandler->GetMUV2ChargeCorrection(Hit->GetChannelID(), abs(y))<<" corr position "<<y<<endl;
			continue;
		}

		if (fDebug) thisstream <<"Hit on Channel "<<Hit->GetChannelID()<<" at "<<Time<<endl;

		int idx = static_cast<int>(Abs(Hit->GetScintillatorPosition() - x)/65.);
		if (idx>2) idx=2;

		if (Abs( Time - TrackTime ) > SeedTimeCut[2]) continue;

		MUV2TotalEnergy += Hit->GetCharge()*fMUV12CorrHandler->GetMUV2ChargeCorrection(Hit->GetChannelID(),0)/1.12;
		MUV2TotalNhits++;

		HACChannelEnergy[Hit->GetSide()%2][Hit->GetScintillatorNumber()-1] += Energy;
		MUV2ChannelEnergy[Hit->GetSide()][Hit->GetScintillatorNumber()-1] += Energy;
		MUV2ChannelTime[Hit->GetSide()][Hit->GetScintillatorNumber()-1] = Time;
	}
	RecoCluster->SetMUV2InTimeEnergy(MUV2TotalEnergy);
	RecoCluster->SetMUV2OuterEnergy(MUV2TotalEnergy);
	RecoCluster->SetMUV2OuterNhits(MUV2TotalNhits);

	if (fDebug) {
		thisstream <<"HAC map "<<endl;
		for_each(HACChannelEnergy[0],HACChannelEnergy[0]+22,print);
		thisstream<<endl;
		for_each(HACChannelEnergy[1],HACChannelEnergy[1]+22,print);
		auto channels = fMUV12CorrHandler->GetMUV2Channels(PosAtMUV2.X(), PosAtMUV2.Y());
		thisstream <<"Seed expected at "<<channels.first<<" "<<channels.second<<endl;
	}

	int NHAC_H = count_if(HACChannelEnergy[0],HACChannelEnergy[0]+22, [] (float x){ return x>0; });
	int NHAC_V = count_if(HACChannelEnergy[1],HACChannelEnergy[1]+22, [] (float x){ return x>0; });

	if (fDebug) thisstream <<"Found "<<NHAC_H<<" H hits "<<NHAC_V<<" V hits"<<endl;
	if (NHAC_H==0 || NHAC_V==0) return;

	auto expSeed = fMUV12CorrHandler->GetMUV2Channels(PosAtMUV2.X(),PosAtMUV2.Y());
	int ExpSeedH = (expSeed.first%50) - 1;
	int ExpSeedV = (expSeed.second%50) - 1;

	int LimLow = Min(21,Max(0,ExpSeedH-3)),LimHigh = Max(0,Min(ExpSeedH+3,21));
	int SeedH = LocMax(1+LimHigh-LimLow,HACChannelEnergy[0]+LimLow) + LimLow;
	LimLow = Min(21,Max(0,ExpSeedV-3)), LimHigh = Max(0,Min(ExpSeedV+3,21));
	int SeedV = LocMax(1+LimHigh-LimLow,HACChannelEnergy[1]+LimLow) + LimLow;

	if (HACChannelEnergy[0][SeedH]<=0 || HACChannelEnergy[1][SeedV]<=0) return;

	float SeedX = fMUV12CorrHandler->GetMUV2ScintillatorCenter(101+SeedH);
	float SeedY = fMUV12CorrHandler->GetMUV2ScintillatorCenter(151+SeedV);

	//if (Max(Abs(SeedH-ExpSeedH),Abs(SeedV-ExpSeedV))>3) return;

	int last_ch_H = SeedH, first_ch_H = SeedH;
	int last_ch_V = SeedV, first_ch_V = SeedV;
	float ClusterEnergy_H = HACChannelEnergy[0][SeedH], ClusterEnergy_V = HACChannelEnergy[1][SeedV];
	float ClusterWeight = Power(HACChannelEnergy[0][SeedH],4) + Power(HACChannelEnergy[1][SeedV],4);
	float ClusterX = SeedX*HACChannelEnergy[0][SeedH], ClusterY = SeedY*HACChannelEnergy[1][SeedV];

	while (HACChannelEnergy[0][first_ch_H]>0 && first_ch_H>0 && (SeedH-first_ch_H)<5){
		first_ch_H--;
		ClusterEnergy_H += HACChannelEnergy[0][first_ch_H];
		ClusterWeight += Power(HACChannelEnergy[0][first_ch_H],4);
		ClusterX += MUV2ChPositions[first_ch_H]*HACChannelEnergy[0][first_ch_H];
	}
	while (HACChannelEnergy[0][last_ch_H]>0 && last_ch_H<21 && (last_ch_H - SeedH)<5){
		last_ch_H++;
		ClusterEnergy_H += HACChannelEnergy[0][last_ch_H];
		ClusterWeight += Power(HACChannelEnergy[0][last_ch_H],4);
		ClusterX += MUV2ChPositions[last_ch_H]*HACChannelEnergy[0][last_ch_H];
	}
	while (HACChannelEnergy[1][first_ch_V]>0 && first_ch_V>0 && (SeedV-first_ch_V)<5){
		first_ch_V--;
		ClusterEnergy_V += HACChannelEnergy[1][first_ch_V];
		ClusterWeight += Power(HACChannelEnergy[1][first_ch_V],4);
		ClusterY += MUV2ChPositions[first_ch_V]*HACChannelEnergy[1][first_ch_V];
	}
	while (HACChannelEnergy[1][last_ch_V]>0 && last_ch_V<21 && (last_ch_V - SeedV)<5){
		last_ch_V++;
		ClusterEnergy_V += HACChannelEnergy[1][last_ch_V];
		ClusterWeight += Power(HACChannelEnergy[1][last_ch_V],4);
		ClusterY += MUV2ChPositions[last_ch_V]*HACChannelEnergy[1][last_ch_V];
	}

	ClusterX /= ClusterEnergy_H; ClusterY /= ClusterEnergy_V;
	ClusterWeight /= Power(ClusterEnergy_H+ClusterEnergy_V,4);
	ClusterWeight = Power(ClusterWeight,0.25);

	float ClusterSW_X = 0, ClusterSW_Y = 0;
	NHAC_H = count_if(HACChannelEnergy[0]+first_ch_H,HACChannelEnergy[0]+last_ch_H, [] (float x){ return x>0; });
	NHAC_V = count_if(HACChannelEnergy[1]+first_ch_V,HACChannelEnergy[1]+last_ch_V, [] (float x){ return x>0; });

	if (NHAC_H>1) ClusterSW_X = RMS(last_ch_H-first_ch_H+1,&MUV2ChPositions[first_ch_H],&HACChannelEnergy[0][first_ch_H]);
	if (NHAC_V>1) ClusterSW_Y = RMS(last_ch_V-first_ch_V+1,&MUV2ChPositions[first_ch_V],&HACChannelEnergy[1][first_ch_V]);

	if (fDebug){
		thisstream <<"HAC Cluster at "<<ClusterX<<", "<<ClusterY<<" with energy "<<ClusterEnergy_H<<" "<<ClusterEnergy_V<<endl;
		thisstream <<"Start H "<<first_ch_H<<" Last H "<<last_ch_H<<" Start V "<<first_ch_V<<" Last V "<<last_ch_V<<endl;
	}

	RecoCluster->SetHACWeight(ClusterWeight);
	RecoCluster->SetHACShowerWidth(Sqrt(ClusterSW_X*ClusterSW_X+ClusterSW_Y*ClusterSW_Y));
	RecoCluster->SetHACPosition(ClusterX,ClusterY);
	int NMUV2_H = count_if(MUV2ChannelEnergy[0]+first_ch_H,MUV2ChannelEnergy[0]+last_ch_H, [] (float x){ return x>0; });
	NMUV2_H += count_if(MUV2ChannelEnergy[2]+first_ch_H,MUV2ChannelEnergy[2]+last_ch_H, [] (float x){ return x>0; });
	int NMUV2_V = count_if(MUV2ChannelEnergy[1]+first_ch_V,MUV2ChannelEnergy[1]+last_ch_V, [] (float x){ return x>0; });
	NMUV2_V += count_if(MUV2ChannelEnergy[3]+first_ch_V,MUV2ChannelEnergy[3]+last_ch_V, [] (float x){ return x>0; });

	if (fDebug) thisstream <<"Found "<<NMUV2_H<<" H hits "<<NMUV2_V<<" V hits"<<endl;

	if (NMUV2_H==0 && NMUV2_V==0){
		RecoCluster->SetMUV2OuterEnergy(MUV2TotalEnergy);
		RecoCluster->SetMUV2OuterNhits(MUV2OuterNhits);
		RecoCluster->SetMUV2InTimeEnergy(MUV2TotalEnergy);
		return;
	}

	MUV2RecoCandidate = static_cast<TRecoMUV2Candidate*>(fMUV2Candidates->ConstructedAt(fMUV2Candidates->GetEntries()));

	int SeedChSide[4] = {-1,-1,-1,-1};
	float SeedChEnSide[4] = {0,0,0,0};

	for (int i=0; i<4; i++){
		if (i%2==0)
			SeedChSide[i] = LocMax(last_ch_H-first_ch_H+1, MUV2ChannelEnergy[i]+first_ch_H) + first_ch_H;
		else
			SeedChSide[i] = LocMax(last_ch_V-first_ch_V+1, MUV2ChannelEnergy[i]+first_ch_V) + first_ch_V;
		SeedChEnSide[i] = MUV2ChannelEnergy[i][SeedChSide[i]];
	}

	MUV2SeedSide_H = (SeedChEnSide[0] > SeedChEnSide[2] ? 0 : 2);
	MUV2SeedSide_V = (SeedChEnSide[1] > SeedChEnSide[3] ? 1 : 3);
	MUV2SeedChannel_H = SeedChSide[MUV2SeedSide_H];
	MUV2SeedChannel_V = SeedChSide[MUV2SeedSide_V];
	MUV2SeedTime_H = MUV2ChannelTime[MUV2SeedSide_H][MUV2SeedChannel_H];
	MUV2SeedTime_V = MUV2ChannelTime[MUV2SeedSide_V][MUV2SeedChannel_V];
	MUV2SeedEnergy_H = SeedChEnSide[MUV2SeedSide_H];
	MUV2SeedEnergy_V = SeedChEnSide[MUV2SeedSide_V];


	if (fDebug){
		thisstream<<"Using as MUV2 seeds "<<100 + 50*MUV2SeedSide_H + MUV2SeedChannel_H<<", "<<100 + 50*MUV2SeedSide_V + MUV2SeedChannel_V<<endl;
		thisstream <<"Seed Times "<<MUV2SeedTime_H<<" , "<<MUV2SeedTime_V<<endl;
	}

	SeedH++; SeedV++;
	first_ch_H++; last_ch_H++; first_ch_V++; last_ch_V++;

	double MUV2EnergyChannels[2][11];
	double MUV2PositionChannels[2][11];
	double MUV2TimeChannels[2][11];
	int    MUV2NHitsChannels[2][11];
	int    MUV2HitIndexes[2][2][11];

	for (int i=0; i<11; i++){
		MUV2EnergyChannels[0][i] = MUV2EnergyChannels[1][i] = 0.;
		MUV2PositionChannels[0][i] = MUV2PositionChannels[1][i] = 0.;
		MUV2TimeChannels[0][i] = MUV2TimeChannels[1][i] = 0.;
		MUV2NHitsChannels[0][i] = MUV2NHitsChannels[1][i] = 0.;
		MUV2HitIndexes[0][0][i] = MUV2HitIndexes[0][1][i] = -1;
		MUV2HitIndexes[1][0][i] = MUV2HitIndexes[1][1][i] = -1;
	}

	for (int iHit=0; iHit<NMUV2Hits; iHit++){

		TRecoMUV2Hit *Hit = static_cast<TRecoMUV2Hit*>(MUV2Hits->At(iHit));

		int SeedSide = ( (Hit->GetSide()%2) ? MUV2SeedSide_V : MUV2SeedSide_H );

		double y = ( (Hit->GetSide()%2) ? ClusterX : ClusterY );

		if (Hit->GetSide()!=SeedSide) y = 0;

		if (fDebug) thisstream <<"Found Hit on Channel "<<Hit->GetChannelID()<<endl;

		if (Hit->GetSide()%2==0 && MUV2SeedEnergy_H>0){

			if (Hit->GetScintillatorNumber() < first_ch_H || Hit->GetScintillatorNumber() > last_ch_H){

				if (fDebug) thisstream <<"Out of the cluster limits "<<first_ch_H<<" "<<last_ch_H<<endl;

				double HitTime = Hit->GetTime() + fMUV12CorrHandler->GetMUV2TimeCorrection(Hit->GetChannelID(),0);

				if (Abs(HitTime - MUV2SeedTime_H)>10.) continue;

				double HitEnergy = fMUV12CorrHandler->GetMUV2ChargeCorrection(Hit->GetChannelID(),0) * Hit->GetCharge()/(1.12);

				MUV2OuterEnergy += HitEnergy;
				MUV2OuterNhits ++;

				continue;
			}

			double HitTime = Hit->GetTime() + fMUV12CorrHandler->GetMUV2TimeCorrection(Hit->GetChannelID(),Abs(y));
			double HitEnergy = fMUV12CorrHandler->GetMUV2ChargeCorrection(Hit->GetChannelID(),Abs(y)) * Hit->GetCharge()/(1.12);

			if (!GetWithMC()){
				if (Hit->GetSide()%2==1) HitEnergy *= 1.15;
				HitEnergy *= 0.985;
			}

			if (fDebug) thisstream <<"Hit Time "<<HitTime<<endl;

			if ( Abs(HitTime - MUV2SeedTime_H) > 10. ) continue;


			MUV2PositionChannels[0][Hit->GetScintillatorNumber() - SeedH + 5] = Hit->GetScintillatorPosition();

			if (SeedSide!=Hit->GetSide() && (SeedSide>1 ? last_ch_V>11 : last_ch_V<10)) continue;

			if (fDebug) {
			    thisstream <<"Added!"<<endl;
			    thisstream <<"Hit Position: "<<MUV2PositionChannels[0][Hit->GetScintillatorNumber() - SeedH + 5]<<endl;
			}
			MUV2EnergyChannels[0][Hit->GetScintillatorNumber() - SeedH + 5]+= HitEnergy;
			MUV2TimeChannels[0][Hit->GetScintillatorNumber() - SeedH + 5]+= HitEnergy*HitTime;
			MUV2NHitsChannels[0][Hit->GetScintillatorNumber() - SeedH + 5] = 1;
			MUV2HitIndexes[0][Hit->GetSide()/2][Hit->GetScintillatorNumber() - SeedH + 5] = iHit;
			MUV2MatchingHits_H.push_back(Hit);
		}

		else if (Hit->GetSide()%2==1 && MUV2SeedEnergy_V>0) {

			if (Hit->GetScintillatorNumber() < first_ch_V || Hit->GetScintillatorNumber() > last_ch_V){

				if (fDebug) thisstream <<"Out of the cluster limits "<<first_ch_V<<" "<<last_ch_V<<endl;

				if (Abs(Hit->GetTime() - MUV2SeedTime_V)>10.) continue;

				double HitEnergy = Hit->GetCharge()*fMUV12CorrHandler->GetMUV2ChargeCorrection(Hit->GetChannelID(),0)/(1.12);

				MUV2OuterEnergy += HitEnergy;
				MUV2OuterNhits ++;

				continue;
			}


			double HitTime = Hit->GetTime() + fMUV12CorrHandler->GetMUV2TimeCorrection(Hit->GetChannelID(),Abs(y));
			double HitEnergy = fMUV12CorrHandler->GetMUV2ChargeCorrection(Hit->GetChannelID(),Abs(y)) * Hit->GetCharge()/(1.12);
			if (!GetWithMC()){
				HitEnergy *= 0.985;
			}

			if (fDebug) thisstream <<"Hit Time "<<HitTime<<endl;

			if (Abs(HitTime - MUV2SeedTime_V) > 10.) continue;

			if (SeedSide!=Hit->GetSide() && (SeedSide>1 ? last_ch_H>11 : last_ch_H<10)) continue;

			if (fDebug) thisstream <<"Added!"<<endl;

			MUV2PositionChannels[1][Hit->GetScintillatorNumber() - SeedV + 5] = Hit->GetScintillatorPosition();
			if (fDebug) thisstream <<"Hit Position: "<<MUV2PositionChannels[1][Hit->GetScintillatorNumber() - SeedV + 5]<<endl;

			MUV2EnergyChannels[1][Hit->GetScintillatorNumber() - SeedV + 5]+= HitEnergy;
			MUV2TimeChannels[1][Hit->GetScintillatorNumber() - SeedV + 5]+= HitEnergy*HitTime;
			MUV2NHitsChannels[1][Hit->GetScintillatorNumber() - SeedV + 5] = 1;
			MUV2HitIndexes[1][Hit->GetSide()/2][Hit->GetScintillatorNumber() - SeedV + 5] = iHit;
			MUV2MatchingHits_V.push_back(Hit);
		}

	}


	int startH = first_ch_H-SeedH+5, stopH = last_ch_H-SeedH+5;
	int startV = first_ch_V-SeedV+5, stopV = last_ch_V-SeedV+5;
	int NM2HitsH = 0, NM2HitsV =0;
	for (int i=0; i<11; i++){

		if (MUV2EnergyChannels[0][i]>0 && i>=startH && i<=stopH){
			NM2HitsH++;
			if (abs(i-5)<2) MUV2InnerEnergy_H += MUV2EnergyChannels[0][i];
			MUV2TimeChannels[0][i] /= MUV2EnergyChannels[0][i];
			MUV2Energy_H += MUV2EnergyChannels[0][i];
			MUV2Position_H += MUV2PositionChannels[0][i] * MUV2EnergyChannels[0][i];
			MUV2Time_H += MUV2TimeChannels[0][i] * MUV2EnergyChannels[0][i];
			MUV2Weight += Power(MUV2EnergyChannels[0][i],4.);
			MUV2Weight_H += Power(MUV2EnergyChannels[0][i],4.);
			MUV2WeightOld += MUV2EnergyChannels[0][i]*MUV2EnergyChannels[0][i];
			MUV2WeightOld_H += MUV2EnergyChannels[0][i]*MUV2EnergyChannels[0][i];
			if (MUV2HitIndexes[0][0][i]>=0) MUV2RecoCandidate->AddHit(MUV2HitIndexes[0][0][i]);
			if (MUV2HitIndexes[0][1][i]>=0) MUV2RecoCandidate->AddHit(MUV2HitIndexes[0][1][i]);
		}
		else if (MUV2EnergyChannels[0][i]>0) {
			MUV2OuterEnergy += MUV2EnergyChannels[0][i];
			MUV2OuterNhits ++;
		}

		if (MUV2EnergyChannels[1][i]>0 && i>=startV && i<=stopV){
			NM2HitsV++;
			if (abs(i-5)<2) MUV2InnerEnergy_V += MUV2EnergyChannels[1][i];
			MUV2TimeChannels[1][i] /= MUV2EnergyChannels[1][i];
			MUV2Energy_V += MUV2EnergyChannels[1][i];
			MUV2Position_V += MUV2PositionChannels[1][i] * MUV2EnergyChannels[1][i];
			MUV2Time_V += MUV2TimeChannels[1][i] * MUV2EnergyChannels[1][i];
			MUV2Weight += Power(MUV2EnergyChannels[1][i],4.);
			MUV2Weight_V += Power(MUV2EnergyChannels[1][i],4.);
			MUV2WeightOld += MUV2EnergyChannels[1][i]*MUV2EnergyChannels[1][i];
			MUV2WeightOld_V += MUV2EnergyChannels[1][i]*MUV2EnergyChannels[1][i];
			if (MUV2HitIndexes[1][0][i]>=0) MUV2RecoCandidate->AddHit(MUV2HitIndexes[1][0][i]);
			if (MUV2HitIndexes[1][1][i]>=0) MUV2RecoCandidate->AddHit(MUV2HitIndexes[1][1][i]);
		}
		else if (MUV2EnergyChannels[1][i]>0) {
			MUV2OuterEnergy += MUV2EnergyChannels[1][i];
			MUV2OuterNhits ++;
		}
	}


	if (MUV2Energy_H>0){
		MUV2Position_H /= MUV2Energy_H;
		MUV2Time_H /= MUV2Energy_H;
		MUV2Weight_H = Power(MUV2Weight_H/Power(MUV2Energy_H,4.),0.25);
		MUV2WeightOld_H /= MUV2Energy_H*MUV2Energy_H;
		if (NM2HitsH>1) MUV2ShowerWidth_H = RMS(stopH-startH,&MUV2PositionChannels[0][startH],&MUV2EnergyChannels[0][startH]);
	}

	if (MUV2Energy_V>0){
		MUV2Position_V /= MUV2Energy_V;
		MUV2Time_V /= MUV2Energy_V;
		MUV2Weight_V = Power(MUV2Weight_V/Power(MUV2Energy_V,4.),0.25);
		MUV2WeightOld_V /= MUV2Energy_V*MUV2Energy_V;
		if (NM2HitsV>1) MUV2ShowerWidth_V = RMS(stopV-startV,&MUV2PositionChannels[1][startV],&MUV2EnergyChannels[1][startV]);
	}

	if (MUV2Energy_H>0 && MUV2Energy_V>0){
		MUV2Time = 0.5*(MUV2Time_H + MUV2Time_V);
		MUV2Weight_H = Power(MUV2Weight/Power(MUV2Energy_H+MUV2Energy_V,4.),0.25);
		MUV2WeightOld /= (MUV2Energy_H + MUV2Energy_V) * (MUV2Energy_H + MUV2Energy_V);
		MUV2ShowerWidth = 0.5 * (MUV2ShowerWidth_H + MUV2ShowerWidth_V);
	}
	else if (MUV2Energy_H>0){
		MUV2Time = MUV2Time_H;
		MUV2Weight = MUV2Weight_H;
		MUV2WeightOld = MUV2WeightOld_H;
		MUV2ShowerWidth = MUV2ShowerWidth_H;
	}
	else{
		MUV2Time = MUV2Time_V;
		MUV2Weight = MUV2Weight_V;
		MUV2WeightOld = MUV2WeightOld_V;
		MUV2ShowerWidth = MUV2ShowerWidth_V;
	}

	if (fDebug) thisstream<<"MUV2 Cluster position "<<MUV2Position_H<<","<<MUV2Position_V<<endl;

	MUV2RecoCandidate->SetTime(MUV2Time);
	MUV2RecoCandidate->SetPosition(MUV2Position_H,MUV2Position_V);
	MUV2RecoCandidate->SetEnergy(0.5*(MUV2Energy_H+MUV2Energy_V));
	MUV2RecoCandidate->SetCharge(2.24*0.5*(MUV2Energy_H+MUV2Energy_V));
	MUV2RecoCandidate->SetShowerWidth(MUV2ShowerWidth);

	MUV2RecoCandidate->SetTimeHorizontal(MUV2Time_H);
	MUV2RecoCandidate->SetTimeVertical(MUV2Time_V);

	MUV2RecoCandidate->SetEnergyHorizontal(0.5*MUV2Energy_H);
	MUV2RecoCandidate->SetEnergyVertical(0.5*MUV2Energy_V);

	MUV2RecoCandidate->SetChargeHorizontal(2.24*0.5*MUV2Energy_H);
	MUV2RecoCandidate->SetChargeVertical(2.24*0.5*MUV2Energy_V);

	MUV2RecoCandidate->SetShowerWidthHorizontal(MUV2ShowerWidth_H);
	MUV2RecoCandidate->SetShowerWidthVertical(MUV2ShowerWidth_V);

	MUV2RecoCandidate->SetSeedEnergyHorizontal(0.5*MUV2SeedEnergy_H);
	MUV2RecoCandidate->SetSeedEnergyVertical(0.5*MUV2SeedEnergy_V);

	MUV2RecoCandidate->SetInnerEnergyHorizontal(0.5*MUV2InnerEnergy_H);
	MUV2RecoCandidate->SetInnerEnergyVertical(0.5*MUV2InnerEnergy_V);

	MUV2RecoCandidate->SetSeedIndexHorizontal(MUV2SeedChannel_H);
	MUV2RecoCandidate->SetSeedIndexVertical(MUV2SeedChannel_V);

	RecoCluster->SetMUV2Candidate(MUV2RecoCandidate);
	RecoCluster->SetMUV2Energy(0.5*(MUV2Energy_H+MUV2Energy_V));
	RecoCluster->SetMUV2Time(MUV2Time);
	RecoCluster->SetMUV2Weight(MUV2Weight);
	RecoCluster->SetOldMUV2Weight(MUV2WeightOld);
	RecoCluster->SetMUV2OuterEnergy(0.5*MUV2OuterEnergy);
	RecoCluster->SetMUV2OuterNhits(MUV2OuterNhits);

}

double SpectrometerCalorimetersAssociation::LKrCellEnergyFraction (double Energy, double Distance){

	Distance *= 0.1;
	Energy *= 1.e-3;

	double Scale=0.935*1.204/1.07309997;
	double Efrac = 0;

	if (Distance>=25.){
		if (Distance<30.) Efrac = Exp(-6.6552-0.13689*Distance);
		else return Efrac = Exp(-6.8839-0.12969*Distance);
	}

	int ie1,ie2;

	if (Energy<=2.) ie1=ie2=1;
	else if (Energy<=5.) { ie1=1; ie2=2; }
	else if (Energy<=10) { ie1=2; ie2=3; }
	else if (Energy<=20) { ie1=3; ie2=4; }
	else if (Energy<=50) { ie1=4; ie2=5; }
	else ie1=ie2=5;

	int id1 = Max( int(Distance / fLKrProfDx - 0.5)+1, 1);
	int id2 = Min(fLKrProfNbins-1,id1+1);
	id1 = Min(fLKrProfNbins-1,id1);

	if (ie1==ie2 && id1==id2) Efrac = fLKrProf[ie1][id1];
	else if (ie1==ie2){
		Efrac += fLKrProf[ie1][id1]*((id2-0.5)*fLKrProfDx-Distance);
		Efrac += fLKrProf[ie1][id2]*(Distance-(id1-0.5)*fLKrProfDx);
		Efrac /= fLKrProfDx;
	}
	else if (id1==id2){
		Efrac += fLKrProf[ie1][id1]*(fLKrProfEbin[ie2]-Energy);
		Efrac += fLKrProf[ie2][id1]*(Energy-fLKrProfEbin[ie1]);
		Efrac /= (fLKrProfEbin[ie2] - fLKrProfEbin[ie1]);
	}
	else{
		Efrac += fLKrProf[ie1][id1]*((id2-0.5)*fLKrProfDx-Distance)*(fLKrProfEbin[ie2]-Energy);
		Efrac += fLKrProf[ie2][id1]*((id2-0.5)*fLKrProfDx-Distance)*(Energy-fLKrProfEbin[ie1]);
		Efrac += fLKrProf[ie1][id2]*(Distance-(id1-0.5)*fLKrProfDx)*(fLKrProfEbin[ie2]-Energy);
		Efrac += fLKrProf[ie2][id2]*(Distance-(id1-0.5)*fLKrProfDx)*(Energy-fLKrProfEbin[ie1]);
		Efrac /= fLKrProfDx * (fLKrProfEbin[ie2] - fLKrProfEbin[ie1]);
	}
	return Scale * Efrac;
}

void SpectrometerCalorimetersAssociation::LKrProfInit (){
	fLKrProfEbin[1]= 2.;
	fLKrProfEbin[2]= 5.;
	fLKrProfEbin[3]=10.;
	fLKrProfEbin[4]=20.;
	fLKrProfEbin[5]=50.;

	fLKrProf[ 1][  1] =      .378827;
	fLKrProf[ 1][  2] =      .377036;
	fLKrProf[ 1][  3] =      .358761;
	fLKrProf[ 1][  4] =      .315450;
	fLKrProf[ 1][  5] =      .267165;
	fLKrProf[ 1][  6] =      .195020;
	fLKrProf[ 1][  7] =      .136000;
	fLKrProf[ 1][  8] =      .095778;
	fLKrProf[ 1][  9] =      .069846;
	fLKrProf[ 1][ 10] =      .052237;
	fLKrProf[ 1][ 11] =      .040718;
	fLKrProf[ 1][ 12] =      .032560;
	fLKrProf[ 1][ 13] =      .026032;
	fLKrProf[ 1][ 14] =      .021034;
	fLKrProf[ 1][ 15] =      .017311;
	fLKrProf[ 1][ 16] =      .014989;
	fLKrProf[ 1][ 17] =      .012750;
	fLKrProf[ 1][ 18] =      .010973;
	fLKrProf[ 1][ 19] =      .009256;
	fLKrProf[ 1][ 20] =      .008210;
	fLKrProf[ 1][ 21] =      .007211;
	fLKrProf[ 1][ 22] =      .006147;
	fLKrProf[ 1][ 23] =      .005467;
	fLKrProf[ 1][ 24] =      .004829;
	fLKrProf[ 1][ 25] =      .004361;
	fLKrProf[ 1][ 26] =      .003928;
	fLKrProf[ 1][ 27] =      .003564;
	fLKrProf[ 1][ 28] =      .003293;
	fLKrProf[ 1][ 29] =      .002999;
	fLKrProf[ 1][ 30] =      .002667;
	fLKrProf[ 1][ 31] =      .002401;
	fLKrProf[ 1][ 32] =      .002298;
	fLKrProf[ 1][ 33] =      .002049;
	fLKrProf[ 1][ 34] =      .001913;
	fLKrProf[ 1][ 35] =      .001786;
	fLKrProf[ 1][ 36] =      .001695;
	fLKrProf[ 1][ 37] =      .001579;
	fLKrProf[ 1][ 38] =      .001433;
	fLKrProf[ 1][ 39] =      .001403;
	fLKrProf[ 1][ 40] =      .001272;
	fLKrProf[ 1][ 41] =      .001217;
	fLKrProf[ 1][ 42] =      .001091;
	fLKrProf[ 1][ 43] =      .001044;
	fLKrProf[ 1][ 44] =      .001055;
	fLKrProf[ 1][ 45] =      .000938;
	fLKrProf[ 1][ 46] =      .000894;
	fLKrProf[ 1][ 47] =      .000848;
	fLKrProf[ 1][ 48] =      .000810;
	fLKrProf[ 1][ 49] =      .000773;
	fLKrProf[ 1][ 50] =      .000706;
	fLKrProf[ 1][ 51] =      .000655;
	fLKrProf[ 1][ 52] =      .000657;
	fLKrProf[ 1][ 53] =      .000620;
	fLKrProf[ 1][ 54] =      .000585;
	fLKrProf[ 1][ 55] =      .000541;
	fLKrProf[ 1][ 56] =      .000518;
	fLKrProf[ 1][ 57] =      .000506;
	fLKrProf[ 1][ 58] =      .000479;
	fLKrProf[ 1][ 59] =      .000455;
	fLKrProf[ 1][ 60] =      .000431;
	fLKrProf[ 1][ 61] =      .000405;
	fLKrProf[ 1][ 62] =      .000400;
	fLKrProf[ 1][ 63] =      .000377;
	fLKrProf[ 1][ 64] =      .000358;
	fLKrProf[ 1][ 65] =      .000338;
	fLKrProf[ 1][ 66] =      .000331;
	fLKrProf[ 1][ 67] =      .000313;
	fLKrProf[ 1][ 68] =      .000305;
	fLKrProf[ 1][ 69] =      .000282;
	fLKrProf[ 1][ 70] =      .000263;
	fLKrProf[ 1][ 71] =      .000270;
	fLKrProf[ 1][ 72] =      .000254;
	fLKrProf[ 1][ 73] =      .000237;
	fLKrProf[ 1][ 74] =      .000235;
	fLKrProf[ 1][ 75] =      .000223;
	fLKrProf[ 1][ 76] =      .000225;
	fLKrProf[ 1][ 77] =      .000210;
	fLKrProf[ 1][ 78] =      .000203;
	fLKrProf[ 1][ 79] =      .000192;
	fLKrProf[ 1][ 80] =      .000181;
	fLKrProf[ 1][ 81] =      .000175;
	fLKrProf[ 1][ 82] =      .000168;
	fLKrProf[ 1][ 83] =      .000164;
	fLKrProf[ 1][ 84] =      .000157;
	fLKrProf[ 1][ 85] =      .000147;
	fLKrProf[ 1][ 86] =      .000142;
	fLKrProf[ 1][ 87] =      .000146;
	fLKrProf[ 1][ 88] =      .000136;
	fLKrProf[ 1][ 89] =      .000125;
	fLKrProf[ 1][ 90] =      .000131;
	fLKrProf[ 1][ 91] =      .000120;
	fLKrProf[ 1][ 92] =      .000111;
	fLKrProf[ 1][ 93] =      .000115;
	fLKrProf[ 1][ 94] =      .000107;
	fLKrProf[ 1][ 95] =      .000103;
	fLKrProf[ 1][ 96] =      .000103;
	fLKrProf[ 1][ 97] =      .000098;
	fLKrProf[ 1][ 98] =      .000096;
	fLKrProf[ 1][ 99] =      .000091;
	fLKrProf[ 1][100] =      .000083;
	fLKrProf[ 1][101] =      .000086;
	fLKrProf[ 1][102] =      .000079;
	fLKrProf[ 1][103] =      .000080;
	fLKrProf[ 1][104] =      .000074;
	fLKrProf[ 1][105] =      .000074;
	fLKrProf[ 1][106] =      .000071;
	fLKrProf[ 1][107] =      .000067;
	fLKrProf[ 1][108] =      .000066;
	fLKrProf[ 1][109] =      .000064;
	fLKrProf[ 1][110] =      .000062;
	fLKrProf[ 1][111] =      .000061;
	fLKrProf[ 1][112] =      .000059;
	fLKrProf[ 1][113] =      .000056;
	fLKrProf[ 1][114] =      .000055;
	fLKrProf[ 1][115] =      .000055;
	fLKrProf[ 1][116] =      .000052;
	fLKrProf[ 1][117] =      .000052;
	fLKrProf[ 1][118] =      .000048;
	fLKrProf[ 1][119] =      .000049;
	fLKrProf[ 1][120] =      .000044;
	fLKrProf[ 2][  1] =      .396067;
	fLKrProf[ 2][  2] =      .389678;
	fLKrProf[ 2][  3] =      .364202;
	fLKrProf[ 2][  4] =      .326276;
	fLKrProf[ 2][  5] =      .271005;
	fLKrProf[ 2][  6] =      .205248;
	fLKrProf[ 2][  7] =      .139566;
	fLKrProf[ 2][  8] =      .095598;
	fLKrProf[ 2][  9] =      .069204;
	fLKrProf[ 2][ 10] =      .052209;
	fLKrProf[ 2][ 11] =      .040183;
	fLKrProf[ 2][ 12] =      .031524;
	fLKrProf[ 2][ 13] =      .026170;
	fLKrProf[ 2][ 14] =      .020942;
	fLKrProf[ 2][ 15] =      .017466;
	fLKrProf[ 2][ 16] =      .014622;
	fLKrProf[ 2][ 17] =      .012258;
	fLKrProf[ 2][ 18] =      .010584;
	fLKrProf[ 2][ 19] =      .009183;
	fLKrProf[ 2][ 20] =      .007906;
	fLKrProf[ 2][ 21] =      .006906;
	fLKrProf[ 2][ 22] =      .006099;
	fLKrProf[ 2][ 23] =      .005328;
	fLKrProf[ 2][ 24] =      .004945;
	fLKrProf[ 2][ 25] =      .004301;
	fLKrProf[ 2][ 26] =      .003942;
	fLKrProf[ 2][ 27] =      .003517;
	fLKrProf[ 2][ 28] =      .003116;
	fLKrProf[ 2][ 29] =      .002905;
	fLKrProf[ 2][ 30] =      .002643;
	fLKrProf[ 2][ 31] =      .002458;
	fLKrProf[ 2][ 32] =      .002266;
	fLKrProf[ 2][ 33] =      .002071;
	fLKrProf[ 2][ 34] =      .001914;
	fLKrProf[ 2][ 35] =      .001794;
	fLKrProf[ 2][ 36] =      .001646;
	fLKrProf[ 2][ 37] =      .001556;
	fLKrProf[ 2][ 38] =      .001430;
	fLKrProf[ 2][ 39] =      .001364;
	fLKrProf[ 2][ 40] =      .001276;
	fLKrProf[ 2][ 41] =      .001200;
	fLKrProf[ 2][ 42] =      .001134;
	fLKrProf[ 2][ 43] =      .001045;
	fLKrProf[ 2][ 44] =      .000984;
	fLKrProf[ 2][ 45] =      .000950;
	fLKrProf[ 2][ 46] =      .000891;
	fLKrProf[ 2][ 47] =      .000832;
	fLKrProf[ 2][ 48] =      .000794;
	fLKrProf[ 2][ 49] =      .000754;
	fLKrProf[ 2][ 50] =      .000720;
	fLKrProf[ 2][ 51] =      .000684;
	fLKrProf[ 2][ 52] =      .000637;
	fLKrProf[ 2][ 53] =      .000599;
	fLKrProf[ 2][ 54] =      .000559;
	fLKrProf[ 2][ 55] =      .000550;
	fLKrProf[ 2][ 56] =      .000521;
	fLKrProf[ 2][ 57] =      .000494;
	fLKrProf[ 2][ 58] =      .000479;
	fLKrProf[ 2][ 59] =      .000451;
	fLKrProf[ 2][ 60] =      .000425;
	fLKrProf[ 2][ 61] =      .000414;
	fLKrProf[ 2][ 62] =      .000395;
	fLKrProf[ 2][ 63] =      .000381;
	fLKrProf[ 2][ 64] =      .000354;
	fLKrProf[ 2][ 65] =      .000342;
	fLKrProf[ 2][ 66] =      .000332;
	fLKrProf[ 2][ 67] =      .000305;
	fLKrProf[ 2][ 68] =      .000298;
	fLKrProf[ 2][ 69] =      .000291;
	fLKrProf[ 2][ 70] =      .000277;
	fLKrProf[ 2][ 71] =      .000266;
	fLKrProf[ 2][ 72] =      .000248;
	fLKrProf[ 2][ 73] =      .000244;
	fLKrProf[ 2][ 74] =      .000229;
	fLKrProf[ 2][ 75] =      .000225;
	fLKrProf[ 2][ 76] =      .000211;
	fLKrProf[ 2][ 77] =      .000212;
	fLKrProf[ 2][ 78] =      .000198;
	fLKrProf[ 2][ 79] =      .000189;
	fLKrProf[ 2][ 80] =      .000184;
	fLKrProf[ 2][ 81] =      .000177;
	fLKrProf[ 2][ 82] =      .000167;
	fLKrProf[ 2][ 83] =      .000168;
	fLKrProf[ 2][ 84] =      .000155;
	fLKrProf[ 2][ 85] =      .000150;
	fLKrProf[ 2][ 86] =      .000147;
	fLKrProf[ 2][ 87] =      .000138;
	fLKrProf[ 2][ 88] =      .000136;
	fLKrProf[ 2][ 89] =      .000128;
	fLKrProf[ 2][ 90] =      .000124;
	fLKrProf[ 2][ 91] =      .000119;
	fLKrProf[ 2][ 92] =      .000113;
	fLKrProf[ 2][ 93] =      .000112;
	fLKrProf[ 2][ 94] =      .000106;
	fLKrProf[ 2][ 95] =      .000106;
	fLKrProf[ 2][ 96] =      .000102;
	fLKrProf[ 2][ 97] =      .000093;
	fLKrProf[ 2][ 98] =      .000091;
	fLKrProf[ 2][ 99] =      .000090;
	fLKrProf[ 2][100] =      .000087;
	fLKrProf[ 2][101] =      .000083;
	fLKrProf[ 2][102] =      .000081;
	fLKrProf[ 2][103] =      .000077;
	fLKrProf[ 2][104] =      .000076;
	fLKrProf[ 2][105] =      .000075;
	fLKrProf[ 2][106] =      .000071;
	fLKrProf[ 2][107] =      .000068;
	fLKrProf[ 2][108] =      .000064;
	fLKrProf[ 2][109] =      .000064;
	fLKrProf[ 2][110] =      .000061;
	fLKrProf[ 2][111] =      .000059;
	fLKrProf[ 2][112] =      .000056;
	fLKrProf[ 2][113] =      .000054;
	fLKrProf[ 2][114] =      .000054;
	fLKrProf[ 2][115] =      .000053;
	fLKrProf[ 2][116] =      .000051;
	fLKrProf[ 2][117] =      .000051;
	fLKrProf[ 2][118] =      .000049;
	fLKrProf[ 2][119] =      .000046;
	fLKrProf[ 2][120] =      .000045;
	fLKrProf[ 3][  1] =      .394201;
	fLKrProf[ 3][  2] =      .393743;
	fLKrProf[ 3][  3] =      .366454;
	fLKrProf[ 3][  4] =      .325474;
	fLKrProf[ 3][  5] =      .278475;
	fLKrProf[ 3][  6] =      .196172;
	fLKrProf[ 3][  7] =      .141981;
	fLKrProf[ 3][  8] =      .095255;
	fLKrProf[ 3][  9] =      .069226;
	fLKrProf[ 3][ 10] =      .052384;
	fLKrProf[ 3][ 11] =      .040158;
	fLKrProf[ 3][ 12] =      .031777;
	fLKrProf[ 3][ 13] =      .025467;
	fLKrProf[ 3][ 14] =      .021123;
	fLKrProf[ 3][ 15] =      .017336;
	fLKrProf[ 3][ 16] =      .014680;
	fLKrProf[ 3][ 17] =      .012255;
	fLKrProf[ 3][ 18] =      .010720;
	fLKrProf[ 3][ 19] =      .009125;
	fLKrProf[ 3][ 20] =      .007906;
	fLKrProf[ 3][ 21] =      .006758;
	fLKrProf[ 3][ 22] =      .006117;
	fLKrProf[ 3][ 23] =      .005375;
	fLKrProf[ 3][ 24] =      .004825;
	fLKrProf[ 3][ 25] =      .004336;
	fLKrProf[ 3][ 26] =      .003892;
	fLKrProf[ 3][ 27] =      .003497;
	fLKrProf[ 3][ 28] =      .003211;
	fLKrProf[ 3][ 29] =      .002895;
	fLKrProf[ 3][ 30] =      .002632;
	fLKrProf[ 3][ 31] =      .002429;
	fLKrProf[ 3][ 32] =      .002285;
	fLKrProf[ 3][ 33] =      .002073;
	fLKrProf[ 3][ 34] =      .001927;
	fLKrProf[ 3][ 35] =      .001824;
	fLKrProf[ 3][ 36] =      .001687;
	fLKrProf[ 3][ 37] =      .001555;
	fLKrProf[ 3][ 38] =      .001443;
	fLKrProf[ 3][ 39] =      .001362;
	fLKrProf[ 3][ 40] =      .001255;
	fLKrProf[ 3][ 41] =      .001180;
	fLKrProf[ 3][ 42] =      .001136;
	fLKrProf[ 3][ 43] =      .001051;
	fLKrProf[ 3][ 44] =      .000995;
	fLKrProf[ 3][ 45] =      .000939;
	fLKrProf[ 3][ 46] =      .000881;
	fLKrProf[ 3][ 47] =      .000840;
	fLKrProf[ 3][ 48] =      .000788;
	fLKrProf[ 3][ 49] =      .000737;
	fLKrProf[ 3][ 50] =      .000714;
	fLKrProf[ 3][ 51] =      .000673;
	fLKrProf[ 3][ 52] =      .000624;
	fLKrProf[ 3][ 53] =      .000610;
	fLKrProf[ 3][ 54] =      .000565;
	fLKrProf[ 3][ 55] =      .000536;
	fLKrProf[ 3][ 56] =      .000522;
	fLKrProf[ 3][ 57] =      .000497;
	fLKrProf[ 3][ 58] =      .000467;
	fLKrProf[ 3][ 59] =      .000449;
	fLKrProf[ 3][ 60] =      .000421;
	fLKrProf[ 3][ 61] =      .000410;
	fLKrProf[ 3][ 62] =      .000389;
	fLKrProf[ 3][ 63] =      .000370;
	fLKrProf[ 3][ 64] =      .000352;
	fLKrProf[ 3][ 65] =      .000340;
	fLKrProf[ 3][ 66] =      .000320;
	fLKrProf[ 3][ 67] =      .000314;
	fLKrProf[ 3][ 68] =      .000292;
	fLKrProf[ 3][ 69] =      .000289;
	fLKrProf[ 3][ 70] =      .000274;
	fLKrProf[ 3][ 71] =      .000264;
	fLKrProf[ 3][ 72] =      .000255;
	fLKrProf[ 3][ 73] =      .000237;
	fLKrProf[ 3][ 74] =      .000227;
	fLKrProf[ 3][ 75] =      .000222;
	fLKrProf[ 3][ 76] =      .000216;
	fLKrProf[ 3][ 77] =      .000202;
	fLKrProf[ 3][ 78] =      .000195;
	fLKrProf[ 3][ 79] =      .000188;
	fLKrProf[ 3][ 80] =      .000185;
	fLKrProf[ 3][ 81] =      .000173;
	fLKrProf[ 3][ 82] =      .000167;
	fLKrProf[ 3][ 83] =      .000159;
	fLKrProf[ 3][ 84] =      .000158;
	fLKrProf[ 3][ 85] =      .000149;
	fLKrProf[ 3][ 86] =      .000146;
	fLKrProf[ 3][ 87] =      .000141;
	fLKrProf[ 3][ 88] =      .000133;
	fLKrProf[ 3][ 89] =      .000129;
	fLKrProf[ 3][ 90] =      .000126;
	fLKrProf[ 3][ 91] =      .000119;
	fLKrProf[ 3][ 92] =      .000113;
	fLKrProf[ 3][ 93] =      .000109;
	fLKrProf[ 3][ 94] =      .000107;
	fLKrProf[ 3][ 95] =      .000104;
	fLKrProf[ 3][ 96] =      .000101;
	fLKrProf[ 3][ 97] =      .000096;
	fLKrProf[ 3][ 98] =      .000094;
	fLKrProf[ 3][ 99] =      .000089;
	fLKrProf[ 3][100] =      .000089;
	fLKrProf[ 3][101] =      .000085;
	fLKrProf[ 3][102] =      .000080;
	fLKrProf[ 3][103] =      .000080;
	fLKrProf[ 3][104] =      .000075;
	fLKrProf[ 3][105] =      .000073;
	fLKrProf[ 3][106] =      .000071;
	fLKrProf[ 3][107] =      .000070;
	fLKrProf[ 3][108] =      .000066;
	fLKrProf[ 3][109] =      .000065;
	fLKrProf[ 3][110] =      .000061;
	fLKrProf[ 3][111] =      .000059;
	fLKrProf[ 3][112] =      .000058;
	fLKrProf[ 3][113] =      .000056;
	fLKrProf[ 3][114] =      .000054;
	fLKrProf[ 3][115] =      .000054;
	fLKrProf[ 3][116] =      .000051;
	fLKrProf[ 3][117] =      .000049;
	fLKrProf[ 3][118] =      .000047;
	fLKrProf[ 3][119] =      .000046;
	fLKrProf[ 3][120] =      .000044;
	fLKrProf[ 4][  1] =      .401484;
	fLKrProf[ 4][  2] =      .392441;
	fLKrProf[ 4][  3] =      .369372;
	fLKrProf[ 4][  4] =      .336254;
	fLKrProf[ 4][  5] =      .276725;
	fLKrProf[ 4][  6] =      .200815;
	fLKrProf[ 4][  7] =      .137714;
	fLKrProf[ 4][  8] =      .094554;
	fLKrProf[ 4][  9] =      .069094;
	fLKrProf[ 4][ 10] =      .051520;
	fLKrProf[ 4][ 11] =      .040232;
	fLKrProf[ 4][ 12] =      .031693;
	fLKrProf[ 4][ 13] =      .025811;
	fLKrProf[ 4][ 14] =      .020761;
	fLKrProf[ 4][ 15] =      .017251;
	fLKrProf[ 4][ 16] =      .014537;
	fLKrProf[ 4][ 17] =      .012128;
	fLKrProf[ 4][ 18] =      .010466;
	fLKrProf[ 4][ 19] =      .009035;
	fLKrProf[ 4][ 20] =      .007822;
	fLKrProf[ 4][ 21] =      .006956;
	fLKrProf[ 4][ 22] =      .006092;
	fLKrProf[ 4][ 23] =      .005375;
	fLKrProf[ 4][ 24] =      .004794;
	fLKrProf[ 4][ 25] =      .004306;
	fLKrProf[ 4][ 26] =      .003854;
	fLKrProf[ 4][ 27] =      .003530;
	fLKrProf[ 4][ 28] =      .003140;
	fLKrProf[ 4][ 29] =      .002908;
	fLKrProf[ 4][ 30] =      .002662;
	fLKrProf[ 4][ 31] =      .002440;
	fLKrProf[ 4][ 32] =      .002251;
	fLKrProf[ 4][ 33] =      .002117;
	fLKrProf[ 4][ 34] =      .001930;
	fLKrProf[ 4][ 35] =      .001789;
	fLKrProf[ 4][ 36] =      .001669;
	fLKrProf[ 4][ 37] =      .001575;
	fLKrProf[ 4][ 38] =      .001446;
	fLKrProf[ 4][ 39] =      .001355;
	fLKrProf[ 4][ 40] =      .001264;
	fLKrProf[ 4][ 41] =      .001181;
	fLKrProf[ 4][ 42] =      .001125;
	fLKrProf[ 4][ 43] =      .001051;
	fLKrProf[ 4][ 44] =      .000984;
	fLKrProf[ 4][ 45] =      .000935;
	fLKrProf[ 4][ 46] =      .000883;
	fLKrProf[ 4][ 47] =      .000837;
	fLKrProf[ 4][ 48] =      .000790;
	fLKrProf[ 4][ 49] =      .000745;
	fLKrProf[ 4][ 50] =      .000701;
	fLKrProf[ 4][ 51] =      .000681;
	fLKrProf[ 4][ 52] =      .000637;
	fLKrProf[ 4][ 53] =      .000610;
	fLKrProf[ 4][ 54] =      .000581;
	fLKrProf[ 4][ 55] =      .000550;
	fLKrProf[ 4][ 56] =      .000520;
	fLKrProf[ 4][ 57] =      .000493;
	fLKrProf[ 4][ 58] =      .000470;
	fLKrProf[ 4][ 59] =      .000450;
	fLKrProf[ 4][ 60] =      .000429;
	fLKrProf[ 4][ 61] =      .000407;
	fLKrProf[ 4][ 62] =      .000392;
	fLKrProf[ 4][ 63] =      .000374;
	fLKrProf[ 4][ 64] =      .000357;
	fLKrProf[ 4][ 65] =      .000341;
	fLKrProf[ 4][ 66] =      .000326;
	fLKrProf[ 4][ 67] =      .000307;
	fLKrProf[ 4][ 68] =      .000300;
	fLKrProf[ 4][ 69] =      .000288;
	fLKrProf[ 4][ 70] =      .000275;
	fLKrProf[ 4][ 71] =      .000265;
	fLKrProf[ 4][ 72] =      .000250;
	fLKrProf[ 4][ 73] =      .000242;
	fLKrProf[ 4][ 74] =      .000239;
	fLKrProf[ 4][ 75] =      .000223;
	fLKrProf[ 4][ 76] =      .000217;
	fLKrProf[ 4][ 77] =      .000204;
	fLKrProf[ 4][ 78] =      .000197;
	fLKrProf[ 4][ 79] =      .000189;
	fLKrProf[ 4][ 80] =      .000182;
	fLKrProf[ 4][ 81] =      .000180;
	fLKrProf[ 4][ 82] =      .000168;
	fLKrProf[ 4][ 83] =      .000164;
	fLKrProf[ 4][ 84] =      .000153;
	fLKrProf[ 4][ 85] =      .000147;
	fLKrProf[ 4][ 86] =      .000144;
	fLKrProf[ 4][ 87] =      .000137;
	fLKrProf[ 4][ 88] =      .000134;
	fLKrProf[ 4][ 89] =      .000129;
	fLKrProf[ 4][ 90] =      .000123;
	fLKrProf[ 4][ 91] =      .000119;
	fLKrProf[ 4][ 92] =      .000117;
	fLKrProf[ 4][ 93] =      .000110;
	fLKrProf[ 4][ 94] =      .000106;
	fLKrProf[ 4][ 95] =      .000105;
	fLKrProf[ 4][ 96] =      .000098;
	fLKrProf[ 4][ 97] =      .000096;
	fLKrProf[ 4][ 98] =      .000094;
	fLKrProf[ 4][ 99] =      .000092;
	fLKrProf[ 4][100] =      .000084;
	fLKrProf[ 4][101] =      .000085;
	fLKrProf[ 4][102] =      .000080;
	fLKrProf[ 4][103] =      .000077;
	fLKrProf[ 4][104] =      .000076;
	fLKrProf[ 4][105] =      .000073;
	fLKrProf[ 4][106] =      .000070;
	fLKrProf[ 4][107] =      .000070;
	fLKrProf[ 4][108] =      .000068;
	fLKrProf[ 4][109] =      .000065;
	fLKrProf[ 4][110] =      .000060;
	fLKrProf[ 4][111] =      .000061;
	fLKrProf[ 4][112] =      .000058;
	fLKrProf[ 4][113] =      .000055;
	fLKrProf[ 4][114] =      .000055;
	fLKrProf[ 4][115] =      .000052;
	fLKrProf[ 4][116] =      .000049;
	fLKrProf[ 4][117] =      .000049;
	fLKrProf[ 4][118] =      .000047;
	fLKrProf[ 4][119] =      .000045;
	fLKrProf[ 4][120] =      .000044;
	fLKrProf[ 5][  1] =      .400362;
	fLKrProf[ 5][  2] =      .392871;
	fLKrProf[ 5][  3] =      .370629;
	fLKrProf[ 5][  4] =      .334484;
	fLKrProf[ 5][  5] =      .277036;
	fLKrProf[ 5][  6] =      .201738;
	fLKrProf[ 5][  7] =      .138497;
	fLKrProf[ 5][  8] =      .096808;
	fLKrProf[ 5][  9] =      .068208;
	fLKrProf[ 5][ 10] =      .051224;
	fLKrProf[ 5][ 11] =      .039831;
	fLKrProf[ 5][ 12] =      .031669;
	fLKrProf[ 5][ 13] =      .025360;
	fLKrProf[ 5][ 14] =      .020826;
	fLKrProf[ 5][ 15] =      .017357;
	fLKrProf[ 5][ 16] =      .014624;
	fLKrProf[ 5][ 17] =      .012311;
	fLKrProf[ 5][ 18] =      .010534;
	fLKrProf[ 5][ 19] =      .009006;
	fLKrProf[ 5][ 20] =      .007815;
	fLKrProf[ 5][ 21] =      .006858;
	fLKrProf[ 5][ 22] =      .006045;
	fLKrProf[ 5][ 23] =      .005347;
	fLKrProf[ 5][ 24] =      .004828;
	fLKrProf[ 5][ 25] =      .004324;
	fLKrProf[ 5][ 26] =      .003913;
	fLKrProf[ 5][ 27] =      .003488;
	fLKrProf[ 5][ 28] =      .003166;
	fLKrProf[ 5][ 29] =      .002904;
	fLKrProf[ 5][ 30] =      .002678;
	fLKrProf[ 5][ 31] =      .002459;
	fLKrProf[ 5][ 32] =      .002235;
	fLKrProf[ 5][ 33] =      .002073;
	fLKrProf[ 5][ 34] =      .001919;
	fLKrProf[ 5][ 35] =      .001793;
	fLKrProf[ 5][ 36] =      .001663;
	fLKrProf[ 5][ 37] =      .001551;
	fLKrProf[ 5][ 38] =      .001440;
	fLKrProf[ 5][ 39] =      .001347;
	fLKrProf[ 5][ 40] =      .001260;
	fLKrProf[ 5][ 41] =      .001184;
	fLKrProf[ 5][ 42] =      .001116;
	fLKrProf[ 5][ 43] =      .001047;
	fLKrProf[ 5][ 44] =      .000993;
	fLKrProf[ 5][ 45] =      .000938;
	fLKrProf[ 5][ 46] =      .000882;
	fLKrProf[ 5][ 47] =      .000835;
	fLKrProf[ 5][ 48] =      .000781;
	fLKrProf[ 5][ 49] =      .000743;
	fLKrProf[ 5][ 50] =      .000707;
	fLKrProf[ 5][ 51] =      .000669;
	fLKrProf[ 5][ 52] =      .000635;
	fLKrProf[ 5][ 53] =      .000602;
	fLKrProf[ 5][ 54] =      .000583;
	fLKrProf[ 5][ 55] =      .000548;
	fLKrProf[ 5][ 56] =      .000522;
	fLKrProf[ 5][ 57] =      .000494;
	fLKrProf[ 5][ 58] =      .000472;
	fLKrProf[ 5][ 59] =      .000446;
	fLKrProf[ 5][ 60] =      .000422;
	fLKrProf[ 5][ 61] =      .000408;
	fLKrProf[ 5][ 62] =      .000389;
	fLKrProf[ 5][ 63] =      .000376;
	fLKrProf[ 5][ 64] =      .000358;
	fLKrProf[ 5][ 65] =      .000343;
	fLKrProf[ 5][ 66] =      .000325;
	fLKrProf[ 5][ 67] =      .000310;
	fLKrProf[ 5][ 68] =      .000298;
	fLKrProf[ 5][ 69] =      .000286;
	fLKrProf[ 5][ 70] =      .000272;
	fLKrProf[ 5][ 71] =      .000262;
	fLKrProf[ 5][ 72] =      .000248;
	fLKrProf[ 5][ 73] =      .000241;
	fLKrProf[ 5][ 74] =      .000229;
	fLKrProf[ 5][ 75] =      .000224;
	fLKrProf[ 5][ 76] =      .000211;
	fLKrProf[ 5][ 77] =      .000204;
	fLKrProf[ 5][ 78] =      .000196;
	fLKrProf[ 5][ 79] =      .000188;
	fLKrProf[ 5][ 80] =      .000183;
	fLKrProf[ 5][ 81] =      .000174;
	fLKrProf[ 5][ 82] =      .000167;
	fLKrProf[ 5][ 83] =      .000162;
	fLKrProf[ 5][ 84] =      .000156;
	fLKrProf[ 5][ 85] =      .000149;
	fLKrProf[ 5][ 86] =      .000146;
	fLKrProf[ 5][ 87] =      .000139;
	fLKrProf[ 5][ 88] =      .000135;
	fLKrProf[ 5][ 89] =      .000129;
	fLKrProf[ 5][ 90] =      .000124;
	fLKrProf[ 5][ 91] =      .000119;
	fLKrProf[ 5][ 92] =      .000115;
	fLKrProf[ 5][ 93] =      .000112;
	fLKrProf[ 5][ 94] =      .000107;
	fLKrProf[ 5][ 95] =      .000104;
	fLKrProf[ 5][ 96] =      .000099;
	fLKrProf[ 5][ 97] =      .000096;
	fLKrProf[ 5][ 98] =      .000093;
	fLKrProf[ 5][ 99] =      .000090;
	fLKrProf[ 5][100] =      .000087;
	fLKrProf[ 5][101] =      .000084;
	fLKrProf[ 5][102] =      .000080;
	fLKrProf[ 5][103] =      .000077;
	fLKrProf[ 5][104] =      .000076;
	fLKrProf[ 5][105] =      .000073;
	fLKrProf[ 5][106] =      .000071;
	fLKrProf[ 5][107] =      .000069;
	fLKrProf[ 5][108] =      .000065;
	fLKrProf[ 5][109] =      .000064;
	fLKrProf[ 5][110] =      .000061;
	fLKrProf[ 5][111] =      .000058;
	fLKrProf[ 5][112] =      .000058;
	fLKrProf[ 5][113] =      .000056;
	fLKrProf[ 5][114] =      .000054;
	fLKrProf[ 5][115] =      .000053;
	fLKrProf[ 5][116] =      .000050;
	fLKrProf[ 5][117] =      .000049;
	fLKrProf[ 5][118] =      .000048;
	fLKrProf[ 5][119] =      .000046;
	fLKrProf[ 5][120] =      .000045;
}
