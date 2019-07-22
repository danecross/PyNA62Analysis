// --------------------------------------------------------------
// History:
//
// Created by Letizia Peruzzo (letizia.peruzzo@cern.ch) 2016-06-02
//
// Inherit from MUV1/2 Reconstruction
// --------------------------------------------------------------

/// \class SAVReconstruction
/// \Brief
/// \EndBrief
/// \Detailed
/// The reconstruction makes use of the base class NA62VReconstruction and of the classes in NA62MC/SAC/Persistency (TRecoSAVEvent, TRecoSAVCandidate, TRecoSAVHit).
///\n
/// The SAV reconstruction basic parameters are set in NA62Reconstruction/config/SAV.conf
/// \n
/// =========
/// \n
/// SAV.conf
/// \n
/// =========
/// \n
/// Description of parameters can be found in SAV.conf, here the parameters you may want to change to perform your own reconstruction:
/// \n\n
/// =====================================================================================================================================
/// \n
/// SAV Reconstruction
/// \n
/// \n
/// \EndDetailed
/// \author Created by Letizia Peruzzo (letizia.peruzzo@cern.ch)

#include "Riostream.h"
#include "FADCEvent.hh"

#include "SAVReconstruction.hh"
#include "SAVDigitizer.hh"
#include "TSAVDigi.hh"

#include "TRecoSAVEvent.hh"
#include "TSlimRecoSAVEvent.hh"
#include "TRecoSAVHit.hh"
#include "TSpecialTriggerEvent.hh"
#include "NA62VRawDecoder.hh"
#include "NA62RecoManager.hh"
#include "NA62ConditionsService.hh"

#include "TH1.h"
#include "TRegexp.h"
#include <algorithm>
#include <vector>

//Variables for Minuit Minimizer
Double_t   *fSAVFitDataXptr=0, *fSAVFitDataYptr=0;
Double_t   *fSAVFitDataErrorXptr=0, *fSAVFitDataErrorYptr=0;
Int_t  fNSAVFitPoints=0;

SAVReconstruction::SAVReconstruction(TFile* HistoFile, TString ConfigFileName) : NA62VReconstruction(HistoFile, "SAV", ConfigFileName){

  fRecoEvent = new TRecoSAVEvent();
  fSlimRecoEvent = new TSlimRecoSAVEvent();

  //Parsing config file
  fLastNsamp = 32;
  fFirstSlot = 31;
  fLastSlot = 0;

  for (Int_t i=0; i<8; i++) fSAVT0[i]=0.;

  ParseConfFile(ConfigFileName);
  ResetHistograms();

  //Initializing the fitter
  fFitter = new TMinuit(7);
  Double_t   arg = 1.;
  Int_t  ierflg = 0;
  fFitter->SetPrintLevel(-1);
  fFitter->mnexcm("SET ERR", &arg ,1,ierflg);
}

SAVReconstruction::~SAVReconstruction(){
  ResetHistograms();
  if (fFitter) {
    delete fFitter;
    fFitter=0;
  }
}

//Reading parameters from the config files
void SAVReconstruction::ParseConfFile(TString ConfFileName) {

  std::cout <<"Parsing config file "<<ConfFileName<<std::endl;

  //Default file for time alignment, SAC channel equalization and SAV calibration
  fT0FileName="SAV-T0.dat";
  TString SACchannelEqFile="SAV-SACchannelEq.dat";
  TString SAVcalibrationFile="SAV-calibration.dat";

  std::ifstream confFile(ConfFileName.Data());
  if (!confFile.is_open()) {
    perror(ConfFileName);
    exit(kWrongConfiguration);
  }

  //Data from the config file
  TString Line;
  while (Line.ReadLine(confFile)) {
    if (Line.BeginsWith("#")) continue;
    else if (Line.BeginsWith("FirstSlot")) {
      fFirstSlot = TString(Line(TRegexp("[0-9]+"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("T0FileInput")) {
      TObjArray * line = Line.Tokenize(" ");
      fT0FileName = static_cast<TObjString*>(line->At(1))->GetString();
      delete line;
      continue;
    }
    else if (Line.BeginsWith("SACchannelEq")){
      TObjArray * line = Line.Tokenize(" ");
      SACchannelEqFile = static_cast<TObjString*>(line->At(1))->GetString();
      delete line;
      continue;
    }
    else if (Line.BeginsWith("SAVcalibration")){
      TObjArray * line = Line.Tokenize(" ");
      SAVcalibrationFile = static_cast<TObjString*>(line->At(1))->GetString();
      delete line;
      continue;
    }
    else if (Line.BeginsWith("SaveSaturated")){
      fSaveSaturated = TString(Line(TRegexp("[0-9]+"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("SaveUnderflow")){
      fSaveUnderflow = TString(Line(TRegexp("[0-9]+"))).Atoi();
      continue;
    }
  }
  confFile.close();

  TObjArray * line;

  //Getting Time Aligment Parameters
  if(NA62ConditionsService::GetInstance()->Open(fT0FileName)==kSuccess){
    while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fT0FileName))) {
      if (Line.BeginsWith("#")) continue;

      line = Line.Tokenize(" ");
      Int_t channel = static_cast<TObjString*>(line->At(1))->GetString().Atof();
      Double_t time = static_cast<TObjString*>(line->At(2))->GetString().Atof();
      if (TMath::Abs(time) < 999.) fSAVT0[(channel/10)*4 + (channel%10)-1] = time;

      delete line;
    }
    NA62ConditionsService::GetInstance()->Close(fT0FileName);
  }

  //Getting Calibration Parameters: SAC channel equalization.
  NA62ConditionsService::GetInstance()->Open(SACchannelEqFile);
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(SACchannelEqFile))) {
    if (Line.BeginsWith("#")) continue;

    line = Line.Tokenize(" ");
    Int_t channel = static_cast<TObjString*>(line->At(0))->GetString().Atof();
    Double_t eqFactor = static_cast<TObjString*>(line->At(1))->GetString().Atof();
    fSACchannelEq[channel-1] = eqFactor;

    delete line;
  }
  NA62ConditionsService::GetInstance()->Close(SACchannelEqFile);

  //Getting Calibration Parameters: SAV calibration.
  NA62ConditionsService::GetInstance()->Open(SAVcalibrationFile);
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(SAVcalibrationFile))) {
    if (Line.BeginsWith("#")) continue;

    line = Line.Tokenize(" ");

    Int_t channel = static_cast<TObjString*>(line->At(0))->GetString().Atof();
    Double_t par0 = static_cast<TObjString*>(line->At(1))->GetString().Atof();
    Double_t par1 = static_cast<TObjString*>(line->At(2))->GetString().Atof();
    fSAVcalibration[0][channel] = par0;
    fSAVcalibration[1][channel] = par1;

    delete line;
  }
  NA62ConditionsService::GetInstance()->Close(SAVcalibrationFile);
  //std::cout <<"GlobalOffset "<<fStationsT0[0]<< std::endl;
}

void SAVReconstruction::Init(NA62VReconstruction* MainReco) {

  //common part for all the subdetectors
  NA62VReconstruction::Init(MainReco);

  //Initializing Histograms
  InitHistograms();
}

TDetectorVEvent * SAVReconstruction::Trigger(TDetectorVEvent * tEvent, Event* /*tGenEvent*/){
  return tEvent;
}

void SAVReconstruction::StartOfBurst(){
  NA62VReconstruction::StartOfBurst(); // common part for all the subdetectors
}

void SAVReconstruction::EndOfBurst(){
  NA62VReconstruction::EndOfBurst(); // common part for all the subdetectors
}

TRecoVEvent * SAVReconstruction::ProcessEvent(TDetectorVEvent* tEvent, Event* tGenEvent){

  //Special Triggers
  if(tEvent->IsA() == TSpecialTriggerEvent::Class()){
    //read it
    return 0;
  }

  //common part for all the subdetectors
  NA62VReconstruction::ProcessEvent(tEvent, tGenEvent);

  UInt_t EventFlag = static_cast<FADCEvent*>(tEvent)->GetEventFlag();
  if (EventFlag&(0x1 << kCREAML1ErrorBit)){ //CREAM L1 Error
    for (int i=100; i<300; i++)
      fHCREAMFlags->Fill(i,3);
  }

  if (EventFlag != 0) fRecoEvent->SetErrorMaskBit(0,1);

  Int_t  NDigis = tEvent->GetNHits();
  if (NDigis < 1) return fRecoEvent; //No digi found

  TClonesArray &Digis = *static_cast<FADCEvent*>(tEvent)->GetHits();

  //Preparing to store hits
  std::vector <Int_t > PeakPosition;
  TRecoSAVHit* ThisHit;

  // -------------------------------------------------------------------------------------------- //
  // Loop over digis and produce hits ------------------------------------------------------------------------ //
  // -------------------------------------------------------------------------------------------- //
  for (Int_t  iHit=0; iHit<NDigis; iHit++){

    TSAVDigi* Digi = static_cast<TSAVDigi*>( Digis[iHit]);
    Double_t *Amplitude = Digi->GetAllSamples();
    Int_t  Nsamples = Digi->GetNSamples();
    Int_t  DetectorID = Digi->GetChannelID()/10; // 0=SAC 1=IRC
    Int_t  Channel = Digi->GetChannelID()%10 - 1;
    Int_t  Slot = fFirstSlot + fRawDecoder->GetDecoder()->GetChannelRO(Digi->GetChannelID())/32;
    //Int_t  CREAMChannel = fRawDecoder->GetDecoder()->GetChannelRO(Digi->GetChannelID())%32;

    if (Slot > fLastSlot) fLastSlot = Slot;
    if (Slot < fFirstSlot) fFirstSlot = Slot;
    fLastNsamp = Nsamples;

    // Hit Position
    TVector3 HitPos (0,0,0);
    HitPos[0] = Digi->GetChannelPosition().X();
    HitPos[1] = Digi->GetChannelPosition().Y();

    UInt_t flag = Digi->GetFlags();
    Bool_t Saturation = ((flag&(1<<kCREAMSaturationBit))!=0); //event with points saturating the ADC scale
    Bool_t Underflow  = ((flag&(1<<kCREAMUnderflowBit))!=0); //event with points under the ADC scale

    if (Saturation) fHCREAMFlags->Fill(Digi->GetChannelID(),1);
    if (Underflow) fHCREAMFlags->Fill(Digi->GetChannelID(),2);
    else if (!Saturation && !Underflow) fHCREAMFlags->Fill(Digi->GetChannelID(),0);

    Saturation = Saturation && fSaveSaturated;
    Underflow  = Underflow  && fSaveUnderflow;


    //-------------------------------------------
    // Hits.
    //-------------------------------------------

    //-------------------------------------------
    //Determining baseline value for this event
    //-------------------------------------------
    Int_t  Npoint = 0; //Number of points for baseline calculation
    Double_t   mean = 0; //Baseline variable
    Double_t   minimum = Amplitude[0], maximum = Amplitude[0]; //maximum oscillation of the baseline
    Double_t   min = minimum, max=maximum; //global maximum and minimum

    Double_t   int_base = 0, int_sig = 0;
    Int_t skip = 0;
    for (Int_t i=0; i<Nsamples; i++) {
    	fSAVFitDataX[i] = i;
    	fSAVFitDataErrorX[i] = 1./ClockPeriod;
    	fSAVFitDataY[i] = Amplitude[i];
    	fSAVFitDataErrorY[i] = 3.;
    	if (Amplitude[i]>max) max = Amplitude[i];
    	if (Amplitude[i]<min) min = Amplitude[i];

    	if (i < 5) int_base += (Amplitude[i])/5.;
    	if (i >= (Nsamples/2. - 2) && i<= Nsamples/2.+2) int_sig += (Amplitude[i])/5.;

    	if (i<1 || i>Nsamples-2) continue;
    	if (skip > 0) {
    		skip--;
    		continue;
    	}

    	Double_t  der2 = -1.;
    	if(i<Nsamples-1) der2 = TMath::Abs((Amplitude[i+1] - Amplitude[i-1])/2.);
    	if (der2 > 5){ skip = 2; continue; }
    	mean += Amplitude[i];
    	if (Amplitude[i]<minimum) minimum = Amplitude[i];
    	if (Amplitude[i]>maximum) maximum = Amplitude[i];
    	Npoint++;
    }

    if (max - min < 10.) continue; //If any peak is present it is too small

    //--------------------------------------------
    //Peaks finding
    //--------------------------------------------
    if (Npoint >= 5) mean /= Npoint;
    else             mean = min+10;
    //absolute minimum taken as reference if there are not enough points for the baseline extimation

    if ((mean - min)>50. && min>100.) mean = min + 50; //too high distance between minimum and extimated baseline

    const Double_t   DerLim = 15.; //Limit on the derivative
    const Double_t   DerLim2 = 20.; //Tighter limit on the derivative
    const Double_t   AmpLim = 20.; //Limit on the Amplitude

    PeakPosition.clear();
    Double_t   Der2Pt = 0;
    for (Int_t  iPt=1; iPt<Nsamples; iPt++){
    	Der2Pt = (Amplitude[iPt] - Amplitude[iPt-1]);
    	if (Der2Pt<DerLim) continue;
    	if ((Amplitude[iPt] - mean)<AmpLim && Der2Pt < DerLim2) continue;
    	while (iPt+1<Nsamples && Amplitude[iPt]<Amplitude[iPt+1]) iPt++;
    	if (iPt>Nsamples-2) continue;
    	PeakPosition.push_back(iPt);
    }

    if (PeakPosition.size()<1) continue; //No peak found


    //----------------------
    //Fitting the peaks
    //----------------------

    for (UInt_t  iPeak=0; iPeak<PeakPosition.size(); iPeak++){

    	//Single peak baseline determination
    	Double_t  base = 0.;
    	Int_t ibase = PeakPosition[iPeak];
    	if (PeakPosition[iPeak]>Nsamples/2.){
    		while (ibase-1>=0 && Amplitude[ibase]>Amplitude[ibase-1]) {
    			ibase--;
    		}
    	}
    	else{
    		while (ibase+1<Nsamples-1 && Amplitude[ibase]>Amplitude[ibase+1]) {
    			ibase++;
    		}
    	}
    	base = Amplitude[ibase];

    	//Comparison with the event baseline
		if ((base - mean) > 100) base = mean;
		else if ((base-mean) > 80) base = (2.*mean + base)/3.;
		else if ((base - mean) > 60) base = (base+mean)/2.;
		else if ((base - mean) > 40) base = (2.*base + mean)/3.;

		//----------------------------------
		//Two peaks at less than 8 bins
		//----------------------------------
		if (PeakPosition.size()>1 && iPeak<(PeakPosition.size()-1) && (PeakPosition[iPeak+1] - PeakPosition[iPeak])< 8) {

			//Setting Fit data addresses for Minuit
			fSAVFitDataXptr = &fSAVFitDataX[(PeakPosition[iPeak]-7 >= 0 ? PeakPosition[iPeak]-7 : 0)];
			fSAVFitDataYptr = &fSAVFitDataY[(PeakPosition[iPeak]-7 >= 0 ? PeakPosition[iPeak]-7 : 0)];
			fSAVFitDataErrorXptr = &fSAVFitDataErrorX[(PeakPosition[iPeak]-7 >= 0 ? PeakPosition[iPeak]-7 : 0)];
			fSAVFitDataErrorYptr = &fSAVFitDataErrorY[(PeakPosition[iPeak]-7 >= 0 ? PeakPosition[iPeak]-7 : 0)];
			fNSAVFitPoints = 1 + ((PeakPosition[iPeak+1]+3 < Nsamples ? PeakPosition[iPeak+1]+3 : Nsamples-1) - (PeakPosition[iPeak]-7 >= 0 ? PeakPosition[iPeak]-7 : 0));


			//Determining the starting values for the fit of the first peak
			Double_t  peak_time_1 = PeakPosition[iPeak]*Amplitude[PeakPosition[iPeak]];
			Double_t  peak_amp_1 = Amplitude[PeakPosition[iPeak]];

			for (Int_t i=1; i<5; i++){
				if (PeakPosition[iPeak]-i<0 || PeakPosition[iPeak]+i>Nsamples-1) break;
				Bool_t der = (Amplitude[PeakPosition[iPeak]-i] - Amplitude[PeakPosition[iPeak]-i+1])<0;
				der &= (Amplitude[PeakPosition[iPeak]+i] - Amplitude[PeakPosition[iPeak]+i-1])<0;
				if (!der) break;
				peak_time_1 += (i+PeakPosition[iPeak])*(Amplitude[PeakPosition[iPeak]+i]);
				peak_amp_1 += Amplitude[PeakPosition[iPeak]+i];

				peak_time_1 += (PeakPosition[iPeak]-i)*(Amplitude[PeakPosition[iPeak]-i]);
				peak_amp_1 += Amplitude[PeakPosition[iPeak]-i];

			}
			peak_time_1 /= peak_amp_1;

			Double_t  peak_sigma_1 = 0.;
			peak_amp_1 = 0.;
			Int_t Nsigma_1=0;

			for (Int_t i=0; i<5; i++){

				if (PeakPosition[iPeak]-i>=0){
					Bool_t der = ((Amplitude[PeakPosition[iPeak]-i] - Amplitude[PeakPosition[iPeak]-i+1])<0 || i==0);
					if (der && Amplitude[PeakPosition[iPeak]-i] < Amplitude[PeakPosition[iPeak]]){
						Nsigma_1++;
						peak_sigma_1 += (peak_time_1 - PeakPosition[iPeak] + i)*(peak_time_1 - PeakPosition[iPeak] + i)*(Amplitude[PeakPosition[iPeak]-i] - mean);
						peak_amp_1 += (Amplitude[PeakPosition[iPeak]-i] - mean);
					}
				}
				if (PeakPosition[iPeak]+i<Nsamples && i>0){
					Bool_t der = ((Amplitude[PeakPosition[iPeak]+i] - Amplitude[PeakPosition[iPeak]+i-1])<0);
					if (der && Amplitude[PeakPosition[iPeak]+i] < Amplitude[PeakPosition[iPeak]]){
						Nsigma_1++;
						peak_sigma_1 += (peak_time_1 - PeakPosition[iPeak] - i)*(peak_time_1 - PeakPosition[iPeak] - i)*(Amplitude[PeakPosition[iPeak]+i] - mean);
						peak_amp_1 += (Amplitude[PeakPosition[iPeak]+i] - mean);
					}
				}
			}

			if(peak_sigma_1/peak_amp_1>=0) peak_sigma_1 = TMath::Sqrt(peak_sigma_1/peak_amp_1);
			if (peak_sigma_1<1. || peak_sigma_1>5. || peak_sigma_1/peak_amp_1<0){ Nsigma_1=1; peak_sigma_1 = 1.2; }

			//Determining the starting values for the second peak fitting
			Double_t  peak_time_2 = PeakPosition[iPeak+1]*Amplitude[PeakPosition[iPeak+1]];
			Double_t  peak_amp_2 = Amplitude[PeakPosition[iPeak+1]];

			for (Int_t i=1; i<5; i++){
				if (PeakPosition[iPeak+1]-i<0 || PeakPosition[iPeak+1]+i>Nsamples-1) break;
				Bool_t der = (Amplitude[PeakPosition[iPeak+1]-i] - Amplitude[PeakPosition[iPeak+1]-i+1])<0;
				der &= (Amplitude[PeakPosition[iPeak+1]+i] - Amplitude[PeakPosition[iPeak+1]+i-1])<0;
				if (!der) break;

				peak_time_2 += (i+PeakPosition[iPeak+1])*(Amplitude[PeakPosition[iPeak+1]+i]);
				peak_amp_2 += Amplitude[PeakPosition[iPeak+1]+i];

				peak_time_2 += (PeakPosition[iPeak+1]-i)*(Amplitude[PeakPosition[iPeak+1]-i]);
				peak_amp_2 += Amplitude[PeakPosition[iPeak+1]-i];

			}
			peak_time_2 /= peak_amp_2;


			Double_t  peak_sigma_2 = 0.;
			peak_amp_2 = 0.;
			Int_t Nsigma_2 = 0;

			for (Int_t i=0; i<5; i++){

				if (PeakPosition[iPeak+1]-i>=0){
					Bool_t der = ((Amplitude[PeakPosition[iPeak+1]-i] - Amplitude[PeakPosition[iPeak+1]-i+1])<0 || i==0);
					if (der && Amplitude[PeakPosition[iPeak+1]-i] < Amplitude[PeakPosition[iPeak+1]]){
						Nsigma_2++;
						peak_sigma_2 += (peak_time_2 - PeakPosition[iPeak+1] + i)*(peak_time_2 - PeakPosition[iPeak+1] + i)*(Amplitude[PeakPosition[iPeak+1]-i] - mean);
						peak_amp_2 += (Amplitude[PeakPosition[iPeak+1]-i] - mean);
					}
				}
				if (PeakPosition[iPeak+1]+i<Nsamples && i>0){
					Bool_t der = ((Amplitude[PeakPosition[iPeak+1]+i] - Amplitude[PeakPosition[iPeak+1]+i-1])<0);
					if (der && Amplitude[PeakPosition[iPeak+1]+i] < Amplitude[PeakPosition[iPeak+1]]){
						Nsigma_2++;
						peak_sigma_2 += (peak_time_2 - PeakPosition[iPeak+1] - i)*(peak_time_2 - PeakPosition[iPeak+1] - i)*(Amplitude[PeakPosition[iPeak+1]+i] - mean);
						peak_amp_2 += (Amplitude[PeakPosition[iPeak+1]+i] - mean);
					}
				}
			}

			if(peak_sigma_2/peak_amp_2>=0) peak_sigma_2 = TMath::Sqrt(peak_sigma_2/peak_amp_2);
			if (peak_sigma_2<1. || peak_sigma_2>5. || peak_sigma_2/peak_amp_2<0){ Nsigma_2=1; peak_sigma_2 = 1.2; }

			//Setting up the fitter
			fFitter->SetFCN(ChiSquareDoublePeak);
			Double_t   vstart[7] = {base,Amplitude[PeakPosition[iPeak]]-base,peak_time_1,peak_sigma_1,Amplitude[PeakPosition[iPeak+1]]-mean,peak_time_2,peak_sigma_2};
			Double_t   step[7] = {0.1,0.1,0.001,0.05,0.1,0.001,0.05};
			Int_t  ierflg = 0;

			fFitter->mnparm(0, "Baseline", vstart[0], step[0], 0,0,ierflg);
			fFitter->mnparm(1, "Amplitude_1", vstart[1], step[1], 0,0,ierflg);
			fFitter->mnparm(2, "Time_1", vstart[2], step[2], 0,0,ierflg);
			fFitter->mnparm(3, "Sigma_1", vstart[3], step[3], 0,0,ierflg);
			fFitter->mnparm(4, "Amplitude_2", vstart[4], step[4], 0,0,ierflg);
			fFitter->mnparm(5, "Time_2", vstart[5], step[5], 0,0,ierflg);
			fFitter->mnparm(6, "Sigma_2", vstart[6], step[6], 0,0,ierflg);

			//Setting the limits of the fitting parameters
			Double_t   Limits[3];
			Limits[0] = 1; Limits[1] = mean-30; Limits[2] = mean+30;
			fFitter->mnexcm("SET LIM",Limits,3,ierflg);

			if (PeakPosition[iPeak]>0){
				Limits[0] = 2; Limits[1] = (Amplitude[PeakPosition[iPeak]] - base - 60 > 5 ? Amplitude[PeakPosition[iPeak]] -base - 60 : 5);
				Limits[2] = (Amplitude[PeakPosition[iPeak]] - base + 60 > 60 ? Amplitude[PeakPosition[iPeak]] -base + 60 : 60);
				fFitter->mnexcm("SET LIM",Limits,3,ierflg);
				Limits[0] = 3; Limits[1] = PeakPosition[iPeak] - peak_sigma_1/TMath::Sqrt(Nsigma_1); Limits[2] = PeakPosition[iPeak] + peak_sigma_1/TMath::Sqrt(Nsigma_1);
				fFitter->mnexcm("SET LIM",Limits,3,ierflg);
			}

			Limits[0] = 4; Limits[1] = 1.; Limits[2] = 3.;
			fFitter->mnexcm("SET LIM",Limits,3,ierflg);

			Limits[0] = 5; Limits[1] = 5.;
			Limits[2] = (Amplitude[PeakPosition[iPeak+1]] - base + 60 > 60 ? Amplitude[PeakPosition[iPeak+1]] -base + 60 : 60);
			fFitter->mnexcm("SET LIM",Limits,3,ierflg);

			Limits[0] = 6; Limits[1] = PeakPosition[iPeak+1] - peak_sigma_2/TMath::Sqrt(Nsigma_2); Limits[2] = PeakPosition[iPeak+1] + peak_sigma_2/TMath::Sqrt(Nsigma_2);
			fFitter->mnexcm("SET LIM",Limits,3,ierflg);

			Limits[0] = 7; Limits[1] = 1.; Limits[2] = 3.;
			fFitter->mnexcm("SET LIM",Limits,3,ierflg);

			//Fitting
			Double_t   Args[2]={1000.,1.};
			fFitter->mnexcm("MIGRAD", Args ,2,ierflg);

			//Exporting the fitting results
			for (Int_t ipar=0; ipar<7; ipar++) fFitter->GetParameter(ipar,fPars[ipar],fParErrors[ipar]);

			//Resetting the fitter
			fFitter->mnexcm("CLE",Args,0,ierflg);
			Double_t   baseline = fPars[0];

			//Determining peak 1 position and amplitude
			Double_t   time1 = (fPars[2]) * ClockPeriod;
			Double_t   peak1 = fPars[1];
			Double_t   energy1 = peak1;

			if (fRecoEvent->GetIsMC()) time1 -= ClockPeriod * Double_t(Nsamples)/2.;

			if (peak1>0. && peak1 < 3.e+4) {
				time1 -= GetT0Correction(Digi);
				time1 -= fSAVT0[(Digi->GetChannelID()/10)*4 + (Digi->GetChannelID()%10)-1];

				// SAC channel equalization.
				if (DetectorID==0) {
				  peak1 *= fSACchannelEq[Channel];
				  // SAC energy.
				  energy1 = peak1 / fSAVcalibration[1][0];
				}

				// IRC energy.
				if (DetectorID==1) energy1 = peak1 / fSAVcalibration[1][Channel+1];

				fHChannelsOccupancy -> Fill(Digi->GetChannelID());
				fHAmplitudeVSChannel -> Fill(Digi->GetChannelID(),peak1);
				fHBaselinesVSChannel->Fill(Digi->GetChannelID(),baseline);
				fHEnergyVSChannel -> Fill(Digi->GetChannelID(),energy1);
				if (TMath::Abs(time1 - NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256.) < 20.) fHEnergyVSChannel_L0intime -> Fill(Digi->GetChannelID(),energy1);
				fHHitTime -> Fill(fPars[2],Slot);
				fHHitTime_L0time -> Fill(time1 - NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256.);
				fHHitTimeVSBurst -> Fill(fRecoEvent->GetBurstID(),time1);
				fHHitTimeVSChannel -> Fill(Digi->GetChannelID(),time1);

				// add hit information.
				ThisHit = static_cast<TRecoSAVHit*>( fRecoEvent -> AddHit(Digi));
				ThisHit -> DecodeChannelID(Digi->GetChannelID());
				ThisHit -> SetTime(time1);
				ThisHit -> SetAmplitude(peak1);
				ThisHit -> SetBaseline(baseline);
				ThisHit -> SetEnergy(energy1*1000.);
				ThisHit -> SetPosition(HitPos);
			}

			//Determining peak 2 position and amplitude
			Double_t   time2 = (fPars[5]) * ClockPeriod;
			Double_t   peak2 = fPars[4];
			Double_t   energy2 = peak2;

			if (fRecoEvent->GetIsMC()) time2 -= ClockPeriod * Double_t(Nsamples)/2.;

			if (peak2>0. && peak2 < 3.e+4) {

				time2 -= GetT0Correction(Digi);
				time2 -= fSAVT0[(Digi->GetChannelID()/10)*4 + (Digi->GetChannelID()%10)-1];

				// SAC channel equalization.
				if (DetectorID==0) {
				  peak2 *= fSACchannelEq[Channel];
				  // SAC energy.
				  energy2 = peak2 / fSAVcalibration[1][0];
				}

				// IRC energy.
				if (DetectorID==1) energy2 = peak2 / fSAVcalibration[1][Channel+1];

				fHChannelsOccupancy -> Fill(Digi->GetChannelID());
				fHAmplitudeVSChannel -> Fill(Digi->GetChannelID(),peak2);
				fHEnergyVSChannel -> Fill(Digi->GetChannelID(),energy2);
				if (TMath::Abs(time2 - NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256.) < 20.) fHEnergyVSChannel_L0intime -> Fill(Digi->GetChannelID(),energy2);
				fHHitTime -> Fill(fPars[2],Slot);
				fHHitTime_L0time -> Fill(time2 - NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256.);
				fHHitTimeVSBurst -> Fill(fRecoEvent->GetBurstID(),time2);
				fHHitTimeVSChannel -> Fill(Digi->GetChannelID(),time2);

				// add hit information.
				ThisHit = static_cast<TRecoSAVHit*>( fRecoEvent -> AddHit(Digi));
				ThisHit -> DecodeChannelID(Digi->GetChannelID());
				ThisHit -> SetTime(time2);
				ThisHit -> SetAmplitude(peak2);
				ThisHit -> SetBaseline(baseline);
				ThisHit -> SetEnergy(energy2*1000.);
				ThisHit -> SetPosition(HitPos);
			}

			iPeak++;
			continue;

		}

		//---------------------------------
		//Isolated one peak
		//---------------------------------
		else{

			//Setting Fit data addresses for minuit
			fSAVFitDataXptr = &fSAVFitDataX[(PeakPosition[iPeak]-5 >= 0 ? PeakPosition[iPeak]-5 : 0)];
			fSAVFitDataYptr = &fSAVFitDataY[(PeakPosition[iPeak]-5 >= 0 ? PeakPosition[iPeak]-5 : 0)];
			fSAVFitDataErrorXptr = &fSAVFitDataErrorX[(PeakPosition[iPeak]-5 >= 0 ? PeakPosition[iPeak]-5 : 0)];
			fSAVFitDataErrorYptr = &fSAVFitDataErrorY[(PeakPosition[iPeak]-5 >= 0 ? PeakPosition[iPeak]-5 : 0)];
			fNSAVFitPoints = 1 + ((PeakPosition[iPeak]+3 < Nsamples ? PeakPosition[iPeak]+3 : Nsamples-1) - (PeakPosition[iPeak]-5 >= 0 ? PeakPosition[iPeak]-5 : 0));



			//Determining starting values for the fit
			Double_t  peak_time = PeakPosition[iPeak]*Amplitude[PeakPosition[iPeak]];
			Double_t  peak_amp = Amplitude[PeakPosition[iPeak]];

			for (Int_t i=1; i<7; i++){
				if (PeakPosition[iPeak]-i<0 || PeakPosition[iPeak]+i>Nsamples-1) break;
				Bool_t der = (Amplitude[PeakPosition[iPeak]-i] - Amplitude[PeakPosition[iPeak]-i+1])<0;
				der &= (Amplitude[PeakPosition[iPeak]+i] - Amplitude[PeakPosition[iPeak]+i-1])<0;
				if (!der) break;

				peak_time += (i+PeakPosition[iPeak])*(Amplitude[PeakPosition[iPeak]+i]);
				peak_amp += Amplitude[PeakPosition[iPeak]+i];

				peak_time += (PeakPosition[iPeak]-i)*(Amplitude[PeakPosition[iPeak]-i]);
				peak_amp += Amplitude[PeakPosition[iPeak]-i];

			}
			peak_time /= peak_amp;


			Double_t  peak_sigma = 0.;
			peak_amp = 0.;
			Int_t Nsigma = 0;

			for (Int_t i=0; i<5; i++){

				if (PeakPosition[iPeak]-i>=0){
					Bool_t der = ((Amplitude[PeakPosition[iPeak]-i] - Amplitude[PeakPosition[iPeak]-i+1])<0);
					if (der && Amplitude[PeakPosition[iPeak]-i] < Amplitude[PeakPosition[iPeak]]){
						peak_sigma += (peak_time - PeakPosition[iPeak] + i)*(peak_time - PeakPosition[iPeak] + i)*(Amplitude[PeakPosition[iPeak]-i] - mean);
						peak_amp += (Amplitude[PeakPosition[iPeak]-i] - mean);
						Nsigma++;
					}
				}
				if (PeakPosition[iPeak]+i<Nsamples && i>0){
					Bool_t der = ((Amplitude[PeakPosition[iPeak]+i] - Amplitude[PeakPosition[iPeak]+i-1])<0);
					if (der && Amplitude[PeakPosition[iPeak]+i] < Amplitude[PeakPosition[iPeak]]){
						peak_sigma += (peak_time - PeakPosition[iPeak] - i)*(peak_time - PeakPosition[iPeak] - i)*(Amplitude[PeakPosition[iPeak]+i] - mean);
						peak_amp += (Amplitude[PeakPosition[iPeak]+i] - mean);
						Nsigma++;
					}
				}
			}

			if(peak_sigma/peak_amp>=0) peak_sigma = TMath::Sqrt(peak_sigma/peak_amp);
			if (peak_sigma<1. || peak_sigma>5. || peak_sigma/peak_amp<0) peak_sigma = 1.2;

			//Setting up the fitter
			fFitter->SetFCN(ChiSquareSinglePeak);
			Double_t   vstart[4] = {base,Amplitude[PeakPosition[iPeak]]-mean,peak_time,peak_sigma};
			Double_t   step[4] = {0.1,0.1,0.001,0.05};
			Int_t  ierflg = 0;

			fFitter->mnparm(0, "Baseline", vstart[0], step[0], 0,0,ierflg);
			fFitter->mnparm(1, "Amplitude", vstart[1], step[1], 0,0,ierflg);
			fFitter->mnparm(2, "Time", vstart[2], step[2], 0,0,ierflg);
			fFitter->mnparm(3, "Sigma", vstart[3], step[3], 0,0,ierflg);


			//Setting the limits for the fitting parameters
			Double_t   Limits[3];
			Limits[0] = 1; Limits[1] = mean-30; Limits[2] = mean+30;
			fFitter->mnexcm("SET LIM",Limits,3,ierflg);

			if (PeakPosition[iPeak]>0){
				Limits[0] = 2;
				Limits[1] = ((Amplitude[PeakPosition[iPeak]] - base - 60) > 10 ? (Amplitude[PeakPosition[iPeak]] - base - 60) : 10);
				Limits[2] = ((Amplitude[PeakPosition[iPeak]] - base + 60) > 10 ? (Amplitude[PeakPosition[iPeak]] - base + 60) : 100);
				fFitter->mnexcm("SET LIM",Limits,3,ierflg);
			}

			Limits[0] = 4; Limits[1] = 1.; Limits[2] = 5.;
			fFitter->mnexcm("SET LIM",Limits,3,ierflg);

			if (PeakPosition[iPeak]>0){
				Limits[0] = 3; Limits[1] = PeakPosition[iPeak] - peak_sigma/TMath::Sqrt(Nsigma); Limits[2] = PeakPosition[iPeak] + peak_sigma/TMath::Sqrt(Nsigma);
				fFitter->mnexcm("SET LIM",Limits,3,ierflg);
			}

			//Fitting
			Double_t   Args[2]={500.,1.};
			fFitter->mnexcm("MIGRAD", Args ,2,ierflg);
			for (Int_t ipar=0; ipar<4; ipar++)
			  fFitter->GetParameter(ipar,fPars[ipar],fParErrors[ipar]);
			fFitter->mnexcm("CLE",Args,0,ierflg);

			//Determining peak position and amplitude
			Double_t   baseline = fPars[0];
			Double_t   time = (fPars[2]) * ClockPeriod;
			Double_t   peak = fPars[1];
			Double_t   energy = peak;
			if (fRecoEvent->GetIsMC()) time -= ClockPeriod * Double_t(Nsamples)/2.;

			//Storing the info
			if (peak>0. && peak < 3.e+4) {

				// T0 corrections.
				time -= GetT0Correction(Digi);
				time -= fSAVT0[(Digi->GetChannelID()/10)*4 + (Digi->GetChannelID()%10)-1];

				// SAC channel equalization.
				if (DetectorID==0) {
				  peak *= fSACchannelEq[Channel];
				  energy = peak / fSAVcalibration[1][0];
				}

				// IRC energy.
				if (DetectorID==1) energy = (peak) / fSAVcalibration[1][Channel+1];

				fHChannelsOccupancy -> Fill(Digi->GetChannelID());
				fHAmplitudeVSChannel -> Fill(Digi->GetChannelID(),peak);
				fHBaselinesVSChannel->Fill(Digi->GetChannelID(),baseline);
				fHEnergyVSChannel -> Fill(Digi->GetChannelID(),energy);
				if (TMath::Abs(time - NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256.) < 20.) fHEnergyVSChannel_L0intime -> Fill(Digi->GetChannelID(),energy);
				fHHitTime -> Fill(fPars[1],Slot);
				fHHitTime_L0time -> Fill(time - NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256.);
				fHHitTimeVSBurst -> Fill(fRecoEvent->GetBurstID(),time);
				fHHitTimeVSChannel -> Fill(Digi->GetChannelID(),time);


				ThisHit = static_cast<TRecoSAVHit*>( fRecoEvent -> AddHit(Digi));
				ThisHit -> DecodeChannelID(Digi->GetChannelID());
				ThisHit -> SetTime(time);
				ThisHit -> SetAmplitude(peak);
				ThisHit -> SetBaseline(baseline);
				ThisHit -> SetEnergy(energy*1000.);
				ThisHit -> SetPosition(HitPos);

			}

		}
  }

  }
  // -------------------------------------------------------------------------------------------- //
  // Produce candidates ------------------------------------------------------------------------ //
  // -------------------------------------------------------------------------------------------- //

  TClonesArray *HitsArray = fRecoEvent->GetHits();
  Int_t Nhits = fRecoEvent->GetNHits();
  fHNHitsVSBurst -> Fill(fRecoEvent->GetBurstID(),Nhits);

  std::vector <TRecoSAVHit*> DetectorHits[2];
  std::vector <Bool_t> Used[2];
  Int_t NUsedHits[2] = {0,0};
  Int_t NHits[2] = {0,0};
  Int_t SeedIndex[2] = {-1,-1};
  Double_t SeedAmplitude[2] = {0,0};

  // Search the seed.
  for (Int_t i=0; i<HitsArray->GetEntries(); i++) {

    TRecoSAVHit *hit = static_cast<TRecoSAVHit*>(HitsArray->At(i));
    Int_t DetectorID = hit->GetDetector();
    //Double_t ChannelID = hit->GetChannel() % 10;

    DetectorHits[DetectorID].push_back(hit); // hit in the detector.
    Used[DetectorID].push_back(false); // hit not yet used.
    NHits[DetectorID]++; // number of hits per detector.

    Double_t A = hit->GetAmplitude();

    if (SeedAmplitude[DetectorID] > A) continue;

    SeedAmplitude[DetectorID] = A;
    SeedIndex[DetectorID] = NHits[DetectorID]-1;
  }

  // Grouping hits in clusters.
  for (Int_t jDetector=0; jDetector <2; jDetector++) {
    if (NHits[jDetector] < 1) continue;
    while ((NHits[jDetector] - NUsedHits[jDetector]) > 0) {
      if (SeedAmplitude[jDetector] < 50.) break;
      TRecoSAVCandidate *candidate = static_cast<TRecoSAVCandidate*>( fRecoEvent->AddCandidate());

      // start from seed
      candidate->AddHit(HitsArray->IndexOf(DetectorHits[jDetector][SeedIndex[jDetector]]));

      Used[jDetector][SeedIndex[jDetector]] = true;

      NUsedHits[jDetector]++;

      Int_t NHitsCand = 1;
      Double_t SeedTime = DetectorHits[jDetector][SeedIndex[jDetector]]->GetTime();
      Double_t Time = DetectorHits[jDetector][SeedIndex[jDetector]]->GetTime();
      Double_t Energy = DetectorHits[jDetector][SeedIndex[jDetector]]->GetEnergy();
      Double_t X = DetectorHits[jDetector][SeedIndex[jDetector]]->GetPosition().X() * Energy;
      Double_t Y = DetectorHits[jDetector][SeedIndex[jDetector]]->GetPosition().Y() * Energy;

      for (Int_t jHit=0; jHit < NHits[jDetector]; jHit++) {
        if (Used[jDetector][jHit]) continue;
        Double_t hitTime = DetectorHits[jDetector][jHit]->GetTime();
        fHHitTimeDist[jDetector]->Fill(hitTime - SeedTime);
        if (TMath::Abs(hitTime - SeedTime) > 5. ) continue;
        fHCandidateHitTime[jDetector]->Fill(hitTime - Time/NHitsCand);
        candidate->AddHit(HitsArray->IndexOf(DetectorHits[jDetector][jHit]));
        Used[jDetector][jHit] = true;
        NUsedHits[jDetector]++;
        NHitsCand++;

        Time += hitTime;
        Energy += DetectorHits[jDetector][jHit]->GetEnergy();
        X += (DetectorHits[jDetector][jHit]->GetPosition().X() * DetectorHits[jDetector][jHit]->GetEnergy());       
        Y += (DetectorHits[jDetector][jHit]->GetPosition().Y() * DetectorHits[jDetector][jHit]->GetEnergy());
      }

      Time /= NHitsCand;
      X /= Energy;
      Y /= Energy;

      candidate->SetTime(Time);
      candidate->SetEnergy(Energy);
      candidate->SetPosition(X,Y);

      fHCandidateMap[jDetector]->Fill(candidate->GetX(),candidate->GetY());
      fHNHits[jDetector]->Fill(NUsedHits[jDetector]);
      fHCandidateEnergy[jDetector]->Fill(Energy/1000.);

      SeedAmplitude[jDetector] = 0;
      SeedIndex[jDetector] = -1;

      for (Int_t j=0; j < NHits[jDetector]; j++) {

        if (Used[jDetector][j]) continue;
        if (DetectorHits[jDetector][j]->GetAmplitude() < SeedAmplitude[jDetector]) continue;

        SeedAmplitude[jDetector] = DetectorHits[jDetector][j]->GetAmplitude();
        SeedIndex[jDetector] = j; 
      } 
    }
  }
  return fRecoEvent;
}

void SAVReconstruction::EndProcessing(){
  NA62VReconstruction::EndProcessing(); // call base class for raw hist output
  // Write histos
  SaveHistograms();
}

void SAVReconstruction::FillTimes(Double_t ReferenceTime) {
  //common part for all the subdetectors
  NA62VReconstruction::FillTimes(ReferenceTime);

  //-- add here the user code to fill histos for T0 evaluation --//
  if (fHRecoHitTimeWrtReferenceVsROChannel) {
    TClonesArray *Hits = fRecoEvent->GetHits();
    for (Int_t i=0; i<fRecoEvent->GetNHits(); i++) {
      TRecoSAVHit * Hit = static_cast<TRecoSAVHit*>( Hits->At(i));
      Int_t channel = Hit->GetChannelID();
      Int_t ROChannel = fRawDecoder->GetDecoder()->GetChannelRO(Hit->GetChannelID());
      if(fHRecoHitTimeWrtReferenceVsROChannel) GetHRecoHitTimeWrtReferenceVsROChannel()->Fill(ROChannel,Hit->GetTime()-ReferenceTime);
      if(fHRecoHitTimeWrtReferenceVsROChannelNoT0) GetHRecoHitTimeWrtReferenceVsROChannelNoT0()->Fill(ROChannel,Hit->GetTime()-ReferenceTime+fSAVT0[(channel/10)*4 + (channel%10)-1]);
    }
  }  
}

void SAVReconstruction::InitHistograms() {

  GetOrMakeDir(fHistoFile,"SAVMonitor");
  GetOrMakeDir(fHistoFile,"SAVMonitor/Histos");

  //Hits
  AddHisto(fHCREAMFlags = new TH2I ("CREAMFlags","Digi Flags",20,0,20,4,0,4));
  fHCREAMFlags -> GetYaxis() -> SetBinLabel(1,"No Error");
  fHCREAMFlags -> GetYaxis() -> SetBinLabel(2,"Overflow");
  fHCREAMFlags -> GetYaxis() -> SetBinLabel(3,"Underflow");
  fHCREAMFlags -> GetYaxis() -> SetBinLabel(4,"L1 Error");
  fHCREAMFlags -> GetXaxis() -> SetTitle("Channel ID");

  AddHisto(fHHitTime = new TH2F("HitTime","Hit Time",30,0,30,5,14,18));
  fHHitTime -> GetXaxis() -> SetTitle("Time Slice");
  fHHitTime -> GetYaxis() -> SetTitle("CREAM Slot");

  AddHisto(fHChannelsOccupancy = new TH1I("ChannelsOccupancy","Channels Occupancy",20,0,20));
  fHChannelsOccupancy->GetXaxis()->SetTitle("Channel ID");

  AddHisto(fHAmplitudeVSChannel = new TH2F("AmplitudeVSChannel","Amplitude vs Channel; Channel ID; Amplitude [ADC counts]",20,0,20,500,0,5000));

  AddHisto(fHBaselinesVSChannel = new TH2F("BaselinesVSChannel","Baseline vs Channel; Channel ID; Baseline [ADC counts]",20,0,20,500,0,5000));

  AddHisto(fHEnergyVSChannel = new TH2F("EnergyVSChannel","Energy vs Channel; Channel ID; Energy [GeV]",20,0,20,500,0,100));

  AddHisto(fHEnergyVSChannel_L0intime = new TH2F("EnergyVSChannel_L0intime","Energy vs Channel for hit in time wrt L0 FineTime; Channel ID; Energy [GeV]",20,0,20,500,0,100));

  AddHisto(fHHitTime_L0time = new TH1D("HitTime_L0time","Hit time - L0 FineTime; t [ns]",500,-250.,250.));

  AddHisto(fHHitTimeVSBurst = new TH2F("HitTimeVSBurst","Hit time vs BurstID; Burst ID; Hit time [ns]",3000,0,3000,100,-200.,200.));

  AddHisto(fHHitTimeVSChannel = new TH2F ("HitTimeVSChannel","Hit time vs Channel; Channel ID; Hit time [ns]",20,0,20,100,-200.,200.));

  AddHisto(fHNHitsVSBurst = new TH2I ("NHitsVSBurst","NHits vs BurstID; BurstID; NHits",3000,0,3000,50,0,50));

  // Candidates
  for (Int_t i=0; i<2; i++) {
    AddHisto(fHHitTimeDist[i] = new TH1D(Form("HitTimeDist_dID%d",i),"Hit time distribution; #Deltat_{seed - hit} [ns]",2000,-100.,100.));

    AddHisto(fHCandidateHitTime[i] = new TH1D(Form("CandidateHitTime_dID%d",i),"Candidate hits time distribution; #Deltat_{seed - hit} [ns]",2000,-10.,10.));

    AddHisto(fHCandidateMap[i] = new TH2F(Form("CandidateMap_dID%d",i),"Candidate Map; x [mm]; y [mm]",150,-300.,300.,150,-300.,300.));

    AddHisto(fHNHits[i] = new TH1I(Form("NHits_dID%d",i),"Number of hits; N hits",100,0,100));

    AddHisto(fHCandidateEnergy[i] = new TH1D(Form("CandidateEnergy_dID%d",i),"Candidate energy; E [GeV]",200,0.,100.));
  }
}

void SAVReconstruction::SaveHistograms() {
  for (Int_t i=0; i<fHistoArray->GetEntries(); i++){
    TH1* histo = static_cast<TH1*>( fHistoArray->At(i));
    fHistoFile->cd("/SAVMonitor/");
    histo->Write();
  }
  fHistoFile->cd("/");
}

void SAVReconstruction::ResetHistograms() {}

Double_t   SAVReconstruction::SinglePeakFunction(Double_t   x, Double_t   * pars){
  Double_t  value = pars[0];
  value += pars[1] * TMath::Gaus(x,pars[2],pars[3]);
  return value;
}

void SAVReconstruction::ChiSquareSinglePeak(Int_t  &/*npar*/, Double_t   */*gin*/, Double_t   &f, Double_t   *par, Int_t  /*iflag*/){

  Double_t   chisquare = 0.;
  for (Int_t i=0; i<fNSAVFitPoints; i++) {
    Double_t   delta = (SinglePeakFunction(fSAVFitDataXptr[i],par) - fSAVFitDataYptr[i]);
    Double_t   err2_x = SinglePeakFunction(fSAVFitDataErrorXptr[i] + fSAVFitDataXptr[i],par);
    err2_x -= SinglePeakFunction(fSAVFitDataXptr[i] - fSAVFitDataErrorXptr[i],par);
    err2_x *=  err2_x/4.;
    Double_t   err2_y = fSAVFitDataErrorYptr[i]*fSAVFitDataErrorYptr[i];
    Double_t   err2 = err2_x + err2_y;
    chisquare+=delta*delta / err2;
  }
  f = chisquare;
}

Double_t SAVReconstruction::DoublePeakFunction(Double_t   x, Double_t   * pars){
  Double_t  value = pars[0];
  value += pars[1] * TMath::Gaus(x,pars[2],pars[3]);
  value += pars[4] * TMath::Gaus(x,pars[5],pars[6]);
  return value;
}

void SAVReconstruction::ChiSquareDoublePeak(Int_t  &/*npar*/, Double_t   */*gin*/, Double_t   &f, Double_t   *par, Int_t  /*iflag*/){

  Double_t   chisquare = 0.;
  for (Int_t i=0; i<fNSAVFitPoints; i++) {
    Double_t   delta = (DoublePeakFunction(fSAVFitDataXptr[i],par) - fSAVFitDataYptr[i]);
    Double_t   err2_x = DoublePeakFunction(fSAVFitDataErrorXptr[i] + fSAVFitDataXptr[i],par);
    err2_x -= DoublePeakFunction(fSAVFitDataXptr[i] - fSAVFitDataErrorXptr[i],par);
    err2_x *=  err2_x/4.;
    Double_t   err2_y = fSAVFitDataErrorYptr[i]*fSAVFitDataErrorYptr[i];
    Double_t   err2 = err2_x + err2_y;
    chisquare+=delta*delta / err2;
  }

  f = chisquare;
}
