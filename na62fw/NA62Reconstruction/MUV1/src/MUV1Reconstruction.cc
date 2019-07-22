// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-05-05
//
// Modified by Riccardo Aliberti (riccardo.aliberti@cern.ch) 2015-03-11
// --------------------------------------------------------------

/// \class MUV1Reconstruction
/// \Brief
/// Main MUV1 reconstruction: starts from the analog signals of the photomultipliers (ADC Samples) stored in TMUV1Digi: the recontruction finds and fits the peaks creating a TRecoMUV1Hit for each of them.\n
/// The Hits are organized in vector (one per channel) ordered by time.\n
/// For each readout side ( top, bottom, jura and saleve ) the hits are placed in 1D clusters and then these clusters are used to build the 2D clusters corresponding to the final TRecoMUV1Candidate. \n
/// \EndBrief
/// \Detailed
/// The reconstruction makes use of the base class NA62VReconstruction, the class MUV1HitsCluster and of the classes in NA62MC/MUV1/Persistency (TRecoMUV1Event, TRecoMUV1Candidate, TRecoMUV1Hit).
///\n
/// The MUV1 reconstruction basic parameters are set in NA62Reconstruction/config/MUV1.conf
/// \n
/// =========
/// \n
/// MUV1.conf
/// \n
/// =========
/// \n
/// Description of parameters can be found in MUV1.conf, here the parameters you may want to change to perform your own reconstruction:
/// \n\n
/// ChannelClusterTimeCut= 13 --> maximum time distance ( in ns ) between two hits to be in same 1D cluster.
/// \n\n
/// SideClusterTimeCut= 25 --> maximum time distance between two 1D clusters to be used for generating a Candidate
/// \n\n
/// Calibration= 1478 --> Set the conversion value for Charge to Energy in fC/GeV
/// \n\n
/// SaveSaturated= 0 --> if 1 save in the MUV1Monitor/Histos folder a plot of the analog signals in case of ADC saturation
/// \n\n
/// SaveRefused= 0 --> if 1 save in the MUV1Monitor/Histos folder a plot of the analog signals in case 1 of the fitted peaks has been refused ( for reconstruction debugging )
/// \n
/// \n
///=====================================================================================================================================
/// \n
/// MUV1 Reconstruction
/// \n
/// \n
/// Digi -> Hit Conversion \n
/// The first step of the reconstruction is to find and fit the peaks in the analog signal. If the difference between the maximum and the minimum of the signal is lower than 15 ADC counts the digi is skipped (the mean sigma of the pedestal is 2/3 counts -> 15 = 2 * 2.5 * sigma), otherwise the baseline valued is extimated. \n
/// The peak finding algorithm check for each ADC sample if it is at least 15 counts above the previous sample, if not the aligorithm pass to the next point. A second check is if the signal is 10 counts above the extimated baseline or if the difference respect to the previous sample is bigger than 20, if one of this two conditions is satisfied the algorithm search for the maximum, fit it with a gaussian and generate the TRecoHit.
///\n
///\n
/// 1D Clusters Construction \n
/// For each side of the detector the neighboring channels are clusterized in MUV1HitsCluster if the time distance is below the ChannelClusterTimeCut value defined in the MUV1.conf file.\n
/// The MUV1HitsCuster evaluate the basic characteristics of the cluster: Time, Position, Total collected charge ...
/// \n
/// \n
/// Candidates Construction \n
/// The 1D clusters coming from the different readout side are combined into TRecoMUV1Candidate (2D Cluster), respecting the quadrant structure of the detector, if the time difference is below the SideClusterTimeCut value defined in the MUV1.conf file.\n
/// The corrections for the cluster position along the scintillators are then determined for time and charge.
/// \EndDetailed
/// \author Created by Riccardo Aliberti (riccardo.aliberti@cern.ch)

#include "FADCEvent.hh"
#include "MUV1HitsCluster.hh"
#include "MUV1Reconstruction.hh"
#include "MUV1Digitizer.hh"
#include "TMUV1Digi.hh"
#include "TRecoMUV1Event.hh"
#include "TSlimRecoMUV1Event.hh"
#include "TRecoMUV1Hit.hh"
#include "TSpecialTriggerEvent.hh"
#include "NA62VRawDecoder.hh"
#include "NA62RecoManager.hh"
#include "NA62ConditionsService.hh"

#include "TMath.h"
#include "TH1.h"
#include "TRegexp.h"
#include <vector>

//Variables for Minuit Minimizer
Double_t   *fMUV1FitDataXptr=0, *fMUV1FitDataYptr=0;
Double_t   *fMUV1FitDataErrorXptr=0, *fMUV1FitDataErrorYptr=0;
Int_t  fNMUV1FitPoints=0;

MUV1Reconstruction::MUV1Reconstruction(TFile* HistoFile, TString ConfigFileName) :
    NA62VReconstruction(HistoFile, "MUV1", ConfigFileName),
    fCandidate(nullptr), fFitter(nullptr), fGeomInstance(nullptr),
    fTPosDependency(0), fQPosDependency(0) {
    fRecoEvent = new TRecoMUV1Event();
    fSlimRecoEvent = new TSlimRecoMUV1Event();

    //Parsing config file
    fBirksEnable = 0;
    fSaveRefused = 0;
    fSaveSaturated = 0;
    fSaveUnderflow = 0;
    fLastNsamp = 128;
    fFirstSlot = 31;
    fLastSlot = 0;
    fQPosNParameters = 0;

    for (Int_t i=0; i<4; i++){
      fSideCluster[i].clear();
      for (Int_t j=0; j<44; j++){
        fMUV1T0[i][j]=0.;
        fMUV1QEq_pars[i][j]=0;
      }
    }

    ParseConfFile(ConfigFileName);

    //Initializing the fitter
    fFitter = new TMinuit(7);
    Double_t arg = 1.;
    Int_t ierflg = 0;
    fFitter->SetPrintLevel(-1);
    fFitter->mnexcm("SET ERR", &arg ,1, ierflg);

    //Flags for clusters in central region
    fCentralClusterFlags[0][0] = true;
    fCentralClusterFlags[0][1] = false;
    fCentralClusterFlags[1][0] = true;
    fCentralClusterFlags[1][1] = true;

    //Flags for clusters in corner region
    fCornerClusterFlags[0][0] = true;
    fCornerClusterFlags[0][1] = true;
    fCornerClusterFlags[0][2] = true;
    fCornerClusterFlags[1][0] = false;
    fCornerClusterFlags[1][1] = true;
    fCornerClusterFlags[1][2] = true;
    fCornerClusterFlags[2][0] = false;
    fCornerClusterFlags[2][1] = false;
    fCornerClusterFlags[2][2] = true;

    ResetHistograms();
  }

MUV1Reconstruction::~MUV1Reconstruction(){

  if (fFitter) {
    delete fFitter;
    fFitter=nullptr;
  }
  if (fQPosDependency) {
    delete fQPosDependency;
    fQPosDependency=0;
  }
  if (fTPosDependency) {
    delete fTPosDependency;
    fTPosDependency=0;
  }

  for (UInt_t i=0; i<4; i++){
    for (UInt_t j=0; j<fSideCluster[i].size(); j++){
      delete fSideCluster[i][j];
    }
    fSideCluster[i].clear();
    if (fQPosNParameters>0){
      for (Int_t j=0; j<44; j++){
        delete [] fMUV1QEq_pars[i][j];
        fMUV1QEq_pars[i][j] = 0;
      }
    }
  }

  ResetHistograms();
}

//Reading parameters from the config files
void MUV1Reconstruction::ParseConfFile(TString ConfFileName) {

  //Default file for time alignment, detector calibration and baseline values
  TString T0FileName="MUV1-T0.dat";
  TString QEqFileName="MUV1-QEqualization.dat";
  TString TimeDepFileName="MUV1-TimeDep.dat";

  fCalibrationParameter = 0.440;

  std::ifstream confFile(ConfFileName.Data());
  if (!confFile.is_open()) {
    perror(ConfFileName);
    exit(kWrongConfiguration);
  }

  //Data from the config file
  TString Line;
  while (Line.ReadLine(confFile)) {
    if (Line.BeginsWith("#")) continue;

    if (Line.Contains("File")){

      if (Line.BeginsWith("T0FileInput")) {
        TObjArray * line = Line.Tokenize(" ");
        T0FileName = static_cast<TObjString*>(line->At(1))->GetString();
        delete line;
        continue;
      }
      else if (Line.BeginsWith("QeqFileInput")){
        TObjArray * line = Line.Tokenize(" ");
        QEqFileName = static_cast<TObjString*>(line->At(1))->GetString();
        delete line;
        continue;
      }
      else if (Line.BeginsWith("TimeDepFileInput")){
        TObjArray * line = Line.Tokenize(" ");
        TimeDepFileName = static_cast<TObjString*>(line->At(1))->GetString();
        delete line;
        continue;
      }
    }
    else{
      if (Line.BeginsWith("FirstSlot")){
        fFirstSlot = TString(Line(TRegexp("[0-9]+"))).Atoi();
        continue;
      }
      else if (Line.BeginsWith("StopAfterEmptyChannels")) {
        fStopAfterEmptyChannels = TString(Line(TRegexp("[0-9]+"))).Atoi();
        continue;
      }
      else if (Line.BeginsWith("ChannelClusterTimeCut")) {
        fChannelClusterTimeCut = TString(Line(TRegexp("[0-9]+"))).Atoi();
        continue;
      }
      else if (Line.BeginsWith("SideClusterTimeCut")) {
        fSideClusterTimeCut = TString(Line(TRegexp("[0-9]+"))).Atoi();
        continue;
      }
      else if (Line.BeginsWith("EnableBirksCorrection")) {
        fBirksEnable = TString(Line(TRegexp("[0-9]+"))).Atoi();
        continue;
      }
      else if (Line.BeginsWith("Calibration")){
        TObjArray * line = Line.Tokenize(" ");
        fCalibrationParameter = (static_cast<TObjString*>(line->At(1))->GetString()).Atof();
        fCalibrationParameter /= 1000.;
        delete line;
        continue;
      }
      else if (Line.BeginsWith("SaveRefused")){
        fSaveRefused = TString(Line(TRegexp("[0-9]+"))).Atoi();
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
  }
  confFile.close();

  TObjArray * line;

  //Getting Time Aligment Parameters
  if(NA62ConditionsService::GetInstance()->Open(T0FileName)==kSuccess){
    while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(T0FileName))) {
      if (Line.BeginsWith("#")) continue;
      line = Line.Tokenize(" ");
      Int_t Channel = static_cast<TObjString*>(line->At(1))->GetString().Atof();
      Double_t time = static_cast<TObjString*>(line->At(2))->GetString().Atof();
      if (TMath::Abs(time) < 500.)fMUV1T0[(Channel-100)/50][Channel%50-1] = time;
      delete line;
    }
    NA62ConditionsService::GetInstance()->Close(T0FileName);
  }

  fMeanT0 = 0;

  for (Int_t i=0; i<4; i++) {

    fMeanT0 += TMath::Mean(44,fMUV1T0[i]);
  }
  fMeanT0 /= 4.;

  //Getting Calibration Parameters: QEqualization.
  NA62ConditionsService::GetInstance()->Open(QEqFileName);
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(QEqFileName))) {
	if (Line.BeginsWith("#")) continue;
    else if (Line.BeginsWith("QPositionFunction")){
    		line = Line.Tokenize(" ");
    		fQPosDependency = new TF1("MUV1QPosDependecy",static_cast<TObjString*>(line->At(1))->GetString().Data(),-1500,1500);
    		fQPosNParameters = fQPosDependency->GetNpar();
    		for (Int_t i=0; i<4; i++){
    		  for (Int_t j=0; j<44; j++) fMUV1QEq_pars[i][j] = new Double_t [fQPosNParameters];
    		}
    		delete line;
    		continue;
    }
    line = Line.Tokenize(" ");
    Int_t Channel = static_cast<TObjString*>(line->At(0))->GetString().Atof();
    Double_t QEq = static_cast<TObjString*>(line->At(1))->GetString().Atof();
    fMUV1QEq[(Channel-100)/50][Channel%50-1] = 1./QEq;
    Int_t par_n = 0;

    while (par_n<fQPosNParameters && par_n+2<line->GetEntries()){
      fMUV1QEq_pars[(Channel-100)/50][Channel%50-1][par_n] = static_cast<TObjString*>(line->At(2+par_n))->GetString().Atof();
      par_n++;
    }

    delete line;
  }
  NA62ConditionsService::GetInstance()->Close(QEqFileName);

  //Getting Calibration Parameters: Time-Position dependency.
  NA62ConditionsService::GetInstance()->Open(TimeDepFileName);
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(TimeDepFileName))) {
    if (Line.BeginsWith("#")) continue;
    else if (Line.BeginsWith("TimePositionFunction")){
    		line = Line.Tokenize(" ");
    		fTPosDependency = new TF1 ("MUV1TPosDependency",static_cast<TObjString*>(line->At(1))->GetString().Data(),-1500,1500);
    		delete line;
    		continue;
    }

    line = Line.Tokenize(" ");
    Int_t Channel = static_cast<TObjString*>(line->At(0))->GetString().Atof();
    Double_t par0 = static_cast<TObjString*>(line->At(1))->GetString().Atof();
    Double_t par1 = static_cast<TObjString*>(line->At(2))->GetString().Atof();
    fMUV1TimeDep_par0[(Channel-100)/50][Channel%50-1] = par0;
    fMUV1TimeDep_par1[(Channel-100)/50][Channel%50-1] = par1;
    delete line;
  }
  NA62ConditionsService::GetInstance()->Close(TimeDepFileName);
}

void MUV1Reconstruction::Init(NA62VReconstruction* MainReco){

  //common part for all the subdetectors
  NA62VReconstruction::Init(MainReco);

  fPiSquared = TMath::Sqrt(TMath::Pi());

  // Initialize variables and histos
  fGeomInstance = MUV1Geometry::GetInstance();

  //Initializing Histograms
  InitHistograms();
}

TDetectorVEvent * MUV1Reconstruction::Trigger(TDetectorVEvent * tEvent, Event* /*tGenEvent*/){
  return tEvent;
}

void MUV1Reconstruction::StartOfBurst(){
  NA62VReconstruction::StartOfBurst(); // common part for all the subdetectors
}

void MUV1Reconstruction::EndOfBurst(){
  NA62VReconstruction::EndOfBurst(); // common part for all the subdetectors
  for (Int_t i=0; i<4; i++){
    for (Int_t j=0; j<44; j++){
      Int_t  ChannelMax = fHBaselines[i][j]->GetBinCenter(fHBaselines[i][j]->GetMaximumBin());
      fHBaselines[i][j]->GetXaxis()->SetRangeUser(ChannelMax-50, ChannelMax+50);
    }
  }
  fHHitTime->GetXaxis()->SetRangeUser(0,fLastNsamp);
  fHHitTime->GetYaxis()->SetRangeUser(fFirstSlot,fLastSlot+1);
}

TRecoVEvent * MUV1Reconstruction::ProcessEvent(TDetectorVEvent* tEvent, Event* tGenEvent){

  //Special Triggers
  if(tEvent->IsA() == TSpecialTriggerEvent::Class()){
    //read it
    return 0;
  }

  //common part for all the subdetectors
  NA62VReconstruction::ProcessEvent(tEvent, tGenEvent);

  UInt_t EventFlag = static_cast<FADCEvent*>(tEvent)->GetEventFlag();
  if (EventFlag&(0x1 << kCREAML1ErrorBit)){ //CREAM L1 Error
    for (int i=0; i<200; i++)
      fHCREAMFlags->Fill(100+i,3);
  }

  if (EventFlag != 0) fRecoEvent->SetErrorMaskBit(0,1);

  Int_t  NDigis = tEvent->GetNHits();
  if (NDigis < 1) return fRecoEvent; //No digi found

  TClonesArray &Digis = *static_cast<FADCEvent*>(tEvent)->GetHits();

  //Preparing to store hits
  std::vector <Int_t > PeakPosition;
  TRecoMUV1Hit* ThisHit;
  TString RefusedPeakPosition;
  for (Int_t iside=0; iside<4; iside++){
    fSideCluster[iside].clear();
    for (Int_t ich=0; ich<44; ich++) fHitOnChannel[iside][ich].clear();
  }

  fLastSlot = fFirstSlot + fRawDecoder->GetDecoder()->GetNROBoards();
  Double_t TotalEnergy = 0;
  for (Int_t  iHit=0; iHit<NDigis; iHit++) {
    TMUV1Digi* Digi = static_cast<TMUV1Digi*>( Digis[iHit]);
    if (Digi->GetChannelID()<100 || Digi->GetChannelID()>300) continue;
    Double_t   *Amplitude = Digi -> GetAllSamples();
    Int_t  Nsamples = Digi -> GetNSamples();
    Int_t  ChannelID = Digi->GetChannelID();
    Int_t  Side = Digi->GetSide();
    Int_t  Channel = Digi->GetScintillatorNumber() - 1;
    Int_t  Slot = fFirstSlot + fRawDecoder->GetDecoder()->GetChannelRO(ChannelID)/32;

    fLastNsamp = Nsamples;

    //Hit Position
    TVector3 HitPos (0,0,0);
    HitPos[Digi->GetScintillatorOrientation()] = Digi->GetScintillatorPosition();

    UInt_t flag = Digi->GetFlags();
    Bool_t Saturation = ((flag&(1<<kCREAMSaturationBit))!=0); //event with points saturating the ADC scale
    Bool_t Underflow  = ((flag&(1<<kCREAMUnderflowBit))!=0); //event with points under the ADC scale

    if (Saturation) fHCREAMFlags->Fill(ChannelID,1);
    if (Underflow) fHCREAMFlags->Fill(ChannelID,2);
    if (!Saturation && !Underflow) fHCREAMFlags->Fill(ChannelID,0);

    Saturation = Saturation && fSaveSaturated;
    Underflow  = Underflow  && fSaveUnderflow;

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
      fMUV1FitDataX[i] = i;
      fMUV1FitDataErrorX[i] = 1./ClockPeriod;
      fMUV1FitDataY[i] = Amplitude[i];
      fMUV1FitDataErrorY[i] = 3.;
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

      fHBaselines[Side][Channel] -> Fill(Amplitude[i]);
      fHBaselinesvsChannel->Fill(ChannelID,Amplitude[i]);
      fHDistDerBaseline -> Fill(der2);

      Npoint++;
    }

    //Separation signal-baseline
    fHISigBase -> Fill(Digi->GetChannelID(), int_base);
    fHISigBase -> Fill(Digi->GetChannelID(), int_sig);

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
    Bool_t Refused = false;
    RefusedPeakPosition="_Ref_";
    for (UInt_t iPeak=0; iPeak<PeakPosition.size(); iPeak++){

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
        fMUV1FitDataXptr = &fMUV1FitDataX[(PeakPosition[iPeak]-7 >= 0 ? PeakPosition[iPeak]-7 : 0)];
        fMUV1FitDataYptr = &fMUV1FitDataY[(PeakPosition[iPeak]-7 >= 0 ? PeakPosition[iPeak]-7 : 0)];
        fMUV1FitDataErrorXptr = &fMUV1FitDataErrorX[(PeakPosition[iPeak]-7 >= 0 ? PeakPosition[iPeak]-7 : 0)];
        fMUV1FitDataErrorYptr = &fMUV1FitDataErrorY[(PeakPosition[iPeak]-7 >= 0 ? PeakPosition[iPeak]-7 : 0)];
        fNMUV1FitPoints = 1 + ((PeakPosition[iPeak+1]+3 < Nsamples ? PeakPosition[iPeak+1]+3 : Nsamples-1) - (PeakPosition[iPeak]-7 >= 0 ? PeakPosition[iPeak]-7 : 0));


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
        Double_t Limits[3];
        Limits[0] = 1;
	Limits[1] = mean-30;
	Limits[2] = mean+30;
        fFitter->mnexcm("SET LIM",Limits,3,ierflg);

        if (PeakPosition[iPeak]>0){
          Limits[0] = 2;
	  Limits[1] = (Amplitude[PeakPosition[iPeak]] - base - 60 > 5 ? Amplitude[PeakPosition[iPeak]] -base - 60 : 5);
          Limits[2] = (Amplitude[PeakPosition[iPeak]] - base + 60 > 60 ? Amplitude[PeakPosition[iPeak]] -base + 60 : 60);
          fFitter->mnexcm("SET LIM",Limits,3,ierflg);
          Limits[0] = 3;
	  Limits[1] = PeakPosition[iPeak] - peak_sigma_1/TMath::Sqrt(Nsigma_1);
	  Limits[2] = PeakPosition[iPeak] + peak_sigma_1/TMath::Sqrt(Nsigma_1);
          fFitter->mnexcm("SET LIM",Limits,3,ierflg);
        }

        Limits[0] = 4;
	Limits[1] = 1.;
	Limits[2] = 3.;
        fFitter->mnexcm("SET LIM",Limits,3,ierflg);
        Limits[0] = 5;
	Limits[1] = 5.;
        Limits[2] = (Amplitude[PeakPosition[iPeak+1]] - base + 60 > 60 ? Amplitude[PeakPosition[iPeak+1]] -base + 60 : 60);
        fFitter->mnexcm("SET LIM",Limits,3,ierflg);

        Limits[0] = 6;
	Limits[1] = PeakPosition[iPeak+1] - peak_sigma_2/TMath::Sqrt(Nsigma_2);
	Limits[2] = PeakPosition[iPeak+1] + peak_sigma_2/TMath::Sqrt(Nsigma_2);
        fFitter->mnexcm("SET LIM",Limits,3,ierflg);

        Limits[0] = 7;
	Limits[1] = 1.;
	Limits[2] = 3.;
        fFitter->mnexcm("SET LIM",Limits,3,ierflg);

        //Fitting
        Double_t Args[2]={1000.,1.};
        fFitter->mnexcm("MIGRAD", Args ,2,ierflg);

        //Exporting the fitting results
        for (Int_t ipar=0; ipar<7; ipar++) fFitter->GetParameter(ipar,fPars[ipar],fParErrors[ipar]);

        //Resetting the fitter
        fFitter->mnexcm("CLE",Args,0,ierflg);

        //Determining peak 1 position and amplitude
        Double_t   time1 = (fPars[2]) * ClockPeriod;
        Double_t   terr1 = fParErrors[2] * ClockPeriod;
        Double_t   sigma1 = fPars[3] * ClockPeriod;
        Double_t   Serr1 = fParErrors[3] * ClockPeriod;
        Double_t   peak1 = fPars[1] * 2. /16.384;
        Double_t   perr1 = fParErrors[1] * 2. / 16.384;
        Double_t   charge1 =  peak1 * sigma1 * fPiSquared;
        Double_t   Qerr1 = TMath::Sqrt((perr1/peak1)*(perr1/peak1) + (Serr1/sigma1)*(Serr1/sigma1)) * charge1;

        if (fRecoEvent->GetIsMC()) time1 -= ClockPeriod * Double_t(Nsamples)/2.;

        if (peak1>0. && charge1 < 1.e+7) {
          ThisHit = static_cast<TRecoMUV1Hit*>( fRecoEvent -> AddHit(Digi));
          ThisHit -> DecodeChannelID(Digi->GetChannelID());
          time1 -= GetT0Correction(Digi);

          if (Digi->GetQuadrant()>0) {

            time1 -= fMUV1T0[Side][Channel];
            peak1 *= fMUV1QEq[Side][Channel];
            perr1 *= fMUV1QEq[Side][Channel];
            charge1 *= fMUV1QEq[Side][Channel];
            Qerr1 *= fMUV1QEq[Side][Channel];

            fHChannelsOccupancy -> Fill(Digi->GetChannelID());
            fHAmplitudeVSChannel -> Fill(Digi->GetChannelID(),peak1);
            fHSigmaVSChannel -> Fill(Digi->GetChannelID(),sigma1);
            fHQVSChannel -> Fill(Digi->GetChannelID(),charge1/GetQPosAttenuation(Digi->GetChannelID(),(Side > 1 ? 60 : -60)));
            if (TMath::Abs(time1 -NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256.) < 20.) fHQVSChannel_L0intime -> Fill(Digi->GetChannelID(),charge1/GetQPosAttenuation(Digi->GetChannelID(),(Side > 1 ? 60 : -60)));
            fHHitTime_L0time -> Fill(time1 - NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256.);
            fHHitTimeVSBurst -> Fill(fRecoEvent->GetBurstID(),time1);
            fHHitTimeVSChannel -> Fill(Digi->GetChannelID(),time1);
            fHitOnChannel[Side][Channel].push_back(ThisHit);
          }

          ThisHit -> SetTime(time1); //[ ns ]
          ThisHit -> SetTimeError(terr1);
          ThisHit -> SetSigma(sigma1);
          ThisHit -> SetSigmaError(Serr1);
          ThisHit -> SetPeakAmplitude(peak1); // [ mV ]
          ThisHit -> SetAmplitudeError(perr1);// [ mV ]
          ThisHit -> SetCharge( charge1); //[ fC ]
          ThisHit -> SetChargeError(Qerr1); //[ fC ]
          ThisHit -> SetEnergy(charge1/(2.*fCalibrationParameter));
          ThisHit -> SetPosition(HitPos);
          fHHitTime -> Fill(fPars[2],Slot);
          if (ThisHit->IsLongScintillator())
            TotalEnergy += 0.5*charge1/(2.*fCalibrationParameter);
          else
            TotalEnergy += charge1/(2.*fCalibrationParameter);
        }
        else if (fSaveRefused) {
          RefusedPeakPosition+=Form("_%d_",PeakPosition[iPeak]);
          if (peak1<AmpLim){
            RefusedPeakPosition+=Form("_Amp_%f",peak1);
          }
          else{
            RefusedPeakPosition+=Form("_Q_%f",charge1);
          }
          Refused = true;
        }

        //Determining peak 2 position and amplitude
        Double_t   time2 = (fPars[5]) * ClockPeriod;
        Double_t   terr2 = fParErrors[5] * ClockPeriod;
        Double_t   sigma2 = fPars[6] * ClockPeriod;
        Double_t   Serr2 = fParErrors[6] * ClockPeriod;
        Double_t   peak2 = fPars[4] * 2. / 16.384;
        Double_t   perr2 = fParErrors[4] * 2. / 16.384 ;
        Double_t   charge2 =  peak2 * sigma2 * fPiSquared;
        Double_t   Qerr2 = TMath::Sqrt((perr2/peak2)*(perr2/peak2) + (Serr2/sigma2)*(Serr2/sigma2)) * charge2;

        if (fRecoEvent->GetIsMC()) time2 -= ClockPeriod * Double_t(Nsamples)/2.;

        if (peak2>0. && charge2 < 1.e+7) {

          ThisHit = static_cast<TRecoMUV1Hit*>( fRecoEvent -> AddHit(Digi));
          ThisHit -> DecodeChannelID(Digi->GetChannelID());
          time2 -= GetT0Correction(Digi);

          if (Digi->GetQuadrant()>0) {
            time2 -= fMUV1T0[Side][Channel];
            peak2 *= fMUV1QEq[Side][Channel];
            perr2 *= fMUV1QEq[Side][Channel];
            charge2 *= fMUV1QEq[Side][Channel];
            Qerr2 *= fMUV1QEq[Side][Channel];

            fHChannelsOccupancy -> Fill(Digi->GetChannelID());
            fHAmplitudeVSChannel -> Fill(Digi->GetChannelID(),peak2);
            fHSigmaVSChannel -> Fill(Digi->GetChannelID(),sigma2);
            fHQVSChannel -> Fill(Digi->GetChannelID(),charge2/GetQPosAttenuation(Digi->GetChannelID(),(Side > 1 ? 60 : -60)));
            if (TMath::Abs(time2 -NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256.) < 20.) fHQVSChannel_L0intime -> Fill(Digi->GetChannelID(),charge2/GetQPosAttenuation(Digi->GetChannelID(),(Side > 1 ? 60 : -60)));
            fHHitTime_L0time -> Fill(time2 - NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256.);
            fHHitTimeVSBurst -> Fill(fRecoEvent->GetBurstID(),time2);
            fHHitTimeVSChannel -> Fill(Digi->GetChannelID(),time2);
            fHitOnChannel[Side][Channel].push_back(ThisHit);
          }

          ThisHit -> SetTime(time2); //[ ns ]
          ThisHit -> SetTimeError(terr2);
          ThisHit -> SetSigma(sigma2);
          ThisHit -> SetSigmaError(Serr2);
          ThisHit -> SetPeakAmplitude(peak2); // [ mV ]
          ThisHit -> SetAmplitudeError(perr2);// [ mV ]
          ThisHit -> SetCharge( charge2); //[ fC ]
          ThisHit -> SetChargeError(Qerr2); //[ fC ]
          ThisHit -> SetEnergy(charge2/(2.*fCalibrationParameter));
          ThisHit -> SetPosition(HitPos);
          fHHitTime -> Fill(fPars[5],Slot);

          if (ThisHit->IsLongScintillator())
            TotalEnergy += 0.5*charge2/(2.*fCalibrationParameter);
          else
            TotalEnergy += charge2/(2.*fCalibrationParameter);
        }
        else if (fSaveRefused) {
          RefusedPeakPosition+=Form("_%d_",PeakPosition[iPeak+1]);
          if (peak2<AmpLim){
            RefusedPeakPosition+=Form("_Amp_%f",peak2);
          }
          else{
            RefusedPeakPosition+=Form("_Q_%f",charge2);
          }
          Refused = true;
        }

        iPeak++;
        continue;
      }

      //---------------------------------
      //Isolated one peak
      //---------------------------------
      else{

        //Setting Fit data addresses for minuit
        fMUV1FitDataXptr = &fMUV1FitDataX[(PeakPosition[iPeak]-5 >= 0 ? PeakPosition[iPeak]-5 : 0)];
        fMUV1FitDataYptr = &fMUV1FitDataY[(PeakPosition[iPeak]-5 >= 0 ? PeakPosition[iPeak]-5 : 0)];
        fMUV1FitDataErrorXptr = &fMUV1FitDataErrorX[(PeakPosition[iPeak]-5 >= 0 ? PeakPosition[iPeak]-5 : 0)];
        fMUV1FitDataErrorYptr = &fMUV1FitDataErrorY[(PeakPosition[iPeak]-5 >= 0 ? PeakPosition[iPeak]-5 : 0)];
        fNMUV1FitPoints = 1 + ((PeakPosition[iPeak]+3 < Nsamples ? PeakPosition[iPeak]+3 : Nsamples-1) - (PeakPosition[iPeak]-5 >= 0 ? PeakPosition[iPeak]-5 : 0));

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
        Double_t Limits[3];
        Limits[0] = 1;
	Limits[1] = mean-30;
	Limits[2] = mean+30;
        fFitter->mnexcm("SET LIM",Limits,3,ierflg);

        if (PeakPosition[iPeak]>0) {
          Limits[0] = 2;
          Limits[1] = ((Amplitude[PeakPosition[iPeak]] - base - 60) > 10 ? (Amplitude[PeakPosition[iPeak]] - base - 60) : 10);
          Limits[2] = ((Amplitude[PeakPosition[iPeak]] - base + 60) > 10 ? (Amplitude[PeakPosition[iPeak]] - base + 60) : 100);
          fFitter->mnexcm("SET LIM",Limits,3,ierflg);

	  Limits[0] = 3;
          Limits[1] = PeakPosition[iPeak] - peak_sigma/TMath::Sqrt(Nsigma);
          Limits[2] = PeakPosition[iPeak] + peak_sigma/TMath::Sqrt(Nsigma);
          fFitter->mnexcm("SET LIM",Limits,3,ierflg);
        }

        Limits[0] = 4;
	Limits[1] = 1.;
	Limits[2] = 5.;
        fFitter->mnexcm("SET LIM",Limits,3,ierflg);

        //Fitting
        Double_t Args[2]={500.,1.};
        fFitter->mnexcm("MIGRAD", Args ,2,ierflg);
        for (Int_t ipar=0; ipar<4; ipar++) fFitter->GetParameter(ipar,fPars[ipar],fParErrors[ipar]);
        fFitter->mnexcm("CLE",Args,0,ierflg);

        //Determining peak position and amplitude
        Double_t   time = (fPars[2]) * ClockPeriod;
        Double_t   terr = fParErrors[2] * ClockPeriod;
        Double_t   sigma = fPars[3] * ClockPeriod;
        Double_t   Serr = fParErrors[3] * ClockPeriod;
        Double_t   peak = fPars[1] * 2. /16.384;
        Double_t   perr = fParErrors[1] * 2. / 16.384;
        Double_t   charge =  peak * sigma * fPiSquared;
        Double_t   Qerr = TMath::Sqrt((perr/peak)*(perr/peak) + (Serr/sigma)*(Serr/sigma)) * charge;

        if (fRecoEvent->GetIsMC()) time -= ClockPeriod * Double_t(Nsamples)/2.;

        //Storing the info
        if (peak>0. && charge < 1.e+7) {
          ThisHit = static_cast<TRecoMUV1Hit*>( fRecoEvent -> AddHit(Digi));
          ThisHit -> DecodeChannelID(Digi->GetChannelID());
          time -= GetT0Correction(Digi);

          if (Digi->GetQuadrant()>0){

            time -= fMUV1T0[Side][Channel];
            peak *= fMUV1QEq[Side][Channel];
            perr *= fMUV1QEq[Side][Channel];
            charge *= fMUV1QEq[Side][Channel];
            Qerr *= fMUV1QEq[Side][Channel];

            fHChannelsOccupancy -> Fill(Digi->GetChannelID());
            fHAmplitudeVSChannel -> Fill(Digi->GetChannelID(),peak);
            fHSigmaVSChannel -> Fill(Digi->GetChannelID(),sigma);
            fHQVSChannel -> Fill(Digi->GetChannelID(),charge/GetQPosAttenuation(Digi->GetChannelID(),(Side > 1 ? 60 : -60)));
            if (TMath::Abs(time -NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256.) < 20.) fHQVSChannel_L0intime -> Fill(Digi->GetChannelID(),charge/GetQPosAttenuation(Digi->GetChannelID(),(Side > 1 ? 60 : -60)));
            fHHitTime -> Fill(fPars[2],Slot);
            fHHitTime_L0time -> Fill(time - NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256.);
            fHHitTimeVSBurst -> Fill(fRecoEvent->GetBurstID(),time);
            fHHitTimeVSChannel -> Fill(Digi->GetChannelID(),time);
            fHitOnChannel[Side][Channel].push_back(ThisHit);
          }

          ThisHit -> SetTime(time); //[ ns ]
          ThisHit -> SetTimeError(terr);
          ThisHit -> SetSigma(sigma);
          ThisHit -> SetSigmaError(Serr);
          ThisHit -> SetPeakAmplitude(peak); // [ mV ]
          ThisHit -> SetAmplitudeError(perr);// [ mV ]
          ThisHit -> SetCharge( charge); //[ fC ]
          ThisHit -> SetChargeError(Qerr); //[ fC ]
          ThisHit -> SetEnergy(charge/(2.*fCalibrationParameter));
          ThisHit -> SetPosition(HitPos);

          if (ThisHit->IsLongScintillator())
            TotalEnergy += 0.5*charge/(2.*fCalibrationParameter);
          else
            TotalEnergy += charge/(2.*fCalibrationParameter);

        }
        else if (fSaveRefused) {
          RefusedPeakPosition+=Form("_%d_",PeakPosition[iPeak]);
          if (peak<AmpLim){
            RefusedPeakPosition+=Form("_Amp_%f",peak);
          }
          else{
            RefusedPeakPosition+=Form("_Q_%f",charge);
          }
          Refused = true;
        }
      }
    }

    //Save Events with Refused Peaks or Saturating the ADC (if asked)
    if (Refused || Saturation || Underflow) {
      TGraph histo_temp (Nsamples, fMUV1FitDataX, fMUV1FitDataY);
      histo_temp.SetTitle(Form("Event_%d_Burst_%d_Channel_%d",fRecoEvent->GetID(), fRecoEvent->GetBurstID(),Digi->GetChannelID()));

      if (!(fHistoFile->GetDirectory(Form("MUV1Monitor/Histos/Burst_%d",fRecoEvent->GetBurstID()))))
        fHistoFile->mkdir(Form("MUV1Monitor/Histos/Burst_%d",fRecoEvent->GetBurstID()));

      fHistoFile->cd(Form("MUV1Monitor/Histos/Burst_%d",fRecoEvent->GetBurstID()));
      TString histoname = histo_temp.GetTitle();
      if (Saturation) histoname += "_sat";
      if (Underflow) histoname += "_under";
      if (Refused) histoname += RefusedPeakPosition.Data();
      histo_temp.Write(histoname);
      fHistoFile->cd("/");
    }
  }

  fHTotalEnergy -> Fill(1.e-3*TotalEnergy);

  // ------------------------- //
  // Processing the hits ----- //

  TClonesArray *HitsArray = fRecoEvent->GetHits();
  Int_t Nhits = fRecoEvent->GetNHits();
  fHNHitsVSBurst -> Fill(fRecoEvent->GetBurstID(),Nhits);

  std::vector <TRecoMUV1Hit*> SideHits[4];
  std::vector <Bool_t> Used[4];
  Int_t NUsedHits[4] = {0,0,0,0};
  Int_t NHits[4] = {0,0,0,0};
  Int_t SeedIndex[4] = {-1,-1,-1,-1};
  Double_t SeedCharge[4] = {0,0,0,0};

  for (Int_t i=0; i < HitsArray->GetEntries(); i++) {

    TRecoMUV1Hit *hit = static_cast<TRecoMUV1Hit*>(HitsArray->At(i));
    Double_t channelID = hit->GetChannelID();
    Int_t side = (channelID-100)/50;

    if (side < 0 || side > 3) {
      std::cerr << " MUV1 hit with wrong channelID " << channelID << std::endl;
      continue;
    }

    SideHits[side].push_back(hit); // hit in the side.
    Used[side].push_back(false); // hit not yet used.
    NHits[side]++; // number of hit per side.

    Double_t Q = hit->GetCharge();
    if (SeedCharge[side] > Q) continue;

    SeedCharge[side] = Q;
    SeedIndex[side] = NHits[side]-1;
  }

  //-------------------------
  //Grouping Hits in one dimensional clusters.
  //-------------------------

  for (Int_t iSide=0; iSide < 4; iSide++) {
    if (NHits[iSide] < 1) continue;

    while ((NHits[iSide] - NUsedHits[iSide]) > 0) {
      if (SeedCharge[iSide] < 10) break;

      MUV1HitsCluster *cluster = new MUV1HitsCluster();
      cluster->AddHit(SideHits[iSide][SeedIndex[iSide]]); //start from seed.
      Used[iSide][SeedIndex[iSide]] = true;

      NUsedHits[iSide]++;

      Int_t SeedChannelID = SideHits[iSide][SeedIndex[iSide]]->GetChannelID();
      Double_t SeedTime = SideHits[iSide][SeedIndex[iSide]]->GetTime();
      //const Double_t TimeRange = SideHits[iSide][SeedIndex[iSide]]->GetTimeError();

      Int_t distance = 0;
      Bool_t found_plus = false, found_minus = false;
      Bool_t break_plus = false, break_minus = false;
      Int_t Nmiss_plus = 0, Nmiss_minus = 0;
      //Double_t lastampl_plus = 0, lastampl_minus = 0;

      while (!break_minus || !break_plus) {

        distance++;
        found_plus = false, found_minus = false;

        for (Int_t jHit=0; jHit < NHits[iSide]; jHit++) {

          if (Used[iSide][jHit]) continue;
          Int_t delta = SideHits[iSide][jHit]->GetChannelID() - SeedChannelID;
          if (TMath::Abs(delta) > distance) continue;
          if (delta > 0 && break_plus) continue;
          if (delta < 0 && break_minus) continue;

          Double_t Time = SideHits[iSide][jHit]->GetTime();
          //Double_t TimeCut = TimeRange + SideHits[iSide][jHit]->GetTimeError();
          //Double_t Charge = SideHits[iSide][jHit]->GetCharge();

          Double_t TimeCut = 25.;

          if (TMath::Abs(Time - SeedTime) > TimeCut) continue;
          cluster->AddHit(SideHits[iSide][jHit]);
          Used[iSide][jHit] = true;
          if (delta<0) {
            found_minus = true;
            //lastampl_minus = Charge;
          }
          else {
            found_plus = true;
            //lastampl_plus = Charge;
          }

          if (found_plus && found_minus) break;
          if (found_plus && break_minus) break;
          if (found_minus && break_plus) break;
        }

        if (!found_plus) Nmiss_plus++;
        else Nmiss_plus = 0;
        if (!found_minus) Nmiss_minus++;
        else Nmiss_minus = 0;

        break_plus = ((Nmiss_plus > 1) || break_plus);
        break_minus = ((Nmiss_minus > 1) || break_minus);

      }

      cluster->UpdateValue();
      SeedCharge[iSide] = 0;
      SeedIndex[iSide] = -1;

      for (Int_t j=0; j < NHits[iSide]; j++) {

        if (Used[iSide][j]) continue;
        if (SideHits[iSide][j]->GetCharge() < SeedCharge[iSide]) continue;

        SeedCharge[iSide] = SideHits[iSide][j]->GetCharge();
        SeedIndex[iSide] = j;

      }
      fSideCluster[iSide].push_back(cluster);
    }
  }

  //---------------------------
  //Combining the cluster from all the sides to get 2D clusters
  //---------------------------

  MUV1HitsCluster *HCluster, *VCluster;
  for (Int_t i=0; i<4; i+=2){

    for (UInt_t j=0; j<fSideCluster[i].size(); j++){
      HCluster = static_cast<MUV1HitsCluster*>( fSideCluster[i][j]);
      if (HCluster->GetCharge() <= 0.) continue;
      Int_t  SecondSide = (HCluster->GetX() <= 0 ? 1 : 3);
      Int_t  Quadrant = 0;

      if (i==0){
        if (SecondSide==1) Quadrant = 1;
        else Quadrant = 2;
      }
      else {
        if (SecondSide==1) Quadrant = 4;
        else Quadrant = 3;
      }

      TRecoMUV1Hit **HHits = HCluster->GetHits();
      Int_t  NHHits = HCluster->GetNHits();

      for (UInt_t k=0; k<fSideCluster[SecondSide].size(); k++){

        VCluster = static_cast<MUV1HitsCluster*>( fSideCluster[SecondSide][k]);
        if (VCluster->GetCharge() <= 0.) continue;

        if (i==0 && VCluster->GetY()>0.){ continue; }
        if (i==2 && VCluster->GetY()<0.){ continue; }

        TRecoMUV1Hit **VHits = VCluster->GetHits();
        Int_t NVHits = VCluster->GetNHits();

        // Correction on HHits.
        Double_t ChargeH = 0.;
        Double_t TimeH = 0.;
        Double_t HHits_charge[NHHits];
        Double_t HHits_time[NHHits];
        for (Int_t ihit=0; ihit < NHHits; ihit++) {
          TRecoMUV1Hit *Hhit = HHits[ihit];
          Int_t Channel = Hhit->GetChannelID();

          fQPosDependency->SetParameters(fMUV1QEq_pars[(Channel-100)/50][Channel%50-1]);

          HHits_charge[ihit] = Hhit->GetCharge() / fQPosDependency->Eval(VCluster->GetY());

          fTPosDependency->SetParameters(fMUV1TimeDep_par0[(Channel-100)/50][Channel%50-1],fMUV1TimeDep_par1[(Channel-100)/50][Channel%50-1]);

          HHits_time[ihit] = Hhit->GetTime() + fTPosDependency->Eval(VCluster->GetY());
          ChargeH += HHits_charge[ihit];
          TimeH += HHits_time[ihit]*HHits_charge[ihit];
        }
        TimeH /= ChargeH;

        // Correction on VHits.
        Double_t ChargeV = 0.;
        Double_t TimeV = 0.;
        Double_t VHits_charge[NVHits];
        Double_t VHits_time[NVHits];
        for (Int_t ihit=0; ihit < NVHits; ihit++) {

          TRecoMUV1Hit *Vhit = VHits[ihit];
          Int_t Channel = Vhit->GetChannelID();
          fQPosDependency->SetParameters(fMUV1QEq_pars[(Channel-100)/50][Channel%50-1]);
          VHits_charge[ihit] = Vhit->GetCharge() / fQPosDependency->Eval(HCluster->GetX());
          fTPosDependency->SetParameters(fMUV1TimeDep_par0[(Channel-100)/50][Channel%50-1],fMUV1TimeDep_par1[(Channel-100)/50][Channel%50-1]);

          VHits_time[ihit] = Vhit->GetTime() + fTPosDependency->Eval(HCluster->GetX());
          ChargeV += VHits_charge[ihit];
          TimeV += VHits_time[ihit]*VHits_charge[ihit];
        }
        TimeV /= ChargeV;

        Int_t  Hindex = MUV1Geometry::GetInstance()->GetScintillatorAt(HCluster->GetX());
        Int_t  Vindex = MUV1Geometry::GetInstance()->GetScintillatorAt(VCluster->GetY());

        if (TMath::Abs(TimeV - TimeH) > fSideClusterTimeCut){
          VCluster = nullptr;
          continue;
        }

        //exclude clusters in the central hole
        if (!IsClusterAllowedAtCenter(Vindex,Hindex,Quadrant)) continue;

        //exclude clusters in the corners
        if (!IsClusterAllowedAtCorner(Vindex,Hindex,Quadrant)) continue;

        //-------------------------
        //Found a candidate.
        //Setting the values
        //-------------------------

        TRecoMUV1Candidate *ClusterCandidate = static_cast<TRecoMUV1Candidate*>( fRecoEvent->AddCandidate());

        Int_t SeedPositionX = TMath::LocMax(NHHits,HHits_charge);
        Double_t SeedEnX = HHits_charge[SeedPositionX]*fCalibrationParameter;
        Int_t SeedIndexX = HitsArray->IndexOf(HHits[SeedPositionX]);

        Int_t SeedPositionY = TMath::LocMax(NVHits,VHits_charge);
        Double_t SeedEnY = VHits_charge[SeedPositionY]*fCalibrationParameter;
        Int_t SeedIndexY = HitsArray->IndexOf(VHits[SeedPositionY]);

        for (Int_t ihit=0; ihit<std::max(NHHits,NVHits); ihit++){

          if (ihit<NHHits){
            Int_t index = HitsArray->IndexOf(HHits[ihit]);
            ClusterCandidate->AddHit(index);
          }
          if (ihit<NVHits){
            Int_t index = HitsArray->IndexOf(VHits[ihit]);
            ClusterCandidate->AddHit(index);
          }
        }

        Double_t   InnerEnergyX = SeedEnX;
        Double_t   InnerEnergyY = SeedEnY;

        if (SeedPositionX-1>=0) InnerEnergyX+=HHits_charge[SeedPositionX-1]*fCalibrationParameter;
        if (SeedPositionX+1<NHHits) InnerEnergyX+=HHits_charge[SeedPositionX+1]*fCalibrationParameter;
        if (SeedPositionY-1>=0) InnerEnergyY+=VHits_charge[SeedPositionY-1]*fCalibrationParameter;
        if (SeedPositionY+1<NVHits) InnerEnergyY+=VHits_charge[SeedPositionY+1]*fCalibrationParameter;

        ClusterCandidate -> SetHorizontalIndex(i);
        ClusterCandidate -> SetHorizontalChannel(Hindex);
        ClusterCandidate -> SetVerticalIndex(SecondSide);
        ClusterCandidate -> SetVerticalChannel(Vindex);
        ClusterCandidate -> SetQuadrant(Quadrant);
        ClusterCandidate -> SetPosition(HCluster->GetX(), VCluster->GetY());
        ClusterCandidate -> SetShowerWidthHorizontal(HCluster->GetSigmaX());
        ClusterCandidate -> SetShowerWidthVertical(VCluster->GetSigmaY());
        ClusterCandidate -> SetShowerWidth((HCluster->GetSigmaX()+VCluster->GetSigmaY())/2.);
        ClusterCandidate -> SetTime((TimeH + TimeV) / 2.);
        ClusterCandidate -> SetTimeHorizontal(TimeH);
        ClusterCandidate -> SetTimeVertical(TimeV);
        ClusterCandidate -> SetTimeSigmaVertical(VCluster->GetSigmaT());
        ClusterCandidate -> SetTimeSigmaHorizontal(HCluster->GetSigmaT());
        ClusterCandidate -> SetSeedEnergyHorizontal(SeedEnX);
        ClusterCandidate -> SetSeedIndexHorizontal(SeedIndexX);
        ClusterCandidate -> SetSeedEnergyVertical(SeedEnY);
        ClusterCandidate -> SetSeedIndexVertical(SeedIndexY);
        ClusterCandidate -> SetInnerEnergyHorizontal(InnerEnergyX);
        ClusterCandidate -> SetInnerEnergyVertical(InnerEnergyY);
        ClusterCandidate -> SetCharge((ChargeH+ChargeV)/2.);
        ClusterCandidate -> SetChargeHorizontal(ChargeH);
        ClusterCandidate -> SetChargeVertical(ChargeV);
        ClusterCandidate -> SetEnergy((ChargeH + ChargeV)*fCalibrationParameter/2.); //MeV
        ClusterCandidate -> SetEnergyHorizontal(ChargeH*fCalibrationParameter);
        ClusterCandidate -> SetEnergyVertical(ChargeV*fCalibrationParameter);

        //-----------------------
        //Filling the Histograms
        //-----------------------

        fHClusterLayerTimeDiff -> Fill(TimeH - TimeV);
        fHHitMap -> Fill(ClusterCandidate->GetX()/10., ClusterCandidate->GetY()/10.);
        fHShowerHitMap -> Fill(ClusterCandidate->GetShowerWidthHorizontal(),ClusterCandidate->GetShowerWidthVertical());
        fHClusterSeedChargeVsChannel->Fill(Hindex+100+50*i, HHits_charge[SeedPositionX]);
        fHClusterSeedChargeVsChannel->Fill(Vindex+100+50*SecondSide, VHits_charge[SeedPositionY]);
        fHShowerWidth -> Fill(ClusterCandidate->GetShowerWidth());
        fHSideClusterTimeSpread -> Fill(VCluster->GetSigmaT());
        fHSideClusterTimeSpread -> Fill(HCluster->GetSigmaT());
        fHSideClusterTimeSpreadVSChannelHit -> Fill(VCluster->GetSigmaT(),VCluster->GetNHits());
        fHSideClusterTimeSpreadVSChannelHit -> Fill(HCluster->GetSigmaT(),HCluster->GetNHits());
        fHSeedEnergy->Fill(ClusterCandidate->GetSeedEnergyHorizontal(),ClusterCandidate->GetSeedEnergyVertical());
        fHSeedVSNHits->Fill(0.5 * ClusterCandidate->GetSeedEnergy()/ClusterCandidate->GetEnergy(),1000.*ClusterCandidate->GetNHits()/ClusterCandidate->GetEnergy());
      }
    }
  }

  //---------------------------
  // Building Channel Hit Map
  //---------------------------
  for (Int_t i=0; i<Nhits; i++) {
    TRecoMUV1Hit *Hit1 = static_cast<TRecoMUV1Hit*>( HitsArray->At(i));
    if (Hit1->GetQuadrant()<=0) continue;
    if (Hit1->GetChannelID()%100>50) continue;
    for (Int_t j=0; j<Nhits; j++) {
      TRecoMUV1Hit *Hit2 = static_cast<TRecoMUV1Hit*>( HitsArray->At(j));

      if (Hit1->GetQuadrant()!=Hit2->GetQuadrant()) continue;
      if (Hit2->GetChannelID()%100 < 50) continue;
      if (TMath::Abs(Hit1->GetTime() - Hit2->GetTime())>50.) continue;

      fHChannelsHitMap -> Fill(Hit1->GetChannelID()%50,Hit2->GetChannelID()%50);
    }
  }

  // deleting fSideCluster entries
  for (UInt_t i=0; i<4; i++) {
    for (UInt_t j=0; j<fSideCluster[i].size(); j++){
      delete fSideCluster[i][j];
    }
    fSideCluster[i].clear();
  }

  return fRecoEvent;
}


void MUV1Reconstruction::EndProcessing(){
  NA62VReconstruction::EndProcessing(); // call base class for raw hist output
  // Write histos
  SaveHistograms();
}

void MUV1Reconstruction::FillTimes(Double_t ReferenceTime) {
  //common part for all the subdetectors
  NA62VReconstruction::FillTimes(ReferenceTime);

  //-- add here the user code to fill histos for T0 evaluation --//
  TClonesArray *Hits = fRecoEvent->GetHits();
  for (Int_t i=0; i<fRecoEvent->GetNHits(); i++) {

    TRecoMUV1Hit * Hit = static_cast<TRecoMUV1Hit*>( Hits->At(i));
    Int_t Side = (Hit->GetChannelID() - 100)/50;
    Int_t Channel = Hit->GetChannelID()%50 -1;
    Int_t Quadrant = Hit->GetQuadrant();
    Int_t ROChannel = fRawDecoder->GetDecoder()->GetChannelRO(Hit->GetChannelID());

    Double_t position = 0;
    Double_t total_charge = 0;
    for (Int_t j=0; j<fRecoEvent->GetNHits(); j++){
      if (i==j) continue;
      TRecoMUV1Hit * Hit2 = static_cast<TRecoMUV1Hit*>( Hits->At(j));

      if (Hit2->GetQuadrant()!=Quadrant || Hit2->GetSide()==Side) continue;
      if (TMath::Abs(Hit2->GetTime() - Hit->GetTime()) > 50.) continue;

      position += Hit2->GetScintillatorPosition()*Hit->GetCharge()/GetQPosAttenuation(Hit2->GetChannelID(),(Side > 1 ? 60 : -60));
      total_charge += Hit->GetCharge()/GetQPosAttenuation(Hit2->GetChannelID(),(Side > 1 ? 60 : -60));

    }

    position /= total_charge;

    Double_t time_pos_corr = GetTPosDelay(Hit->GetChannelID(),position);
    if (fHRecoHitTimeWrtReferenceVsROChannel) GetHRecoHitTimeWrtReferenceVsROChannel()->Fill(ROChannel,Hit->GetTime()-ReferenceTime+time_pos_corr);
    if (fHRecoHitTimeWrtReferenceVsROChannelNoT0) GetHRecoHitTimeWrtReferenceVsROChannelNoT0()->Fill(ROChannel,Hit->GetTime()-ReferenceTime+fMUV1T0[Side][Channel]+time_pos_corr);
  }
}

void MUV1Reconstruction::InitHistograms() {

  GetOrMakeDir(fHistoFile,"MUV1Monitor");
  GetOrMakeDir(fHistoFile,"MUV1Monitor/Histos");
  GetOrMakeDir(fHistoFile,"MUV1Monitor/Baselines");


  //Baselines
  fHistoFile->cd("MUV1Monitor/Baselines");

  AddHisto(fHDistDerBaseline = new TH1D ("DistDerBase","der",1000,-50,50));
  for (Int_t i=0; i<4; i++){
    for (Int_t j=0; j<44; j++){
      AddHisto(fHBaselines[i][j] = new TH1D (Form("Baseline_Channel_%d",101 + 50*i + j),Form("Baseline_Channel_%d",101 + 50*i + j),2000,0,2000));
    }
  }

  fHistoFile->cd("MUV1Monitor");

  //Hits
  AddHisto(fHCREAMFlags = new TH2I ("CREAMFlags","Digi Flags",200,100,300,4,0,4));
  fHCREAMFlags -> GetYaxis() -> SetBinLabel(1,"No Error");
  fHCREAMFlags -> GetYaxis() -> SetBinLabel(2,"Overflow");
  fHCREAMFlags -> GetYaxis() -> SetBinLabel(3,"Underflow");
  fHCREAMFlags -> GetYaxis() -> SetBinLabel(4,"L1 Error");
  fHCREAMFlags -> GetXaxis() -> SetTitle("Channel ID");

  AddHisto(fHHitTime = new TH2F("HitTime","Hit Time",128,0.,128.,31,0,31));
  fHHitTime -> GetXaxis() -> SetTitle("Time Slice");
  fHHitTime -> GetYaxis() -> SetTitle("CREAM Slot");

  AddHisto(fHBaselinesvsChannel = new TH2F("BaselineVSChannel","Baseline vs ChannelID",200,100,300,200,0,2000));
  fHBaselinesvsChannel -> GetXaxis() -> SetTitle("ChannelID");
  fHBaselinesvsChannel -> GetYaxis() -> SetTitle("Baseline ( ADC )");

  AddHisto(fHChannelsHitMap = new TH2F("ChannelsHitMap", "Channels Hit Map",44,0,44,44,0,44));

  AddHisto(fHChannelsOccupancy = new TH1I("ChannelsOccupancy","Channels Occupancy",200,100,300));
  fHChannelsOccupancy->GetXaxis()->SetTitle("Channel ID");

  AddHisto(fHQVSChannel = new TH2F("QVSChannel","Charge vs Channel; Channel ID; Charge [fC]",200,100,300,100,0,10000));

  AddHisto(fHQVSChannel_L0intime = new TH2F("QVSChannel_L0intime","Charge vs Channel for hit in time wrt L0 FineTime; Channel ID; Charge [fC]",200,100,300,100,0,10000));

  AddHisto(fHHitTime_L0time = new TH1F("HitTime_L0time","Hit time - L0 FineTime; t [ns]",500,-250.,250.));

  AddHisto(fHAmplitudeVSChannel = new TH2F ("AmplitudeVSChannel","Amplitude vs Channel",200,100,300,250,0,500));
  fHAmplitudeVSChannel -> GetXaxis() -> SetTitle("Channel ID");
  fHAmplitudeVSChannel -> GetYaxis() -> SetTitle("Amplitude [mV]");

  AddHisto(fHSigmaVSChannel = new TH2F ("SigmaVSChannel","Sigma vs Channel",200,100,300,500,25,75));
  fHSigmaVSChannel -> GetXaxis() -> SetTitle("Channel ID");
  fHSigmaVSChannel -> GetYaxis() -> SetTitle("Sigma [ns]");

  AddHisto(fHISigBase = new TH2F ("ISigBase","Mean Amplitude in ADC Counts of Signal and Baseline", 200, 100, 300, 1590, 100, 16000));
  AddHisto(fHHitTimeVSBurst = new TH2F ("HitTimeVSBurst","Hit time vs BurstID; Burst ID; Hit time [ns]",3000,0,3000,200,-200.,200.));
  AddHisto(fHHitTimeVSChannel = new TH2F ("HitTimeVSChannel","Hit time vs Channel; Channel ID; Hit time [ns]",200,100,300,200,-200.,200.));
  AddHisto(fHNHitsVSBurst = new TH2I ("NHitsVSBurst","NHits vs BurstID; BurstID; NHits",3000,0,3000,100,0,100));
  AddHisto(fHTotalEnergy = new TH1D("TotalEnergy","Total Energy in MUV1; Energy ( GeV )",1000,0,100));

  //Candidates

  AddHisto(fHHitMap = new TH2F("HitMap", "Calorimeter Front Face; Cluster x [cm]; Cluster y [cm]", 436, -130.8, 130.8,436, -130.8, 130.8));

  AddHisto(fHClusterSeedChargeVsChannel = new TH2F("ClusterSeedChargeVsChannel", "Cluster Seed Charge vs Seed Channel; Channel ID; Charge [pC]", 200,100,300,100,0,10000));

  AddHisto(fHShowerWidth = new TH1F("ShowerWidth","Shower width; Shower width [cm]; Entries", 200, 0, 200));

  AddHisto(fHShowerHitMap = new TH2F("ShowerHitMap", "Shower width Hit Map; Shower width Horizontal [mm]; Shower width Vertical [mm]",200,0,200,200,0,200));

  AddHisto(fHSideClusterTimeSpread = new TH1D("SideClusterTimeSpread", "Time standard deviation (side cluster)", 80,0,16));

  AddHisto(fHSideClusterTimeSpreadVSChannelHit = new TH2F("SideClusterTimeSpreadVSChannelHit", "Time standard deviation (side cluster) vs amount of hit channels; #sigmaT [ns]; NHits", 80,0,16,44,0,44));

  AddHisto(fHClusterLayerTimeDiff = new TH1D("ClusterLayerTimeDiff", "Time difference between vertical and horizontal orientation; #DeltaT [ns]",600,-30,30));

  AddHisto(fHSeedEnergy = new TH2F("SeedEnergy","Seed Energy Horizontal vs Seed Energy Vertical; E_{seed} X [MeV]; E_{seed} Y [MeV]",100,0,10000,100,0,10000));

  AddHisto(fHSeedVSNHits = new TH2F("SeedVSNHits","SeedEnergy/ClusterEnergy vs NHits/ClusterEnergy; Eseed/Ecluster; NHits/Ecluster [GeV^{-1}]",120,0,1.2,120,0,10));

  fHistoFile->cd("/");
}


void MUV1Reconstruction::SaveHistograms() {
  for (Int_t i=0; i<fHistoArray->GetEntries(); i++){
    TH1* histo = static_cast<TH1*>( fHistoArray->At(i));
    if (TString(histo->GetName()).Contains("Baseline")){
      fHistoFile->cd("/MUV1Monitor/Baselines/");
      histo -> Write();
    }
    else{
      fHistoFile->cd("/MUV1Monitor/");
      histo->Write();
    }
  }
  fHistoFile->cd("/");
}

void MUV1Reconstruction::ResetHistograms() {

  fHDistDerBaseline = 0;
  for (Int_t i=0; i<4; i++){
    for (Int_t j=0; j<22; j++){
      fHBaselines[i][j] = 0;
    }
  }

  fHCREAMFlags = 0;
  fHHitTime = 0;
  fHBaselinesvsChannel = 0;
  fHChannelsHitMap = 0;
  fHChannelsOccupancy = 0;
  fHQVSChannel = 0;
  fHQVSChannel_L0intime = 0;
  fHHitTime_L0time = 0;
  fHAmplitudeVSChannel = 0;
  fHSigmaVSChannel = 0;
  fHISigBase = 0;
  fHHitTimeVSBurst = 0;
  fHHitTimeVSChannel = 0;
  fHNHitsVSBurst = 0;
  fHHitMap = 0;
  fHClusterSeedChargeVsChannel = 0;
  fHShowerWidth = 0;
  fHShowerHitMap = 0;
  fHSideClusterTimeSpread = 0;
  fHSideClusterTimeSpreadVSChannelHit = 0;
  fHClusterLayerTimeDiff = 0;
  fHSeedEnergy = 0;
  fHSeedVSNHits = 0;
  fHTotalEnergy = 0;
}

Double_t   MUV1Reconstruction::SinglePeakFunction(Double_t   x, Double_t   * pars){

  Double_t  value = pars[0];
  value += pars[1] * TMath::Gaus(x,pars[2],pars[3]);

  return value;
}

void MUV1Reconstruction::ChiSquareSinglePeak(Int_t  &/*npar*/, Double_t   */*gin*/, Double_t   &f, Double_t   *par, Int_t  /*iflag*/){

  Double_t   chisquare = 0.;
  for (Int_t i=0; i<fNMUV1FitPoints; i++) {
    Double_t   delta = (SinglePeakFunction(fMUV1FitDataXptr[i],par) - fMUV1FitDataYptr[i]);
    Double_t   err2_x = SinglePeakFunction(fMUV1FitDataErrorXptr[i] + fMUV1FitDataXptr[i],par);
    err2_x -= SinglePeakFunction(fMUV1FitDataXptr[i] - fMUV1FitDataErrorXptr[i],par);
    err2_x *=  err2_x/4.;
    Double_t   err2_y = fMUV1FitDataErrorYptr[i]*fMUV1FitDataErrorYptr[i];
    Double_t   err2 = err2_x + err2_y;
    chisquare+=delta*delta / err2;
  }

  f = chisquare;
}


Double_t   MUV1Reconstruction::DoublePeakFunction(Double_t   x, Double_t   * pars){

  Double_t  value = pars[0];
  value += pars[1] * TMath::Gaus(x,pars[2],pars[3]);
  value += pars[4] * TMath::Gaus(x,pars[5],pars[6]);

  return value;
}

void MUV1Reconstruction::ChiSquareDoublePeak(Int_t  &/*npar*/, Double_t   */*gin*/, Double_t   &f, Double_t   *par, Int_t  /*iflag*/){

  Double_t   chisquare = 0.;
  for (Int_t i=0; i<fNMUV1FitPoints; i++) {
    Double_t   delta = (DoublePeakFunction(fMUV1FitDataXptr[i],par) - fMUV1FitDataYptr[i]);
    Double_t   err2_x = DoublePeakFunction(fMUV1FitDataErrorXptr[i] + fMUV1FitDataXptr[i],par);
    err2_x -= DoublePeakFunction(fMUV1FitDataXptr[i] - fMUV1FitDataErrorXptr[i],par);
    err2_x *=  err2_x/4.;
    Double_t   err2_y = fMUV1FitDataErrorYptr[i]*fMUV1FitDataErrorYptr[i];
    Double_t   err2 = err2_x + err2_y;
    chisquare+=delta*delta / err2;
  }

  f = chisquare;
}

Bool_t MUV1Reconstruction::IsClusterAllowedAtCenter(Int_t vchannel, Int_t hchannel, Int_t quadrant) {

  if (vchannel<21 || vchannel>24) return true;
  if (hchannel<21 || hchannel>24) return true;

  Int_t VIndexShift = (quadrant==1 || quadrant==2) ? 22 : 24;
  Int_t HIndexShift = (quadrant==1 || quadrant==4) ? 21 : 23;

  //The 2x2 matrix of the cluster flags is determined for the 1st quadrant.
  //In order to use it for other qudrants one or both indeces should be inverted depending exactly in which quadrant falls the combined cluster
  Int_t Vindex = (quadrant==1 || quadrant==2) ? TMath::Abs(vchannel-VIndexShift) : 1-TMath::Abs(vchannel-VIndexShift);
  Int_t Hindex = (quadrant==1 || quadrant==4) ? TMath::Abs(hchannel-HIndexShift) : 1-TMath::Abs(hchannel-HIndexShift);

  return fCentralClusterFlags[Vindex][Hindex];

}

Bool_t MUV1Reconstruction::IsClusterAllowedAtCorner(Int_t vchannel, Int_t hchannel, Int_t quadrant) {

  if (vchannel>3 && vchannel<42) return true;
  if (hchannel>3 && hchannel<42) return true;

  Int_t VIndexShift = (quadrant==1 || quadrant==2) ? 3 : 44;
  Int_t HIndexShift = (quadrant==1 || quadrant==4) ? 1 : 42;

  //The 3x3 matrix of the cluster flags is determined for the 1st quadrant.
  //In order to use it for other qudrants one or both indeces should be inverted depending exactly in which quadrant falls the combined cluster
  Int_t Vindex = (quadrant==1 || quadrant==2) ? TMath::Abs(vchannel-VIndexShift) : 2-TMath::Abs(vchannel-VIndexShift);
  Int_t Hindex = (quadrant==1 || quadrant==4) ? TMath::Abs(hchannel-HIndexShift) : 2-TMath::Abs(hchannel-HIndexShift);

  return fCornerClusterFlags[Vindex][Hindex];

}


Double_t MUV1Reconstruction::GetTPosDelay (Int_t Channel, Double_t Position) {

  if (!fTPosDependency) return 0;

  fTPosDependency->SetParameters(fMUV1TimeDep_par0[(Channel-100)/50][Channel%50-1],fMUV1TimeDep_par1[(Channel-100)/50][Channel%50-1]);

  return fTPosDependency->Eval(Position);

}


Double_t MUV1Reconstruction::GetQPosAttenuation (Int_t Channel, Double_t Position) {

  if (!fQPosDependency) return 1;

  fQPosDependency->SetParameters(fMUV1QEq_pars[(Channel-100)/50][Channel%50-1]);

  return fQPosDependency->Eval(Position);

}

//		y
//      ^
//		|
//		|
//		|
//		|-------------->x


//             201                            244
//		               T
//		_______________________________
//      194     |              |               |  294
//              |              |               |
//              |     4        |      3        |
//              |              |               |
//              |              |               |
//       S      |______________|_______________|   J
//              |              |               |
//              |              |               |
//              |              |               |
//              |      1       |       2       |
//              |              |               |
//      151     |______________|_______________|  251
//
//		               B
//             101                            144
