// ---------------------------------------------------------------
// History:
//
// Created by Dmitry Madigozhin (madigo@mail.cern.ch) 2016-11-15
//
// ---------------------------------------------------------------

#include <stdlib.h>
#include <iostream>
#include <TChain.h>

#include "SpectrometerJumpsCorrection.hh"

#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "BaseAnalysis.hh"
#include "ConfigSettings.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class SpectrometerJumpsCorrection
/// \Brief
/// First steps of the database production for Spectrometer T0 25-ns jumps correction 
/// \EndBrief
/// \Detailed
///
/// First, it produces the graphs of the peak position of the raw digi time 
/// distribution vs the timestamp. These graphs needed for the Full Correction Strategy (FCS) 
/// are written to the output root file to be analysed with a stand-alone
/// special T0 jumps software maintained within the separate GitLab project 
/// https://gitlab.cern.ch/NA62-Spectrometer/T0JumpsPreparation .
///
/// Apart from that, a Minimum Correction Strategy (MCS) implementation is 
/// also started here. It assumes only the correction of in-run T0 jumps 
/// without any usage of extra information from previouse runs and without any 
/// manual corrections in doubtful cases.
///
/// It is assumed for MCS, that the given NA62Analysis job processes 
/// a COMPLETE runs list in the histogram mode. Multiple complete runs are 
/// also admitted in a single job list. But all bursts must be time-ordered 
/// inside the full list.
///
/// Two resulting output local files should be used in the next NA62Reco pass for 
/// the same run(s) (for MCS):
///
/// Spectrometer-T0-InGoodRun-jumps.dat and Spectrometer-T0-InGoodRunJumps-runlist.dat .
///
/// But if they are empty, the next NA62Reco pass is not needed at all, one should just 
/// regard the first-pass results as a second pass output. No detected in-run jumps, no correction. 
/// It should happen for the majority of runs with MCS.
///
/// In order to use these two files for the next pass of NA62Reco, one should put them into config 
/// directory and add the following line to the active Spectrometer configuration file:
///
/// T0Jumps= config/Spectrometer-T0-InGoodRun-jumps.dat config/Spectrometer-T0-InGoodRunJumps-runlist.dat
///
/// In the case of multi-run next-pass jobs these separate run-related files must be SORTED together
/// for concatenation in order to be sure that a correct runs order and jumps time order is maintained 
/// inside the joint files.
///
/// Timestamp of the burst and the run number are taken here from the filename, as the availability of 
/// them in data structure is (was) not guaranteed in the case of software versions mismatch. So the burst
/// file names must follow the standard.
///
/// \code
/// Usage:
/// ./SpectrometerJumpsCorrection -l <filelist> -- histo -p "SpectrometerJumpsCorrection:l=<filelist>"
/// \endcode
///
/// So the file list name needs to be specified twice if it is not a default "input.list"
/// (in this case one can just run: ./SpectrometerJumpsCorrection -l input.list --histo)
///
/// \author Dmitry Madigozhin (madigo@mail.cern.ch)
/// \EndDetailed

SpectrometerJumpsCorrection::SpectrometerJumpsCorrection(Core::BaseAnalysis *ba) : Analyzer(ba, "SpectrometerJumpsCorrection")
{
  /// \MemberDescr
  /// \param ba : parent BaseAnalysis
  /// \code

  Configuration::ConfigSettings::SetNoSkipBadBurst(true); // do not skip bad bursts
  fRun=0;
  fPreRun=0;
  fPreTimestamp=0;
  fTimestamp=0;

  for(Int_t i=0; i<512; i++)fToggle[i]=0;

  fRunBeginTimestamp=0;
  fPreRunBeginTimestamp=0;

  AddParam("l", &fListOfInputFiles,"input.list");
  
  /// \endcode
  /// \EndMemberDescr
  
  //	RequestTree("Spectrometer",new TRecoSpectrometerEvent);
  
}

Double_t Significance(TH1D *h, Int_t maxbin){
  
  Double_t maximum = h->GetBinContent(maxbin);
  
  Double_t bg =  h->GetBinContent(maxbin-2);
  
  if(h->GetBinContent(maxbin-3) == 0.0){
    bg = h->GetBinContent(maxbin+8);
  }
  
  if((maximum+bg) > 0.5){
    return (maximum-bg)/sqrt(maximum+bg); // preliminary statistical significance of the signal 
  }else{
    return 0.0;
  }
}

void Cmax(TH1D *h, Double_t *cm, Double_t *ce){
  
  Int_t maxbin = h->GetMaximumBin();
  Double_t cmax = h->GetBinCenter(maxbin);
  Double_t maxc = h->GetBinContent(maxbin);
  Double_t cerr = 0.0;
  
  // search for the maximum of parabola (initially take the first point x1 to be 0)
  Double_t d = REBIN;
  Double_t a = h->GetBinContent(maxbin-1); // a=y1
  Double_t y2 = maxc;
  Double_t y3 = h->GetBinContent(maxbin+1);
  Double_t v = a-2*y2+y3;
  
  if(v != 0.0){
    Double_t u = 3*a-4*y2+y3;
    Double_t q = u/v;
    cmax = h->GetBinCenter(maxbin-1) + 0.5*d*q; // x1 + maximum of the parabola
    cerr = d*sqrt( a*(3.0-q)*(3.0-q) + y2*(2.0*q-4.0)*(2.0*q-4.0) + y3*(1.0-q)*(1.0-q))/(2*fabs(v));    
  }

  *cm=cmax;
  *ce=cerr;  
  return;
}


void SpectrometerJumpsCorrection::InitOutput(){
  /// \MemberDescr
  /// Register the output variables of the analyzer.\n
  /// \EndMemberDescr
}

void SpectrometerJumpsCorrection::InitHist(){
  /// \MemberDescr
  /// Book and Initialize histograms in this function.\n
  /// \code
  
  if(fParams.count("l")==0){
    cout << user_normal() << "List Parameter does not exist" << endl;
  }else{
    fInputListStream.open(fListOfInputFiles.data());
    if(fInputListStream.is_open()){
      cout << user_normal() << fListOfInputFiles << " is open..." << endl << endl;
    }else{
      cout << user_normal() << fListOfInputFiles << " is not open !" << endl << endl;
    }
  }
  
  /// \endcode
  /// 
  /// You can retrieve histograms from the input ROOT\n
  /// \code

  GRunVsTimeStamp = new TGraph();
  GRunVsTimeStamp->SetNameTitle("RunVsTimeStamp","Run vs Timestamp");
  BookHisto(GRunVsTimeStamp);

  GBadBurstVsTimeStamp = new TGraph();
  GBadBurstVsTimeStamp->SetNameTitle("BadBurstVsTimeStamp","Bad burst flag vs Timestamp");
  BookHisto(GBadBurstVsTimeStamp);

  HDigiTimeRaw = static_cast<TH2F *>(RequestHistogram("SpectrometerMonitor", "DigiTimeRaw", false));
  
  if(HDigiTimeRaw){             
    cout << user_normal() << "NEntries = " << HDigiTimeRaw->GetEntries() << endl << endl;
    
    TH2F* tmp = static_cast<TH2F*>(HDigiTimeRaw->Clone("tmp"));
    tmp->Rebin2D(1,REBIN);
    
    for(Int_t cov=0; cov<512; cov++){                    // covers
      GPeakTimeVsTimeStamp[cov] = new TGraphErrors();
      GPeakTimeVsTimeStamp[cov]->SetNameTitle(Form("PeakTimeVsTimeStamp_%d",cov),Form("Peak Time vs Timestamp for the cover %d",cov));
      BookHisto(GPeakTimeVsTimeStamp[cov]);
      HAccumulated[cov][0] = tmp->ProjectionY(Form("Accumulated0%04d", cov), cov+1, cov+1);
      HAccumulated[cov][0]->Reset();
      HAccumulated[cov][1] = tmp->ProjectionY(Form("Accumulated1%04d", cov), cov+1, cov+1);
      HAccumulated[cov][1]->Reset();
    }
    
    delete tmp;
    
  }else{
    cout << endl << user_normal() << "No time histogram is available!" << endl << endl;
  }

  /// \endcode
  /// \EndMemberDescr
}

void SpectrometerJumpsCorrection::DefineMCSimple(){}

void SpectrometerJumpsCorrection::StartOfRunUser(){
  /// \MemberDescr
  /// This method is called at the beginning of the processing (corresponding to a start of run in the normal NA62 data taking)\n
  /// Do here your start of run processing if any
  /// \EndMemberDescr
}

void SpectrometerJumpsCorrection::StartOfBurstUser(){
  /// \MemberDescr
  /// This method is called when a new file is opened in the ROOT TChain (corresponding to a start/end of burst in the normal NA62 data taking) + at the beginning of the first file\n
  /// Do here your start/end of burst processing if any
  /// \EndMemberDescr
}

char *kBurst(char buff[BUFSIZE],int n){
  // modify the buff
  int k = -1; // find the last slash
  int l = -1; // find the last point
  
  for(int i=0; i<n; i++){
    if(buff[i]=='\0')break;
    if(buff[i]=='/'){k=i;}
    if(buff[i]=='.'){l=i;}
  }
  
  k+=4;
  buff[k]='B';
  buff[k+1]='u';
  buff[k+2]='r';
  buff[k+3]='s';
  buff[k+4]='t';
  
  if(l > -1){
    buff[l]='\0';
  }
  
  return &buff[k];
}

Int_t SpectrometerJumpsCorrection::BadBurst(TH2F *h){

   for(Int_t i=0; i<512; i++){
      TH1D *c = h->ProjectionY(Form("tmpcu%04d", i), i+1, i+1);
      Int_t count=0;
      Double_t pre = -1.0;
      for(Int_t j = 1; j <= c->GetNbinsX(); j++){
         Double_t cur = c->GetBinContent(j);
         if((pre>-0.5) && ((pre+cur) > 10000.0)){
            if(fabs(pre-cur) > 0.5*(pre+cur))count++;
         }
         pre=cur;
      }
      delete c;
      if(count>1)return count;
   }

   return 0;
}

void SpectrometerJumpsCorrection::Process(int iEvent){
  /// \MemberDescr
  /// \param iEvent : Event number
  /// \code
  
  cout << user_normal() << "iEvent" << iEvent << endl;

  if(fInputListStream.is_open()){
    char buff[BUFSIZE], buf1[BUFSIZE];
    char *kB;
  
    fInputListStream.getline(buff,BUFSIZE);
    cout << endl << user_normal() << "path: " << buff << endl;
    
    kB = kBurst(buff,BUFSIZE);   // buff is rewritten here
    
    strcpy(buf1,kB);
    buf1[16]='\0';

    Int_t OldTimestamp=fTimestamp;
    sscanf((buf1+6),"%d",&fTimestamp);

    if(fTimestamp <= OldTimestamp){
      std::cout << user_normal() << "Burst is ignored as the order is violated: " << fTimestamp << " <= " << OldTimestamp << std::endl;
      return;
    }

    strcpy(buf1,kB+20);
    buf1[6]='\0';
    sscanf(buf1,"%d",&fRun);
  }

  if(fRun != fPreRun){
    fPreRunBeginTimestamp=fRunBeginTimestamp;
    fRunBeginTimestamp=fTimestamp;

    Int_t n = GRunVsTimeStamp->GetN();
    if(n)GRunVsTimeStamp->SetPoint(n,fPreTimestamp,fPreRun);

    n = GRunVsTimeStamp->GetN();
    GRunVsTimeStamp->SetPoint(n,fRunBeginTimestamp,fRun);

    for(Int_t cov=0; cov<512; cov++)EndOfRunJumps(cov);
  }
  
  if(HDigiTimeRaw){
    
    TH2F *tmp = static_cast<TH2F*>(HDigiTimeRaw->Clone("tmp"));

    Int_t flag = BadBurst(tmp);

    if(flag){
        Int_t np = GBadBurstVsTimeStamp->GetN();
        Double_t grtime = (Double_t)fTimestamp;
        GBadBurstVsTimeStamp->SetPoint(np,grtime,(Double_t)flag);
        delete tmp;
        return;
    }

    tmp->Rebin2D(1,REBIN);
    
    for(Int_t cov=0; cov<512; cov++){  // covers
      
      TH1D *cTime = tmp->ProjectionY(Form("cu%04d", cov), cov+1, cov+1);
      HAccumulated[cov][0]->Add(cTime);
      HAccumulated[cov][1]->Add(cTime);
      delete cTime;
      
      if(HAccumulated[cov][fToggle[cov]]->GetEntries()>100){  // just to avoid the almost empty histogram
	
	Int_t maxbin = HAccumulated[cov][fToggle[cov]]->GetMaximumBin();
	
	if(Significance(HAccumulated[cov][fToggle[cov]],maxbin) > MINSIGNIF){
	  
	  Double_t cmax,cerr;
	  Cmax(HAccumulated[cov][fToggle[cov]],&cmax,&cerr);
	  
	  Int_t np = GPeakTimeVsTimeStamp[cov]->GetN();
	  
	  Double_t grtime = (Double_t)fTimestamp; // for the better visibility on the graph
	  
	  GPeakTimeVsTimeStamp[cov]->SetPoint(np,grtime,cmax);
	  GPeakTimeVsTimeStamp[cov]->SetPointError(np,0.0,cerr);
	  
	  fToggle[cov] = 1 - fToggle[cov];
	  HAccumulated[cov][fToggle[cov]]->Reset();  // Reset the alternative accumulated histogram and
	  // than regard it as the current one. In such a way 
	  // another histogram will always contain all the
	  // statistics after the pre-last point, and we are
	  // always ready to set the last point of the run.
	}
      }

    }
    
    delete tmp;
    
    // keep the previous run and timestamp for the next considered burst
    
    fPreTimestamp=fTimestamp;
    fPreRun=fRun;
    
  }else{
    cout << endl << user_normal() << "No input time histogram is available for the burst!" << endl << endl;
  }
  
  /// \endcode
  /// \EndMemberDescr
  
}

void SpectrometerJumpsCorrection::PostProcess(){
  /// \MemberDescr
  /// This function is called after an event has been processed by all analyzers. It could be used to free some memory allocated
  /// during the Process.
  /// \EndMemberDescr
  
}

void SpectrometerJumpsCorrection::EndOfBurstUser(){
  /// \MemberDescr
  /// This method is called when a new file is opened in the ROOT TChain (corresponding to a start/end of burst 
  /// in the normal NA62 data taking) + at the end of the last file\n
  /// Do here your start/end of burst processing if any.
  /// Be careful: this is called after the event/file has changed.
  /// \EndMemberDescr
}

void SpectrometerJumpsCorrection::EndOfRunUser(){
  /// \MemberDescr
  /// This method is called at the end of the processing (corresponding to a end of run in the normal NA62 data taking)\n
  /// Do here your end of run processing if any\n
  /// \EndMemberDescr
  
}

void SpectrometerJumpsCorrection::EndOfRunJumps(Int_t cov){
  /// \MemberDescr
  /// This method is called both at the end of the run and at the end of the job\n
  /// It finish the previouse run with the last accumulated histograms and 
  /// set the last point of the graph at the last burst of the run.
  /// \code

  // First we check if the current accumulated histogram has occasionally a significant signal yet

  if(HAccumulated[cov][fToggle[cov]]->GetEntries()>100){  // just to avoid the almost empty histogram
    
    Int_t maxbin = HAccumulated[cov][fToggle[cov]]->GetMaximumBin();
    
    if(Significance(HAccumulated[cov][fToggle[cov]],maxbin) > MINSIGNIF){
      
      Double_t cmax,cerr;
      Cmax(HAccumulated[cov][fToggle[cov]],&cmax,&cerr);

      // just add the last point of the run
 
      Int_t np = GPeakTimeVsTimeStamp[cov]->GetN();
      Double_t grtime = (Double_t)fPreTimestamp;  // it should be the last timestamp of the previouse run
      GPeakTimeVsTimeStamp[cov]->SetPoint(np,grtime,cmax);
      GPeakTimeVsTimeStamp[cov]->SetPointError(np,0.0,cerr);
      
      // next run will start from the empty accumulated histograms
      HAccumulated[cov][0]->Reset();
      HAccumulated[cov][1]->Reset();
      fToggle[cov] = 0;
      return;
    }
  }

  // So the last accumulated histogram still does not have a significant signal.
  // In such a case we will include the extra statistics (if it exists) into the 
  // last point and move the point to the last timestamp of the run.

  fToggle[cov] = 1 - fToggle[cov];  // check for the parallel accumulated histogram, that may be includes extra statistics
  
  if(HAccumulated[cov][fToggle[cov]]->GetEntries()>100){  // just to avoid the almost empty histogram
    
    Int_t maxbin = HAccumulated[cov][fToggle[cov]]->GetMaximumBin();
    
    if(Significance(HAccumulated[cov][fToggle[cov]],maxbin) > MINSIGNIF){
      
      Double_t cmax,cerr;
      Cmax(HAccumulated[cov][fToggle[cov]],&cmax,&cerr);
      
      Int_t np = GPeakTimeVsTimeStamp[cov]->GetN() - 1; // we will try to replace the last point
      
      if(np<0){
	np=0; // if there was no points at all, we set the first one
      }else{
	// check if the last point was not before the last run (weried case, may be impossible).
	Double_t t00,y00;
	GPeakTimeVsTimeStamp[cov]->GetPoint(np,t00,y00);
	if(t00 < fPreRunBeginTimestamp){
	  np++;
	  // Return back to the new point creation, as the previouse point was in 
	  // the another run and we don't want to mix data from different runs.
	  // At the start of previouse run the accumulated histograms were clean,
	  // so the new point is statistically independent.
	}
      }
      
      Double_t grtime = (Double_t)fPreTimestamp;  // it should be the last timestamp of the previouse run
      
      GPeakTimeVsTimeStamp[cov]->SetPoint(np,grtime,cmax);
      GPeakTimeVsTimeStamp[cov]->SetPointError(np,0.0,cerr);
      
    }
  }
  
  // next run will start from the empty accumulated histograms
  HAccumulated[cov][0]->Reset();
  HAccumulated[cov][1]->Reset();
  fToggle[cov] = 0;
  return;

  /// \endcode
  /// \EndMemberDescr
  
}

void SpectrometerJumpsCorrection::EndOfJobUser(){
  /// \MemberDescr
  /// Called at the end of job, just before exiting NA62Analysis.\n
  /// \code

  // Write the output graphs for the Full Correction Strategy (FCS)

  Int_t n = GRunVsTimeStamp->GetN();
  GRunVsTimeStamp->SetPoint(n,fTimestamp,fRun);
  GRunVsTimeStamp->Write();

  GBadBurstVsTimeStamp->Write();

  for(Int_t cov=0; cov<512; cov++){
    EndOfRunJumps(cov);
    GPeakTimeVsTimeStamp[cov]->Write();
  }

  // Now we start the Minimum Correction Strategy (MCS) on the basis of these graphs.

  ofstream outjumps;
  outjumps.open(Form("Spectrometer-T0-InGoodRun-jumps.dat"));  // the jumps output file
  ofstream outruns; 
  outruns.open(Form("Spectrometer-T0-InGoodRunJumps-runlist.dat"));  // the list of runs with an in-run jumps

  Int_t Nruns = (GRunVsTimeStamp->GetN())/2; // two points per run

  for(Int_t ir=0; ir<Nruns; ir++){

    bool JumpIsFound = false;

    // get the current run number and its time limits 
    Double_t tbegin,tend,run;
    GRunVsTimeStamp->GetPoint(ir*2,tbegin,run);
    GRunVsTimeStamp->GetPoint(ir*2+1,tend,run);

    Int_t itbegin = (Int_t)round(tbegin);
    Int_t itend = (Int_t)round(tend);

    // start the cover-by-cover analysis
    
    for(int srb=0; srb<32; srb++){
      for(int iCover=0; iCover<16; iCover++)if(srb%2 || ((iCover != 11) && (iCover != 15))){
	  Int_t cov = srb*16+iCover;   // cover index that really exists
	  
	  DRing ring;             // sliding average creation and initialization

	  Int_t jumps=0;          // accumulated jump for the given cover, starts from 0
	  Int_t it0 = 0;          // previous point time
	  Int_t np = GPeakTimeVsTimeStamp[cov]->GetN();
	  for(Int_t i=0; i<np; i++){
	    
	    Double_t t,y,ey;
	    GPeakTimeVsTimeStamp[cov]->GetPoint(i,t,y);

	    Int_t it = (Int_t)round(t);

	    if(it < itbegin)continue;
	    if(it > itend)continue;

	    ey = GPeakTimeVsTimeStamp[cov]->GetErrorY(i);
	    
	    if((ey < 0.2) || (ey>100.0))continue;   // strange error size (a bad point = may be bad burst for the single cover)
	    
	    ey = sqrt(ey*ey+1.0);                   // for the case of unrealistic too small error, we add a systematic 1 ns
	    
	    Double_t yc = y-jumps*UN;               // (accumulated jump) - corrected value to be used in sliding average
	    
	    if(ring.IsNotEmpty()){                  // jumps detection starts from the second good point
	      Double_t val,err;
	      ring.Get(&val,&err);
	      
	      Double_t ump = fabs(yc - val);          // deviation of jump-corrected new point from the current sliding average
	      Double_t urr = sqrt(err*err + ey*ey);   // joint statistical error
	      
	      if((ump > DT0) && (ump > SIGNI*urr)){   // jump is detected by a minimum absolute deviation with a large significance 
		
		Int_t jump = round((yc - val)/UN);    // jump size in units of UN
		
		// Int_t jtime = it-10;               // jump is assumed to happen 10 seconds prior to the current burst
		Int_t jtime = it0+10;                 // 10 seconds after the prevoius point (in-run jump can not happen before the first point)

		outjumps << jtime << " " << cov << " " << jump << endl;
		
		JumpIsFound = true;
		
		jumps += jump;                        // update the accumulated jump
		yc = y-jumps*UN;                      // recalculate the current corrected y with a new summary jump
	      } // jump is detected
	    } // ring.IsNotEmpty()
	    
	    ring.Put(yc,ey);                       // include the jump-corrected value into the sliding average

	    it0=it;

	  } // the graph points loop
	} // cover loop
    } // srb loop
    
    
    if(JumpIsFound){        // create a record for the list of runs having in-run jumps
      outruns << (int)run << " " << itbegin << " " << itend << endl;
    }
    
  } // loop over the runs
  
  outjumps.close();  // if no in-run jumps is detected, the file will be empty
  outruns.close();   // if no in-run jumps is detected, the file will be empty
  
  /// \endcode
  /// \EndMemberDescr
}

void SpectrometerJumpsCorrection::DrawPlot(){
  /// \MemberDescr
  /// This method is called at the end of processing to draw plots when the -g option is used.\n
  /// \EndMemberDescr
}

SpectrometerJumpsCorrection::~SpectrometerJumpsCorrection(){
  /// \MemberDescr
  /// Destructor of the Analyzer. If you allocated any memory for class members, delete them here.
  /// \code
  
  fInputListStream.close(); // close the additional input stream
  
  /// \endcode
  /// \EndMemberDescr
}

DRing::DRing():
  full(false), nemp(false), l(0)
{
}

DRing::~DRing(){
}

void DRing::Reset(){
  full=false;
  nemp=false;
  l=0;
}

void DRing::Put(Double_t x, Double_t e){

  val[l]=x;
  err[l]=e;

  l++;

  if(l>=NRING){
    full=true;
    l=0;
  }

  nemp=true;
  return; 
}

void DRing::Get(Double_t *x, Double_t *e){

  *x=0.0;
  *e=0.0;

  if(nemp){

    Double_t s=0.0;
    Double_t sw=0.0;
    Int_t n;
    
    if(full){
      n=NRING;
    }else{
      if(!l){
	return; // impossible in fact
      }
      n=l;
    }

    for(Int_t i=0; i<n; i++){
      Double_t w = 1.0/(err[i]*err[i]);
      s += val[i]*w;
      sw += w;
    }
    
    *x=s/sw;
    *e=1.0/sqrt(sw);
  }
  
  return;
}

