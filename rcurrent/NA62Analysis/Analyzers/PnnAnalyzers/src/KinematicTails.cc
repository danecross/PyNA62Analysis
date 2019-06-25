// ---------------------------------------------------------
// History:
//
// Created by Zuzana Kucerova (zuzana.kucerova@cern.ch) 2019-05-13
//
// ---------------------------------------------------------
/// \class KinematicTails
/// \Brief
/// Analyser for PNN kinematic tails (K2pi, Kmu2) analysis.
/// \EndBrief
/// \Detailed
/// This analyzer is used to produce K2pi and Kmu2 kinematic tails for PNN analysis using standard tools.
/// This analysis differs (by methodology) from the one of Giuseppe and Rado, as well as from my private analysis, in which 

/// The full kinematic tails analysis consists of several stages:
/// 1. basic selection (PnnKinematicTails_AdditionalTimeAlignment (preanalyzer), CheckTrigger, Preselection)
/// 2. best track selection (FakeTrackSelection, GTK reco, matching, BestTrackSelection)
/// 3. single track event selection (SingleTrackEventSelection)
/// 4. decay selection (K2pi, Kmu2 + EventCleaning, KaonDecaySelection, PhotonRejection, PhotonRejectionLKr, SegmentRejection, MultiplicitySelection, TrackCalorimetricEnergyAssociation)
/// 5. tails (KinematicTails)
/// The final analyzer - KinematicTails - can be ran in two modes: in standard mode it requests output from other analyzers mentioned above and produces plots of squared missing mass for K2pi and Kmu2. When ran in histo mode on its own output the analyzer produces pdf file with plot of kinematic tails in R1 and R2, both for K2pi and Kmu2 and text files (one for K2pi and one for Kmu2) with table of kinematic tails in all regions and 5GeV momentum bins.
/// All important final variables (IDs, vertex position, momenta, missM2) are in the output of this analyzer. 
/// All cuts are made as parameters and are set in the constructor of each subanalyzer to same values as were used in my presentation in April 2019. 
/// The parameter Verbosity (verb) can be used to debug the code as well as to better understand how the analysis works. It is also very useful for analysis of single events. 
///  
/// \author Zuzana Kucerova (zuzana.kucerova@cern.ch)
/// \EndDetailed 

#include "KinematicTails.hh"

#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <TChain.h>
#include <TStyle.h>
#include <TGaxis.h>
#include <TLegend.h>
#include <TList.h>
#include "GeometricAcceptance.hh"
#include "BeamParameters.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "PnnKinematicTailsFunctions.hh"
#include "Event.hh"
#include "Persistency.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

KinematicTails::KinematicTails(Core::BaseAnalysis *ba) : Analyzer(ba, "KinematicTails")
{
  //GeV^2/c^4
  fKmu2_low = -0.05; //3
  fCRKmu2_up = 0.; //
  fSR1_low = 0.; //4
  fSR1_up = 0.01; //
  fCR1_low = 0.01; //5
  fCR1_up = 0.015; //
  fPiPi_low = 0.015; //6
  fPiPi_up = 0.021; //
  fCR2_low = 0.021; //7
  fCR2_up = 0.026; //
  fSR2_low = 0.026; //8
  fSR2_up = 0.068; //
  fK3pi_low = 0.068; //9

  //mm2(RICH)
  fSR1richlowp_low = 0.;
  fSR1richlowp_up = 0.01;
  fSR1richmiddlep_low = 0.;
  fSR1richmiddlep_up = 0.02;
  fSR1richhighp_low = -0.005;
  fSR1richhighp_up = 0.02;
  fSR2rich_low = 0.02;
  fSR2rich_up = 0.07;
  fPiPirich_low = 0.; 
  fPiPirich_up = 0.07;

  //mm2(beam)
  fSR1beamlowp_low = -0.005;
  fSR1beamlowp_up = 0.0135;
  fSR1beammiddlep_low = -0.005;
  fSR1beammiddlep_up = 0.0135;
  fSR1beamhighp_low = 0.;
  fSR1beamhighp_up = 0.0135;
  fSR2beam_low = 0.024;
  fSR2beam_up = 0.068;

  k2pi = "K2pi/";
  kmu2 = "Kmu2/";
  all = "All/";

  ftracker = BlueTubeTracker::GetInstance();

  RequestTree("GigaTracker", new TRecoGigaTrackerEvent, "Reco");
  RequestTree("Spectrometer", new TRecoSpectrometerEvent, "Reco");
  RequestBeamData();  

  AddParam("UseGTK", "bool", &UseGTK, true);
  AddParam("WhichTrigger", "int", &fWhichTrigger, 1);
  AddParam("Verbosity", "bool", &verb, false);
  AddParam("FillHistoCheck", "bool", &fFillHistoCheck, true);

  fOutPDFfileName = fAnalyzerName + ".pdf";
  fOutTableName = fAnalyzerName;
}

void KinematicTails::InitOutput(){
  RegisterOutput("WhichDecay", &fWhichDecay);
  RegisterOutput("TrackID", &fTrackID);
  RegisterOutput("KaonID", &fKaonID);
  RegisterOutput("Track4Momentum", &fTrack);
  RegisterOutput("Kaon4Momentum", &fKaon);
  RegisterOutput("Vertex", &fVertex);
  RegisterOutput("MissM2", &fMissM2);
  RegisterOutput("MissM2beam", &fMissM2beam);
  RegisterOutput("MissM2rich", &fMissM2rich);
}

void KinematicTails::InitHist(){
  fReadingData = GetIsTree();

  if(fReadingData){
    ReconfigureAnalyzer("CheckTrigger", "WhichTrigger", fWhichTrigger);
    ReconfigureAnalyzer("CheckTrigger", "Verbosity", verb);
    ReconfigureAnalyzer("Preselection", "Verbosity", verb);
    ReconfigureAnalyzer("FakeTrackSelection", "Verbosity", verb);
    ReconfigureAnalyzer("BestTrackSelection", "Verbosity", verb);
    ReconfigureAnalyzer("SingleTrackEventSelection", "Verbosity", verb);
    ReconfigureAnalyzer("Kmu2", "Verbosity", verb);
    ReconfigureAnalyzer("K2pi", "Verbosity", verb);
    ReconfigureAnalyzer("MultiplicitySelection", "Verbosity", verb);
    ReconfigureAnalyzer("SegmentRejection", "Verbosity", verb);
    ReconfigureAnalyzer("EventCleaning", "Verbosity", verb);
    ReconfigureAnalyzer("KaonDecaySelection", "Verbosity", verb);
    ReconfigureAnalyzer("TrackCalorimetricEnergyAssociation", "Verbosity", verb);
    ReconfigureAnalyzer("PhotonRejection", "Verbosity", verb);
    ReconfigureAnalyzer("PhotonRejectionLKr", "Verbosity", verb);
    ReconfigureAnalyzer("BestTrackSelection", "UseGTK", UseGTK);
    ReconfigureAnalyzer("SingleTrackEventSelection", "UseGTK", UseGTK);
    ReconfigureAnalyzer("SegmentRejection", "UseGTK", UseGTK);
    ReconfigureAnalyzer("Kmu2", "UseGTK", UseGTK);
    ReconfigureAnalyzer("K2pi", "UseGTK", UseGTK);

    BookHisto(new TH1I("hCut", "hCut", 20, 1, 21));
    BookHisto(new TH1D("hBeamIntensity", "hBeamIntensity", 340, -3.7, 2514.81));
    BookHisto(new TH1D("hNomBeamMom", "hNomBeamMom", 800, 0., 80000.));

    TString dec[2] = {k2pi+all, kmu2+all};
    for(int i=0; i<2; i++){
      BookHisto(new TH1D(dec[i]+"hMissM2_R", "MissM2_R", 375, -0.1, 0.14375));
      BookHisto(new TH1D(dec[i]+"hMissM2", "MissM2", 1000, -0.2, 0.2));
      BookHisto(new TH1D(dec[i]+"hMissM2beam", "MissM2beam", 1000, -0.2, 0.2));
      BookHisto(new TH1D(dec[i]+"hMissM2rich", "MissM2rich", 1000, -0.2, 0.2));
      BookHisto(new TH2D(dec[i]+"hMissM2richVsMissM2", "MissM2richVsMissM2", 200, -0.2, 0.2, 200, -0.2, 0.2));
      BookHisto(new TH2D(dec[i]+"hMissM2beamVsMissM2", "MissM2beamVsMissM2", 200, -0.2, 0.2, 200, -0.2, 0.2));
      BookHisto(new TH2D(dec[i]+"hMissM2richVsMissM2beam", "MissM2richVsMissM2beam", 200, -0.2, 0.2, 200, -0.2, 0.2));
      BookHisto(new TH1D(dec[i]+"hMissM2Reg", "MissM2 in regions", 9, 1, 10)); //
      BookHisto(new TH2D(dec[i]+"hMissM2MomVsReg", "MissM2 Momentum vs regions", 9, 1, 10, 4, 15, 35)); //
      BookHisto(new TH2D(dec[i]+"hMissM2vsTrackMom", "MissM2 vs Track momentum", 200, 15., 35., 200, -0.2, 0.2));
      BookHisto(new TH1D(dec[i]+"hGTKMatchingQuality1", "GTK-track matching quality1", 200, 0., 20.));
    };
  }else{
    hKP1 = static_cast<TH1D*>(RequestHistogram(fAnalyzerName, k2pi+all+"hMissM2Reg", true));
    hKP = static_cast<TH2D*>(RequestHistogram(fAnalyzerName, k2pi+all+"hMissM2MomVsReg", true));

    hKM1 = static_cast<TH1D*>(RequestHistogram(fAnalyzerName, kmu2+all+"hMissM2Reg", true));
    hKM = static_cast<TH2D*>(RequestHistogram(fAnalyzerName, kmu2+all+"hMissM2MomVsReg", true));

    TString labels[9] = {" ", "Kmu2", "CR(Kmu2)", "R1", "CR1(K2pi)", "K2pi", "CR2(K2pi)", "R2", "K3pi"};
    for(int i=1; i<10; i++){
      hKP1->GetXaxis()->SetBinLabel(i, labels[i-1]);
      hKP->GetXaxis()->SetBinLabel(i, labels[i-1]);
      hKM1->GetXaxis()->SetBinLabel(i, labels[i-1]);
      hKM->GetXaxis()->SetBinLabel(i, labels[i-1]);
    };
  };
}

void KinematicTails::DefineMCSimple(){
}

void KinematicTails::StartOfRunUser(){
}

void KinematicTails::StartOfBurstUser(){
}

void KinematicTails::ProcessSpecialTriggerUser(int, unsigned int){
}

void KinematicTails::Process(int){
  if (!fReadingData) return;

  PrepareOutputs();
  ValidateOutputs();

  int cutID = 1;
  FillHisto("hCut", cutID);
  cutID++;

  if(verb){
    cout<<endl;
    cout<<"-------------------"<<endl;
    cout<<"KinematicTails"<<endl;
    cout<<"-------------------"<<endl;
    cout<<endl;
  };

  OutputState state;
  auto preselectedEvent =
    *(bool*)GetOutput("Preselection.PreselectedEvent", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  if(verb) cout<<"Is event preselected? "<<preselectedEvent<<endl;
  if(!preselectedEvent){
    if(verb) cout<<"Event is not preselected"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  auto tTrigger =
    *(double*)GetOutput("CheckTrigger.TriggerTime", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;
  if(verb) cout<<"Trigger time read = "<<tTrigger<<endl;

  fSTRAWEvent = GetEvent<TRecoSpectrometerEvent>();
  FillHisto("hBeamIntensity", GetBeamData()->GetInstantaneousIntensity());
  TVector3 nomBeamMom = BeamParameters::GetInstance()->GetBeamThreeMomentum();
  FillHisto("hNomBeamMom", nomBeamMom.Mag());
  fKaonNom.SetXYZM(nomBeamMom.X(), nomBeamMom.Y(), nomBeamMom.Z(), MKCH);

  K2pi();
  FillHisto("hCut", cutID);
  cutID++;

  Kmu2();
  FillHisto("hCut", cutID);
}

void KinematicTails::PostProcess(){
}

void KinematicTails::EndOfBurstUser(){
}

void KinematicTails::EndOfRunUser(){
}

void KinematicTails::EndOfJobUser(){
  gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed

  if(!fReadingData){
    if(!hKM){
      cout << user_normal() << "Asked to read my own output but cannot find it" << endl;
      return;
    };

    hKM->Write();
    hKP->Write();
    hKM1->Write();
    hKP1->Write();

    gStyle->SetOptStat(0);
    TGaxis::SetMaxDigits(2);
    TH1D *hkmu2R1 = new TH1D("hkmu2R1", "hkmu2R1", 4, 15., 35.);
    hkmu2R1->SetBinContent(1, ((hKM->GetBinContent(2, 1)==0)?0:(hKM->GetBinContent(4, 1)/hKM->GetBinContent(2, 1))));
    hkmu2R1->SetBinContent(2, ((hKM->GetBinContent(2, 2)==0)?0:(hKM->GetBinContent(4, 2)/hKM->GetBinContent(2, 2))));
    hkmu2R1->SetBinContent(3, ((hKM->GetBinContent(2, 3)==0)?0:(hKM->GetBinContent(4, 3)/hKM->GetBinContent(2, 3))));
    hkmu2R1->SetBinContent(4, ((hKM->GetBinContent(2, 4)==0)?0:(hKM->GetBinContent(4, 4)/hKM->GetBinContent(2, 4))));
    if(hkmu2R1->GetBinContent(1)>0.) hkmu2R1->SetBinError(1, ((hKM->GetBinContent(4, 1))/(hKM->GetBinContent(2, 1)))*sqrt(1./hKM->GetBinContent(4, 1) + 1./hKM->GetBinContent(2, 1)));
    if(hkmu2R1->GetBinContent(2)>0.) hkmu2R1->SetBinError(2, ((hKM->GetBinContent(4, 2))/(hKM->GetBinContent(2, 2)))*sqrt(1./hKM->GetBinContent(4, 2) + 1./hKM->GetBinContent(2, 2)));
    if(hkmu2R1->GetBinContent(3)>0.) hkmu2R1->SetBinError(3, ((hKM->GetBinContent(4, 3))/(hKM->GetBinContent(2, 3)))*sqrt(1./hKM->GetBinContent(4, 3) + 1./hKM->GetBinContent(2, 3)));
    if(hkmu2R1->GetBinContent(4)>0.) hkmu2R1->SetBinError(4, ((hKM->GetBinContent(4, 4))/(hKM->GetBinContent(2, 4)))*sqrt(1./hKM->GetBinContent(4, 4) + 1./hKM->GetBinContent(2, 4)));

    TH1D *hkmu2R2 = new TH1D("hkmu2R2", "hkmu2R2", 4, 15., 35.);
    hkmu2R2->SetBinContent(1, ((hKM->GetBinContent(2, 1)==0)?0:(hKM->GetBinContent(8, 1)/hKM->GetBinContent(2, 1))));
    hkmu2R2->SetBinContent(2, ((hKM->GetBinContent(2, 2)==0)?0:(hKM->GetBinContent(8, 2)/hKM->GetBinContent(2, 2))));
    hkmu2R2->SetBinContent(3, ((hKM->GetBinContent(2, 3)==0)?0:(hKM->GetBinContent(8, 3)/hKM->GetBinContent(2, 3))));
    hkmu2R2->SetBinContent(4, ((hKM->GetBinContent(2, 4)==0)?0:(hKM->GetBinContent(8, 4)/hKM->GetBinContent(2, 4))));
    if(hkmu2R2->GetBinContent(1)>0.) hkmu2R2->SetBinError(1, ((hKM->GetBinContent(8, 1))/(hKM->GetBinContent(2, 1)))*sqrt(1./hKM->GetBinContent(8, 1) + 1./hKM->GetBinContent(2, 1)));
    if(hkmu2R2->GetBinContent(2)>0.) hkmu2R2->SetBinError(2, ((hKM->GetBinContent(8, 2))/(hKM->GetBinContent(2, 2)))*sqrt(1./hKM->GetBinContent(8, 2) + 1./hKM->GetBinContent(2, 2)));
    if(hkmu2R2->GetBinContent(3)>0.) hkmu2R2->SetBinError(3, ((hKM->GetBinContent(8, 3))/(hKM->GetBinContent(2, 3)))*sqrt(1./hKM->GetBinContent(8, 3) + 1./hKM->GetBinContent(2, 3)));
    if(hkmu2R2->GetBinContent(4)>0.) hkmu2R2->SetBinError(4, ((hKM->GetBinContent(8, 4))/(hKM->GetBinContent(2, 4)))*sqrt(1./hKM->GetBinContent(8, 4) + 1./hKM->GetBinContent(2, 4)));

    TH1D *hk2piR1 = new TH1D("hk2piR1", "hk2piR1", 4, 15., 35.);
    hk2piR1->SetBinContent(1, ((hKP->GetBinContent(6, 1)==0)?0:(hKP->GetBinContent(4, 1)/hKP->GetBinContent(6, 1))));
    hk2piR1->SetBinContent(2, ((hKP->GetBinContent(6, 2)==0)?0:(hKP->GetBinContent(4, 2)/hKP->GetBinContent(6, 2))));
    hk2piR1->SetBinContent(3, ((hKP->GetBinContent(6, 3)==0)?0:(hKP->GetBinContent(4, 3)/hKP->GetBinContent(6, 3))));
    hk2piR1->SetBinContent(4, ((hKP->GetBinContent(6, 4)==0)?0:(hKP->GetBinContent(4, 4)/hKP->GetBinContent(6, 4))));
    if(hk2piR1->GetBinContent(1)>0.) hk2piR1->SetBinError(1, ((hKP->GetBinContent(4, 1))/(hKP->GetBinContent(6, 1)))*sqrt(1./hKP->GetBinContent(4, 1) + 1./hKP->GetBinContent(6, 1)));
    if(hk2piR1->GetBinContent(2)>0.) hk2piR1->SetBinError(2, ((hKP->GetBinContent(4, 2))/(hKP->GetBinContent(6, 2)))*sqrt(1./hKP->GetBinContent(4, 2) + 1./hKP->GetBinContent(6, 2)));
    if(hk2piR1->GetBinContent(3)>0.) hk2piR1->SetBinError(3, ((hKP->GetBinContent(4, 3))/(hKP->GetBinContent(6, 3)))*sqrt(1./hKP->GetBinContent(4, 3) + 1./hKP->GetBinContent(6, 3)));
    if(hk2piR1->GetBinContent(4)>0.) hk2piR1->SetBinError(4, ((hKP->GetBinContent(4, 4))/(hKP->GetBinContent(6, 4)))*sqrt(1./hKP->GetBinContent(4, 4) + 1./hKP->GetBinContent(6, 4)));

    TH1D *hk2piR2 = new TH1D("hk2piR2", "hk2piR2", 4, 15., 35.);
    hk2piR2->SetBinContent(1, ((hKP->GetBinContent(6, 1)==0)?0:(hKP->GetBinContent(8, 1)/hKP->GetBinContent(6, 1))));
    hk2piR2->SetBinContent(2, ((hKP->GetBinContent(6, 2)==0)?0:(hKP->GetBinContent(8, 2)/hKP->GetBinContent(6, 2))));
    hk2piR2->SetBinContent(3, ((hKP->GetBinContent(6, 3)==0)?0:(hKP->GetBinContent(8, 3)/hKP->GetBinContent(6, 3))));
    hk2piR2->SetBinContent(4, ((hKP->GetBinContent(6, 4)==0)?0:(hKP->GetBinContent(8, 4)/hKP->GetBinContent(6, 4))));
    if(hk2piR2->GetBinContent(1)>0.) hk2piR2->SetBinError(1, ((hKP->GetBinContent(8, 1))/(hKP->GetBinContent(6, 1)))*sqrt(1./hKP->GetBinContent(8, 1) + 1./hKP->GetBinContent(6, 1)));
    if(hk2piR2->GetBinContent(2)>0.) hk2piR2->SetBinError(2, ((hKP->GetBinContent(8, 2))/(hKP->GetBinContent(6, 2)))*sqrt(1./hKP->GetBinContent(8, 2) + 1./hKP->GetBinContent(6, 2)));
    if(hk2piR2->GetBinContent(3)>0.) hk2piR2->SetBinError(3, ((hKP->GetBinContent(8, 3))/(hKP->GetBinContent(6, 3)))*sqrt(1./hKP->GetBinContent(8, 3) + 1./hKP->GetBinContent(6, 3)));
    if(hk2piR2->GetBinContent(4)>0.) hk2piR2->SetBinError(4, ((hKP->GetBinContent(8, 4))/(hKP->GetBinContent(6, 4)))*sqrt(1./hKP->GetBinContent(8, 4) + 1./hKP->GetBinContent(6, 4)));

    hk2piR1->SetLineColor(kRed);
    hk2piR1->SetLineWidth(4);
    hk2piR2->SetLineColor(kBlue);
    hk2piR2->SetLineWidth(4);
    hk2piR1->SetTitle(";#pi^{+} Momentum [GeV];Tail fraction / (5 GeV)");
    TCanvas *CanvasK2pi = new TCanvas("K2pi", "K2pi", 500, 500);
    CanvasK2pi->cd();
    CanvasK2pi->SetLeftMargin(0.13);
    hk2piR1->Draw("PE");
    hk2piR1->GetYaxis()->SetRangeUser(0., 0.0019);
    hk2piR1->GetXaxis()->SetTitleSize(0.04);
    hk2piR1->GetXaxis()->SetLabelSize(0.04);
    hk2piR1->GetYaxis()->SetTitleSize(0.04);
    hk2piR1->GetYaxis()->SetLabelSize(0.04);
    hk2piR2->Draw("PEsame");
    TLegend *legK2pi = new TLegend(0.75, 0.75, 0.88, 0.88);
    legK2pi->AddEntry(hk2piR1, "R1", "lep");
    legK2pi->AddEntry(hk2piR2, "R2", "lep");
    legK2pi->Draw("same");
    CanvasK2pi->Write();
    cout<<"Printing canvas to pdf."<<endl;
    CanvasK2pi->Print(fOutPDFfileName+"(", "pdf");
    cout<<"Canvas printed to pdf."<<endl;

    hkmu2R1->SetLineColor(kRed);
    hkmu2R1->SetLineWidth(4);
    hkmu2R2->SetLineColor(kBlue);
    hkmu2R2->SetLineWidth(4);
    hkmu2R1->SetTitle(";#mu^{+} Momentum [GeV];Tail fraction / (5 GeV)");
    TCanvas *CanvasKmu2 = new TCanvas("Kmu2", "Kmu2", 500, 500);
    CanvasKmu2->cd();
    CanvasKmu2->SetLeftMargin(0.13);
    hkmu2R1->Draw("PE");
    hkmu2R1->GetYaxis()->SetRangeUser(0., 0.00099);
    hkmu2R1->GetXaxis()->SetTitleSize(0.04);
    hkmu2R1->GetXaxis()->SetLabelSize(0.04);
    hkmu2R1->GetYaxis()->SetTitleSize(0.04);
    hkmu2R1->GetYaxis()->SetLabelSize(0.04);
    hkmu2R2->Draw("PEsame");
    TLegend *legKmu2 = new TLegend(0.75, 0.75, 0.88, 0.88);
    legKmu2->AddEntry(hkmu2R1, "R1", "lep");
    legKmu2->AddEntry(hkmu2R2, "R2", "lep");
    legKmu2->Draw("same");
    CanvasKmu2->Write();
    cout<<"Printing canvas to pdf."<<endl;
    CanvasKmu2->Print(fOutPDFfileName+")", "pdf");
    cout<<"Canvas printed to pdf."<<endl;

    int kmEvents = hKM->GetEntries();
    int kpEvents = hKP->GetEntries();

    //.10^-4
    ofstream myfile;
    myfile.open(fOutTableName+"_kmu2.text", ofstream::out);
    myfile<<"Kmu2  ("<<kmEvents<<" events)"<<"\n";
    myfile<<setw(10)<<"   "<<
      setw(14)<<"15-20GeV"<<
      setw(14)<<"20-25GeV"<<
      setw(14)<<"25-30GeV"<<
      setw(14)<<"30-35GeV"<<
      setw(14)<<"total"<<
      setw(14)<<"uncertainty"<<"\n";
    myfile<<"------------------------------------------------------------------------------------------------"<<"\n";
    myfile<<setw(10)<<" "<< //reg
      setw(14)<<((hKM->GetBinContent(1, 1))/(hKM->GetBinContent(2, 1)))*10000.<< //15-20GeV
      setw(14)<<(hKM->GetBinContent(1, 2))/(hKM->GetBinContent(2, 2))*10000.<< //20-25GeV
      setw(14)<<(hKM->GetBinContent(1, 3))/(hKM->GetBinContent(2, 3))*10000.<< //25-30GeV
      setw(14)<<(hKM->GetBinContent(1, 4))/(hKM->GetBinContent(2, 4))*10000.<< //30-35GeV
      setw(14)<<(hKM1->GetBinContent(1))/(hKM1->GetBinContent(2))*10000.<< //total 
      setw(14)<<(hKM1->GetBinContent(1))/(hKM1->GetBinContent(2))*10000.*sqrt((1./(hKM1->GetBinContent(1)))+(1./(hKM1->GetBinContent(2))))<<"\n"; //uncertainty
    
    myfile<<setw(10)<<"Kmu2"<<
      setw(14)<<((hKM->GetBinContent(2, 1))/(hKM->GetBinContent(2, 1)))*10000.<<
      setw(14)<<(hKM->GetBinContent(2, 2))/(hKM->GetBinContent(2, 2))*10000.<<
      setw(14)<<(hKM->GetBinContent(2, 3))/(hKM->GetBinContent(2, 3))*10000.<<
      setw(14)<<(hKM->GetBinContent(2, 4))/(hKM->GetBinContent(2, 4))*10000.<<
      setw(14)<<(hKM1->GetBinContent(2))/(hKM1->GetBinContent(2))*10000.<<
      setw(14)<<(hKM1->GetBinContent(2))/(hKM1->GetBinContent(2))*10000.*sqrt((1./(hKM1->GetBinContent(2)))+(1./(hKM1->GetBinContent(2))))<<"\n";
  
    myfile<<setw(10)<<"CR(Kmu2)"<<
      setw(14)<<((hKM->GetBinContent(3, 1))/(hKM->GetBinContent(2, 1)))*10000.<<
      setw(14)<<(hKM->GetBinContent(3, 2))/(hKM->GetBinContent(2, 2))*10000.<<
      setw(14)<<(hKM->GetBinContent(3, 3))/(hKM->GetBinContent(2, 3))*10000.<<
      setw(14)<<(hKM->GetBinContent(3, 4))/(hKM->GetBinContent(2, 4))*10000.<<
      setw(14)<<(hKM1->GetBinContent(3))/(hKM1->GetBinContent(2))*10000.<<
      setw(14)<<(hKM1->GetBinContent(3))/(hKM1->GetBinContent(2))*10000.*sqrt((1./(hKM1->GetBinContent(3)))+(1./(hKM1->GetBinContent(2))))<<"\n";

    myfile<<setw(10)<<"R1"<<
      setw(14)<<((hKM->GetBinContent(4, 1))/(hKM->GetBinContent(2, 1)))*10000.<<
      setw(14)<<(hKM->GetBinContent(4, 2))/(hKM->GetBinContent(2, 2))*10000.<<
      setw(14)<<(hKM->GetBinContent(4, 3))/(hKM->GetBinContent(2, 3))*10000.<<
      setw(14)<<(hKM->GetBinContent(4, 4))/(hKM->GetBinContent(2, 4))*10000.<<
      setw(14)<<(hKM1->GetBinContent(4))/(hKM1->GetBinContent(2))*10000.<<
      setw(14)<<(hKM1->GetBinContent(4))/(hKM1->GetBinContent(2))*10000.*sqrt((1./(hKM1->GetBinContent(4)))+(1./(hKM1->GetBinContent(2))))<<"\n";

    myfile<<setw(10)<<"CR1(K2pi)"<<
      setw(14)<<((hKM->GetBinContent(5, 1))/(hKM->GetBinContent(2, 1)))*10000.<<
      setw(14)<<(hKM->GetBinContent(5, 2))/(hKM->GetBinContent(2, 2))*10000.<<
      setw(14)<<(hKM->GetBinContent(5, 3))/(hKM->GetBinContent(2, 3))*10000.<<
      setw(14)<<(hKM->GetBinContent(5, 4))/(hKM->GetBinContent(2, 4))*10000.<<
      setw(14)<<(hKM1->GetBinContent(5))/(hKM1->GetBinContent(2))*10000.<<
      setw(14)<<(hKM1->GetBinContent(5))/(hKM1->GetBinContent(2))*10000.*sqrt((1./(hKM1->GetBinContent(5)))+(1./(hKM1->GetBinContent(2))))<<"\n";

    myfile<<setw(10)<<"K2pi"<<
      setw(14)<<((hKM->GetBinContent(6, 1))/(hKM->GetBinContent(2, 1)))*10000.<<
      setw(14)<<(hKM->GetBinContent(6, 2))/(hKM->GetBinContent(2, 2))*10000.<<
      setw(14)<<(hKM->GetBinContent(6, 3))/(hKM->GetBinContent(2, 3))*10000.<<
      setw(14)<<(hKM->GetBinContent(6, 4))/(hKM->GetBinContent(2, 4))*10000.<<
      setw(14)<<(hKM1->GetBinContent(6))/(hKM1->GetBinContent(2))*10000.<<
      setw(14)<<(hKM1->GetBinContent(6))/(hKM1->GetBinContent(2))*10000.*sqrt((1./(hKM1->GetBinContent(6)))+(1./(hKM1->GetBinContent(2))))<<"\n";
    
    myfile<<setw(10)<<"CR2(K2pi)"<<
      setw(14)<<((hKM->GetBinContent(7, 1))/(hKM->GetBinContent(2, 1)))*10000.<<
      setw(14)<<(hKM->GetBinContent(7, 2))/(hKM->GetBinContent(2, 2))*10000.<<
      setw(14)<<(hKM->GetBinContent(7, 3))/(hKM->GetBinContent(2, 3))*10000.<<
      setw(14)<<(hKM->GetBinContent(7, 4))/(hKM->GetBinContent(2, 4))*10000.<<
      setw(14)<<(hKM1->GetBinContent(7))/(hKM1->GetBinContent(2))*10000.<<
      setw(14)<<(hKM1->GetBinContent(7))/(hKM1->GetBinContent(2))*10000.*sqrt((1./(hKM1->GetBinContent(7)))+(1./(hKM1->GetBinContent(2))))<<"\n";

    myfile<<setw(10)<<"R2"<<
      setw(14)<<(hKM->GetBinContent(8, 1))/(hKM->GetBinContent(2, 1))*10000.<<
      setw(14)<<(hKM->GetBinContent(8, 2))/(hKM->GetBinContent(2, 2))*10000.<<
      setw(14)<<(hKM->GetBinContent(8, 3))/(hKM->GetBinContent(2, 3))*10000.<<
      setw(14)<<(hKM->GetBinContent(8, 4))/(hKM->GetBinContent(2, 4))*10000.<<
      setw(14)<<(hKM1->GetBinContent(8))/(hKM1->GetBinContent(2))*10000.<<
      setw(14)<<(hKM1->GetBinContent(8))/(hKM1->GetBinContent(2))*10000.*sqrt((1./(hKM1->GetBinContent(8)))+(1./(hKM1->GetBinContent(2))))<<"\n";

    myfile<<setw(10)<<"K3pi"<<
      setw(14)<<((hKM->GetBinContent(9, 1))/(hKM->GetBinContent(2, 1)))*10000.<<
      setw(14)<<(hKM->GetBinContent(9, 2))/(hKM->GetBinContent(2, 2))*10000.<<
      setw(14)<<(hKM->GetBinContent(9, 3))/(hKM->GetBinContent(2, 3))*10000.<<
      setw(14)<<(hKM->GetBinContent(9, 4))/(hKM->GetBinContent(2, 4))*10000.<<
      setw(14)<<(hKM1->GetBinContent(9))/(hKM1->GetBinContent(2))*10000.<<
      setw(14)<<(hKM1->GetBinContent(9))/(hKM1->GetBinContent(2))*10000.*sqrt((1./(hKM1->GetBinContent(9)))+(1./(hKM1->GetBinContent(2))))<<"\n";
    myfile<<"\n";
    myfile<<"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"<<"\n";
    myfile<<"\n";
    myfile.close();

    myfile.open(fOutTableName+"_k2pi.text", ofstream::out);
    myfile<<"K2pi  ("<<kpEvents<<" events)"<<"\n";
    myfile<<setw(10)<<"   "<<
      setw(14)<<"15-20GeV"<<
      setw(14)<<"20-25GeV"<<
      setw(14)<<"25-30GeV"<<
      setw(14)<<"30-35GeV"<<
      setw(14)<<"total"<<
      setw(14)<<"uncertainty"<<"\n";
    myfile<<"------------------------------------------------------------------------------------------------"<<"\n";
    myfile<<setw(10)<<" "<<
      setw(14)<<(hKP->GetBinContent(1, 1))/(hKP->GetBinContent(6, 1))*10000.<<
      setw(14)<<(hKP->GetBinContent(1, 2))/(hKP->GetBinContent(6, 2))*10000.<<
      setw(14)<<(hKP->GetBinContent(1, 3))/(hKP->GetBinContent(6, 3))*10000.<<
      setw(14)<<(hKP->GetBinContent(1, 4))/(hKP->GetBinContent(6, 4))*10000.<<
      setw(14)<<(hKP1->GetBinContent(1))/(hKP1->GetBinContent(6))*10000.<<
      setw(14)<<(hKP1->GetBinContent(1))/(hKP1->GetBinContent(6))*10000.*sqrt((1./(hKP1->GetBinContent(1)))+(1./(hKP1->GetBinContent(6))))<<"\n";
  
    myfile<<setw(10)<<"Kmu2"<<
      setw(14)<<(hKP->GetBinContent(2, 1))/(hKP->GetBinContent(6, 1))*10000.<<
      setw(14)<<(hKP->GetBinContent(2, 2))/(hKP->GetBinContent(6, 2))*10000.<<
      setw(14)<<(hKP->GetBinContent(2, 3))/(hKP->GetBinContent(6, 3))*10000.<<
      setw(14)<<(hKP->GetBinContent(2, 4))/(hKP->GetBinContent(6, 4))*10000.<<
      setw(14)<<(hKP1->GetBinContent(2))/(hKP1->GetBinContent(6))*10000.<<
      setw(14)<<(hKP1->GetBinContent(2))/(hKP1->GetBinContent(6))*10000.*sqrt((1./(hKP1->GetBinContent(2)))+(1./(hKP1->GetBinContent(6))))<<"\n";
  
    myfile<<setw(10)<<"CR(Kmu2)"<<
      setw(14)<<(hKP->GetBinContent(3, 1))/(hKP->GetBinContent(6, 1))*10000.<<
      setw(14)<<(hKP->GetBinContent(3, 2))/(hKP->GetBinContent(6, 2))*10000.<<
      setw(14)<<(hKP->GetBinContent(3, 3))/(hKP->GetBinContent(6, 3))*10000.<<
      setw(14)<<(hKP->GetBinContent(3, 4))/(hKP->GetBinContent(6, 4))*10000.<<
      setw(14)<<(hKP1->GetBinContent(3))/(hKP1->GetBinContent(6))*10000.<<
      setw(14)<<(hKP1->GetBinContent(3))/(hKP1->GetBinContent(6))*10000.*sqrt((1./(hKP1->GetBinContent(3)))+(1./(hKP1->GetBinContent(6))))<<"\n";
  
    myfile<<setw(10)<<"R1"<<
      setw(14)<<(hKP->GetBinContent(4, 1))/(hKP->GetBinContent(6, 1))*10000.<<
      setw(14)<<(hKP->GetBinContent(4, 2))/(hKP->GetBinContent(6, 2))*10000.<<
      setw(14)<<(hKP->GetBinContent(4, 3))/(hKP->GetBinContent(6, 3))*10000.<<
      setw(14)<<(hKP->GetBinContent(4, 4))/(hKP->GetBinContent(6, 4))*10000.<<
      setw(14)<<(hKP1->GetBinContent(4))/(hKP1->GetBinContent(6))*10000.<<
      setw(14)<<(hKP1->GetBinContent(4))/(hKP1->GetBinContent(6))*10000.*sqrt((1./(hKP1->GetBinContent(4)))+(1./(hKP1->GetBinContent(6))))<<"\n";

    myfile<<setw(10)<<"CR1(K2pi)"<<
      setw(14)<<(hKP->GetBinContent(5, 1))/(hKP->GetBinContent(6, 1))*10000.<<
      setw(14)<<(hKP->GetBinContent(5, 2))/(hKP->GetBinContent(6, 2))*10000.<<
      setw(14)<<(hKP->GetBinContent(5, 3))/(hKP->GetBinContent(6, 3))*10000.<<
      setw(14)<<(hKP->GetBinContent(5, 4))/(hKP->GetBinContent(6, 4))*10000.<<
      setw(14)<<(hKP1->GetBinContent(5))/(hKP1->GetBinContent(6))*10000.<<
      setw(14)<<(hKP1->GetBinContent(5))/(hKP1->GetBinContent(6))*10000.*sqrt((1./(hKP1->GetBinContent(5)))+(1./(hKP1->GetBinContent(6))))<<"\n";

    myfile<<setw(10)<<"K2pi"<<
      setw(14)<<(hKP->GetBinContent(6, 1))/(hKP->GetBinContent(6, 1))*10000.<<
      setw(14)<<(hKP->GetBinContent(6, 2))/(hKP->GetBinContent(6, 2))*10000.<<
      setw(14)<<(hKP->GetBinContent(6, 3))/(hKP->GetBinContent(6, 3))*10000.<<
      setw(14)<<(hKP->GetBinContent(6, 4))/(hKP->GetBinContent(6, 4))*10000.<<
      setw(14)<<(hKP1->GetBinContent(6))/(hKP1->GetBinContent(6))*10000.<<
      setw(14)<<(hKP1->GetBinContent(6))/(hKP1->GetBinContent(6))*10000.*sqrt((1./(hKP1->GetBinContent(6)))+(1./(hKP1->GetBinContent(6))))<<"\n";

    myfile<<setw(10)<<"CR2(K2pi)"<<
      setw(14)<<(hKP->GetBinContent(7, 1))/(hKP->GetBinContent(6, 1))*10000.<<
      setw(14)<<(hKP->GetBinContent(7, 2))/(hKP->GetBinContent(6, 2))*10000.<<
      setw(14)<<(hKP->GetBinContent(7, 3))/(hKP->GetBinContent(6, 3))*10000.<<
      setw(14)<<(hKP->GetBinContent(7, 4))/(hKP->GetBinContent(6, 4))*10000.<<
      setw(14)<<(hKP1->GetBinContent(7))/(hKP1->GetBinContent(6))*10000.<<
      setw(14)<<(hKP1->GetBinContent(7))/(hKP1->GetBinContent(6))*10000.*sqrt((1./(hKP1->GetBinContent(7)))+(1./(hKP1->GetBinContent(6))))<<"\n";

    myfile<<setw(10)<<"R2"<<
      setw(14)<<(hKP->GetBinContent(8, 1))/(hKP->GetBinContent(6, 1))*10000.<<
      setw(14)<<(hKP->GetBinContent(8, 2))/(hKP->GetBinContent(6, 2))*10000.<<
      setw(14)<<(hKP->GetBinContent(8, 3))/(hKP->GetBinContent(6, 3))*10000.<<
      setw(14)<<(hKP->GetBinContent(8, 4))/(hKP->GetBinContent(6, 4))*10000.<<
      setw(14)<<(hKP1->GetBinContent(8))/(hKP1->GetBinContent(6))*10000.<<
      setw(14)<<(hKP1->GetBinContent(8))/(hKP1->GetBinContent(6))*10000.*sqrt((1./(hKP1->GetBinContent(8)))+(1./(hKP1->GetBinContent(6))))<<"\n";

    myfile<<setw(10)<<"K3pi"<<
      setw(14)<<(hKP->GetBinContent(9, 1))/(hKP->GetBinContent(6, 1))*10000.<<
      setw(14)<<(hKP->GetBinContent(9, 2))/(hKP->GetBinContent(6, 2))*10000.<<
      setw(14)<<(hKP->GetBinContent(9, 3))/(hKP->GetBinContent(6, 3))*10000.<<
      setw(14)<<(hKP->GetBinContent(9, 4))/(hKP->GetBinContent(6, 4))*10000.<<
      setw(14)<<(hKP1->GetBinContent(9))/(hKP1->GetBinContent(6))*10000.<<
      setw(14)<<(hKP1->GetBinContent(9))/(hKP1->GetBinContent(6))*10000.*sqrt((1./(hKP1->GetBinContent(9)))+(1./(hKP1->GetBinContent(6))))<<"\n";
    myfile<<"\n";
    myfile<<"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"<<"\n";
    myfile<<"\n";
    myfile.close();

    cout<<"end"<<endl;
  }else{
    SaveAllPlots();
  };
  gErrorIgnoreLevel = -1; // suppress messages generated for each page printed
}

void KinematicTails::DrawPlot(){
}

KinematicTails::~KinematicTails(){
}

void KinematicTails::PrepareOutputs(){
  fWhichDecay = -1;
  fTrackID = -1;
  fKaonID = -1;
  fMissM2 = 9999.;
  fMissM2beam = 9999.;
  fMissM2rich = 9999.;
  TVector3 defv(0., 0., 0.);
  fVertex = defv;
  TLorentzVector deflv;
  deflv.SetVectM(defv, 0.);
  fTrack = deflv;
  fKaon = deflv;
  SetOutputState("WhichDecay", kOInvalid);
  SetOutputState("TrackID", kOInvalid);
  SetOutputState("KaonID", kOInvalid);
  SetOutputState("Track4Momentum", kOInvalid);
  SetOutputState("Kaon4Momentum", kOInvalid);
  SetOutputState("Vertex", kOInvalid);
  SetOutputState("MissM2", kOInvalid);
  SetOutputState("MissM2beam", kOInvalid);
  SetOutputState("MissM2rich", kOInvalid);
}

void KinematicTails::ValidateOutputs(){
  SetOutputState("WhichDecay", kOValid);
  SetOutputState("TrackID", kOValid);
  SetOutputState("KaonID", kOValid);
  SetOutputState("Track4Momentum", kOValid);
  SetOutputState("Kaon4Momentum", kOValid);
  SetOutputState("Vertex", kOValid);
  SetOutputState("MissM2", kOValid);
  SetOutputState("MissM2beam", kOValid);
  SetOutputState("MissM2rich", kOValid);
}

void KinematicTails::Kmu2(){
  if(verb){
    cout<<endl;
    cout<<"----Kmu2----"<<endl;
  };
  
  OutputState state;
  auto kmu2Selected = *(bool*)GetOutput("Kmu2.Kmu2EventSelected", state);
  auto v =
    *(std::vector<TVector3>*)GetOutput("BestTrackSelection.GTKAssocVertex", state);
  auto vNom =
    *(std::vector<TVector3>*)GetOutput("BestTrackSelection.NomVertex", state);
  auto kaonMom =
    *(TVector3*)GetOutput("Kmu2.Kmu2KaonMomentum", state);
  auto kaonNomMom =
    *(TVector3*)GetOutput("Kmu2.Kmu2NomKaonMomentum", state);
  auto quality1 =
    *(std::vector<double>*)GetOutput("BestTrackSelection.GTKAssocQuality1", state);
  auto kaonID =
    *(int*)GetOutput("Kmu2.Kmu2KaonID", state);
  auto trackID =
    *(int*)GetOutput("Kmu2.Kmu2TrackID", state);
  auto trackMom =
    *(TVector3*)GetOutput("Kmu2.Kmu2TrackMomentum", state);
  auto trackNomMom =
    *(TVector3*)GetOutput("Kmu2.Kmu2NomTrackMomentum", state);
  auto kmu2RICHMom = *(double*)GetOutput("Kmu2.Kmu2RICHMomentum", state);
  if(state!=kOValid) return;
  if(verb) cout<<"Is Kmu2 selected? "<<kmu2Selected<<endl;
  if(!kmu2Selected) return;
  fTrackID = trackID;
  fKaonID = kaonID;
  fWhichDecay = 2;

  TVector3 vertex;
  if(!UseGTK){
    vertex = vNom.at(trackID);
  }else{
    vertex = v.at(trackID);
  };
  fVertex = vertex;

  TLorentzVector kaon;
  if(!UseGTK){
    kaon.SetVectM(kaonNomMom, MKCH);
  }else{
    kaon.SetVectM(kaonMom, MKCH);
  };
  fKaon = kaon;

  TLorentzVector track;
  if(!UseGTK){
    track.SetVectM(trackNomMom, MPI);
  }else{
    track.SetVectM(trackMom, MPI);
  };
  fTrack = track;

  TLorentzVector trackRICH;
  if(!UseGTK){
    trackRICH.SetVectM(trackNomMom*(kmu2RICHMom/trackNomMom.Mag()), MPI);
  }else{
    trackRICH.SetVectM(trackMom*(kmu2RICHMom/trackMom.Mag()), MPI);
  };

  double missm2 = (kaon-track).M2()/1000000.;
  double missm2beam = (fKaonNom-track).M2()/1000000.;
  double missm2rich = (kaon-trackRICH).M2()/1000000.;
  fMissM2 = missm2;
  fMissM2beam = missm2beam;
  fMissM2rich = missm2rich;

  FillHisto(kmu2+all+"hMissM2", missm2);
  FillHisto(kmu2+all+"hMissM2_R", missm2);
  FillHisto(kmu2+all+"hMissM2beam", missm2beam);
  FillHisto(kmu2+all+"hMissM2rich", missm2rich);
  FillHisto(kmu2+all+"hMissM2richVsMissM2", missm2, missm2rich);
  FillHisto(kmu2+all+"hMissM2beamVsMissM2", missm2, missm2beam);
  FillHisto(kmu2+all+"hMissM2richVsMissM2beam", missm2beam, missm2rich);
  FillHistoRegions(kmu2+all, "hMissM2Reg", "hMissM2MomVsReg", missm2, missm2beam, missm2rich, trackMom.Mag()/1000., 2);
  FillHisto(kmu2+all+"hMissM2vsTrackMom", trackMom.Mag()/1000., missm2);
  FillHisto(kmu2+all+"hGTKMatchingQuality1", quality1.at(0));

  if(verb) cout<<"MissM2 = "<<missm2<<endl;
  if(verb) cout<<"MissM2rich = "<<missm2rich<<endl;
  if(verb) cout<<"MissM2beam = "<<missm2beam<<endl;
  if(verb) cout<<"Track momentum = "<<trackMom.Mag()<<endl;
}

void KinematicTails::K2pi(){
  if(verb){
    cout<<endl;
    cout<<"----K2pi----"<<endl;
  };

  OutputState state;
  auto k2piSelected = *(bool*)GetOutput("K2pi.K2piEventSelected", state);
  auto v =
    *(std::vector<TVector3>*)GetOutput("BestTrackSelection.GTKAssocVertex", state);
  auto vNom =
    *(std::vector<TVector3>*)GetOutput("BestTrackSelection.NomVertex", state);
  auto quality1 =
    *(std::vector<double>*)GetOutput("BestTrackSelection.GTKAssocQuality1", state);
  auto kaonMom =
    *(TVector3*)GetOutput("K2pi.K2piKaonMomentum", state);
  auto kaonNomMom =
    *(TVector3*)GetOutput("K2pi.K2piNomKaonMomentum", state);
  auto kaonID =
    *(int*)GetOutput("K2pi.K2piKaonID", state);
  auto trackID =
    *(int*)GetOutput("K2pi.K2piTrackID", state);
  auto trackMom =
    *(TVector3*)GetOutput("K2pi.K2piTrackMomentum", state);
  auto trackNomMom =
    *(TVector3*)GetOutput("K2pi.K2piNomTrackMomentum", state);
  auto k2piRICHMom = *(double*)GetOutput("K2pi.K2piRICHMomentum", state);
  if(state!=kOValid) return;
  if(verb) cout<<"Is K2pi selected? "<<k2piSelected<<endl;
  if(!k2piSelected) return;
  fTrackID = trackID;
  fKaonID = kaonID;
  fWhichDecay = 1;

  TVector3 vertex;
  if(!UseGTK){
    vertex = vNom.at(trackID);
  }else{
    vertex = v.at(trackID);
  };
  fVertex = vertex;

  TLorentzVector kaon;
  if(!UseGTK){
    kaon.SetVectM(kaonNomMom, MKCH);
  }else{
    kaon.SetVectM(kaonMom, MKCH);
  };
  fKaon = kaon;

  TLorentzVector track;
  if(!UseGTK){
    track.SetVectM(trackNomMom, MPI);
  }else{
    track.SetVectM(trackMom, MPI);
  };
  fTrack = track;

  TLorentzVector trackRICH;
  if(!UseGTK){
    trackRICH.SetVectM(trackNomMom*(k2piRICHMom/trackNomMom.Mag()), MPI);
  }else{
    trackRICH.SetVectM(trackMom*(k2piRICHMom/trackMom.Mag()), MPI);
  };

  double missm2 = (kaon-track).M2()/1000000.;
  double missm2beam = (fKaonNom-track).M2()/1000000.;
  double missm2rich = (kaon-trackRICH).M2()/1000000.;
  fMissM2 = missm2;
  fMissM2beam = missm2beam;
  fMissM2rich = missm2rich;

  FillHisto(k2pi+all+"hMissM2", missm2);
  FillHisto(k2pi+all+"hMissM2_R", missm2);
  FillHisto(k2pi+all+"hMissM2beam", missm2beam);
  FillHisto(k2pi+all+"hMissM2rich", missm2rich);
  FillHisto(k2pi+all+"hMissM2richVsMissM2", missm2, missm2rich);
  FillHisto(k2pi+all+"hMissM2beamVsMissM2", missm2, missm2beam);
  FillHisto(k2pi+all+"hMissM2richVsMissM2beam", missm2beam, missm2rich);
  FillHistoRegions(k2pi+all, "hMissM2Reg", "hMissM2MomVsReg", missm2, missm2beam, missm2rich, trackMom.Mag()/1000., 1);
  FillHisto(k2pi+all+"hMissM2vsTrackMom", trackMom.Mag()/1000., missm2);
  FillHisto(k2pi+all+"hGTKMatchingQuality1", quality1.at(0));

  if(verb) cout<<"MissM2 = "<<missm2<<endl;
  if(verb) cout<<"MissM2rich = "<<missm2rich<<endl;
  if(verb) cout<<"MissM2beam = "<<missm2beam<<endl;
  if(verb) cout<<"Track momentum = "<<trackMom.Mag()<<endl;
}

void KinematicTails::FillHistoRegions(TString decString, string h_1, string h, double missM2, double missM2beam, double missM2rich, double momPiplus, int decay){
  double mm = (MPI*MPI/1e6 - MMU*MMU/1e6)*(1.-75./momPiplus);
  double sigma = 0.001;
  //decay = 1 k2pi
  //decay = 2 kmu2

  if(missM2<=fKmu2_low){
    FillHisto(decString+h_1, 1);
  }else if((missM2>fKmu2_low) && (missM2<=mm+3.*sigma)){
    FillHisto(decString+h_1, 2);
  }else if((missM2>mm+3*sigma) && (missM2<=fCRKmu2_up)){
    FillHisto(decString+h_1, 3);
  }else if((missM2>fSR1_low) && (missM2<=fSR1_up)){
    if(decay==2){
      if(((momPiplus<=20.) && (missM2beam>fSR1beamlowp_low) && (missM2beam<=fSR1beamlowp_up)) || 
	 ((momPiplus>20. && momPiplus<25.) && (missM2beam>fSR1beammiddlep_low) && (missM2beam<=fSR1beammiddlep_up)) || 
	 ((momPiplus>=25.) && (missM2beam>fSR1beamhighp_low) && (missM2beam<=fSR1beamhighp_up))){
	FillHisto(decString+h_1, 4);
      };
    }else if(decay==1){
      if(((momPiplus<=20.) && (missM2beam>fSR1beamlowp_low) && (missM2beam<=fSR1beamlowp_up) && (missM2rich>fSR1richlowp_low) && (missM2rich<=fSR1richlowp_up)) || 
	 ((momPiplus>20. && momPiplus<25.) && (missM2beam>fSR1beammiddlep_low) && (missM2beam<=fSR1beammiddlep_up) && (missM2rich>fSR1richmiddlep_low) && (missM2rich<=fSR1richmiddlep_up)) || 
	 ((momPiplus>=25.) && (missM2beam>fSR1beamhighp_low) && (missM2beam<=fSR1beamhighp_up) && (missM2rich>fSR1richhighp_low) && (missM2rich<=fSR1richhighp_up))){
	FillHisto(decString+h_1, 4);
      };
    };
  }else if((missM2>fCR1_low) && (missM2<=fCR1_up)){
    FillHisto(decString+h_1, 5);
  }else if((missM2>fPiPi_low) && (missM2<=fPiPi_up)){
    if(decay==2){
      FillHisto(decString+h_1, 6);
    }else if(decay==1){
      if((missM2rich>fPiPirich_low) && (missM2rich<=fPiPirich_up)){
	FillHisto(decString+h_1, 6);
      };
    };
  }else if((missM2>fCR2_low) && (missM2<=fCR2_up)){
    FillHisto(decString+h_1, 7);
  }else if((missM2>fSR2_low) && (missM2<=fSR2_up) && (missM2beam>fSR2beam_low) && (missM2beam<=fSR2beam_up)){
    if(decay==2){
      FillHisto(decString+h_1, 8);
    }else if(decay==1){
      if((missM2rich>fSR2rich_low) && (missM2rich<=fSR2rich_up)){
	FillHisto(decString+h_1, 8);
      };
    };
  }else if(missM2>fK3pi_low){
    FillHisto(decString+h_1, 9);
  };
  
  if(missM2<=fKmu2_low){
    FillHisto(decString+h, 1, momPiplus);
  }else if((missM2>fKmu2_low) && (missM2<=mm+3*sigma)){
    FillHisto(decString+h, 2, momPiplus);
  }else if((missM2>mm+3*sigma) && (missM2<=fCRKmu2_up)){
    FillHisto(decString+h, 3, momPiplus);
  }else if((missM2>fSR1_low) && (missM2<=fSR1_up)){
    if((momPiplus<=20.) && (missM2beam>fSR1beamlowp_low) && (missM2beam<=fSR1beamlowp_up)){
      if(decay==2){
	FillHisto(decString+h, 4, momPiplus);
      }else if(decay==1){
	if((missM2rich>fSR1richlowp_low) && (missM2rich<=fSR1richlowp_up)){
	  FillHisto(decString+h, 4, momPiplus);
	};
      };
    }else if((momPiplus>20. && momPiplus<25.) && (missM2beam>fSR1beammiddlep_low) && (missM2beam<=fSR1beammiddlep_up)){
      if(decay==2){
	FillHisto(decString+h, 4, momPiplus);
      }else if(decay==1){
	if((missM2rich>fSR1richmiddlep_low) && (missM2rich<=fSR1richmiddlep_up)){
	  FillHisto(decString+h, 4, momPiplus);
	};
      };  
    }else if((momPiplus>=25.) && (missM2beam>fSR1beamhighp_low) && (missM2beam<=fSR1beamhighp_up)){
      if(decay==2){
	FillHisto(decString+h, 4, momPiplus);
      }else if(decay==1){
	if((missM2rich>fSR1richhighp_low) && (missM2rich<=fSR1richhighp_up)){
	  FillHisto(decString+h, 4, momPiplus);
	};
      };
    };
  }else if((missM2>fCR1_low) && (missM2<=fCR1_up)){
    FillHisto(decString+h, 5, momPiplus);
  }else if((missM2>fPiPi_low) && (missM2<=fPiPi_up)){
    FillHisto(decString+h, 6, momPiplus);
  }else if((missM2>fCR2_low) && (missM2<=fCR2_up)){
    FillHisto(decString+h, 7, momPiplus);
  }else if((missM2>fSR2_low) && (missM2<=fSR2_up) && (missM2beam>fSR2beam_low) && (missM2beam<=fSR2beam_up)){
    if(decay==2){
      FillHisto(decString+h, 8, momPiplus);
    }else if(decay==1){
      if((missM2rich>fSR2rich_low) && (missM2rich<=fSR2rich_up)){
	FillHisto(decString+h, 8, momPiplus);
      };
    };
  }else if(missM2>fK3pi_low){
    FillHisto(decString+h, 9, momPiplus);
  };
}

int KinematicTails::WhichRegion(double missM2, double missM2beam, double missM2rich, double momPiplus, int decay){
  double mm = (MPI*MPI/1e6 - MMU*MMU/1e6)*(1.-75./momPiplus);
  double sigma = 0.001;
  //decay = 1 k2pi
  //decay = 2 kmu2
 
  if(missM2<=fKmu2_low){
    return 1;
  }else if((missM2<=mm+3*sigma)){
    return 2;
  }else if((missM2>mm+3*sigma) && (missM2<=fCRKmu2_up)){
    return 3;
  }else if((missM2>fSR1_low) && (missM2<=fSR1_up)){
    if(decay==2){
      if(((momPiplus<=20.) && (missM2beam>fSR1beamlowp_low) && (missM2beam<=fSR1beamlowp_up)) || 
	 ((momPiplus>20. && momPiplus<25.) && (missM2beam>fSR1beammiddlep_low) && (missM2beam<=fSR1beammiddlep_up)) || 
	 ((momPiplus>=25.) && (missM2beam>fSR1beamhighp_low) && (missM2beam<=fSR1beamhighp_up))){
	return 4;
      };
    }else if(decay==1){
      if(((momPiplus<=20.) && (missM2beam>fSR1beamlowp_low) && (missM2beam<=fSR1beamlowp_up) && (missM2rich>fSR1richlowp_low) && (missM2rich<=fSR1richlowp_up)) || 
	 ((momPiplus>20. && momPiplus<25.) && (missM2beam>fSR1beammiddlep_low) && (missM2beam<=fSR1beammiddlep_up) && (missM2rich>fSR1richmiddlep_low) && (missM2rich<=fSR1richmiddlep_up)) || 
	 ((momPiplus>=25.) && (missM2beam>fSR1beamhighp_low) && (missM2beam<=fSR1beamhighp_up) && (missM2rich>fSR1richhighp_low) && (missM2rich<=fSR1richhighp_up))){
	return 4;
      };
    };
  }else if((missM2>fCR1_low) && (missM2<=fCR1_up)){
    return 5;
  }else if((missM2>fPiPi_low) && (missM2<=fPiPi_up)){
    if(decay==2){
      return 6;
    }else if(decay==1){
      if((missM2rich>fPiPirich_low) && (missM2rich<=fPiPirich_up)){
	return 6;
      };
    };
  }else if((missM2>fCR2_low) && (missM2<=fCR2_up)){
    return 7;
  }else if((missM2>fSR2_low) && (missM2<=fSR2_up) && (missM2beam>fSR2beam_low) && (missM2beam<=fSR2beam_up)){
    if(decay==2){
      return 8;
    }else if(decay==1){
      if((missM2rich>fSR2rich_low) && (missM2rich<=fSR2rich_up)){
	return 8;
      };
    };
  }else if(missM2>fK3pi_low){
    return 9;
  };
  return -1;
}
