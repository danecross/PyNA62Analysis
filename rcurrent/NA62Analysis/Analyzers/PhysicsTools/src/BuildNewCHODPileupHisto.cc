// ---------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2018-05-03
//
// ---------------------------------------------------------

/// \class BuildNewCHODPileupHisto
/// \Brief
/// Analyzer that builds hit pileup histograms for the NewCHOD.
/// Used for L0NewCHODEmulator, can also be used to check acceptance in presence of pileup.
/// \EndBrief
/// \Detailed
/// In 'data' stage the analyzer produces a 2D histogram with the number of hits in the NewCHOD
/// as a function of the instantaneous beam intensity measured by the GTK.
/// The hits are counted in 4ns windows. The small window is needed to properly reproduce the
/// time-structure of hits that is seen in the NewCHOD, see documentation of L0NewCHODEmulator.
/// Seven samples of 4ns are taken from each event, from the negative-time sideband where the
/// hits are only from pileup.
///
/// In addition to the number of hits, the TileID of the hits is stored for tight and loose hits.
/// This information is needed to add pileup to MC events in a way that is meaningful when
/// extracting the event acceptance.
///
/// Furthermore, the QuadrantID of the hits is stored for tight and loose hits (this time in
/// a single histogram). This is used to include pileup hits in L0NewCHODEmulator, where the
/// tile number is irrelevant and only the QuadrantID is needed.
///
/// Running the analyzer in 'histo' mode will convert the histograms into cumulative PDFs.
/// This feature is exploited in L0NewCHODEmulator.
///
/// \author Chris Parkinson (chris.parkinson@cern.ch)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "BuildNewCHODPileupHisto.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

BuildNewCHODPileupHisto::BuildNewCHODPileupHisto(Core::BaseAnalysis *ba) : Analyzer(ba, "BuildNewCHODPileupHisto")
{
  RequestL0Data();
  RequestBeamData();
  RequestTree("NewCHOD", new TRecoNewCHODEvent, "Reco");
  fMyHist = nullptr;
}

void BuildNewCHODPileupHisto::InitOutput(){}

void BuildNewCHODPileupHisto::InitHist(){

  if(GetIsTree()){
    BookHisto(new TH1F("HitTT","NewCHODTT", 251, -100.5, 150.5));

    BookHisto(new TH1F("HitTight","HitTight", 400, 100.5, 500.5));
    BookHisto(new TH1F("HitLoose","HitLoose", 400, 100.5, 500.5));

    BookHisto(new TH1F("Quadrant","Quadrant", 8, -0.5, 7.5));

    Int_t nbins = 162;
    Double_t binsize = 1200.0/double(nbins);
    Double_t halfbin = binsize/2.0;

    BookHisto(new TH1F("NewCHOD","NewCHOD", 101, -0.5, 100.5));
    BookHisto(new TH2F("NewCHOD2D","NewCHOD", nbins+1, -halfbin, 1200+halfbin, 101, -0.5, 100.5));

    BookHisto(new TH1F("NBin","NBin", 11, -0.5,10.5));
  }
  else{
    fMyHist  = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "NewCHOD2D", true));
    fMyHistA = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "HitTight", true));
    fMyHistB = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "HitLoose", true));
    fMyHistC = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "Quadrant", true));
  }
}

void BuildNewCHODPileupHisto::DefineMCSimple(){}

void BuildNewCHODPileupHisto::StartOfRunUser(){}

void BuildNewCHODPileupHisto::StartOfBurstUser(){}

void BuildNewCHODPileupHisto::ProcessSpecialTriggerUser(int, unsigned int){}

void BuildNewCHODPileupHisto::Process(int){
  if(!GetIsTree()) return;
  if(!GetWithMC()) return; // only run on data

  Bool_t ControlData = (GetL0Data()->GetDataType() & 0x10);
  if(!ControlData) return;
  Int_t PrimID = GetL0Data()->GetPrimitive(kL0TriggerSlot, kL0CHOD).GetPrimitiveID();
  if(!PrimID) return;

  Int_t Ref = GetL0Data()->GetPrimitive(kL0TriggerSlot, kL0CHOD).GetFineTime();
  Double_t RefTime = Ref*TdcCalib;

  Int_t nbins = 7;
  Int_t hits[7]={0,0,0,0,0,0,0};

  // Get the instantaneous beam intensity
  Double_t fIntensity = GetBeamData()->GetInstantaneousIntensity();

  TRecoNewCHODEvent* event = GetEvent<TRecoNewCHODEvent>("Reco");
  Int_t nhits = event->GetNHits();
  for(Int_t i=0; i<nhits; ++i){
    TRecoNewCHODHit* hit = static_cast<TRecoNewCHODHit*>(event->GetHit(i));
    Double_t time = hit->GetTime()-RefTime;
    if(fabs(time)>10.0) FillHisto("HitTT", time);

    // using the flat part of the low timing sideband
    time += 74.0;
    if(time>28.0 || time<0.0) continue;

    //fill hit histograms
    if(hit->GetType()==kTightCandidate){
      FillHisto("HitTight", hit->GetTileID());
      FillHisto("Quadrant", hit->GetQuadrantID()-1);
    }
    else{
      FillHisto("HitLoose", hit->GetTileID());
      FillHisto("Quadrant", hit->GetQuadrantID()-1+4);
    }

    int bin = time/4; // 28/4 = 7 (i.e. seven bins of 4ns between -74 and -46)
    FillHisto("NBin", bin);
    if( !(bin<0) && bin<nbins) hits[bin]++;
  }

  for(int j=0;j<nbins;++j){
    FillHisto("NewCHOD", hits[j]);
    FillHisto("NewCHOD2D", fIntensity, hits[j]);
  }
}

void BuildNewCHODPileupHisto::PostProcess(){}

void BuildNewCHODPileupHisto::EndOfBurstUser(){}

void BuildNewCHODPileupHisto::EndOfRunUser(){}

void BuildNewCHODPileupHisto::EndOfJobUser(){

  SaveAllPlots();
  if(GetIsTree()) return ;

  // build cumulative pdf for hit tile and type
  TH1F* histA = new TH1F("CumulativePDFTightHits", "CumulativePDFTightHits", 400, 100.5, 500.5);
  TH1F* histB = new TH1F("CumulativePDFLooseHits", "CumulativePDFLooseHits", 400, 100.5, 500.5);

  //total number of hits
  Double_t nHits = fMyHistC->GetEntries();

  Double_t count=0;

  // fraction of tight hits in each tile
  for(int x=1; x<401; ++x){
    count += fMyHistA->GetBinContent(x);
    histA->SetBinContent(x, count/nHits);
  }

  // fraction of loose hits in each tile
  for(int x=1; x<401; ++x){
    count += fMyHistB->GetBinContent(x);
    histB->SetBinContent(x, count/nHits);
  }

  // build cumulative pdf for quadrants
  TH1F* histC = new TH1F("CumulativePDFQuadrant", "CumulativePDFQuadrant", 8, -0.5, 7.5);

  count=0;
  // fraction of tight/loose hits in each quadrant
  for(int x=1; x<9; ++x){
    count += fMyHistC->GetBinContent(x);
    histC->SetBinContent(x, count/nHits);
  }

  // build cumulative pdf for number of hits
  TH2F* hist = new TH2F("CumulativePDFCount", "CumulativePDFCount", 121, -5, 1205, 101, -0.5, 100.5);
  for(int x=1; x<122; ++x){ // intensity bins

    Double_t total = 0.0;
    for(int y=1; y<102; ++y){ // n hits
      total += fMyHist->GetBinContent(x,y);
    }

    Double_t cont=0;
    for(int y=1; y<102; ++y){ // n hits
      cont += fMyHist->GetBinContent(x,y);
      hist->SetBinContent(x,y, cont/total);
    }
  }

  hist->Write();
  histA->Write();
  histB->Write();
  histC->Write();
}

void BuildNewCHODPileupHisto::DrawPlot(){}

BuildNewCHODPileupHisto::~BuildNewCHODPileupHisto(){}
