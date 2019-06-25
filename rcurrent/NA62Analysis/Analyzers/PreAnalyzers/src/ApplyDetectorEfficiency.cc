#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "ApplyDetectorEfficiency.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

#include "TRecoNewCHODHit.hh"

ApplyDetectorEfficiency::ApplyDetectorEfficiency(Core::BaseAnalysis *ba) : Analyzer(ba, "ApplyDetectorEfficiency")
{
  RequestTree("MUV3", new TRecoMUV3Event, "Reco");
  RequestTree("NewCHOD", new TRecoNewCHODEvent, "Reco");
  RequestTree("Spectrometer", new TRecoSpectrometerEvent, "Reco");

  gRandom->SetSeed(19876543);
  fFile = nullptr;
  fFile = new TFile("./DetEfficiency.root","READ");
  if(!fFile) std::cout << user_normal() << "WARNING CANNOT APPLY DETECTOR EFFICIENCY." << std::endl;

  // MUV3 detector efficiency
  TH2F* hMUV32DNum=nullptr;
  TH2F* hMUV32DDen=nullptr;  
  fFile->GetObject("MUV3Efficiency/hMatched",hMUV32DNum);
  fFile->GetObject("MUV3Efficiency/hExpected",hMUV32DDen);
  TH1D* hMUV31DNum = hMUV32DNum->ProjectionY();
  TH1D* hMUV31DDen = hMUV32DDen->ProjectionY();

  hMUV3Efficiency = nullptr;
  hMUV3Efficiency = new TEfficiency(*hMUV31DNum, *hMUV31DDen);

  // NewCHOD detector efficiency
  TH2F* hNewC2DNum=nullptr;
  TH2F* hNewC2DDen=nullptr;  
  fFile->GetObject("NewCHODEfficiency/hMatched",hNewC2DNum);
  fFile->GetObject("NewCHODEfficiency/hExpected",hNewC2DDen);
  TH1D* hNewC1DNum = hNewC2DNum->ProjectionY();
  TH1D* hNewC1DDen = hNewC2DDen->ProjectionY();

  hNewCEfficiency = nullptr;
  hNewCEfficiency = new TEfficiency(*hNewC1DNum, *hNewC1DDen);

  // Track reconstruction efficiency
  fSTRAWFileData = nullptr; 
  fSTRAWFileData = new TFile("./RecoEffData-2.root","READ");
  if(!fSTRAWFileData) std::cout << user_normal() << "Missing fSTRAWFileData " << std::endl;

  fSTRAWDataPosNum = nullptr;
  fSTRAWFileData->GetObject("ThreeTrackRecoEfficiency/PositiveTrackNum_PI", fSTRAWDataPosNum);
  if(!fSTRAWDataPosNum) std::cout << user_normal() << "Missing fSTRAWDataPosNum " << std::endl;

  fSTRAWDataPosDen = nullptr;
  fSTRAWFileData->GetObject("ThreeTrackRecoEfficiency/PositiveTrackDen_PI", fSTRAWDataPosDen);
  if(!fSTRAWDataPosDen) std::cout << user_normal() << "Missing fSTRAWDataPosDen " << std::endl;

  fSTRAWDataNegNum = nullptr;
  fSTRAWFileData->GetObject("ThreeTrackRecoEfficiency/NegativeTrackNum_PI", fSTRAWDataNegNum);
  if(!fSTRAWDataNegNum) std::cout << user_normal() << "Missing fSTRAWDataNegNum " << std::endl;

  fSTRAWDataNegDen = nullptr;
  fSTRAWFileData->GetObject("ThreeTrackRecoEfficiency/NegativeTrackDen_PI", fSTRAWDataNegDen);
  if(!fSTRAWDataNegDen) std::cout << user_normal() << "Missing fSTRAWDataNegDen " << std::endl;

  fSTRAWFileMC = nullptr; 
  fSTRAWFileMC = new TFile("./RecoEffMC-2.root","READ");
  if(!fSTRAWFileMC) std::cout << user_normal() << "Missing fSTRAWFileMC " << std::endl;

  fSTRAWMCPosNum = nullptr;
  fSTRAWFileMC->GetObject("ThreeTrackRecoEfficiency/PositiveTrackNum_PI", fSTRAWMCPosNum);
  if(!fSTRAWMCPosNum) std::cout << user_normal() << "Missing fSTRAWMCPosNum " << std::endl;

  fSTRAWMCPosDen = nullptr;
  fSTRAWFileMC->GetObject("ThreeTrackRecoEfficiency/PositiveTrackDen_PI", fSTRAWMCPosDen);
  if(!fSTRAWMCPosDen) std::cout << user_normal() << "Missing fSTRAWMCPosDen " << std::endl;

  fSTRAWMCNegNum = nullptr;
  fSTRAWFileMC->GetObject("ThreeTrackRecoEfficiency/NegativeTrackNum_PI", fSTRAWMCNegNum);
  if(!fSTRAWMCNegNum) std::cout << user_normal() << "Missing fSTRAWMCNegNum " << std::endl;

  fSTRAWMCNegDen = nullptr;
  fSTRAWFileMC->GetObject("ThreeTrackRecoEfficiency/NegativeTrackDen_PI", fSTRAWMCNegDen);
  if(!fSTRAWMCNegDen) std::cout << user_normal() << "Missing fSTRAWMCNegDen " << std::endl;

  AddParam("Intensity",&fIntensity, 0.0);
}

void ApplyDetectorEfficiency::InitOutput(){}

void ApplyDetectorEfficiency::InitHist(){}

void ApplyDetectorEfficiency::DefineMCSimple(){}

void ApplyDetectorEfficiency::StartOfRunUser(){}

void ApplyDetectorEfficiency::StartOfBurstUser(){}

void ApplyDetectorEfficiency::ProcessSpecialTriggerUser(int, unsigned int) {}

void ApplyDetectorEfficiency::Process(int) {
  
  TRecoMUV3Event* mevent = GetEvent<TRecoMUV3Event>("Reco");
  for(int i=0 ; i<mevent->GetNCandidates(); ++i){
    TRecoMUV3Candidate* cand = static_cast<TRecoMUV3Candidate*>(mevent->GetCandidate(i));
    Int_t bin = hMUV3Efficiency->FindFixBin(cand->GetTileID());
    Double_t eff = hMUV3Efficiency->GetEfficiency(bin);
    if(gRandom->Rndm()<eff) continue;
    std::cout << user_normal() << "Removing MUV3 candidate" << std::endl;
    mevent->RemoveCandidate(i);
    i--;
    std::cout << user_normal() << "Removed MUV3 candidate" << std::endl;
  }

  TRecoNewCHODEvent* nevent = GetEvent<TRecoNewCHODEvent>("Reco");
  for(int i=0; i<nevent->GetNHits(); ++i){
    TRecoNewCHODHit* hit = static_cast<TRecoNewCHODHit*>(nevent->GetHit(i));
    Int_t bin = hNewCEfficiency->FindFixBin(hit->GetTileID());
    Double_t eff = hNewCEfficiency->GetEfficiency(bin);
    if(gRandom->Rndm()<eff) continue;
    std::cout << user_normal() << "Removed NewCHOD hit." << std::endl;
    nevent->RemoveHit(i);
    i--;
  }

  TRecoSpectrometerEvent* sevent = GetEvent<TRecoSpectrometerEvent>("Reco");
  for(int i=0; i<sevent->GetNCandidates(); ++i){
    TRecoSpectrometerCandidate* cand = static_cast<TRecoSpectrometerCandidate*>(sevent->GetCandidate(i));
    Bool_t positive   = (cand->GetCharge()>0);
    Double_t momentum = cand->GetMomentum();
    Int_t bin = fSTRAWDataPosNum->FindFixBin(momentum, fIntensity);

    Double_t eff=1.00;
    if(positive){
      eff  = fSTRAWDataPosNum->GetBinContent(bin)/fSTRAWDataPosDen->GetBinContent(bin);
      eff /= fSTRAWMCPosNum->GetBinContent(bin)/fSTRAWMCPosDen->GetBinContent(bin);
    }
    else{
      eff  = fSTRAWDataNegNum->GetBinContent(bin)/fSTRAWDataNegDen->GetBinContent(bin);
      eff /= fSTRAWMCNegNum->GetBinContent(bin)/fSTRAWMCNegDen->GetBinContent(bin);
    }
    
    if(gRandom->Rndm()<eff) continue;
    std::cout << user_normal() << "Removed straw candidate" << std::endl;
    sevent->RemoveCandidate(i);
    i--;
  }

}

void ApplyDetectorEfficiency::PostProcess(){
}

void ApplyDetectorEfficiency::EndOfBurstUser(){
}

void ApplyDetectorEfficiency::EndOfRunUser(){
}

void ApplyDetectorEfficiency::EndOfJobUser(){
  hMUV3Efficiency->Write();
  hNewCEfficiency->Write();
}

void ApplyDetectorEfficiency::DrawPlot(){
}

ApplyDetectorEfficiency::~ApplyDetectorEfficiency(){
  fFile->Close();
}

