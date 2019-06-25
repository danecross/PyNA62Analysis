#include "TrackCalorimetricEnergyAssociation.hh"

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "SpectrometerCalorimetersAssociation.hh"
#include "GeometricAcceptance.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

TrackCalorimetricEnergyAssociation::TrackCalorimetricEnergyAssociation(Core::BaseAnalysis *ba) : Analyzer(ba, "TrackCalorimetricEnergyAssociation")
{
  RequestTree(new TRecoMUV1Event);
  RequestTree(new TRecoMUV2Event);
  RequestTree(new TRecoSpectrometerEvent);

  AddParam("Verbosity", "bool", &verb, false);
  AddParam("LKrEnergyCenter", "double", &fLKrEnergyCenter, 600.); //561.3
  AddParam("LKrEnergySigma", "double", &fLKrEnergySigma, 1.5*80.); //1.5*54.9
  AddParam("MUV1EnergyCenter", "double", &fMUV1EnergyCenter, 1380.); //1183.8
  AddParam("MUV1EnergySigma", "double", &fMUV1EnergySigma, 1.5*220.); //1.5*165.8
  AddParam("MUV2EnergyCenter", "double", &fMUV2EnergyCenter, 1250.); //1042.5
  AddParam("MUV2EnergySigma", "double", &fMUV2EnergySigma, 1.5*240.); //1.5*141.1
}

void TrackCalorimetricEnergyAssociation::InitOutput(){
  RegisterOutput("AssocMUV1ID", &fAssocMUV1ID);
  RegisterOutput("AssocMUV2ID", &fAssocMUV2ID);
  RegisterOutput("TotalEnergy", &fTotalEnergy);
  RegisterOutput("TimeOfEnergy", &fTimeOfEnergy);
  RegisterOutput("MuonProbability", &fMuonProbability);
  RegisterOutput("PionProbability", &fPionProbability);
  RegisterOutput("ElectronProbability", &fElectronProbability);
  RegisterOutput("ExtraMUV1Energy", &fExtraMUV1Energy);
  RegisterOutput("ExtraMUV2Energy", &fExtraMUV2Energy);
  RegisterOutput("MUV1Energy", &fMUV1Energy);
  RegisterOutput("MUV2Energy", &fMUV2Energy);
  RegisterOutput("DMIP", &fDMIP);
}

void TrackCalorimetricEnergyAssociation::InitHist(){
  fReadingData = GetIsTree();

  if(fReadingData){
    BookHisto(new TH1I("hCut", "hCut", 20, 1, 21));
    BookHisto(new TH2D("hPosAtMUV1TrackYvsX", "hPosAtMUV1TrackYvsX", 300, -1500., 1500., 300, -1500., 1500.));
    BookHisto(new TH2D("hPosAtMUV2TrackYvsX", "hPosAtMUV2TrackYvsX", 300, -1500., 1500., 300, -1500., 1500.));
    BookHisto(new TH1D("hPosDiffMUV1Track", "hPosDiffMUV1Track", 250, 0., 2500.));
    BookHisto(new TH1I("hMUV1Associated", "hMUV1Associated", 2, 0, 2));
    BookHisto(new TH1D("hPosDiffMUV2Track", "hPosDiffMUV2Track", 250, 0., 2500.));
    BookHisto(new TH1I("hMUV2Associated", "hMUV2Associated", 2, 0, 2));
    BookHisto(new TH1D("hDMIP", "hDMIP", 100, 0., 100.));
  };
}

void TrackCalorimetricEnergyAssociation::DefineMCSimple(){
}

void TrackCalorimetricEnergyAssociation::StartOfRunUser(){
}

void TrackCalorimetricEnergyAssociation::StartOfBurstUser(){
}

void TrackCalorimetricEnergyAssociation::ProcessSpecialTriggerUser(int, unsigned int){
}

void TrackCalorimetricEnergyAssociation::Process(int iEvent){
  if(!fReadingData) return;

  (void)iEvent;
  int cutID = 1;
  FillHisto("hCut", cutID);
  cutID++;

  if(verb){
    cout<<endl;
    cout<<"-------------------"<<endl;
    cout<<"TrackCalorimetricEnergyAssociation"<<endl;
    cout<<"-------------------"<<endl;
    cout<<endl;
  };

  PrepareOutputs();

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

  TVector2 posAtMUV1;
  TVector2 posAtMUV2;
  TRecoMUV1Event* MUV1Event = GetEvent<TRecoMUV1Event>();
  TRecoMUV1Candidate *MUV1Cand;
  TRecoMUV2Event* MUV2Event = GetEvent<TRecoMUV2Event>();
  TRecoMUV2Candidate *MUV2Cand;
  TClonesArray *specCalo = (TClonesArray*)GetOutput("SpectrometerCalorimetersAssociation.MatchedClusters");
  TRecoSpectrometerEvent* STRAWEvent = GetEvent<TRecoSpectrometerEvent>();
  for(int i=0; i<STRAWEvent->GetNCandidates(); i++){
    if(verb) cout<<"STRAW candidate "<<i<<endl;
    TRecoSpectrometerCandidate *STRAWCand = static_cast<TRecoSpectrometerCandidate*>(STRAWEvent->GetCandidate(i));
    posAtMUV1.SetX(STRAWCand->xAt(GeometricAcceptance::GetInstance()->GetZMUV1()));
    posAtMUV1.SetY(STRAWCand->yAt(GeometricAcceptance::GetInstance()->GetZMUV1()));
    posAtMUV2.SetX(STRAWCand->xAt(GeometricAcceptance::GetInstance()->GetZMUV2()));
    posAtMUV2.SetY(STRAWCand->yAt(GeometricAcceptance::GetInstance()->GetZMUV2()));

    //MUV1
    double minD1 = 1000000.;
    int minID1 = -1;
    FillHisto("hPosAtMUV1TrackYvsX", posAtMUV1.X(), posAtMUV1.Y());
    for(int j=0; j<MUV1Event->GetNCandidates(); j++){
      MUV1Cand = static_cast<TRecoMUV1Candidate*>(MUV1Event->GetCandidate(j));
      double dist = (posAtMUV1-MUV1Cand->GetPosition()).Mod();
      FillHisto("hPosDiffMUV1Track", dist);
      if(dist<minD1){
	minD1=dist;
	minID1 = j;
      };
    };
    fAssocMUV1ID.push_back(minID1);
    if(verb) cout<<"MUV1 assoc ID = "<<minID1<<endl;
    FillHisto("hMUV1Associated", minID1==-1 ? 0 : 1);

    //MUV2
    double minD2 = 1000000.;
    int minID2 = -1;
    FillHisto("hPosAtMUV2TrackYvsX", posAtMUV2.X(), posAtMUV2.Y());
    for(int j=0; j<MUV2Event->GetNCandidates(); j++){
      MUV2Cand = static_cast<TRecoMUV2Candidate*>(MUV2Event->GetCandidate(j));
      double dist = (posAtMUV2-MUV2Cand->GetPosition()).Mod();
      FillHisto("hPosDiffMUV2Track", dist);
      if(dist<minD2){
	minD2=dist;
	minID2 = j;
      };
    };
    fAssocMUV2ID.push_back(minID2);
    if(verb) cout<<"MUV2 assoc ID = "<<minID2<<endl;
    FillHisto("hMUV2Associated", minID2==-1 ? 0 : 1);

    //Aliberti tool
    if(verb) cout<<"Using Riccardo Aliberti tool"<<endl;
    CalorimeterCluster *clus = static_cast<CalorimeterCluster*>(specCalo->ConstructedAt(i));
    fTotalEnergy.push_back(clus->GetEnergy());
    fTimeOfEnergy.push_back(clus->GetTime());
    fMuonProbability.push_back(clus->GetIsMuonProbability());
    fPionProbability.push_back(clus->GetIsPionProbability());
    fElectronProbability.push_back(clus->GetIsElectronProbability());
    fMUV1Energy.push_back(clus->GetMUV1Energy());
    fMUV2Energy.push_back(clus->GetMUV2Energy());
    double DLKr = ((clus->GetLKrEnergy()<=0.)?0.:((clus->GetLKrEnergy() - fLKrEnergyCenter)/fLKrEnergySigma));
    double DMUV1 = ((clus->GetMUV1Energy()<=0.)?0.:((clus->GetMUV1Energy() - fMUV1EnergyCenter)/fMUV1EnergySigma));
    double DMUV2 = ((clus->GetMUV2Energy()<=0.)?0.:((clus->GetMUV2Energy() - fMUV2EnergyCenter)/fMUV2EnergySigma));
    int denom = (int)(clus->GetLKrEnergy()>0.) + (int)(clus->GetMUV1Energy()>0.) + (int)(clus->GetMUV2Energy()>0.);
    double D = (sqrt(DLKr*DLKr + DMUV1*DMUV1 + DMUV2*DMUV2))/denom;
    FillHisto("hDMIP", D);
    fDMIP.push_back(D);
    fExtraMUV1Energy.push_back(clus->GetMUV1OuterEnergy());
    fExtraMUV2Energy.push_back(clus->GetMUV2OuterEnergy());
    if(verb) cout<<"cluster energy = "<<clus->GetEnergy()<<"  cluster time = "<<clus->GetTime()<<"  muon prob = "<<clus->GetIsMuonProbability()<<"  pion prob = "<<clus->GetIsPionProbability()<<"  electron prob = "<<clus->GetIsElectronProbability()<<"  MUV1 energy = "<<clus->GetMUV1Energy()<<"  MUV2 energy = "<<clus->GetMUV2Energy()<<"  D = "<<D<<"  MUV1 outer energy = "<<clus->GetMUV1OuterEnergy()<<"  MUV2 outer energy = "<<clus->GetMUV2OuterEnergy()<<endl;
  };
  FillHisto("hCut", cutID);

  ValidateOutputs();
}

void TrackCalorimetricEnergyAssociation::PostProcess(){

}

void TrackCalorimetricEnergyAssociation::EndOfBurstUser(){
}

void TrackCalorimetricEnergyAssociation::EndOfRunUser(){

}

void TrackCalorimetricEnergyAssociation::EndOfJobUser(){
  if(fReadingData){
    SaveAllPlots();
  };
}

void TrackCalorimetricEnergyAssociation::DrawPlot(){
}

TrackCalorimetricEnergyAssociation::~TrackCalorimetricEnergyAssociation(){
}

void TrackCalorimetricEnergyAssociation::PrepareOutputs(){
  fAssocMUV1ID.clear();
  fAssocMUV2ID.clear();
  fTotalEnergy.clear();
  fTimeOfEnergy.clear();
  fMuonProbability.clear();
  fPionProbability.clear();
  fElectronProbability.clear();
  fMUV1Energy.clear();
  fMUV2Energy.clear();
  fExtraMUV1Energy.clear();
  fExtraMUV2Energy.clear();
  fDMIP.clear();
  SetOutputState("AssocMUV1ID", kOInvalid);
  SetOutputState("AssocMUV2ID", kOInvalid);
  SetOutputState("TotalEnergy", kOInvalid);
  SetOutputState("TimeOfEnergy", kOInvalid);
  SetOutputState("MuonProbability", kOInvalid);
  SetOutputState("PionProbability", kOInvalid);
  SetOutputState("ElectronProbability", kOInvalid);
  SetOutputState("MUV1Energy", kOInvalid);
  SetOutputState("MUV2Energy", kOInvalid);
  SetOutputState("ExtraMUV1Energy", kOInvalid);
  SetOutputState("ExtraMUV2Energy", kOInvalid);
  SetOutputState("DMIP", kOInvalid);
}

void TrackCalorimetricEnergyAssociation::ValidateOutputs(){
  SetOutputState("AssocMUV1ID", kOValid);
  SetOutputState("AssocMUV2ID", kOValid);
  SetOutputState("TotalEnergy", kOValid);
  SetOutputState("TimeOfEnergy", kOValid);
  SetOutputState("MuonProbability", kOValid);
  SetOutputState("PionProbability", kOValid);
  SetOutputState("ElectronProbability", kOValid);
  SetOutputState("MUV1Energy", kOValid);
  SetOutputState("MUV2Energy", kOValid);
  SetOutputState("ExtraMUV1Energy", kOValid);
  SetOutputState("ExtraMUV2Energy", kOValid);
  SetOutputState("DMIP", kOValid);
}
