#include "TROOT.h"
#include "TSystem.h"
#include "TStyle.h"
#include "Riostream.h"
#include "TApplication.h"
#include "TEveManager.h"
#include "TEveBrowser.h"
#include "TGeoManager.h"
#include "TGeoNode.h"
#include "TEveGeoNode.h"
#include "TEveArrow.h"
#include "TEveLine.h"
#include "TEveViewer.h"
#include "TEveEventManager.h"
#include "TEveVSDStructs.h"
#include "TEveTrack.h"
#include "TEveTrackPropagator.h"
#include "TColor.h"
#include "TCanvas.h"
#include "TH1.h"

#include "NA62Reconstruction.hh"
#include "NA62ConditionsService.hh"
#include "SpectrometerReconstruction.hh"

#include "KinePart.hh"
#include "TNA62MagField.hh"

#include <sys/stat.h>
#include <unistd.h>

NA62Reconstruction * NA62Reco;

void usage(char* name){
  std::cout << "Usage: "<< name << " [-h] [-B #MaxFiles] [-i InputFile.dat/.root] [-l InputListFile.txt] [-n #MaxEvents] [-o OutputFile.root] [-s seed] [-c ConfigFileName.conf] [-j #JumpFirstNEvents] [-N #MaxEventsPerBurst] [-C ConditionsDirectoryPath] [-e #ExitLevel]"
    << std::endl;
}

int main(Int_t argc, char **argv){
  extern char *optarg;
  int opt;
  TString OutputFileName("OutputFile.root");
  TString InputFileName("InputFile.root");
  TString ConfFileName("config/NA62Reconstruction.conf");
  // Available Exit Levels:
  //   0: exit if any not found or empty file is detected [for production]
  //   1: ignore not found files, exit if any empty file is detected [default]
  // >=2: ignore not found or empty files
  Int_t   ExitLevel=0;
  TString ConditionsDirPath("");
  TString InputListFileName;
  Int_t NFiles = 100000, NEvt = 100000000, JumpNEvt=0, NEvtPerFile=100000000;
  UInt_t Seed = 4357;

  Int_t n_options_read = 0;
  Int_t nb=0, nc=0, nC=0, ne=0, ni=0, nj=0, nl=0, nn=0, nN=0, no=0, ns=0;
  while ((opt = getopt(argc, argv, "B:c:C:e:h:i:j:l:n:N:o:s:")) != -1) { //it must end with a :
    n_options_read++;
    switch (opt) {
      case 'B':
        nb++;
        NFiles = TString(optarg).Atoi();
        break;
      case 'c':
        nc++;
        ConfFileName = TString(optarg);
        break;
      case 'C':
        nC++;
        ConditionsDirPath = TString(optarg);
        break;
      case 'e':
        ne++;
        ExitLevel = TString(optarg).Atoi();
        break;
      case 'h':
        usage(argv[0]);
        return 0;
      case 'i':
        ni++;
        InputFileName = TString(optarg);
        break;
      case 'j':
        nj++;
        JumpNEvt = TString(optarg).Atoi();
        break;
      case 'l':
        nl++;
        InputListFileName = TString(optarg);
        break;
      case 'n':
        nn++;
        NEvt = TString(optarg).Atoi();
        break;
      case 'N':
        nN++;
        NEvtPerFile = TString(optarg).Atoi();
        break;
      case 'o':
        no++;
        OutputFileName = TString(optarg);
        break;
      case 's':
        ns++;
        Seed = (UInt_t)TString(optarg).Atoi();
        break;
      default:
        usage(argv[0]);
        return 0;
    }
  }

  // Sanity checks on the input
  if (!n_options_read || NEvt<=0 || NFiles<=0 || JumpNEvt<0 || NEvtPerFile<0) {
    usage(argv[0]);
    return 1;
  }
  if (nb>1 || nc>1 || nC>1 || ne>1 || ni>1 || nj>1 || nl>1 || nn>1 || nN>1 || no>1 || ns>0) {
    std::cerr << "[NA62EventDisplay] Multiple arguments of the same type are not allowed" << std::endl;
    return 1;
  }

  // Protection against potentially incorrect output filenames
  struct stat buffer;
  if (!OutputFileName.EndsWith(".root") && !stat(OutputFileName.Data(), &buffer)) {
    std::cout << " [NA62EventDisplay] Output file exists and is not *.root: potentially a destructive call" << std::endl;
    return 1;
  }

  TObjArray InputFileNameList;
  TFile * OutputFile;
  NA62ConditionsService::GetInstance()->SetExitLevel(ExitLevel);
  NA62ConditionsService::GetInstance()->SetExternalCDBDirectoryPath(ConditionsDirPath);
  if(InputListFileName.CompareTo("")) { //-l option used
    NA62Reco = new NA62Reconstruction(InputListFileName, ConfFileName, OutputFileName, NEvt, NEvtPerFile, JumpNEvt, Seed, NFiles);
  }
  else if(InputFileName.CompareTo("")){ //-i option used
    InputFileNameList.Add(new TObjString(InputFileName.Data()));
    OutputFile = TFile::Open(OutputFileName.Data(),"RECREATE");
    NA62Reco = new NA62Reconstruction(&InputFileNameList, ConfFileName, OutputFile, NEvt, NEvtPerFile, JumpNEvt, Seed);
  }
  NA62Reco->NextEvent(); //find the modules to be disabled

  //if(InputFileNameList.GetEntries() == 0) {
  //    perror(Form("Input File"));
  //    exit(kWrongConfiguration);
  //}

  TApplication NA62EventDisplayApp("NA62EventDisplayApp", &argc, argv);
  gROOT->Reset("a");
  gSystem->ResetSignal(kSigBus, true);
  gSystem->ResetSignal(kSigSegmentationViolation, true);
  gSystem->ResetSignal(kSigIllegalInstruction, true);
  gSystem->ResetSignal(kSigFloatingException, true);

  if(gROOT->IsBatch()) {
    std::cout << "error: NA62EventDisplay cannot run in batch mode." << std::endl;
    exit(kWrongConfiguration);
  }

  TEveManager::Create();
  gEve->GetMainWindow()->SetWindowName("NA62 Event Display");
  //gEve->GetMainWindow()->MoveResize(0, 0, 1398, 1136);

  gGeoManager = gEve->GetGeometry("NA62.root");
  TGeoNode* NA62World = gGeoManager->GetTopNode();
  TEveGeoTopNode* NA62EveWorld = new TEveGeoTopNode(gGeoManager, NA62World);
  gEve->AddGlobalElement(NA62EveWorld);

  TNA62MagField* MagField = new TNA62MagField(gGeoManager);
  TEveTrackPropagator* TrackPropagator = new TEveTrackPropagator();
  TrackPropagator->SetMagFieldObj(MagField);
  TrackPropagator->SetStepper(TEveTrackPropagator::kRungeKutta);
  TrackPropagator->SetFitDecay(kTRUE);
  TrackPropagator->SetMagField(30,0,0);
  TrackPropagator->SetMaxStep(10.);
  TrackPropagator->SetMaxZ(35000.);
  TrackPropagator->SetMaxR(1000.);

  TEveRecTrack* EveRecTrack;
  TEvePointSetArray* EveSpectrometerHits;
  TEveMCTrack* EveMCTrack;
  TEveTrack* EveTrack;
  TEvePathMark* EndPoint;
  TEveElementList *Event = 0, *MCTruth = 0, *Hits = 0, *Reconstruction = 0;
  SpectrometerReconstruction * SpecReco = static_cast<SpectrometerReconstruction*>(NA62Reco->FindReco("Spectrometer"));
  TSpectrometerEvent * SpecEvent = static_cast<TSpectrometerEvent*>(NA62Reco->FindMCEvent("Spectrometer"));
  for(Int_t iEvent = 0; iEvent < 1000 && NA62Reco->NextEvent(); iEvent++){
    if(!iEvent){
      Event = new TEveElementList();
      gEve->AddElement(Event);
      Hits = new TEveElementList();
      Reconstruction = new TEveElementList();
      Hits->SetName("Hits");
      gEve->AddElement(Hits, Event);
      Reconstruction->SetName("Reconstruction");
      gEve->AddElement(Reconstruction, Event);
      MCTruth = new TEveElementList();
      MCTruth->SetName("Generated Event");
      gEve->AddElement(MCTruth, Event);
    }
    if(!(iEvent%3)){
      Event->SetName(Form("Event %d",iEvent));
      if(NA62Reco->GetMCTruthEvent()){     
        Reconstruction->RemoveElements();
        Hits->RemoveElements();
        MCTruth->RemoveElements();

        TClonesArray Tracks = (*(NA62Reco->GetMCTruthEvent()->GetKineParts()));
        Int_t nMCTracks = Tracks.GetEntries();
        Int_t nSpectrometerHits = 0; 
        TClonesArray * SpectrometerHits = (SpecEvent ? SpecEvent->GetHits() : 0);
        if(SpectrometerHits){
          nSpectrometerHits = SpectrometerHits->GetEntries();
        }
        if(nMCTracks + nSpectrometerHits > 0){
          gEve->GetCurrentEvent()->DisableListElements();
        }
        if(nSpectrometerHits > 0){
          //gEve->AddElement((TEveElement*)RecTrackPropagator, (TEveElement*)Reconstruction);
        }
        if(nSpectrometerHits > 0){
          // Getting Hits and creating Eve structures
          EveSpectrometerHits = new TEvePointSetArray("Spectrometer Hits - Energy Slices", "");
          gEve->AddElement(static_cast<TEveElement*>(EveSpectrometerHits), static_cast<TEveElement*>(Hits));
          //EveSpectrometerHits->SetSourceCS(TEvePointSelectorConsumer::kTVT_RPhiZ);
          EveSpectrometerHits->SetMarkerColor(3);
          EveSpectrometerHits->SetMarkerStyle(2); // cross
          EveSpectrometerHits->SetMarkerSize(0.8);

          EveSpectrometerHits->InitBins("Energy", 10, 0, 10);//KeV

          TColor::SetPalette(1, 0); // Spectrum palette
          const Int_t nCol = TColor::GetNumberOfColors();
          for (Int_t i = 1; i <= 10; ++i)
            EveSpectrometerHits->GetBin(i)->SetMainColor(TColor::GetColorPalette(i * nCol / 10));

          EveSpectrometerHits->GetBin(0) ->SetMainColor(kGray);
          EveSpectrometerHits->GetBin(10)->SetMainColor(kWhite);        
          for(Int_t iHit = 0; iHit < nSpectrometerHits; iHit++){
            Double_t E = static_cast<TSpectrometerHit*>(SpectrometerHits->At(iHit))->GetEnergy()*1000.; //KeV
            TVector3 Pos = static_cast<TSpectrometerHit*>(SpectrometerHits->At(iHit))->GetPosition()*0.1; //TGeo package uses cm as native lenght unit
            EveSpectrometerHits->Fill(Pos.X(),Pos.Y(),Pos.Z(),E);
          }
          EveSpectrometerHits->CloseBins();
        }
        if(SpecReco != 0){
          // Getting track candidates and adding to Eve
          Int_t nTrackCandidates = SpecReco->GetNTracks();
          for(Int_t iTrack = 0; iTrack < nTrackCandidates; iTrack++){
            TVector3 Position = SpecReco->GetCandidate(iTrack)->GetPositionBeforeMagnet(); 
            TVector3 Direction (SpecReco->GetCandidate(iTrack)->GetSlopeXBeforeMagnet(),
                SpecReco->GetCandidate(iTrack)->GetSlopeYBeforeMagnet(),1);
            Direction = Direction*(1./Direction.Mag());
            TVector3 Momentum = 0.001*SpecReco->GetCandidate(iTrack)->GetMomentum()*Direction;
            EveRecTrack = new TEveRecTrack();
            EveRecTrack->fV.Set((Position-Direction*
                  (Position.Perp()/Direction.Perp() > Position.Z() - 10000 ? Position.Z() - 10000 : 
                   Position.Perp()/Direction.Perp())
                  ));
            EveRecTrack->fP.Set(Momentum);
            EveRecTrack->fSign = 1;
            EveTrack = new TEveTrack(EveRecTrack, TrackPropagator);
            EveTrack->SetMainColor(kBlue);
            //          EveTrack->SetName(Form("P=%3.2fGeV - Chi2=%1.2f",Momentum.Mag(), 
            //                      SpecReco->GetTrackCollector()->GetTrack(iTrack)->GetChi2()));
            EveTrack->SetName(Form("P=%3.2fGeV - Chi2=%1.2f",Momentum.Mag(), 
                  SpecReco->GetCandidate(iTrack)->GetChi2()));
            EveTrack->MakeTrack();
            //gEve->AddElement((TEveElement*)EveTrack, (TEveElement*)RecTrackPropagator);
            gEve->AddElement(static_cast<TEveElement*>(EveTrack), static_cast<TEveElement*>(Reconstruction));
          }
        }
        if(nMCTracks > 0){
          // Getting generated tracks and creating Eve structures
          //gEve->AddElement((TEveElement*)TrackPropagator, (TEveElement*)MCTruth);
          for(Int_t iTrack = 0; iTrack < nMCTracks; iTrack++){
            Int_t ID = static_cast<KinePart*>(Tracks[iTrack])->GetID();
            TLorentzVector ProdPos = static_cast<KinePart*>(Tracks[iTrack])->GetProdPos()*0.1; //TGeo package uses cm as native length unit
            TLorentzVector EndPos = static_cast<KinePart*>(Tracks[iTrack])->GetEndPos()*0.1;
            TVector3 Momentum = static_cast<KinePart*>(Tracks[iTrack])->GetInitialMomentum()*0.001; //GeV and Tesla give cm in propagation
            Double_t Energy = static_cast<KinePart*>(Tracks[iTrack])->GetInitialEnergy()*0.001; //GeV and Tesla give cm in propagation
            Int_t PDGcode = static_cast<KinePart*>(Tracks[iTrack])->GetPDGcode();
            //Double_t PDGcharge = static_cast<(KinePart*>(Tracks[iTrack])->GetPDGcharge());
            EveMCTrack = new TEveMCTrack();
            EveMCTrack->SetProductionVertex(ProdPos);
            EveMCTrack->SetMomentum(TLorentzVector(Momentum, Energy));
            EveMCTrack->SetPdgCode(PDGcode);
            EveMCTrack->fIndex = ID;
            EndPoint = new TEvePathMark(TEvePathMark::kDecay);
            EndPoint->fV.Set(EndPos.Vect());
            EveTrack = new TEveTrack(EveMCTrack, TrackPropagator);
            EveTrack->AddPathMark(*EndPoint);
            EveTrack->SetName(Form("P=%3.2fGeV - PDG %d",Momentum.Mag(), PDGcode));
            //EveTrack->SetCharge((Int_t)PDGcharge);
            EveTrack->MakeTrack();
            EveTrack->SetMainColor((PDGcode == 22 ? kGreen : kBlue));
            //gEve->AddElement((TEveElement*)EveTrack, (TEveElement*)TrackPropagator);
            gEve->AddElement(static_cast<TEveElement*>(EveTrack), static_cast<TEveElement*>(MCTruth));
          }
        }
      }
    }
  }

  gEve->FullRedraw3D(kTRUE);
  NA62EventDisplayApp.Run();
  exit(0);
}
