// LAVDigitizer.cc
// --------------------------------------------------------------
// History:
//
// 2015-03-19 T. Spadaro (tommaso.spadaro@lnf.infn.it)
// - promoting c++ variables to root types whenever possible
// - adjusting static geometry methods to "as build geometry"
// 2015-01-22 Totally modified and revised by T. Spadaro and E. Leonardi
// 2013-02-28 Order of Clear commands corrected T. Spadaro
// 2012-03-30 First implementation to hande the signal generation process and digi storage through TLAVDigi by V. Palladino and T. Spadaro
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-17
//
// --------------------------------------------------------------
#include "Riostream.h"

#include "string"

#include "LAVDigitizer.hh"
#include "LAVReconstruction.hh"

#include "TLAVEvent.hh"
#include "TLAVHit.hh"
#include "TLAVDigi.hh"
#include "TDCEvent.hh"
#include "TSpecialTriggerEvent.hh"
#include "LAVDataCardMessenger.hh"
#include "LAVCalibration.hh"
#include "NA62RecoManager.hh"

#define ELECTRON_CHARGE 1.6025e-19 
#define MAXOPTICALPHOTONTIME 1e6

LAVDigitizer::LAVDigitizer(NA62VReconstruction* Reco) :
  NA62VDigitizer(Reco, "LAV"),
  fHistoFile(nullptr)
{
      
  fDigiEvent = new TDCEvent( TLAVDigi::Class() );
  fDigiBlockHandler = new LAVDigiBlockHandler();

  LAVDataCardMessenger* DataCard = LAVDataCardMessenger::GetInstance();
  fMakeDigiHistos = DataCard->GetDigiHistoLevel();
  InitHisto();
}

LAVDigitizer::~LAVDigitizer(){
  CloseHisto();
  delete fDigiBlockHandler;
}

void LAVDigitizer::InitHisto(){
  if (fMakeDigiHistos) {
    fHOptPhotons          = new TH1D("OptPhotons","OptPhotons",100,0.,10000.);
    fHPhotoElectrons      = new TH1D("PhotoElectrons","PhotoElectrons",100,0.,10000.);
    fHRiseTime            = new TH1D("SignalRiseTime","SignalRiseTime",100,0.,10.);
    fHMCTimeResolution    = new TH2F("MCTimeResolution","MCTimeResolution",100,15.,25.,50,0.,200.);
    fHMCEnergyCorrelation = new TH2F("MCEnergyCorrelation","MCEnergyCorrelationn",100,0.,50.,100,0.,5.);
    fHMCGain              = new TH1D("MCGain","MCGain",100,0.,5.);
  }
}

void LAVDigitizer::CloseHisto(){
  if (fMakeDigiHistos) {  
    fHOptPhotons          ->Write();
    fHPhotoElectrons      ->Write();
    fHRiseTime            ->Write();
    fHMCTimeResolution    ->Write();
    fHMCEnergyCorrelation ->Write();
    fHMCGain              ->Write();
    delete fHOptPhotons         ;
    delete fHPhotoElectrons     ;
    delete fHRiseTime           ;
    delete fHMCTimeResolution   ;
    delete fHMCEnergyCorrelation;
    delete fHMCGain             ;
  }
  //  delete fHistoFile;
}

void LAVDigitizer::FillHisto() {

  if (fMakeDigiHistos==0) return;

  fDigiBlockHandler->LoopInit();

  Int_t endLoop = 0;
  Int_t nNull = 0;

  while (!endLoop) {
    LAVDigiBlock* DigiBlock = fDigiBlockHandler->NextBlock();
    if (!DigiBlock) {
      if (nNull==1) endLoop = 1;
      else {
	nNull = 1;
      }
    }
    else {
      nNull = 0;
      if (fMakeDigiHistos>1 && DigiBlock->GetNPhotoElectrons() > 0 && DigiBlock->GetNPhotoElectrons() < 30 && DigiBlock->GetNBins()) {
	TH1D* hOpticalPhotons = new TH1D(Form("OPBlock%d",DigiBlock->GetBlockID()),"Optical Photons",DigiBlock->GetNBins(),DigiBlock->GetTimeBegin(),DigiBlock->GetTimeEnd());
	for (Int_t i=0; i<DigiBlock->GetNOpticalPhotons(); i++) hOpticalPhotons->Fill(DigiBlock->GetOpticalPhotonTimeElement(i));
	if (hOpticalPhotons->GetRMS() > 1.) {
	  hOpticalPhotons->Write();
	  
	  TH1D* hSignal = new TH1D(Form("SignalBlock%d",DigiBlock->GetBlockID()),"Signal",DigiBlock->GetNBins(),DigiBlock->GetTimeBegin(),DigiBlock->GetTimeEnd());
	  for (Int_t i=0; i<DigiBlock->GetNBins(); i++) hSignal->SetBinContent(i,DigiBlock->GetGeneratedSignalElement(i));
	  hSignal->Write();
	  delete hSignal;
	  
	  TH1D* hEdges = new TH1D(Form("EdgeBlock%d",DigiBlock->GetBlockID()),"Edges",DigiBlock->GetNBins(),DigiBlock->GetTimeBegin(),DigiBlock->GetTimeEnd());
	  for (Int_t i=0; i<DigiBlock->GetNEdgesLow(); i++) hEdges->Fill(DigiBlock->GetEdgeLowElement(i));
	  for (Int_t i=0; i<DigiBlock->GetNEdgesHigh(); i++) hEdges->Fill(DigiBlock->GetEdgeHighElement(i));
	  hEdges->Write();
	  delete hEdges;
	}
	delete hOpticalPhotons;	
      }

      fHOptPhotons          ->Fill(DigiBlock->GetNOpticalPhotons());
      fHPhotoElectrons      ->Fill(DigiBlock->GetNPhotoElectrons());
      if (DigiBlock->GetNPhotoElectrons()) {

	fHRiseTime            ->Fill(DigiBlock->GetRiseTime());
	fHMCTimeResolution    ->Fill(DigiBlock->GetTSignalMax() - DigiBlock->GetHitTime(),DigiBlock->GetNPhotoElectrons());
	fHMCEnergyCorrelation ->Fill(DigiBlock->GetMCHit()->GetEnergy(),DigiBlock->GetCharge());

	Double_t gain = DigiBlock->GetCharge()/(DigiBlock->GetNPhotoElectrons()*1.E6*ELECTRON_CHARGE*1.e12);	
	fHMCGain              ->Fill(gain);

      }

    }    
  }
  //  fHistoFile->Write();
  fHOptPhotons          ->Write();
  fHPhotoElectrons      ->Write();
  fHRiseTime            ->Write();
  fHMCTimeResolution    ->Write();
  fHMCEnergyCorrelation ->Write();
  fHMCGain              ->Write();
}


TDetectorVEvent * LAVDigitizer::ProcessEvent(TDetectorVEvent * tEvent){
  if(tEvent->GetHits()->GetClass()->InheritsFrom("TVDigi") || tEvent->IsA() == TSpecialTriggerEvent::Class())
    return tEvent; 
  fDigiEvent->Clear();
  fDigiBlockHandler->Clear();
  
  TLAVEvent * LAVEvent = static_cast<TLAVEvent*>(tEvent);
  (*(TVEvent*)fDigiEvent)=(*(TVEvent*)LAVEvent);
    
  Int_t NProcessHits = 0;
  Int_t NHits = (Int_t)LAVEvent->GetNHits();
  for(Int_t iHit=0; iHit<NHits; iHit++){
    
    TLAVHit *Hit = static_cast<TLAVHit*>(LAVEvent->GetHit(iHit));
    Hit->DecodeChannelID(); // This ensures that the channelID will be compliant with the persistent methods of encoding/decoding

    Hit->SetLAVID(Hit->GetLAVID()+1); // needed because MC hit uses the 0-11 station convention, while we need 1-12.
    Hit->EncodeChannelID(); // ensures that fChannelID is compliant with the numbering format

    // Create a DigiBlock object from the MCHit

    if (CreateDigiBlock(Hit)) NProcessHits++;
    
  }

  if (NProcessHits) {


    fDigiBlockHandler->CompactifyOpticalPhotons(); // merge optical photon distributions if they overlap

    DigitizeHits(); // apply PMT multiplication and signal generation; apply low and high thresholds
  }

  FillHisto();
  
  return fDigiEvent;

}


Int_t LAVDigitizer::CreateDigiBlock(TLAVHit* Hit){

  // evaluate block ID and station ID and pass info to LAVpmt

  Int_t Bid = Hit->GetChannelID(); // geographic ChannelID

  // protect DigiBlock from unrealistic optical photon times
  Int_t nGoodHits =0;
  for(Int_t i=0;i<(Int_t) Hit->GetPhotonsNumber();i++){
    if (Hit->GetPhotonsTime()[i]<MAXOPTICALPHOTONTIME) nGoodHits++;
  } 

	
  // Store optical photons in DigiBlock using DigiBlockHandler

  LAVDigiBlock* DigiBlock = fDigiBlockHandler->GetDigiBlock(Bid, nGoodHits);   

  if (DigiBlock) {
    DigiBlock->SetHitTime(Hit->GetTime());
    DigiBlock->SetMCHit(Hit);

    Float_t* photonTime = Hit->GetPhotonsTime();
    Float_t* photonEnergy = Hit->GetPhotonsEnergy();

    Double_t tMin = 0; // min time will be assigned using the first photon and updated afterwards
    Double_t tMax = 0; // max time will be assigned using the first photon and updated afterwards

    Int_t idigi =0;
    
    for (Int_t i=0; i<Hit->GetPhotonsNumber(); i++) {
      Double_t phTime = photonTime[i]+Hit->GetTime();
      if (photonTime[i]<MAXOPTICALPHOTONTIME){
	DigiBlock->SetOpticalPhotonElement(idigi, phTime, (Double_t) photonEnergy[i]);
	idigi++;
      }
      if (i==0) {
	tMin = phTime;
	tMax = phTime;
      }
      else {
	if (phTime < tMin) tMin = phTime; 
	if (phTime > tMax) tMax = phTime; 
      }
    }

    DigiBlock->SetOpticalPhotonTimeRange(tMin,tMax);

    return 1;
  }
  else { 
    return 0;
  }
}

void LAVDigitizer::DigitizeHits(){

  // loop on firing blocks
  
  fDigiBlockHandler->ApplyPMT(fRandom);

  fDigiBlockHandler->CompactifyLastDynode();

  fDigiBlockHandler->Digitize();

  fDigiBlockHandler->LoopInit();

  Int_t endLoop = 0;
  Int_t newBlock = 1;
  //  Int_t blockID = 0;

  Double_t dummy = 0;

  while (!endLoop) {

    LAVDigiBlock* DigiBlock = fDigiBlockHandler->NextBlock();

    if (newBlock) {
      if (!DigiBlock) {
	endLoop = 1; // stop the loop if there is no new block
      }
      else {
	// preparing structure for signal sum for a new block
	//	blockID = DigiBlock->GetBlockID(); 
	newBlock = 0;
      }
    }

    if (!DigiBlock) {

      //  closing signal sum

      newBlock = 1;
    }

    else {

      // retrieve digitization output

      Int_t nEdge[2];
      nEdge[0] = DigiBlock->GetNEdgesLow();
      nEdge[1] = DigiBlock->GetNEdgesHigh();

      for (Int_t ithr = 0; ithr<2; ithr++) {

	Double_t edgeOld;
	Double_t edge;
	Int_t openDigi = 0;

	for (Int_t iEd = 0; iEd < nEdge[ithr]; iEd++) {
	  Int_t edgeType = DigiBlock->GetEdgeTypeElement(iEd,ithr);
	  edge  = DigiBlock->GetEdgeElement(iEd,ithr);
	
	  if (openDigi) {
	    if (edgeType == 1) {
	      CreateDigi(1,edgeOld,dummy,DigiBlock,ithr); // second edge is leading again: save the old edge in a leading-only Digi and store the new leading edge
	      edgeOld = edge;
	    }
	    else {
	      CreateDigi(3,edgeOld,edge,DigiBlock,ithr); // save golden digi and close
	      openDigi = 0;
	    }
	  }
	  else {
	    if (edgeType == 1) { // first edge is leading: store waiting for next edge
	      openDigi = 1;
	      edgeOld = edge;
	    }	    
	    else {
	      CreateDigi(2,dummy,edge,DigiBlock,ithr); // first edge is trailing: save it
	    }
	  }
	}
	if (openDigi) {
	  CreateDigi(1,edge,dummy,DigiBlock,ithr); // the final edge was leading: save it
	}
      }
    }
  }
}


void LAVDigitizer::CreateDigi(Int_t digiFlag, Double_t leadingEdge, Double_t trailingEdge, LAVDigiBlock* DigiBlock, Int_t thresholdFlag){

  Double_t FineTime = NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256.;

  TLAVDigi* Digi = static_cast<TLAVDigi*>(fDigiEvent->AddDigi());
  
  Digi->SetMCTrackID( DigiBlock->GetMCHit()->GetMCTrackID() );
  Digi->SetMCHit( DigiBlock->GetMCHit() );
  
  Digi->SetChannelID( DigiBlock->GetBlockID() + 1000000*thresholdFlag);
  Digi->DecodeChannelID(); // populate the geographic information
  Int_t station = (DigiBlock->GetMCHit())->GetStationID()-1;
  Double_t MCTimeOffset = static_cast<LAVReconstruction*>(fReco)->GetStationMCToF(station);

  Int_t layer = (DigiBlock->GetMCHit())->GetLayerID();
  Double_t MCLayerOffset = LAVDataCardMessenger::GetInstance()->GetLAVMCToFLayer(layer,station);

  Double_t t0 = LAVCalibration::GetInstance()->GetBlockT0(DigiBlock->GetBlockID());
  Double_t totalTimeOffset = FineTime+fReco->GetT0Correction(Digi)+t0-MCTimeOffset-MCLayerOffset;
  Digi->SetEdges(digiFlag,leadingEdge + totalTimeOffset,trailingEdge + totalTimeOffset);
}
