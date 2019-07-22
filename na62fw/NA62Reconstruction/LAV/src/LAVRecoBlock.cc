#include "LAVRecoBlock.hh"
#include "LAVGeometry.hh"
#include "LAVConfiguration.hh"
#include "TString.h"

LAVRecoBlock::LAVRecoBlock(Int_t blockID) :
  fBlockID(blockID),
  fEdgesAreSorted(0),
  fNHits(0),
  fOverFlow(false),
  fOpenHit(false)
{ // BlockID without trheshold identifier
  fEdgeCollection.clear();
  for(int i=0; i<MAX_RECOHITS_PER_BLOCK; ++i) fRecoHits[i] = nullptr;
} 


LAVRecoBlock::~LAVRecoBlock(){ 
  //  fEdgeCollection.clear(); 
  for (Int_t i=0; i<fNHits; i++) delete fRecoHits[i];
}

void LAVRecoBlock::Print(){ 
  std::cout << "---LAVRecoBlock Summary for Block = " << fBlockID << " " << fEdgeCollection.size() << " edges which are sorted: " << fEdgesAreSorted << std::endl;
  for (Int_t i=0; i<(Int_t) fEdgeCollection.size(); i++) {
    std::cout << " Edge " << i << " = " << fEdgeCollection.at(i).Threshold << " " << fEdgeCollection.at(i).Type << " " << fEdgeCollection.at(i).Time << std::endl;
  }
  std::cout << "---LAVRecoBlock Hit Summary: Nhits = " << fNHits << std::endl;
  for (Int_t i=0; i<fNHits; i++) fRecoHits[i]->Print();

}


void LAVRecoBlock::CreateHits(){

  LAVGeometry* Geom = LAVGeometry::GetInstance();
  Double_t leadingSafetyMargin = Geom->GetLeadingSafetyMargin();
  Double_t riseTimeSafetyMargin = Geom->GetRiseTimeSafetyMargin();         


  if (!fEdgesAreSorted) {
    std::cout << "---LAVRecoBlock Warning: calling CreateHits but no sorting done\n";
    LAVRecoBlock::SortEdges();
  }

  if (fNHits) {
    std::cout << "---LAVRecoBlock Warning: Hits for this block " << fBlockID << " are already present and are " << fNHits << std::endl;
    return;
  }

  fOverFlow = 0;
  fOpenHit = 0;

  for (Int_t i=0; i<(Int_t) fEdgeCollection.size(); i++) {

    if (fOverFlow) {
      std::cout << "---LAVRecoBlock CreateHits warning: LAVCRecoHit overflow!! " << fEdgeCollection.size() << std::endl;
      break;
    }
    
    if (fOpenHit) {

      if (fEdgeCollection.at(i).Type == 0 && fEdgeCollection.at(i).Threshold == 0){ // Leading Low threshold

	if (fRecoHits[fNHits]->GetEdgeMask() == 2) {
	  Double_t deltaT = fEdgeCollection.at(i).Time - fRecoHits[fNHits]->GetLeadingEdgeHigh();
	  if (deltaT < leadingSafetyMargin) {
	    fRecoHits[fNHits]->SetLeadingEdgeHigh(fEdgeCollection.at(i).Time);
	    fRecoHits[fNHits]->SetDigiLeadingEdgeHigh(fEdgeCollection.at(i).Digi);
	  }
	  else {
	    LAVRecoBlock::CloseHit();
	    if (OpenHit()) {
	      fRecoHits[fNHits] = new LAVRecoHit(fBlockID);      
	      fRecoHits[fNHits]->SetLeadingEdgeLow(fEdgeCollection.at(i).Time);
	      fRecoHits[fNHits]->SetDigiLeadingEdgeLow(fEdgeCollection.at(i).Digi);
	    }
	  }
	}
	else {
	  LAVRecoBlock::CloseHit();
	  if (OpenHit()) {
	    fRecoHits[fNHits] = new LAVRecoHit(fBlockID);      
	    fRecoHits[fNHits]->SetLeadingEdgeLow(fEdgeCollection.at(i).Time);
	    fRecoHits[fNHits]->SetDigiLeadingEdgeLow(fEdgeCollection.at(i).Digi);
	  }
	}
      }

      if (fEdgeCollection.at(i).Type == 0 && fEdgeCollection.at(i).Threshold == 1){ // Leading High threshold
	if (fRecoHits[fNHits]->GetEdgeMask() == 1) {
	  Double_t deltaT = fEdgeCollection.at(i).Time - fRecoHits[fNHits]->GetLeadingEdgeLow();
	  if (deltaT < riseTimeSafetyMargin) {
	    fRecoHits[fNHits]->SetLeadingEdgeHigh(fEdgeCollection.at(i).Time);
	    fRecoHits[fNHits]->SetDigiLeadingEdgeHigh(fEdgeCollection.at(i).Digi);
	  }
	  else {
	    LAVRecoBlock::CloseHit();
	    if (OpenHit()) {
	      fRecoHits[fNHits] = new LAVRecoHit(fBlockID);      
	      fRecoHits[fNHits]->SetLeadingEdgeHigh(fEdgeCollection.at(i).Time);
	      fRecoHits[fNHits]->SetDigiLeadingEdgeHigh(fEdgeCollection.at(i).Digi);
	    }
	  }
	}	
	else {
	  LAVRecoBlock::CloseHit();
	  if (OpenHit()) {
	    fRecoHits[fNHits] = new LAVRecoHit(fBlockID); // possibly lost?
	    fRecoHits[fNHits]->SetLeadingEdgeHigh(fEdgeCollection.at(i).Time);
	    fRecoHits[fNHits]->SetDigiLeadingEdgeHigh(fEdgeCollection.at(i).Digi);
	  }
	}
      }
	  
      if (fEdgeCollection.at(i).Type == 1 && fEdgeCollection.at(i).Threshold == 1){ // Trailing High threshold

	if (fRecoHits[fNHits]->GetEdgeMask() & 12) { // at least one trailing is present
	  LAVRecoBlock::CloseHit();
	  if (OpenHit()) {
	    fRecoHits[fNHits] = new LAVRecoHit(fBlockID);      
	    fRecoHits[fNHits]->SetTrailingEdgeHigh(fEdgeCollection.at(i).Time);
	    fRecoHits[fNHits]->SetDigiTrailingEdgeHigh(fEdgeCollection.at(i).Digi);
	  }
	}
	else {
	  fRecoHits[fNHits]->SetTrailingEdgeHigh(fEdgeCollection.at(i).Time);
	  fRecoHits[fNHits]->SetDigiTrailingEdgeHigh(fEdgeCollection.at(i).Digi);
	}
      }

      if (fEdgeCollection.at(i).Type == 1 && fEdgeCollection.at(i).Threshold == 0){ // Trailing Low threshold
	fRecoHits[fNHits]->SetTrailingEdgeLow(fEdgeCollection.at(i).Time);
	fRecoHits[fNHits]->SetDigiTrailingEdgeLow(fEdgeCollection.at(i).Digi);
	LAVRecoBlock::CloseHit();
      }
    }
    else {

      if (LAVRecoBlock::OpenHit()) {
	fRecoHits[fNHits] = new LAVRecoHit(fBlockID);
      
	if (fEdgeCollection.at(i).Type == 0 && fEdgeCollection.at(i).Threshold == 0) {
	  fRecoHits[fNHits]->SetLeadingEdgeLow(fEdgeCollection.at(i).Time); // LL
	  fRecoHits[fNHits]->SetDigiLeadingEdgeLow(fEdgeCollection.at(i).Digi); // LL
	}
	if (fEdgeCollection.at(i).Type == 0 && fEdgeCollection.at(i).Threshold == 1) {
	  fRecoHits[fNHits]->SetLeadingEdgeHigh(fEdgeCollection.at(i).Time); // LH
	  fRecoHits[fNHits]->SetDigiLeadingEdgeHigh(fEdgeCollection.at(i).Digi); // LH
	}
	if (fEdgeCollection.at(i).Type == 1 && fEdgeCollection.at(i).Threshold == 1) {
	  fRecoHits[fNHits]->SetTrailingEdgeHigh(fEdgeCollection.at(i).Time); // TH
	  fRecoHits[fNHits]->SetDigiTrailingEdgeHigh(fEdgeCollection.at(i).Digi); // TH
	}
	if (fEdgeCollection.at(i).Type == 1 && fEdgeCollection.at(i).Threshold == 0){
	  fRecoHits[fNHits]->SetTrailingEdgeLow(fEdgeCollection.at(i).Time);
	  fRecoHits[fNHits]->SetDigiTrailingEdgeLow(fEdgeCollection.at(i).Digi);
	  LAVRecoBlock::CloseHit();
	}
      }
    }
  }

  if (fOpenHit) LAVRecoBlock::CloseHit();

}

void LAVRecoBlock::ChargeReconstruct(){

  LAVConfiguration* Config = LAVConfiguration::GetInstance();
  if (!Config->HasReadConfigurationInfos()) {
    std::cerr << "LAVRecoBlock: cannot retrieve threshold values " << std::endl;
    exit(kWrongConfiguration);
  }

  // here take threshold values

  Double_t lowThr =  Config->GetBlockThresholdLow(fBlockID);
  Double_t highThr = Config->GetBlockThresholdHigh(fBlockID);
  Double_t hyst =    Config->GetBlockHysteresis(fBlockID);         

  for (Int_t iHit=0; iHit< fNHits; iHit++) {
    fRecoHits[iHit]->SetLowThreshold(lowThr,0.2E-3);
    fRecoHits[iHit]->SetHighThreshold(highThr,0.2E-3);
    fRecoHits[iHit]->SetHysteresis(hyst,0.4*hyst);
    //    fRecoHits[iHit]->SetRiseTime(riseTime);
    fRecoHits[iHit]->HitReconstruct();
  }
}


Bool_t LAVRecoBlock::OpenHit(){
  if (fOverFlow) return 0;
  fOpenHit = 1;
  return 1;
}

void LAVRecoBlock::CloseHit(){
  fOpenHit = 0;
  fNHits++;
  if (fNHits == MAX_RECOHITS_PER_BLOCK) fOverFlow = 1;  
}
