// LAVDigiBlockHandler.cc
// --------------------------------------------------------------
// History:
//
// 2015-03-19 T. Spadaro (tommaso.spadaro@lnf.infn.it)
// - promoting c++ variables to root types whenever possible
// - added doxygen compliant documentation
// Created by T. Spadaro and E. Leonardi (tommaso.spadaro@cern.ch, emanuele.leonardi@cern.ch) 2015-01-22
// --------------------------------------------------------------
/// \class LAVDigiBlockHandler
/// \Brief
/// Class for LAV DigiBlock handlings (i.e., the digi information soft class living in the context of the reconstruction code)
/// \EndBrief
#include <stdlib.h>
#include "LAVDigiBlockHandler.hh"

LAVDigiBlockHandler::LAVDigiBlockHandler(){

//  for (Int_t i=0; i<MAXBLOCKMAP; i++) 
//    fHitBlockMap[i] = -1; // Initialize at -1, since the index start from zero
  
  
  for (Int_t i=0; i<MAXBLOCKS; i++) {
    fHitBlock[i] = -1;
    fNHitBlock[i] = 0;
    for (Int_t j=0; j<MAXHITPERBLOCK; j++) fDigiBlockList[i][j] = 0;    
  }
  
  LoopInit();

}

LAVDigiBlockHandler::~LAVDigiBlockHandler(){
  LAVDigiBlockHandler::Clear();
}

void LAVDigiBlockHandler::Clear(){


  for (Int_t idx= 0; idx< (Int_t) fHitBlockMap.size(); idx++) {

    Int_t ncp = fNHitBlock[idx];

    for (Int_t icp=0; icp<ncp; icp++) {

      if (!fDigiBlockList[idx][icp]) {
	cerr << "LAVDigiBlockHandler >> Internal inconsistency " << idx << " " << icp << " " << ncp << " " << fHitBlockMap.size() << std::endl;
	exit(kGenericError);
      }	else {
	delete fDigiBlockList[idx][icp];
	fDigiBlockList[idx][icp] = 0;
      }
    }
    fNHitBlock[idx] = 0;

  }

  fHitBlockMap.clear();

}

void LAVDigiBlockHandler::Print() {
  std::cout << "---LAVDBH Printing Block Map" << std::endl;
  for (std::map<Int_t,Int_t>::iterator dd = fHitBlockMap.begin(); dd != fHitBlockMap.end(); ++dd) {

    if (fNHitBlock[dd->second]>0) {
      std::cout << "---LAVDBH Map " << dd->first << " " << dd->second << " " << fNHitBlock[dd->second] << " " << fHitBlock[dd->second] << " " << fDigiBlockList[dd->second][0]->GetBlockID() << std::endl;
      std::cout << " Block has " << fNHitBlock[dd->second] << " copies " << std::endl;
      for (Int_t i=0; i< fNHitBlock[dd->second]; i++) {
	cout << " Block copy " << i << " / " << fNHitBlock[dd->second] << std::endl;
	fDigiBlockList[dd->second][i]->Print();      
      }
    }
  }



}


LAVDigiBlock* LAVDigiBlockHandler::GetDigiBlock(Int_t BlockID, Int_t PhotonsNumber){


  if (BlockID < 0 || BlockID >= MAXBLOCKMAP) { 
    std::cerr << "LAVDigiBlockHandler::GetDigiBlock Error in input " << BlockID << std::endl;
    exit(kGenericError);
  }

  if (PhotonsNumber == 0) return NULL;
 
  map<Int_t,Int_t>::iterator idxIterator = fHitBlockMap.find(BlockID);

  if (idxIterator != fHitBlockMap.end()) {
    
    Int_t idx = idxIterator->second;
    
    // Block was already hit in the same event  

    Int_t ncp = fNHitBlock[idx];    // Number of times the block is fired

    if (ncp < MAXHITPERBLOCK) {    

      if(fDigiBlockList[idx][ncp]) delete fDigiBlockList[idx][ncp];
      fDigiBlockList[idx][ncp] = new LAVDigiBlock(BlockID, PhotonsNumber);

      fNHitBlock[idx]++; // Number of times the block is fired
      
      return fDigiBlockList[idx][ncp];
    }
    else {
      std::cout << "LAVDigiBlockHandler >> Warning: Max N copies exceeded for " << BlockID << " " << idx << std::endl;
      return NULL;
    }
  }
  else {

    // new Block in the map of fired blocks
    Int_t NBlocks = fHitBlockMap.size();
    fHitBlockMap[BlockID] = NBlocks; // Index of block fired
    fHitBlock[NBlocks] = BlockID; // ID of block fired

    if(fDigiBlockList[NBlocks][0]) delete fDigiBlockList[NBlocks][0];
    fDigiBlockList[NBlocks][0] = new LAVDigiBlock(BlockID, PhotonsNumber);
    fNHitBlock[NBlocks] = 1; // Number of times the block is fired

    return fDigiBlockList[NBlocks][0];

  }
}

LAVDigiBlock* LAVDigiBlockHandler::NextBlock(){

  if (fIBlock == fHitBlockMap.end()) {
    return NULL;
  }
  else if (fIBlockCnt >= fNHitBlock[fIBlock->second]) {

    ++fIBlock;
    fIBlockCnt = 0;
    return NULL;
  }
  else{
    return fDigiBlockList[fIBlock->second][fIBlockCnt++];
  }
}

void LAVDigiBlockHandler::CompactifyOpticalPhotons(){
  
  for (Int_t ib = 0; ib < (Int_t) fHitBlockMap.size(); ib++) {

    Int_t noCompact = 0;
    while(noCompact==0) {

      noCompact = 1;

      for (Int_t ic = 0; ic < fNHitBlock[ib]-1 && noCompact==1; ic++) {

	Double_t tIni = fDigiBlockList[ib][ic]->GetOpticalPhotonTimeMin(); 
	Double_t tEndi= fDigiBlockList[ib][ic]->GetOpticalPhotonTimeMax(); 

	for (Int_t jc = ic+1; jc < fNHitBlock[ib] && noCompact==1; jc++) {

	  Double_t tInj = fDigiBlockList[ib][jc]->GetOpticalPhotonTimeMin(); 
	  Double_t tEndj= fDigiBlockList[ib][jc]->GetOpticalPhotonTimeMax(); 

	  if (tEndj >= tIni && tInj <= tEndi) {

	    // merge

	    fDigiBlockList[ib][ic]->Add(fDigiBlockList[ib][jc]);
	    
	    // delete pointed object

	    delete fDigiBlockList[ib][jc];

	    // shrink the copy list

	    for (Int_t kc=jc+1; kc < fNHitBlock[ib]; kc++){
	      fDigiBlockList[ib][kc-1] = fDigiBlockList[ib][kc];
	    }
	    fDigiBlockList[ib][fNHitBlock[ib]--] = NULL;
	    
	    noCompact = 0;
	  }
	}
	
      }
    }
  }
}


void LAVDigiBlockHandler::CompactifyLastDynode(){
  
  for (Int_t ib = 0; ib < (Int_t) fHitBlockMap.size(); ib++) {

    Int_t noCompact = 0;
    while(noCompact==0) {

      noCompact = 1;

      for (Int_t ic = 0; ic < fNHitBlock[ib]-1 && noCompact==1; ic++) {

	Double_t tIni = fDigiBlockList[ib][ic]->GetTimeBegin(); 
	Double_t tEndi= fDigiBlockList[ib][ic]->GetTimeEnd(); 

	for (Int_t jc = ic+1; jc < fNHitBlock[ib] && noCompact==1; jc++) {

	  Double_t tInj = fDigiBlockList[ib][jc]->GetTimeBegin(); 
	  Double_t tEndj= fDigiBlockList[ib][jc]->GetTimeEnd(); 

	  // check if merging is needed

	  if (tEndj >= tIni && tInj <= tEndi) {

	    // merge

	    fDigiBlockList[ib][ic]->Add(fDigiBlockList[ib][jc]);

	    // delete pointed object

	    delete fDigiBlockList[ib][jc];

	    // shrink the copy list

	    for (Int_t kc=jc+1; kc < fNHitBlock[ib]; kc++){
	      fDigiBlockList[ib][kc-1] = fDigiBlockList[ib][kc];
	    }
	    fDigiBlockList[ib][fNHitBlock[ib]--] = NULL;

	    noCompact = 0;
	  }
	}

      }
    }
  }

}

void LAVDigiBlockHandler::ApplyPMT(TRandom3* aRandom){
  for (Int_t ib = 0; ib < (Int_t) fHitBlockMap.size(); ib++) {
    for (Int_t ic = 0; ic < fNHitBlock[ib]; ic++) {
      fDigiBlockList[ib][ic]->ApplyPMT(aRandom);
    }
  }
}


void LAVDigiBlockHandler::Digitize(){
  for (Int_t ib = 0; ib < (Int_t) fHitBlockMap.size(); ib++) {
    for (Int_t ic = 0; ic < fNHitBlock[ib]; ic++) {
      if (fDigiBlockList[ib][ic]->GetNBins()) {
	fDigiBlockList[ib][ic]->Digitize();
      }
    }
  }
}
