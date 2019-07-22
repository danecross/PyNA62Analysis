// LAVClusterMaker.cc
// --------------------------------------------------------------
// History:
//
// 2015-03-19 T. Spadaro (tommaso.spadaro@lnf.infn.it)
// - promoting c++ variables to root types whenever possible
// - added new method to evaluate cluster properties
// Created by T. Spadaro and E. Leonardi (tommaso.spadaro@cern.ch, emanuele.leonardi@cern.ch) 2015-01-22
//
// --------------------------------------------------------------
#include "LAVClusterMaker.hh"

LAVClusterMaker::LAVClusterMaker(){
  fGeometryInstance = LAVGeometry::GetInstance();
  for (Int_t i=0; i<MAXBLOCKMAP; i++) {
    fNeighbouringBlocksHash[i] = NULL;
    fBlockMapHash[i] = -1;
  }
  fNFiredBlock = 0;
  for (Int_t i=0; i<MAXBLOCKS; i++) {
    fNHitsPerBlock[i] = 0;
    fBlockChannelID[i] = 0;
  }
  fNClusters = 0;
  fUsedRecoHit = NULL;

  fTimeDistanceMax = 3.; // ns
  fTimeDistanceMarginal = 5.; // ns
}

LAVClusterMaker::~LAVClusterMaker(){
  for (Int_t i=0; i<MAXBLOCKMAP; i++) delete[] fNeighbouringBlocksHash[i];
}

void LAVClusterMaker::Clear(){
  if (fNClusters) {
    for (Int_t i = 0; i< fNClusters; i++) {
      if (fLAVClusters.at(i)) delete fLAVClusters.at(i);
    }
    fLAVClusters.clear();
    fNClusters = 0;
  }


  if (fUsedRecoHit) {
    delete[] fUsedRecoHit;
    fUsedRecoHit = NULL;
  }

// clear of the static block map

  for (Int_t i=0; i<fNFiredBlock; i++) {
    Int_t blockID = fBlockChannelID[i];
    fBlockMapHash[blockID] = -1;
    fNHitsPerBlock[i] = 0;
    fBlockChannelID[i] = 0;
    delete[] fHitPointers.at(i); // array of indexes to TClonesArray of TRecoLAVHits
  }
  fHitPointers.clear();
  fNFiredBlock = 0;
  
}

void LAVClusterMaker::Clusterize(TRecoLAVEvent* RecoEvent){


  UInt_t nReco = RecoEvent->GetNHits();
  if (nReco == 0) return;

  TClonesArray & hitArray = (* RecoEvent->GetHits());

// build block-wise map

  for (UInt_t iReco = 0; iReco < nReco; iReco++) {

    TRecoLAVHit* LAVHit = static_cast<TRecoLAVHit*>( hitArray[iReco]);
    Int_t BlockID = LAVHit->GetChannelID();

    if (fBlockMapHash[BlockID] == -1) {
      fBlockMapHash[BlockID] = fNFiredBlock;
      Int_t* pointerArray = new Int_t[MAXHITSPERBLOCK];
      fHitPointers.push_back(pointerArray);
      pointerArray[0] = iReco;
      fNHitsPerBlock[fNFiredBlock] = 1;
      fBlockChannelID[fNFiredBlock] = BlockID;

      fNFiredBlock++;
    }
    else {	 
      Int_t blockIndex = fBlockMapHash[BlockID];
      Int_t nhits = fNHitsPerBlock[blockIndex];
      if (nhits < MAXHITSPERBLOCK) {
	fHitPointers.at(blockIndex)[nhits] = iReco;
	fNHitsPerBlock[blockIndex]++;
      }
      else {
	cout << "LAVClusterMaker Warning >> Max number of hits per block reached " << std::endl;
      }
    }
  }

// given the map, clusterize

  fUsedRecoHit = new Bool_t[nReco];
  for (UInt_t iReco = 0; iReco < nReco; iReco++) fUsedRecoHit[iReco] = kFALSE;

  Double_t phiMin;
  Double_t phiMax;
  TVector3 blockPosition;
  
  for (UInt_t iReco = 0; iReco < nReco; iReco++) {
    TRecoLAVHit* LAVHit = static_cast<TRecoLAVHit*>( hitArray[iReco]);

    if (!fUsedRecoHit[iReco] && ClusterSeedQuality(LAVHit)) {

// new Cluster

      LAVCluster* newClus = new LAVCluster();
      newClus->SetEvent(RecoEvent);
      newClus->AddHit(iReco);
      LAVHit->GetBlockPosition(blockPosition);
      phiMin = blockPosition.Phi();
      phiMax = blockPosition.Phi();
      fUsedRecoHit[iReco] = kTRUE;

// check Neighbouring blocks

      Int_t nTestedBlocks = 0;
      Int_t testedBlocks[256]={0};
      Int_t nAcceptedBlocks;
      Int_t acceptedBlocks[256]={0};
      Int_t lastVerifiedCenter = 0;
      
      acceptedBlocks[0] = LAVHit->GetChannelID();
      nAcceptedBlocks = 1;

      Bool_t jError = kFALSE;
      while (acceptedBlocks[lastVerifiedCenter] != 0 && !jError) {
	Int_t BlockID = acceptedBlocks[lastVerifiedCenter];
	Int_t* neighbours = NeighbouringBlocks(BlockID); // there can be from 5 up to 8 blocks (block under test included)

	for (Int_t iN = 0; iN<8; iN++) {
	  if (neighbours[iN] == 0) continue;

	  Int_t iBlock = neighbours[iN]; // channelID of neighbouring block
	  if (fBlockMapHash[iBlock] == -1) continue; // block was not fired in this event
	  Bool_t wasTested = kFALSE;
	  for (Int_t ii = 0; ii<nTestedBlocks; ii++) if (testedBlocks[ii] == iBlock) {wasTested = kTRUE; break;}

	  if (!wasTested) {
	    Int_t iHash = fBlockMapHash[iBlock]; // index in the block list
	    Int_t nhits = fNHitsPerBlock[iHash]; // number of recoHits for this block
	    Int_t* hitP = fHitPointers[iHash]; // list of indexes to recoHit list for this block
	    
	    for (Int_t j = 0; j < nhits; j++) {
	      Int_t jReco = hitP[j];
	      TRecoLAVHit* jLAVHit = static_cast<TRecoLAVHit*>( hitArray[jReco]);
	      if (!fUsedRecoHit[jReco] && ClusterMemberQuality(jLAVHit)) {
		if (CloseInTime(LAVHit, jLAVHit)) {
		  if (newClus->AddHit(jReco)) {
		    fUsedRecoHit[jReco] = kTRUE;
		    TVector3 NewblockPosition;
		    jLAVHit->GetBlockPosition(NewblockPosition);
		    if (NewblockPosition.Phi() < phiMin) phiMin = NewblockPosition.Phi();
		    if (NewblockPosition.Phi() > phiMax) phiMax = NewblockPosition.Phi();

		    if (nAcceptedBlocks < 256) acceptedBlocks[nAcceptedBlocks++] = iBlock;
		    else jError = kTRUE; 
		  }
		  else jError = kTRUE;
		}
		else if (MarginalInTime(LAVHit, jLAVHit)) { // this case will be treated in future
		}
	      }

	    }	    
	    if (nTestedBlocks < 256) testedBlocks[nTestedBlocks++] = iBlock; 
	    else jError = kTRUE; 	    
	  }
	}
	if (lastVerifiedCenter < 256) lastVerifiedCenter++;
	else jError = kTRUE;
      }

// if cluster created is good, add it to the cluster list
      
      newClus->SetPhiRange(phiMin,phiMax);
      fLAVClusters.push_back(newClus);
      fNClusters++;

    }
  }

}

void LAVClusterMaker::ComputeObservables(){
  if (fNClusters) {
    for (Int_t i=0; i< fNClusters; i++) {      
      fLAVClusters.at(i)->ComputeClusterProperties();  //      fLAVClusters.at(i)->ComputeEnergyAndTime();
    }  
  }
}


void LAVClusterMaker::Print(){
  if (fNClusters) {
    std::cout << " Printing " << fNClusters << " clusters " << std::endl;
    for (Int_t i=0; i< fNClusters; i++) {
      std::cout << "LAVClusterMaker printing Cluster " << i << " " << fLAVClusters.at(i) << std::endl;
      fLAVClusters.at(i)->Print();
    }
  }
}


Bool_t LAVClusterMaker::ClusterSeedQuality(TRecoLAVHit* hit){
  return (hit->GetEdgeMask() == 9 || hit->GetEdgeMask() == 11 || hit->GetEdgeMask() == 15);
}
Bool_t LAVClusterMaker::ClusterMemberQuality(TRecoLAVHit* hit){
  return (hit->GetEdgeMask() == 9 || hit->GetEdgeMask() == 11 || hit->GetEdgeMask() == 15);
}
Bool_t LAVClusterMaker::CloseInTime(TRecoLAVHit* ihit, TRecoLAVHit* jhit){
  Double_t dt = TMath::Abs(ihit->GetTime() - jhit->GetTime());
  return dt < fTimeDistanceMax;
}
Bool_t LAVClusterMaker::MarginalInTime(TRecoLAVHit* ihit, TRecoLAVHit* jhit){
  Double_t dt = TMath::Abs(ihit->GetTime() - jhit->GetTime());
  return dt < fTimeDistanceMarginal;
}

Int_t* LAVClusterMaker::NeighbouringBlocks(Int_t blockID){
  if (fNeighbouringBlocksHash[blockID]) return fNeighbouringBlocksHash[blockID];
  
  fNeighbouringBlocksHash[blockID] = new Int_t[8];
  for (Int_t i=0; i<8; i++) fNeighbouringBlocksHash[blockID][i] = 0;

  Int_t iStation = blockID/10000;
  Int_t NumberOfLayers   = fGeometryInstance->GetNumberOfLayers(iStation-1);
  Int_t NumberOfBananas  = fGeometryInstance->GetNumberOfBananas(iStation-1);

  Int_t iLayer  = (blockID-10000*iStation)/1000;
  Int_t iBanana = (blockID-10000*iStation-1000*iLayer)/10;
  Int_t iBlock  = blockID-10000*iStation-1000*iLayer-10*iBanana;

  Int_t iN = 0;
  for (Int_t iL = -1; iL <= 1; iL++) {
  
    Int_t testLayer = iLayer+iL; 
    if ((testLayer<0)||(testLayer>NumberOfLayers-1)) continue;
    
    for (Int_t iB = -1; iB <= 1; iB++) {

      if (iL==0 && iB==0) continue; // avoid the block under test

      Int_t testBanana = iBanana;
      Int_t testBlock = iBlock+iB;
      if (testBlock < 0) {
	testBlock = 3;
	testBanana--;
	if (testBanana < 0) testBanana = NumberOfBananas-1;
      }
      else if (testBlock > 3) {
	testBlock = 0;
	testBanana++;
	if (testBanana >= NumberOfBananas) testBanana = 0;
      }
      fNeighbouringBlocksHash[blockID][iN++] = iStation*10000 + testLayer*1000+ testBanana*10 + testBlock;      
    }
  }
  return fNeighbouringBlocksHash[blockID];
}
