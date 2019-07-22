#include "LAVTrackFinder.hh"
#include "TRecoLAVHit.hh"


LAVTrackFinder::LAVTrackFinder() :
  fInitStatus(-1),
  fNClusters(0),
  fPrintLevel(0),
  fPhiBinWidth(10./180.*TMath::Pi()), // 10 degrees
  fNPhiBins(2*TMath::Pi()/fPhiBinWidth), // 360deg / 10deg. 10 deg is ~ 1 blocks for LAVs 1-5), ~ 1.7 blocks for LAV12
  fTimeBinWidth(2.) // ns
{
}

LAVTrackFinder::~LAVTrackFinder(){
  LAVTrackFinder::Clear();
}

void LAVTrackFinder::Clear(){

// clear structures

  if ((Int_t) fLAVClusters.size()>0) {
    for (Int_t i = 0; i< (Int_t) fLAVClusters.size(); i++) {
      if (fLAVClusters.at(i)) delete fLAVClusters.at(i);
    }
    fLAVClusters.clear();
  }
  fNClusters = 0;

  for (Int_t j=0; j<12; j++) {
    fPhiRecoHit[j].clear();
    fTRecoHit[j].clear();
    fIRecoHit[j].clear();
    fTMinStation[j] = 1E28;
    fTMaxStation[j] = -1E28;
  }

}

void LAVTrackFinder::Init(TRecoLAVEvent* LAVEvent){

  LAVTrackFinder::Clear();

// retrieve residual slewing correction if needed
// initialize vectors in time and azimuthal angle

  Int_t nLAVRecoHits = LAVEvent->GetNHits();
  TClonesArray& hitArray = (* (LAVEvent->GetHits()));
  for (Int_t i=0; i<nLAVRecoHits; i++) {

    TRecoLAVHit* hit = static_cast<TRecoLAVHit*>( hitArray[i]);
    Int_t channel = hit->GetChannelID();
    Int_t station = channel/10000;

    Double_t correctedTime = hit->GetTime();

    if (HitTrackQuality(hit)){
      TVector3 blockPosition;
      hit->GetBlockPosition(blockPosition);
      
      fPhiRecoHit[station-1].push_back(blockPosition.Phi()); // between -pi and pi
      fTRecoHit[station-1].push_back(correctedTime);
      if (correctedTime < fTMinStation[station-1]) fTMinStation[station-1] = correctedTime;
      if (correctedTime > fTMaxStation[station-1]) fTMaxStation[station-1] = correctedTime;
      fIRecoHit[station-1].push_back(i);     
    }
  }
    
  fInitStatus = 0;
}

LAVCluster* LAVTrackFinder::GetCluster(Int_t i){
  if (i<fNClusters) return fLAVClusters.at(i);
  else {
    std::cout << "LAVTrackFinder >> GetCluster wrong input " << i << " " << fNClusters << std::endl;
    return NULL;
  }
}


void LAVTrackFinder::FindTracks(TRecoLAVEvent* LAVEvent){

  TClonesArray& hitArray = (* (LAVEvent->GetHits()));
  if (fInitStatus != 0) {
    std::cout << "LAVTrackFinder >> FindTracks called without initialization" << std::endl;
    return;
  }

// project

  for (Int_t iSt=0; iSt<12; iSt++) {
    Int_t nBlocksInStation = fPhiRecoHit[iSt].size();
    if (nBlocksInStation > 1) {

// define projection histograms in time and in phi

      Int_t nTimeBins = (fTMaxStation[iSt]-fTMinStation[iSt])/fTimeBinWidth+2; 

      //cout << "Station " << iSt << " nTimeBins " << nTimeBins << " start at " << fTMinStation[iSt] << " end at TMin + " << fTimeBinWidth*(nTimeBins+2)  << std::endl;

      if(nTimeBins<=0) {
        std::cerr << "[LAVTrackFinder]     WARNING: wrong nTimeBins! [nTimeBins: " << nTimeBins << " fTMaxStation: " << fTMaxStation[iSt] << " fTMinStation: " << fTMinStation[iSt] << " fTimeBinWidth: " << fTimeBinWidth << "]" << std::endl;
        return;
      }
      if(fNPhiBins<=0) {
        std::cerr << "[LAVTrackFinder]     WARNING: wrong fNPhiBins! [fNPhiBins: " << fNPhiBins << "]" << std::endl;
        return;
      }
      Int_t* timeProjection = new Int_t[nTimeBins];
      for (Int_t j=0; j<nTimeBins; j++) timeProjection[j] = 0;	
      
      Int_t* phiProjection = new Int_t[fNPhiBins];
      for (Int_t j=0; j<fNPhiBins; j++) phiProjection[j] = 0;	
      
// project

      for (Int_t i=0; i < (Int_t) fPhiRecoHit[iSt].size(); i++){

	Int_t iPhiBin = (fPhiRecoHit[iSt].at(i)+TMath::Pi())/fPhiBinWidth; 
	if (iPhiBin==fNPhiBins) iPhiBin = 0;
	if (iPhiBin > fNPhiBins || iPhiBin < 0) std::cout << "Error in phi projection! " << fPhiRecoHit[iSt].at(i) << " " << i <<  " " << iSt << " " << iPhiBin << std::endl;
	phiProjection[iPhiBin]++;

	Int_t iTimeBin = (fTRecoHit[iSt].at(i)-fTMinStation[iSt]+0.5*fTimeBinWidth)/fTimeBinWidth; 
	if (iTimeBin < 0 || iTimeBin >= nTimeBins) std::cout << "Error in time projection! " << fTRecoHit[iSt].at(i) << " " << i << " " << iSt << " " << iTimeBin << std::endl;
	timeProjection[iTimeBin]++;	
      }

// find intervals in phi

      Int_t* phiEmptyInit = new Int_t[nBlocksInStation+1];
      Int_t* phiEmptyNbin = new Int_t[nBlocksInStation+1];
      Int_t nEmptyPhi = 0;
      Int_t intervalOpen = 0;

      // first, count contiguous intervals of empty bins with length of 2 or more bins 

      for (Int_t j=0; j<fNPhiBins-1; j++) {
	if (intervalOpen){
	  if (phiProjection[j]==0) phiEmptyNbin[nEmptyPhi]++;
	  else {
	    intervalOpen = 0;
	    if (phiEmptyNbin[nEmptyPhi] > 1) nEmptyPhi++;
	  }
	}
	else {
	  if (phiProjection[j]==0) {
	    intervalOpen = 1;
	    phiEmptyInit[nEmptyPhi] = j;
	    phiEmptyNbin[nEmptyPhi] = 1;
	  }
	}
      }

      // handle separately the last block to take into account the angle continuation

      if (intervalOpen) {
	if (phiProjection[fNPhiBins-1] > 0) {
	  if (phiEmptyNbin[nEmptyPhi] > 1) nEmptyPhi++;
	}
	else {
	  phiEmptyNbin[nEmptyPhi]++;
	  if (phiEmptyInit[0] == 0) { // merge last interval to the first
	    phiEmptyInit[0] = phiEmptyInit[nEmptyPhi] - fNPhiBins;
	    phiEmptyNbin[0]+= phiEmptyNbin[nEmptyPhi];
	  }
	  else {
	    if (phiProjection[0] == 0) phiEmptyNbin[nEmptyPhi]++; // add isolated empty first block to current interval
	    nEmptyPhi++;
	  }
	}
      }
      else { // interval closed
	if (phiProjection[fNPhiBins-1] == 0) {
	  if (phiEmptyInit[0] == 0) { //  add isolated empty last block to first interval
	    phiEmptyInit[0] = -1;
	    phiEmptyNbin[0]++;
	  }
	  else {
	    if (phiProjection[0] == 0) { // merge isolated empty last and first blocks into a single interval
	      phiEmptyInit[nEmptyPhi] = fNPhiBins-1;
	      phiEmptyNbin[nEmptyPhi] = 2;
	      nEmptyPhi++;
	    }
	  }
	}
      }

// find intervals in phi

      Int_t* phiIntervalsInit = new Int_t[nBlocksInStation];
      Int_t* phiIntervalsNbin = new Int_t[nBlocksInStation];
      Int_t* phiIntervalsSum  = new Int_t[nBlocksInStation];      
      Int_t nIntervalsPhi = 0;

      if (nEmptyPhi == 0) { // all bins are non empty
	phiIntervalsInit[0] = 0;
	phiIntervalsNbin[0] = fNPhiBins;
	nIntervalsPhi = 1;
      }
      else if (nEmptyPhi == 1) { // only 1 empty interval
	phiIntervalsInit[0] = phiEmptyInit[0] + phiEmptyNbin[0];
	phiIntervalsNbin[0] = fNPhiBins - phiEmptyNbin[0];
	nIntervalsPhi = 1;
      }
      else { // multiple empty intervals
	for (Int_t iE = 0; iE<nEmptyPhi-1; iE++) {
	  phiIntervalsInit[nIntervalsPhi] = phiEmptyInit[iE] + phiEmptyNbin[iE];
	  phiIntervalsNbin[nIntervalsPhi] = phiEmptyInit[iE+1] - phiIntervalsInit[nIntervalsPhi];
	  nIntervalsPhi++;
	}
	phiIntervalsInit[nIntervalsPhi] = phiEmptyInit[nEmptyPhi-1] + phiEmptyNbin[nEmptyPhi-1];
	phiIntervalsNbin[nIntervalsPhi] = phiEmptyInit[0] + fNPhiBins - phiIntervalsInit[nIntervalsPhi] ;
	if (phiIntervalsInit[nIntervalsPhi] >= fNPhiBins) phiIntervalsInit[nIntervalsPhi] -= fNPhiBins; 
	nIntervalsPhi++;
      }

      for (Int_t iP = 0; iP<nIntervalsPhi; iP++) {
	if (phiIntervalsInit[iP] >= fNPhiBins) phiIntervalsInit[iP] -= fNPhiBins; // make sure all intervals start in the [0,2pi) range	
	
	phiIntervalsSum[iP] = 0;
	for (Int_t iB = phiIntervalsInit[iP]; iB< phiIntervalsInit[iP] + phiIntervalsNbin[iP]; iB++) {
	  if (iB < fNPhiBins) phiIntervalsSum[iP] += phiProjection[iB];
	  else phiIntervalsSum[iP] += phiProjection[iB-fNPhiBins];
	}
      }

// Find contigous intervals in time

      Int_t* timeIntervalsInit = new Int_t[nBlocksInStation];
      Int_t* timeIntervalsNbin = new Int_t[nBlocksInStation];
      Int_t* timeIntervalsSum  = new Int_t[nBlocksInStation];
      Int_t nIntervalsTime = 0;

      Int_t intervalStart;

      intervalOpen = 0;
      Int_t intervalBins = 0;
      Int_t intervalSum = 0;
      for (Int_t j=0; j<nTimeBins; j++) {

	if (intervalOpen){
	  if (timeProjection[j]==0) {
	    timeIntervalsInit[nIntervalsTime] = intervalStart;
	    timeIntervalsNbin[nIntervalsTime] = intervalBins;
	    timeIntervalsSum[nIntervalsTime] = intervalSum;
	    nIntervalsTime++;
	    intervalOpen = 0;
	    intervalBins = 0;
	    intervalSum = 0;
	  }
	  else {
	    intervalBins ++;
	    intervalSum += timeProjection[j];
	  }
	}
	else {
	  if (timeProjection[j]) {
	    intervalOpen = 1;
	    intervalBins = 1;
	    intervalStart = j;
	    intervalSum = timeProjection[j];	  
	  }
	}
      }
      if (intervalOpen) {
	timeIntervalsInit[nIntervalsTime] = intervalStart;
	timeIntervalsNbin[nIntervalsTime] = intervalBins;
	timeIntervalsSum[nIntervalsTime] = intervalSum;
	nIntervalsTime++;	
      }

      if (fPrintLevel == 1) {
	cout << "LAVTrackFinder >> Projection in phi with " << fNPhiBins << std::endl;
	for (Int_t j=0; j<fNPhiBins; j++) if (phiProjection[j]) std::cout << "PhiBin " << j << " ProjectionCounts= " <<  phiProjection[j] << std::endl;	

	cout << "NIntervalsPhi " << nIntervalsPhi << std::endl;
	for (Int_t j=0; j<nIntervalsPhi; j++) {
	  std::cout << " Interval " << j ;
	  std::cout << " " << phiIntervalsInit[j];
	  std::cout << " " << phiIntervalsNbin[j];
	  std::cout << " " << phiIntervalsSum[j] << std::endl;	
	}
	
	cout << "Projection in time with " << nTimeBins << std::endl;
	for (Int_t j=0; j<nTimeBins; j++) if (timeProjection[j]) std::cout << "TimeBin " << j << " ProjectionCounts= " <<  timeProjection[j] << std::endl;	
	
	cout << "NIntervalsTime " << nIntervalsTime << std::endl;
	for (Int_t j=0; j<nIntervalsTime; j++) {
	  std::cout << " Interval " << j ;
	  std::cout << " " << timeIntervalsInit[j];
	  std::cout << " " << timeIntervalsNbin[j];
	  std::cout << " " << timeIntervalsSum[j] << std::endl;
	}
	
	cout << "Print inner arrays " << (Int_t) fPhiRecoHit[iSt].size() << std::endl;
	for (Int_t i=0; i < (Int_t) fPhiRecoHit[iSt].size(); i++){
	  TRecoLAVHit* hit = static_cast<TRecoLAVHit*>( hitArray[fIRecoHit[iSt].at(i)]);
	  std::cout << "Blk " << hit->GetChannelID() << " Phi " << fPhiRecoHit[iSt].at(i) << " Time = " << fTRecoHit[iSt].at(i) << " Torig = " << hit->GetTime() << " edge = " << hit->GetEdgeMask() << std::endl;
	}      
      }

// select intervals and clusterize
      
      for (Int_t iT=0; iT<nIntervalsTime; iT++) {
	if (timeIntervalsSum[iT] < 2) continue;
	Double_t tMin = fTMinStation[iSt]-0.5*fTimeBinWidth + fTimeBinWidth*timeIntervalsInit[iT];
	Double_t tMax = tMin + fTimeBinWidth*timeIntervalsNbin[iT];
	
	for (Int_t iP=0; iP<nIntervalsPhi; iP++) {
	  if (phiIntervalsSum[iP] < 2) continue;
	  Double_t phiMin = -TMath::Pi() + fPhiBinWidth*phiIntervalsInit[iP];
	  Double_t phiMax = phiMin + fPhiBinWidth*phiIntervalsNbin[iP];
	  
	  Int_t nHitsPerCluster=0;
	  Int_t* iHitPerCluster = new Int_t[NMAXHITSPERCLUSTER];
	  for (Int_t iN=0; iN<(Int_t) fIRecoHit[iSt].size(); iN++) {
	    if (
		fTRecoHit[iSt].at(iN) < tMax && fTRecoHit[iSt].at(iN) > tMin && 
		( (fPhiRecoHit[iSt].at(iN) > phiMin && fPhiRecoHit[iSt].at(iN) < phiMax) || 
		  (phiMax > TMath::Pi() && (fPhiRecoHit[iSt].at(iN) > phiMin || fPhiRecoHit[iSt].at(iN) < phiMax-2*TMath::Pi()) )
		  )) {
	      if (nHitsPerCluster < NMAXHITSPERCLUSTER) {
		iHitPerCluster[nHitsPerCluster] = fIRecoHit[iSt].at(iN);
		nHitsPerCluster++;
	      }
	    }
	  }
	  if (nHitsPerCluster > 1) {

	    if (fPrintLevel == 1) std::cout << "LAVTrackFinder >> Creating new Cluster with "<< nHitsPerCluster << " hits " << std::endl;

	    Int_t iBlocksInCluster[10000]={0};
	    LAVCluster* newCluster = new LAVCluster();
	    newCluster->SetEvent(LAVEvent);
	    for (Int_t iH=0; iH<nHitsPerCluster; iH++) {
	      if (newCluster->AddHit(iHitPerCluster[iH])) {
		TRecoLAVHit* hit = static_cast<TRecoLAVHit*>( hitArray[iHitPerCluster[iH]]);
		Int_t chid = hit->GetChannelID()-10000*(iSt+1);

		if (iBlocksInCluster[chid] != 0) {
		  TRecoLAVHit* hitold = static_cast<TRecoLAVHit*>( hitArray[iHitPerCluster[iBlocksInCluster[chid]-1]]);
		  std::cout << "TrackFinder >> double block in cluster " << chid << " " << iBlocksInCluster[chid] << " " << iSt+1 << " " << hit->GetTime() << " " << hitold->GetTime() << std::endl;
		}
		iBlocksInCluster[chid] = iH+1;
	      }
	    }
	    newCluster->SetPhiRange(phiMin,phiMax);
	    newCluster->ComputeClusterProperties();
	    fLAVClusters.push_back(newCluster);
	  }
	  delete[] iHitPerCluster;
	}
      }

      if (fPrintLevel == 1) {
	cout << "LAVTrackFinder >>  In this event " << (Int_t) fLAVClusters.size() << " clusters are created " << std::endl;
	for (Int_t iC=0; iC < (Int_t) fLAVClusters.size(); iC++) {
	  fLAVClusters.at(iC)->Print();
	}
      }
      
      delete[] phiEmptyInit;
      delete[] phiEmptyNbin;
      delete[] phiProjection;
      delete[] timeProjection;

      delete[] phiIntervalsInit;
      delete[] phiIntervalsNbin;
      delete[] phiIntervalsSum ;
      delete[] timeIntervalsInit;
      delete[] timeIntervalsNbin;
      delete[] timeIntervalsSum ;

    }
  }
  fNClusters = fLAVClusters.size();
}


Bool_t LAVTrackFinder::HitTrackQuality(TRecoLAVHit* hit){
  Int_t edgeMask = hit->GetEdgeMask();
  if (edgeMask == 3 ||edgeMask == 9 || edgeMask == 11 || edgeMask == 15) return kTRUE;
  return kFALSE;
}
