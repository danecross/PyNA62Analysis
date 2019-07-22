// ---------------------------------------------------------------
// History:
//
// Created by Tommaso Spadaro (tommaso.spadaro@lnf.infn.it) 2016-02-26
//
// ---------------------------------------------------------------

/// \class SAVMatching
/// \Brief
/// Provides the output of various algorithms for time-matching SAV information with respect to a given reference detector. Only TDC-based data is used.
/// \EndBrief
/// \Detailed
/// Provided a reference time is given in input, the class will search for IRC, SAC hits in time, withing a given window (default +-5ns).
/// Among all of the reconstructed hits in time, only specific edge-combination types are considered.
/// Edge combination ("edge mask") is ranked from the most-ill defined (isolated trailings for low or high threshold),
/// to the most complete (both edges of low and high threshold). 
/// A "priority" of the edgemasks is therefore build, defined as follows:<br>
///  SAVPriorityMask = 0; // --SNH--    <br>
///  SAVPriorityMask = 1; // __ __ __ TL<br>
///  SAVPriorityMask = 2; // __ __ TH __<br>
///  SAVPriorityMask = 3; // __ __ TH TL<br>
///  SAVPriorityMask = 4; // LL __ __ __<br>
///  SAVPriorityMask = 5; // __ LH __ __ <br>
///  SAVPriorityMask = 6; // __ LH __ TL<br>
///  SAVPriorityMask = 7; // LL __ TH __<br>
///  SAVPriorityMask = 8; // LL __ TH TL<br>
///  SAVPriorityMask = 9; // __ LH TH TL <-- to be selected <br>
///  SAVPriorityMask =10; // LL LH __ __ <-- to be selected <br>
///  SAVPriorityMask =11; // __ LH TH __ <-- to be selected <br>
///  SAVPriorityMask =12; // LL __ __ TL <-- to be selected <br>
///  SAVPriorityMask =13; // LL LH TH __ <-- to be selected <br>
///  SAVPriorityMask =14; // LL LH __ TL <-- to be selected <br>
///  SAVPriorityMask =15; // LL LH TH TL <-- to be selected <br>
///  \n
/// The integer SAVHasTimeMatching method returns a word with bit 0(1) set if at least one IRC(SAC) hit is found in time with priority >=9. All other bits of the word are set to 0.
/// The list of the blocks in time with the reference detector (of all priorities) can be obtained as output via the GetIndexOfIRCMatchedBlocks and GetIndexOfSACMatchedBlocks methods.
/// The highest priority available ("bestEdgeMask") can be output via the GetBestEdgeMaskOfIRCMatchedBlocks() and GetBestEdgeMaskOfSACMatchedBlocks() methods.
/// The time difference with the smallest absolute value can be output via the GetBestTimeOfIRCMatchedBlocks() and GetBestTimeOfSACMatchedBlocks() methods.
/// The edgemasks of the "best" hits with the smallest time differences is accessed with GetEdgeMaskOfBestIRCHit() and GetEdgeMaskOfBestSACHit() methods.<br><br>
/// In the reconstruction algorithm, hits time are slewing-corrected assuming a linear signal between low- and high-threshold leading times.
/// Hits with no high-threshold leading are not corrected.
/// By default, additional slewing corrections are applied here. These are parametrized as a function of the time over the low threshold (trailing minus leading).
/// Slewing corrections parameters are retrieved for data 2015 and for data 2016 (as defined by run-number).
/// For 2016 p+p0 events, it has been assessed that improvement of time resolution is achieved with additional slewing corrections, passing from 810 ps to 560 ps for SAC and
/// from 1.2 ns to 820 ps for IRC (reference time: RICH). The number of matched events in a 5-ns window slightly increase, by 3% for SAC and by 10% for IRC.
/// The initialization call can be performed transparent to the user by using the PhotonVetoHandler analyzer.
/// \n Example use-case code exploiting the PhotonVetoHandler analyzer:
/// \code
///  SAVMatching* pSAVMatching = (SAVMatching*)GetOutput("PhotonVetoHandler.SAVMatching"); 
///  pSAVMatching->SetReferenceTime(yourReferenceTime); 
///  Int_t SAVMatchedFlag = pSAVMatching->SAVHasTimeMatching(fIRCEvent, fSACEvent);
///  if (fVerboseFlag) {
///   cout << "Did any of SAV match? " << matched << endl;
///   pSAVMatching->Print();
///  }
///  if (SAVMatchedFlag) --> reject the event
/// \endcode
/// If the user wants to apply SAC and IRC TELL-Based matching without slewing corrections applied, 
/// same code as above except:
/// \code
///  Int_t SAVMatchedFlag = pSAVMatching->SAVHasTimeMatching(fIRCEvent, fSACEvent, 1);
/// \endcode
/// If using the class standalone, the MC flag should be set by the user in the startOfBurst (default: 2016 data):
/// \code
/// // Init part of an Analyzer
///   fSAVMatching = new SAVMatching(); // fSAVMatching is a private SAVMatching* variable
/// // StartOfBurstUser part of an Analyzer:
///   fSAVMatching->SetIsMC(GetWithMC());
///   fSAVMatching->InitializeInputForSlewing();
/// // Event process part of an Analyzer:
///   fSAVMatching->SetReferenceTime(yourReferenceTime);
///   Bool_t matched = fSAVMatching->SAVHasTimeMatching(IRCEvent,SACEvent); // input 1 as third argument if no additional slewing correction has to be applied.
///   if (fVerboseFlag) {	
///     cout << "Did SAV match? " << matched << endl;
///     fSAVMatching->Print();
///   }
/// \endcode
/// \author Tommaso Spadaro (tommaso.spadaro@lnf.infn.it)
/// \EndDetailed

#include "SAVMatching.hh"
#include "NA62ConditionsService.hh"

using namespace std;
using namespace NA62Analysis;

SAVMatching::SAVMatching()
{
  Clear();
  fRefTime = -999;
  fIsMC = kFALSE; // default
  fSAVEvent[0] = nullptr;
  fSAVEvent[1] = nullptr;

  fSAVPriorityMask[0] = 0; // --SNH--
  fSAVPriorityMask[1] = 4; // LL __ __ __ 
  fSAVPriorityMask[2] = 5; // __ LH __ __ 
  fSAVPriorityMask[3] =10; // LL LH __ __ <-- to be selected
  fSAVPriorityMask[4] = 2; // __ __ TH __
  fSAVPriorityMask[5] = 7; // LL __ TH __
  fSAVPriorityMask[6] =11; // __ LH TH __ <-- to be selected
  fSAVPriorityMask[7] =13; // LL LH TH __ <-- to be selected
  fSAVPriorityMask[8] = 1; // __ __ __ TL
  fSAVPriorityMask[9] =12; // LL __ __ TL <-- to be selected
  fSAVPriorityMask[10]= 6; // __ LH __ TL
  fSAVPriorityMask[11]=14; // LL LH __ TL <-- to be selected
  fSAVPriorityMask[12]= 3; // __ __ TH TL
  fSAVPriorityMask[13]= 8; // LL __ TH TL
  fSAVPriorityMask[14]= 9; // __ LH TH TL <-- to be selected
  fSAVPriorityMask[15]=15; // LL LH TH TL <-- to be selected

  fLowThrSAVTimeCut[0] = 5.; // [ns] default for IRC
  fHighThrSAVTimeCut[0] = 5.; // [ns] default for IRC
  fLowThrSAVTimeCut[1] = 5.; // [ns] default for SAC
  fHighThrSAVTimeCut[1] = 5.; // [ns] default for SAC

  // input slewing corrections
  fSlewingInputRetrieved = kFALSE;
  fMaxTOTBins = 1000; // Maximum number of bins allowed
  fNTOTBins[0] = 0; // number of bins retrieved for the IRC
  fNTOTBins[1] = 0; // number of bins retrieved for the SAC

  // allocate arrays
  for (Int_t iDet = 0; iDet<2; iDet++) {
    fTOTLow[iDet] = new Double_t[fMaxTOTBins];
    fAverageCorrection[iDet] = new Double_t*[4];
    for (Int_t channelID=0; channelID<4; channelID++) fAverageCorrection[iDet][channelID] = new Double_t[fMaxTOTBins];
  }
}

SAVMatching::~SAVMatching(){
  for (Int_t iDet = 0; iDet<2; iDet++) {
    delete[] fTOTLow[iDet];
    for (Int_t channelID=0; channelID<4; channelID++) delete[] fAverageCorrection[iDet][channelID];
    delete[] fAverageCorrection[iDet];
  }
}

Int_t SAVMatching::SAVHasTimeMatching(TRecoIRCEvent* IRCEvent, TRecoSACEvent* SACEvent, Int_t mode) {
  // By default, mode==0 so the slewing corrections are applied

  for (Int_t i=0; i<2; i++) {
    fNMatched[i] = 0;
    fBestEdgeMask[i] = 0;
    fEdgeMaskOfBestHit[i] = -1;
    fDeltaTBest[i] = 999.;
  }
  fSAVEvent[0] = IRCEvent;
  fSAVEvent[1] = SACEvent;
  Int_t outputFlag = 0;

  for (Int_t iDet = 0; iDet<2; iDet++) {
    TClonesArray& hitArray = (*(fSAVEvent[iDet]->GetHits()));

    for (Int_t i=0; i< fSAVEvent[iDet]->GetNHits(); i++) {
      Int_t edgeMask = (iDet==0) ?
	static_cast<TRecoIRCHit*>(hitArray[i])->GetEdgeMask() :
	static_cast<TRecoSACHit*>(hitArray[i])->GetEdgeMask();

      TRecoVHit* hit = static_cast<TRecoVHit*>(hitArray[i]);
      int chid = hit->GetChannelID();
      Double_t dt = hit->GetTime() - fRefTime;
      if (!fIsMC && mode == 0) { // apply slewing corrections, for data only
	Double_t dtSlew = dt - GetSlewingCorrection(iDet,hit);
	if (TMath::Abs(dtSlew) < TMath::Abs(dt)) dt = dtSlew;
      }
      if ( (edgeMask & 1 && edgeMask & 2) && TMath::Abs(dt)>fHighThrSAVTimeCut[iDet]) continue;
      if (!(edgeMask & 1 && edgeMask & 2) && TMath::Abs(dt)>fLowThrSAVTimeCut[iDet] ) continue;

      bool noisymatch = kFALSE;
      for (int is = 0; is< (int) fNoisyChannels[iDet].size(); is++){
	if (fNoisyChannels[iDet].at(is) == hit->GetChannelID()) {
	  noisymatch = kTRUE;
	  break;
	}
      }
      if (noisymatch) continue;

      if (fSAVPriorityMask[edgeMask] > fBestEdgeMask[iDet]) fBestEdgeMask[iDet] = fSAVPriorityMask[edgeMask];

      if (fSAVPriorityMask[edgeMask] >= 9) {
	if (fabs(dt) < fabs(fDeltaTBest[iDet])) {
	  fDeltaTBest[iDet] = dt;
	  fEdgeMaskOfBestHit[iDet] = edgeMask;
	}

	if (fNMatched[iDet]<100) {
	  fIndexMatched[iDet][fNMatched[iDet]] = i;
	  fChannelIDMatched[iDet][fNMatched[iDet]] = chid;
	  fNMatched[iDet]++;
	}
      }
    }
    if (fBestEdgeMask[iDet]>=9) outputFlag |= (1<<iDet); 
  }
  return outputFlag;
}

void SAVMatching::Clear() {
  for (Int_t i=0; i<2; i++) {
    fNMatched[i] = 0;
    fBestEdgeMask[i] = 0;
    fEdgeMaskOfBestHit[i] = -1;
    fDeltaTBest[i] = 999;
    fNoisyChannels[i].clear();
  }
  fRefTime = -1000000; 
}

void SAVMatching::Print() {
  cout << "[SAVMatching]: Printing---" << endl;
  cout << "Have matched " << fNMatched[0] << " IRC blocks, the best of which has edgemask priority (ranging from 1 to 15) = " << fBestEdgeMask[0] << endl;
  cout << "Have matched " << fNMatched[1] << " SAC blocks, the best of which has edgemask priority (ranging from 1 to 15) = " << fBestEdgeMask[1] << endl;

  if (!(fSAVEvent[0]) || !(fSAVEvent[1])) {
    cout << "Wrong SAVEvents provided " << fSAVEvent[0] << " " << fSAVEvent[1] << endl;
    return;
  }
  TString DetS[2] = {"IRC","SAC"};
  for (Int_t iDet = 0; iDet<2; iDet++) {
    TClonesArray& hitArray = (* (fSAVEvent[iDet]->GetHits()));  

    for (Int_t i=0; i<fNMatched[iDet]; i++) {
      if (fIndexMatched[iDet][i]<0 || fIndexMatched[iDet][i]>= fSAVEvent[iDet]->GetNHits()) {
	cout << " SAVMatching Print: wrong index stored or wrong SAVEvent provided " << i << " " << fIndexMatched[iDet][i] << " " << fSAVEvent[iDet]->GetNHits() << DetS[iDet].Data()<<endl;
	return;
      }
      // TRecoVHit* hit = (TRecoVHit*) hitArray[fIndexMatched[iDet][i]];
      Int_t edgeMask;
      if (iDet==0) edgeMask = static_cast<TRecoIRCHit*>(hitArray[i])->GetEdgeMask();
      else edgeMask = static_cast<TRecoSACHit*>(hitArray[i])->GetEdgeMask();

      cout << " Matched block " << i << " chid " << fChannelIDMatched[iDet][i] << " edge " << edgeMask << " priority " << fSAVPriorityMask[edgeMask] << DetS[iDet].Data()<< endl;
    }
  }
  cout << "[SAVMatching]: Printing end" << endl;
}

Double_t SAVMatching::GetSlewingCorrection(Int_t iDet, TRecoVHit* hit){
  if (!fSlewingInputRetrieved) return 0;

  Int_t edgeMask;
  Double_t tot;
  if (iDet==0) edgeMask = static_cast<TRecoIRCHit*>(hit)->GetEdgeMask();
  else         edgeMask = static_cast<TRecoSACHit*>(hit)->GetEdgeMask();

  if (!(edgeMask & 1 && edgeMask & 8)) return 0.0;

  if (iDet==0) tot = static_cast<TRecoIRCHit*>(hit)->GetTimeOverThresholdLowThr();
  else         tot = static_cast<TRecoSACHit*>(hit)->GetTimeOverThresholdLowThr();

  int chid = hit->GetChannelID();

  if (tot<fTOTLow[iDet][0]) return fAverageCorrection[iDet][chid][0];
  if (tot>fTOTLow[iDet][fNTOTBins[iDet]-1]) return fAverageCorrection[iDet][chid][fNTOTBins[iDet]-1];

  Int_t bin = (tot - fTOTLow[iDet][0])/fTOTBinning+0.5;
  Double_t slope = 0;
  if (bin < fNTOTBins[iDet]-1) slope = (fAverageCorrection[iDet][chid][bin+1]-fAverageCorrection[iDet][chid][bin])/fTOTBinning;

  // cout << "Get Slewing correction " << iDet << " edgeMask " << edgeMask << " tot=" << tot << " correction = " << 
  //   fAverageCorrection[iDet][chid][bin] << " additionalCorr = " << slope*(tot-fTOTLow[iDet][bin]) << endl;

  return fAverageCorrection[iDet][chid][bin] + slope*(tot-fTOTLow[iDet][bin]);
}

void SAVMatching::InitializeInputForSlewing() {
  if (fSlewingInputRetrieved) return;
  if (fIsMC) return; // no corrections for MC

  TString DetName[2] = {"IRC","SAC"};
  for (Int_t iDet=0; iDet<2; iDet++) { // detector loop: IRC, SAC

    // Read corrections from the CDB
    TString FileName = Form("%s-ResidualSlewing.dat", DetName[iDet].Data());
    if (NA62ConditionsService::GetInstance()->Open(FileName)==kSuccess) {
      TString Line;
      while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(FileName)) &&
	     fNTOTBins[iDet] < fMaxTOTBins) {
	TObjArray * l = Line.Tokenize(" ");
	Int_t numberOfStrings = l->GetEntriesFast();
	if (numberOfStrings != 5) {
	  cout << "[SAVMatching] Wrong string in slewing file: " << numberOfStrings << "---" << Line.Data() << endl;
	  exit(kGenericError);
	}
	fTOTLow[iDet][fNTOTBins[iDet]] = ((TObjString*)(l->At(0)))->GetString().Atof();
	for (Int_t channelID=0; channelID<4; channelID++) {
	  fAverageCorrection[iDet][channelID][fNTOTBins[iDet]] = ((TObjString*)(l->At(1+channelID)))->GetString().Atof();
	}
	delete l;
	fNTOTBins[iDet]++;
      }
      NA62ConditionsService::GetInstance()->Close(FileName);
    }

    if (fNTOTBins[iDet] == fMaxTOTBins) {
      cout << "[SAVMatching] Max number of bins in slewing file reached: " << DetName[iDet].Data() << endl;
      exit(kGenericError);
    }
    if (fNTOTBins[iDet] <= 1) {
      cout << "[SAVMatching] No bins in slewing file found: " << DetName[iDet].Data() << endl;
      exit(kGenericError);
    }
    Double_t binning = fTOTLow[iDet][fNTOTBins[iDet]-1] - fTOTLow[iDet][fNTOTBins[iDet]-2];
    if (iDet == 0) fTOTBinning = binning;
    else if (TMath::Abs(fTOTBinning - binning) > 1.E-3) {
      cout << "[SAVMatching] different binning in slewing file found: " << fTOTBinning << " != " << binning << endl;
      exit(kGenericError);
    }
  }
  fSlewingInputRetrieved = kTRUE;
}
