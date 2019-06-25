// ---------------------------------------------------------------
// History:
//
// Created by Tommaso Spadaro (tommaso.spadaro@lnf.infn.it) 2015-12-13
//
// ---------------------------------------------------------------

#include <iostream>
#include "LAVMatching.hh"

using namespace std;

/// \class LAVMatching
/// \Brief
/// Provides the output of various algorithms for time-matching LAV information with respect to a given reference detector
/// \EndBrief
/// \Detailed
/// Provided a reference time is given in input, the class will search for LAV blocks hits in time, withing a given window (default +-5ns).
/// By default, certain blocks are masked as noisy when allocating an object of type LAVMatching.
/// Among all of the reconstructed hits in time, only specific edge-combination types are considered.
/// Edge combination ("edge mask") is ranked from the most-ill defined (isolated trailings for low or high threshold),
/// to the most complete (both edges of low and high threshold). 
/// A "priority" of the edgemasks is therefore build, defined as follows:<br>
///  LAVPriorityMask = 0; // --SNH--     <br>
///  LAVPriorityMask = 1; // __ __ __ TL <br>
///  LAVPriorityMask = 2; // __ __ TH __ <br>
///  LAVPriorityMask = 3; // __ __ TH TL <br>
///  LAVPriorityMask = 4; // LL __ __ __ <br>
///  LAVPriorityMask = 5; // __ LH __ __ <br>
///  LAVPriorityMask = 6; // __ LH __ TL <br>
///  LAVPriorityMask = 7; // LL __ TH __ <br>
///  LAVPriorityMask = 8; // LL __ TH TL <br>
///  LAVPriorityMask = 9; // __ LH TH TL <-- to be selected <br>
///  LAVPriorityMask =10; // LL LH __ __ <-- to be selected <br>
///  LAVPriorityMask =11; // __ LH TH __ <-- to be selected <br>
///  LAVPriorityMask =12; // LL __ __ TL <-- to be selected <br>
///  LAVPriorityMask =13; // LL LH TH __ <-- to be selected <br>
///  LAVPriorityMask =14; // LL LH __ TL <-- to be selected <br>
///  LAVPriorityMask =15; // LL LH TH TL <-- to be selected <br>
///  \n
/// The first L/T letters in letter pairs above mean leading/trailing edges; the second L/H mean low/high thresholds.
/// The boolean LAVHasTimeMatching method returns true if at least one block is found in time with priority >=9.
/// The list of the blocks in time with the reference detector (of all priorities) can be obtained as output via the GetIndexOfMatchedBlocks method.
/// The highest priority available ("bestEdgeMask") can be output via the GetBestEdgeMaskOfMatchedBlocks method.
/// The time difference with the smallest absolute value can be output via the GetBestTimeOfMatchedBlocks. 
/// For further details, see: https://indico.cern.ch/event/489423/contribution/1/attachments/1217640/1778693/LAVMatchingClass_pinunu_wg_26_1_2016-1.pdf
/// \n Example use-case code:
/// \code
/// // Init part of an Analyzer
/// fLAVMatching = new LAVMatching(); // fLAVMatching is a private LAVMatching* variable
/// ...
/// // Event processing part of an Analyzer:
/// fLAVMatching->SetReferenceTime(CHODTime);
/// Bool_t matched = fLAVMatching->LAVHasTimeMatching(LAVEvent);
/// if (fVerboseFlag) {	
///   cout << "Did LAV match? " << matched << endl;
///   fLAVMatching->Print();
/// }
/// \endcode
/// NB: LAVMatching should always be used via PhotonVetoHandler to achieve optimal results.
/// \author Tommaso Spadaro (tommaso.spadaro@lnf.infn.it)
/// \EndDetailed

LAVMatching::LAVMatching() :
  fBurstID(0),
  fRefTime(-999),
  fLowThrLAVTimeCut (5.0), // ns
  fHighThrLAVTimeCut(5.0), // ns
  fLAVEvent(nullptr)
{

  fLAVPriorityMask[0] = 0; // --SNH--
  fLAVPriorityMask[1] = 4; // LL __ __ __ 
  fLAVPriorityMask[2] = 5; // __ LH __ __ 
  fLAVPriorityMask[3] =10; // LL LH __ __ <-- to be selected
  fLAVPriorityMask[4] = 2; // __ __ TH __
  fLAVPriorityMask[5] = 7; // LL __ TH __
  fLAVPriorityMask[6] =11; // __ LH TH __ <-- to be selected
  fLAVPriorityMask[7] =13; // LL LH TH __ <-- to be selected
  fLAVPriorityMask[8] = 1; // __ __ __ TL
  fLAVPriorityMask[9] =12; // LL __ __ TL <-- to be selected
  fLAVPriorityMask[10]= 6; // __ LH __ TL
  fLAVPriorityMask[11]=14; // LL LH __ TL <-- to be selected
  fLAVPriorityMask[12]= 3; // __ __ TH TL
  fLAVPriorityMask[13]= 8; // LL __ TH TL
  fLAVPriorityMask[14]= 9; // __ LH TH TL <-- to be selected
  fLAVPriorityMask[15]=15; // LL LH TH TL <-- to be selected


  // When calling LAVMatching via PhotonVetoHandler, the noisy channel list
  // is re-initialized burst-by-burst from the via the ConditionsService.
  // LAVMatching should always be used via PhotonVetoHandler to achieve optimal results.

  ClearNoisyChannels();
  UnmaskAllStations();
}

Bool_t LAVMatching::ChannelIsNoisy(Int_t id) {
  vector<Int_t>::iterator i = find(fNoisyChannels[fBurstID].begin(), fNoisyChannels[fBurstID].end(), id);
  return (i!=fNoisyChannels[fBurstID].end());
}

Bool_t LAVMatching::LAVHasTimeMatching(TRecoLAVEvent* LAVEvent) {
  fLAVEvent = LAVEvent;
  TClonesArray& hitArray = (*(LAVEvent->GetHits()));

  fNMatched = 0;
  fNMatchedLowThr = 0;
  fNMatchedHighThr = 0;
  fBestEdgeMask = 0;
  fEdgeMaskOfBestHit = -1;
  fDeltaTBest = -99999.;

  for (Int_t i=0; i<LAVEvent->GetNHits(); i++) {

    TRecoLAVHit* hit = static_cast<TRecoLAVHit*>(hitArray[i]);
    int station = hit->GetLAVID()-1;
    if (fMaskedStations[station]) continue;

    Int_t chid = hit->GetChannelID();
    Double_t dt = hit->GetTime() - fRefTime;

    if ( (hit->GetEdgeMask() & 1 && hit->GetEdgeMask() & 2) && TMath::Abs(dt)>fHighThrLAVTimeCut) continue; 
    if (!(hit->GetEdgeMask() & 1 && hit->GetEdgeMask() & 2) && TMath::Abs(dt)>fLowThrLAVTimeCut ) continue;

    Bool_t noisymatch = kFALSE;
    for (UInt_t is=0; is<fNoisyChannels[fBurstID].size(); is++) {
      if (fNoisyChannels[fBurstID].at(is) == hit->GetChannelID()) {
	noisymatch = kTRUE;
	break;
      }
    }
    if (noisymatch) continue;

    if (hit->GetEdgeMask() & 1 && hit->GetEdgeMask() & 8) fNMatchedLowThr++;
    if (hit->GetEdgeMask() & 2 && hit->GetEdgeMask() & 4) fNMatchedHighThr++;
    if (fLAVPriorityMask[hit->GetEdgeMask()] > fBestEdgeMask) fBestEdgeMask = fLAVPriorityMask[hit->GetEdgeMask()];

    if (fLAVPriorityMask[hit->GetEdgeMask()] >= 9) {
      if (fabs(dt) < fabs(fDeltaTBest)) {
	fDeltaTBest = dt;
	fEdgeMaskOfBestHit = hit->GetEdgeMask();
      }
      if (fNMatched<100) {
	fIndexMatched[fNMatched] = i;
	fChannelIDMatched[fNMatched] = chid;
	fNMatched++;
      }
    }
  }
  return (fBestEdgeMask>=9);
}

void LAVMatching::SetMaskedStation(Int_t val, Bool_t masked) {
  if (val<1 || val>12) { // station IDs are 1-12
    cout << "[LAVMatching::SetMaskedStation] Warning: input station ID ("<<val<<") out of allowed range [1,12]" << endl;
    return;
  }
  fMaskedStations[val-1] = masked; // indices of fMaskedStations are 0-11
}

void LAVMatching::MaskAllStations() {
  for (Int_t i=1; i<=12; i++) SetMaskedStation(i, kTRUE);
}
void LAVMatching::UnmaskAllStations() {
  for (Int_t i=1; i<=12; i++) SetMaskedStation(i, kFALSE);
}

// Default for noisy channels for 2015 data (runs 3809,4068/9,4141)
// LAVMatching should be used via PhotonVetoHandler to handle noisy channel lists dynamically.
void LAVMatching::InitializeNoisyChannels2015() {
  ClearNoisyChannels();
  for (Int_t iBur=0; iBur<5000; iBur++) {
    SetNoisyChannel(iBur, 84083);  // 227 of LAV8
    SetNoisyChannel(iBur, 90083);  //  35 of LAV9
    SetNoisyChannel(iBur, 103040); // 196 of LAV10
    SetNoisyChannel(iBur, 121101); // 105 of LAV12
    SetNoisyChannel(iBur, 23083);  // 227 of LAV12
  }
}

void LAVMatching::ClearNoisyChannels() {
  for (Int_t iBur=0; iBur<5000; iBur++) fNoisyChannels[iBur].clear();
}

void LAVMatching::SetNoisyChannel(Int_t iBurst, Int_t channel) {
  fNoisyChannels[iBurst].push_back(channel);
}

void LAVMatching::Print() {
  cout << "[LAVMatching] Printing:" << endl;
  cout << "Have matched " << fNMatched << " blocks, the best of which has edgemask priority (ranging from 1 to 15) = " << fBestEdgeMask << endl;
  cout << "Have matched " << fNMatchedLowThr << " low-threshold blocks " << endl;
  cout << "Have matched " << fNMatchedHighThr << " high-threshold blocks " << endl;
  if (!fLAVEvent) {
    cout << "Wrong LAVEvent provided" << endl;
    return;
  }

  TClonesArray& hitArray = (*(fLAVEvent->GetHits()));

  for (Int_t i=0; i<TMath::Min(fNMatched,100); i++) {
    if (fIndexMatched[i]<0 || fIndexMatched[i]>= fLAVEvent->GetNHits()) {
      cout << "[LAVMatching::Print] wrong index stored or wrong LAVEvent provided " << i << " " << fIndexMatched[i] << " " << fLAVEvent->GetNHits() << endl;
      return;
    }
    TRecoLAVHit* hit = static_cast<TRecoLAVHit*>(hitArray[fIndexMatched[i]]);
    cout << " Matched block " << i << " chid " << hit->GetChannelID() << " edge " << hit->GetEdgeMask() << " priority " << fLAVPriorityMask[hit->GetEdgeMask()] << endl;
  }
  cout << "[LAVMatching]: Printing end" << endl;
}

void LAVMatching::PrintMaskedStations() {
  cout << "[LAVMatching] Masked stations are:";
  Int_t NumberOfMaskedStations = 0;
  for (Int_t i=0; i<12; i++) {
    if (fMaskedStations[i]) {
      cout << " " << i+1;
      NumberOfMaskedStations++;
    }
  }
  if (!NumberOfMaskedStations) cout << " none";
  cout << endl;
}

void LAVMatching::PrintNoisyChannels() {
  cout << "[LAVMatching] Noisy channels, burst " << fBurstID << ":";
  for (UInt_t i=0; i<fNoisyChannels[fBurstID].size(); i++) {
    cout << " " << fNoisyChannels[fBurstID][i];
  }
  if (!fNoisyChannels[fBurstID].size()) cout << " none";
  cout << endl;
}

void LAVMatching::PrintNoisyChannelsForAllBursts() {
  Int_t id = fBurstID;
  for (Int_t i=1; i<5000; i++) {
    fBurstID = i;
    PrintNoisyChannels();
  }
  fBurstID = id;
}

void LAVMatching::SetBurstID(Int_t BurstID) {
  if (BurstID<0 || BurstID>=5000) {
    cout << "[LAVMatching] Warning: invalid burst ID " << BurstID << endl;
    BurstID = 0; // this burst ID is impossible: there are no masked channels for it
  }
  fBurstID = BurstID;
  //PrintNoisyChannels();
}
