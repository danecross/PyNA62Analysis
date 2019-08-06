// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-27
//
// ---------------------------------------------------------------  

/// \class NewCHODReconstruction
/// \Brief
/// NewCHOD reconstruction
/// \EndBrief
/// \Detailed
/// The NewCHOD reconstruction produces RecoHits and no candidates in its output.
/// Tight NewCHOD RecoHits are defined as coincidences in the two channels in the same tile.
/// If one of the channels in a tile is masked
/// (as specified via the MaskedChannels cards in NewCHOD.conf),
/// then LooseMasked RecoHits based on a single hit are produced for that tile.
/// For unmatched Digis, Loose RecoHits based on a single Digi are produced.
/// Reconstruction of Loose and LooseMasked RecoHits can be suppressed in NewCHOD.conf.
/// The possible RecoHit types are defined by an enumerated type:
/// kTightCandidate, kLooseCandidate, kLooseMaskedCandidate, kUndefinedCandidate.
/// Tight RecoHit time is defined as the average of the two Digi times.
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "Riostream.h"
#include "NA62RecoManager.hh"
#include "NewCHODReconstruction.hh"
#include "NewCHODChannel.hh"
#include "TRecoNewCHODEvent.hh"
#include "TSlimRecoNewCHODEvent.hh"
#include "TRecoNewCHODHit.hh"
#include "TNewCHODDigi.hh"
#include "TDCBRawDecoder.hh"
#include "NA62Buffer.hh"
#include "NA62BufferProto.hh"
#include "TTDCBSpecialTrigger.hh"
#include "TString.h"
#include "TRegexp.h"

#include "CLHEP/Units/SystemOfUnits.h"
using namespace CLHEP;

NewCHODReconstruction::NewCHODReconstruction (TFile* HistoFile, TString ConfigFileName) :
  NA62VReconstruction(HistoFile, "NewCHOD", ConfigFileName),
  fTdcEvent                  (nullptr),
  fTimeWindow                (-999),
  fEdgeRequirement           (kBothEdges),
  fNTiles                    (4*38), // in total, 4*38 = 152 tiles
  fPrintCandidateInfo        (kFALSE),
  fBuildLooseCandidates      (kTRUE),
  fBuildLooseMaskedCandidates(kTRUE) {

  fRawDecoder = nullptr;
  fRecoEvent = new TRecoNewCHODEvent();
  fSlimRecoEvent = new TSlimRecoNewCHODEvent();
  fGeo = NewCHODGeometry::GetInstance();

  ResetHistograms();
  fMaskedChannels.clear();
  ParseConfFile(fConfigFileName);
  fTiles = new NewCHODTile*[fNTiles];
  for (Int_t i=0; i<fNTiles; i++) fTiles[i] = 0;
}

NewCHODReconstruction::~NewCHODReconstruction() {
  if (fRecoEvent) {
    delete fRecoEvent;
    fRecoEvent = 0;
  }
  if (fTiles) {
    for (Int_t itl=0; itl<fNTiles; itl++) {
      if (fTiles[itl]) delete fTiles[itl];
      fTiles[itl] = 0;
    }
    delete [] fTiles;
    fTiles = 0;
  }
  DeleteHistograms();
}

void NewCHODReconstruction::Init(NA62VReconstruction* MainReco) {

  // Common part for all the subdetectors
  NA62VReconstruction::Init(MainReco);

  // Initialize channels
  for (Int_t ich=0; ich<fNChannels; ich++) {
    Int_t PositionID = static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRemap(ich);
    fChannels[ich] = new NewCHODChannel(PositionID, ich, kFALSE);
  }

  for (UInt_t i=0; i<fMaskedChannels.size(); i++) { // channels masked from the config file
    Int_t ich  = fMaskedChannels[i];
    Int_t ROch = static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRO(ich);
    if (ROch>=0) fChannels[ROch]->Disable();
  }

  ///////////////////////////////////////
  // Initialize tiles: 4 * 38 = 152 tiles

  for (Int_t i=0; i<fNTiles; i++) {
    Int_t Quadrant  = 1 + i/38;
    Int_t Position1 = Quadrant*100 + i%38 + 1;
    Int_t Position2 = Position1 + 50;
    Int_t ROch1 = -1, ROch2 = -1;
    for (Int_t j=0; j<fNChannels; j++) {
      if (fChannels[j]->GetGeoChannelID() == Position1) ROch1 = j;
      if (fChannels[j]->GetGeoChannelID() == Position2) ROch2 = j;
    }
    Bool_t TileEnabled = (ROch1>=0 && ROch2>=0);
    if (TileEnabled) {
      TileEnabled = TileEnabled && fChannels[ROch1]->GetEnabled();
      TileEnabled = TileEnabled && fChannels[ROch2]->GetEnabled();
    }
    fTiles[i] = new NewCHODTile
      (Position1, Position1, Position2, ROch1, ROch2, TileEnabled, fChannelHistograms);
  }

  //////////////////////
  // Motherboard mapping
  
  Int_t MbMapping[40] =
    {4,5,11,15,16,20,21,25,3,9,10,14,18,19,22,23,1,2,6,7,8,12,13,17,
     24,26,27,30,31,33,36,38,28,29,32,34,35,37,-1,-1};

  for (Int_t i=0; i<5; i++) {
    for (Int_t j=0; j<8; j++) {
      if (i*8+j >= 38) continue;
      fMotherBoardMap[MbMapping[i*8+j]] = i+1;
    }
  }

  ReadT0s(); // Load the T0 constants
  InitHistograms();
}

///////////////////////////////////////////////////////////////////
// Read NewCHOD reconstruction parameters from a configuration file

void NewCHODReconstruction::ParseConfFile (TString ConfFileName) {

  std::ifstream confFile(ConfFileName.Data());
  if (!confFile.is_open()) {
    perror(ConfFileName);
    exit(kWrongConfiguration);
  }

  TString Line;
  while (Line.ReadLine(confFile)) {
    if (Line.BeginsWith("#")) continue;
    else if (Line.BeginsWith("TimeWindow")) {
      fTimeWindow = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof() * ns;
      continue;
    }
    else if (Line.BeginsWith("EdgeRequirement")) {
      fEdgeRequirement = TString(Line(TRegexp(".[0-9]"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("PrintCandidateInfo")) {
      fPrintCandidateInfo = TString(Line(TRegexp("[0-1]"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("BuildLooseCandidates")) {
      fBuildLooseCandidates = TString(Line(TRegexp("[0-1]"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("BuildLooseMaskedCandidates")) {
       fBuildLooseMaskedCandidates = TString(Line(TRegexp("[0-1]"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("MaskedChannels")) {
      TObjArray *l = Line.Tokenize(" ");
      for (Int_t i=0; i<(l->GetEntries()-1); i++) {
	Int_t id = static_cast<TObjString*>(l->At(i+1))->GetString().Atoi();
	fMaskedChannels.push_back(id);
      }
      delete l;
      continue;
    }
  }
  confFile.close();

  // Sanity checks
  if (fTimeWindow<=0.0) {
    std::cerr << "[NewCHODReconstruction] error: time window <= 0" << std::endl;
    exit(kWrongConfiguration);
  }
  if (abs(fEdgeRequirement)>3) {
    std::cerr << "[NewCHODReconstruction] error: invalid edge requirement" << std::endl;
    exit(kWrongConfiguration);
  }
}

TDetectorVEvent* NewCHODReconstruction::Trigger(TDetectorVEvent* tEvent, Event* /*tGenEvent*/) {
  return tEvent;
}

/////////////////////////////////////////////////////////

TRecoVEvent* NewCHODReconstruction::ProcessEvent(TDetectorVEvent* tEvent, Event* tGenEvent) {
  if (tEvent->IsA() == TSpecialTriggerEvent::Class()) {
    EOBEventMonitor(tEvent);
    return 0;
  }

  // Common part for all the subdetectors 
  NA62VReconstruction::ProcessEvent(tEvent, tGenEvent);

  // Data type: 0x1=physics, 0x2=periodics, 0x4=calibration, 0x8=LKr calibration, 0x10=control, ...
  fL0DataType = NA62RecoManager::GetInstance()->GetEventHeader()->GetL0TPData()->GetDataType();
  Bool_t PhysicsData = fL0DataType & 0x1;
  //Bool_t ControlData = fL0DataType & 0x10;

  // L0 trigger bits as in run conditions database
  fL0TriggerFlags = NA62RecoManager::GetInstance()->GetEventHeader()->GetL0TPData()->GetTriggerFlags();
  // L1 trigger bits
  fL1TriggerType  = (NA62RecoManager::GetInstance()->GetEventHeader()->GetTriggerType()&0xff00) >> 8;
  // L2 trigger bits (currently always 0x3)
  fL2TriggerType  = (NA62RecoManager::GetInstance()->GetEventHeader()->GetTriggerType()&0xff0000) >> 16;
  // Event time stamp
  fTimeStamp      = NA62RecoManager::GetInstance()->GetEventHeader()->GetTimeStamp();

  fTdcEvent = static_cast<TDCEvent*>(tEvent);
  fNDigis   = fTdcEvent->GetNHits();
  TClonesArray& Digis = (*(fTdcEvent->GetHits()));

  TDCEventMonitor();

  // Edge requirement for the Digis:
  //  1 = leading edge exists, 2 = trailing edge exists, 3 = leading+trailing edges exist (default)
  //  0 = any hit, -1 = leading edge only, -2 = trailing edge only

  Bool_t Done[fNDigis];
  for (Int_t iDigi=0; iDigi<fNDigis; iDigi++) {
    TNewCHODDigi *Digi = static_cast<TNewCHODDigi*>(Digis[iDigi]);
    if (Digi->GetChannelID()<100) continue; // mask mean-timer channels (0-31)

    Bool_t EdgeOK = kTRUE;
    if (fEdgeRequirement>0)
      EdgeOK = (Digi->GetDetectedEdge()==kBothEdges || Digi->GetDetectedEdge()==fEdgeRequirement);
    if (fEdgeRequirement<0)
      EdgeOK = (Digi->GetDetectedEdge()==abs(fEdgeRequirement));

    Done[iDigi] = !EdgeOK; // hits not matching required edge condition are ignored
  }

  ////////////////////////////////////////////////////////
  // Build tight RecoHits (i.e. coincidences in two digis)
  
  for (Int_t i=0; i<fNDigis; i++) { // potential low channels
    if (Done[i]) continue;
    TNewCHODDigi *Digi1 = static_cast<TNewCHODDigi*>(Digis[i]);
    if (Digi1->GetChannelID()<100) continue; // mask mean-timer channels (0-31)
    if (Digi1->IsHigh()) continue;

    for (Int_t j=0; j<fNDigis; j++) { // potential high channels
      if (Done[j]) continue;
      TNewCHODDigi *Digi2 = static_cast<TNewCHODDigi*>(Digis[j]);
      if (Digi2->GetChannelID()<100) continue; // mask mean-timer channels (0-31)
      if (!Digi2->IsHigh()) continue;
      if (Digi1->GetTileID() != Digi2->GetTileID()) continue;

      Double_t Time1 = GetRecoTime(Digi1, 1); // low-number channel
      Double_t Time2 = GetRecoTime(Digi2, 1); // high-number channel
      Double_t Time  = 0.5*(Time1+Time2);
      if (fabs(Time1-Time2)>fTimeWindow) continue;

      Double_t Time1NoT0 = GetRecoTimeNoT0(Digi1, 1); // low-number channel
      Double_t Time2NoT0 = GetRecoTimeNoT0(Digi2, 1); // high-number channel
      Double_t TimeNoT0  = 0.5*(Time1NoT0+Time2NoT0);

      TRecoNewCHODHit *RecoHit = static_cast<TRecoNewCHODHit*>(fRecoEvent->AddHit(Digi1));
      RecoHit->SetChannelID(Digi1->GetTileID()); // this is tile ID
      RecoHit->DecodeChannelID();
      RecoHit->SetChannel1(Digi1->GetChannelID());
      RecoHit->SetChannel2(Digi2->GetChannelID());
      RecoHit->SetROChannel1
	(static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRO(Digi1->GetChannelID()));
      RecoHit->SetROChannel2
	(static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRO(Digi2->GetChannelID()));
      RecoHit->SetX(fGeo->GetTileCentreX(Digi1->GetTileID()));
      RecoHit->SetY(fGeo->GetTileCentreY(Digi1->GetTileID()));
      RecoHit->SetTime1(Time1);
      RecoHit->SetTime2(Time2);
      RecoHit->SetTime (Time);
      RecoHit->SetTime1NoT0(Time1NoT0);
      RecoHit->SetTime2NoT0(Time2NoT0);
      RecoHit->SetTimeNoT0 (TimeNoT0);
      RecoHit->SetType(kTightCandidate);
      Done[i] = Done[j] = kTRUE;

      fHNRecoHitsPerTile->Fill(Digi1->GetTileID());
    }
  }

  ////////////////////////////////////////////////////////////////////////////////////
  // Build the loose RecoHits using the remaining Digis.
  // There are two types of loose RecoHits (and candidates).
  // LooseMasked: partner channel masked in the config file;
  // Loose:       partner channel not masked but a tight candidate match is not found.

  if (fBuildLooseCandidates || fBuildLooseMaskedCandidates) {
    for (Int_t i=0; i<fNDigis; i++) {
      if (Done[i]) continue;

      TNewCHODDigi *Digi1 = static_cast<TNewCHODDigi*>(Digis[i]);
      if (Digi1->GetChannelID()<100) continue; // mask mean-timer channels (0-31)
      Int_t ch1  = Digi1->GetChannelID();
      Int_t ch2  = (ch1%100<50) ? ch1+50 : ch1-50;
      Int_t Type = ChannelMasked(ch2) ? kLooseMaskedCandidate : kLooseCandidate;

      if (Type==kLooseMaskedCandidate && !fBuildLooseMaskedCandidates) continue;
      if (Type==kLooseCandidate       && !fBuildLooseCandidates)       continue;

      Double_t Time1     = GetRecoTime(Digi1, 1);
      Double_t Time1NoT0 = GetRecoTimeNoT0(Digi1, 1); // low-number channel

      TRecoNewCHODHit *RecoHit = static_cast<TRecoNewCHODHit*>(fRecoEvent->AddHit(Digi1));
      RecoHit->SetChannelID(Digi1->GetTileID()); // this is tile ID
      RecoHit->DecodeChannelID();
      RecoHit->SetChannel1(Digi1->GetChannelID());
      RecoHit->SetChannel2(-1);
      RecoHit->SetROChannel1
	(static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRO(Digi1->GetChannelID()));
      RecoHit->SetROChannel2(-1);
      RecoHit->SetX(fGeo->GetTileCentreX(Digi1->GetTileID()));
      RecoHit->SetY(fGeo->GetTileCentreY(Digi1->GetTileID()));
      RecoHit->SetType(Type);
      RecoHit->SetTime1(Time1);
      RecoHit->SetTime2(-999);
      RecoHit->SetTime (Time1);
      RecoHit->SetTime1NoT0(Time1NoT0);
      RecoHit->SetTime2NoT0(-999);
      RecoHit->SetTimeNoT0 (Time1NoT0);
      Done[i] = kTRUE;

      fHNRecoHitsPerTile->Fill(Digi1->GetTileID());
    }
  }

  Int_t NRecoHits = fRecoEvent->GetNHits();
  fHNRecoHits->Fill(NRecoHits);
  if (fHistosLevel>0 && PhysicsData) {
    for (Int_t i=0; i<8; i++) {
      if ((fL0TriggerFlags>>i)&1) fHNRecoHitsVsL0TriggerBit->Fill(NRecoHits, i);
      else                        fHNRecoHitsVsNoL0TriggerBit->Fill(NRecoHits, i);
    }
  }
  fNRecoHitsPerBurst += NRecoHits;

  ///////////////////////////////////////////////////////////////////////////
  // Reconstruct NewCHOD candidates from RecoHits:
  // currently off, i.e. no candidates are produced, tht output are RecoHits

  /*
  for (Int_t i=0; i<NRecoHits; i++) {
    TRecoNewCHODCandidate *Candidate = static_cast<TRecoNewCHODCandidate*>(fRecoEvent->AddCandidate());
    Candidate->AddHit(i); // associate a hit to candidate
  }
  */

  ///////////////////////////////////////////////////////////////
  // Digi time differences in tiles: T(channel 50+N)-T(channel N)

  Int_t DigisInChannels[500];
  for (Int_t i=101; i<500; i++) DigisInChannels[i] = 0;

  for (Int_t i=0; i<fNDigis; i++) {
    TNewCHODDigi *Digi1 = static_cast<TNewCHODDigi*>(Digis[i]);
    if (Digi1->GetChannelID()<100) continue; // mask mean-timer channels (0-31)
    DigisInChannels[Digi1->GetChannelID()]++;
    if (Digi1->IsHigh()) continue;

    for (Int_t j=0; j<fNDigis; j++) {
      TNewCHODDigi *Digi2 = static_cast<TNewCHODDigi*>(Digis[j]);
      if (Digi2->GetChannelID()<100) continue; // mask mean-timer channels (0-31)
      if (!Digi2->IsHigh()) continue;
      if (Digi1->GetTileID() != Digi2->GetTileID()) continue;

      Int_t    Tile  = Digi1->GetTileID();
      Double_t Time1 = Digi1->GetTime(); //  low number-channel, T0-corrected
      Double_t Time2 = Digi2->GetTime(); // high number-channel, T0-corrected
      Double_t DeltaTime = Time2 - Time1;
      fHDeltaTime->Fill(DeltaTime);
      if (fHDeltaTimeVsTile) fHDeltaTimeVsTile->Fill(Tile, DeltaTime);
      if (fPDeltaTimeVsTile) fPDeltaTimeVsTile->Fill(Tile, DeltaTime);
    }
  }

  ////////////////////////////////
  // AND and OR rates in each tile

  for (Int_t iQuad=1; iQuad<=4; iQuad++) {
    for (Int_t iTile=1; iTile<=38; iTile++) {
      Int_t iGlobal = 100*iQuad+iTile;
      Bool_t iAND = (DigisInChannels[iGlobal] && DigisInChannels[iGlobal+50]);
      Bool_t iOR  = (DigisInChannels[iGlobal] || DigisInChannels[iGlobal+50]);
      if (iAND) fHTileAND->Fill(iGlobal);
      if (iOR)  fHTileOR->Fill(iGlobal);
    }
  }

  //////////////////
  // RecoHit monitor

  Int_t HitsInQuadtants[4] = {0, 0, 0, 0};
  for (Int_t iHit=0; iHit<fRecoEvent->GetNHits(); iHit++) {
    TRecoNewCHODHit *Hit = static_cast<TRecoNewCHODHit*>(fRecoEvent->GetHit(iHit));
    HitsInQuadtants[Hit->GetQuadrantID()-1]++;
    Int_t Tile = Hit->GetTileID();
    fHRecoHitProfile->Fill(Tile);
    if (Hit->GetType()==kTightCandidate) {
      fHTightRecoHitProfile->Fill(Tile);
      fHDeltaTimeRecoHit->Fill(Hit->GetDeltaTime());
      if (fHistosLevel>0 && PhysicsData) {
	for (Int_t i=0; i<8; i++) {
	  if ((fL0TriggerFlags>>i)&1) fHTightRecoHitProfileVsL0TriggerBit->Fill(Tile, i);
	  else                        fHTightRecoHitProfileVsNoL0TriggerBit->Fill(Tile, i);
	}
      }
    }
    for (Int_t i=0; i<100; i++) {
      if (fGeo->GetScintMap(i)==Tile%100) {
	Int_t BrickID = 100*(Tile/100)+i;
	Double_t x = fGeo->GetBrickCentreX(BrickID) * mm;
	Double_t y = fGeo->GetBrickCentreY(BrickID) * mm;
	fHRecoHitProfile2D->Fill(x/m, y/m);
	if (Hit->GetType()==kTightCandidate) fHTightRecoHitProfile2D->Fill(x/m, y/m);
      }
    }

    Int_t MotherBoard = fMotherBoardMap[Tile%100];
    switch (MotherBoard) {
    case 1: fHRecoHitProfileMb1->Fill(Tile); break;
    case 2: fHRecoHitProfileMb2->Fill(Tile); break;
    case 3: fHRecoHitProfileMb3->Fill(Tile); break;
    case 4: fHRecoHitProfileMb4->Fill(Tile); break;
    case 5: fHRecoHitProfileMb5->Fill(Tile); break;
    // default: std::cout << " RecoHit in unknown motherboard " << MotherBoard << std::endl;
    }

    if (fPrintCandidateInfo) PrintRecoHitInfo(iHit);
  }

  Int_t NQuadrants = 0;
  for (Int_t i=0; i<4; i++) if (HitsInQuadtants[i]) NQuadrants++;
  fHNQuadrants->Fill(NQuadrants);
  if (!NQuadrants)   fHQuadrantTrigger->Fill(0.0);
  if (NQuadrants>=1) fHQuadrantTrigger->Fill(1.0);
  if (NQuadrants>=2) fHQuadrantTrigger->Fill(2.0);
  if (NQuadrants>=3) fHQuadrantTrigger->Fill(3.0);
  if ((HitsInQuadtants[0] && HitsInQuadtants[2]) ||
      (HitsInQuadtants[1] && HitsInQuadtants[3])) fHQuadrantTrigger->Fill(4.0);

  // Distance between RecoHits
  if (fRecoEvent->GetNHits()==2) {
    TRecoNewCHODHit *c1 = static_cast<TRecoNewCHODHit*>(fRecoEvent->GetHit(0));
    TRecoNewCHODHit *c2 = static_cast<TRecoNewCHODHit*>(fRecoEvent->GetHit(1));
    Double_t Dist = sqrt(pow(c1->GetX()-c2->GetX(),2) + pow(c1->GetY()-c2->GetY(),2));
    fHInterRecoHitDistance->Fill(Dist);
  }

  return fRecoEvent;
}

//////////////////////////////////////////////////////////////////////////////////
// Candidate printout for comparison with the NewCHOD primitive dumps.
// Format: event time (TS units), muon fine time (FT=TS/256 units), Tile ID, Type.

void NewCHODReconstruction::PrintRecoHitInfo(Int_t i) { 
  TRecoNewCHODHit *Hit = static_cast<TRecoNewCHODHit*>(fRecoEvent->GetHit(i));
  TString TypeString = "T";
  if      (Hit->GetType()==kLooseCandidate)       TypeString = "L";
  else if (Hit->GetType()==kLooseMaskedCandidate) TypeString = "LM";
  else if (Hit->GetType()==kUndefinedCandidate)   TypeString = "U";
  Int_t Tile = Hit->GetTileID();
  Double_t UncorrectedTime = Hit->GetTimeNoT0() + fStationsT0[0];
  UncorrectedTime /= ClockPeriod; // convert into timestamp units of 24.951059536 ns
  UncorrectedTime *= 256.0;       // convert into fine time units of 97.465076313 ps
  printf("@@@ %9d %4i %3d %s\n", fTimeStamp, Int_t(UncorrectedTime), Tile, TypeString.Data());
}

/////////////////////////
// Start of burst actions

Bool_t NewCHODReconstruction::ChannelMasked (Int_t ch) {
  std::vector<Int_t>::iterator i = find(fMaskedChannels.begin(), fMaskedChannels.end(), ch);
  return (i != fMaskedChannels.end());
}

void NewCHODReconstruction::StartOfBurst() {
  NA62VReconstruction::StartOfBurst(); // common part for all the subdetectors
  fNRecoHitsPerBurst = 0;
}

///////////////////////
// End of burst actions

void NewCHODReconstruction::EndOfBurst() {
  NA62VReconstruction::EndOfBurst(); // common part for all the subdetectors
}

///////////////////////
// TDC event monitoring

void NewCHODReconstruction::TDCEventMonitor() {

  fHNErrorWord->Fill(fTdcEvent->GetNErrors());
  fHNDigis->Fill(fNDigis);

  TClonesArray& Digis = (*(fTdcEvent->GetHits()));
  for (Int_t iDigi=0; iDigi<fNDigis; iDigi++) {
    TNewCHODDigi *Digi = static_cast<TNewCHODDigi*>(Digis[iDigi]);
    Int_t    ChannelID       = Digi->GetChannelID();
    Int_t    ROID            =
      static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRO(ChannelID);
    if (!fChannels[ROID]->GetEnabled()) continue;
    Double_t T0              = (fEnableT0) ? fChannels[ROID]->GetT0() : 0.0;
    Int_t    Tile            = Digi->GetTileID();
    Bool_t   IsHigh          = Digi->IsHigh();
    Int_t    Status          = Digi->GetDetectedEdge(); // 1=leading, 2=trailing, 3=both
    Double_t LeadingTimeRaw  = Digi->GetLeadingEdge(); // histogram DigiTimeRaw is filled centrally
    Double_t TrailingTimeRaw = Digi->GetTrailingEdge();
    Double_t LeadingTime     = LeadingTimeRaw  - GetT0Correction(Digi) - T0;
    Double_t TrailingTime    = TrailingTimeRaw - GetT0Correction(Digi) - T0;
    Double_t Width           = TrailingTimeRaw - LeadingTimeRaw;

    fHChannelProfile->Fill(ChannelID);
    fHROChannelProfile->Fill(ROID);
    if (fHChannelProfileVsBurst) {
      fHChannelProfileVsBurst->Fill(fRecoEvent->GetBurstID(), ChannelID);
    }
    if (ChannelID<100) continue; // ignore mean-timer channels

    // Fill the 2D channel occupancy plots
    for (Int_t i=0; i<100; i++) {
      if (fGeo->GetScintMap(i)==Tile%100) {
	Int_t BrickID = 100*(Tile/100)+i;
	Double_t x = fGeo->GetBrickCentreX(BrickID) * mm;
	Double_t y = fGeo->GetBrickCentreY(BrickID) * mm;
	if (IsHigh) fHChannelProfile2D_PM1->Fill(x/m, y/m);
	else        fHChannelProfile2D_PM0->Fill(x/m, y/m);
      }
    }

    fHHitStatus->Fill(Status);
    if (fHHitStatusVsChannel)   fHHitStatusVsChannel->Fill(ChannelID, Status);
    if (fHHitStatusVsROChannel) fHHitStatusVsROChannel->Fill(ROID, Status);

    if (Status==kLeadingEdge) {
      fHLeadingTimeRaw_NoTrailing->Fill(LeadingTimeRaw);
    }
    else if (Status==kTrailingEdge) {
      fHTrailingTimeRaw_NoLeading->Fill(TrailingTimeRaw);
    }
    else if (Status==kBothEdges) { // both leading & trailing edges are present
      fHLeadingTime->Fill(LeadingTime);
      fHTrailingTime->Fill(TrailingTime);
      //fHSlotVsLeadingTime->Fill(LeadingTime, Digi->GetSlot());
      fHWidth->Fill(Width);

      // leading time is in ns units, and relative to event timestamp
      // convert to TS units
      Double_t LeadingTimeInTSUnits = LeadingTimeRaw/ClockPeriod;

      // account for -ve leading times with large +ve offset
      Double_t RealFineTimeInTSUnits = 100 + LeadingTimeInTSUnits;

      // remove the large offset, only left with TDC Fine Time in TS units
      RealFineTimeInTSUnits -= int(RealFineTimeInTSUnits);

      // convert to FT units
      RealFineTimeInTSUnits *= 256;

      // Already an integer, in fact, but just to hammer the point home
      Int_t RealFineTime = RealFineTimeInTSUnits;

      fHHitFineTime256->Fill(RealFineTime);
      std::bitset< 8 > FT = (RealFineTime);
      for (Int_t iBit=0; iBit<8; iBit++) {
	size_t index = 7 - iBit;
	if (FT[index]==1) fHHitFineTimeBits->Fill(iBit-7);
      }
    }
  }
}

/////////////////////////////////
// EOB event (scalers) monitoring

void NewCHODReconstruction::EOBEventMonitor(TDetectorVEvent* tEvent) {

  Int_t TotalHitCount = 0;
  TSpecialTriggerEvent *EOB = static_cast<TSpecialTriggerEvent*>(tEvent);
  for (Int_t iEOB=0; iEOB<EOB->GetNSpecialTriggers(); iEOB++) {

    TTDCBSpecialTrigger *EOBTrigger = reinterpret_cast<TTDCBSpecialTrigger*>(tEvent->GetHit(iEOB));
    if (!EOBTrigger) continue;
    if (!isL0EOB(EOBTrigger->GetTriggerType())) continue;

    //////////////////////////////////////////////
    // The standard PP EOB scalers: channel counts

    for (Int_t i=0; i<128; i++) {
      Int_t ROid = 128*EOBTrigger->GetFPGAID() + i;
      if (ROid >= fNChannels) continue;

      Int_t  chid   = fChannels[ROid]->GetGeoChannelID();
      if (chid<100) continue; // ignore mean-timer channels
      Int_t  count  = EOBTrigger->GetCounter("CHANNEL_COUNT_L")->GetValue(i);
      Int_t  Tile   = 100*(chid/100) + chid%50;
      Bool_t IsHigh = (chid%100>=50);

      fHChannelProfileEOB->Fill(chid, count);
      fHROChannelProfileEOB->Fill(ROid, count);
      if (fHChannelProfileVsBurstEOB) {
	fHChannelProfileVsBurstEOB->Fill(fRecoEvent->GetBurstID(), chid, count);
      }
      TotalHitCount += count;

      for (Int_t iCheck=0; iCheck<100; iCheck++) { // a tile can correspond to multiple elementary bricks
	if (fGeo->GetScintMap(iCheck)==Tile%100) {
	  Int_t BrickID = 100*(Tile/100)+iCheck;
	  Double_t x = fGeo->GetBrickCentreX(BrickID) * mm;
	  Double_t y = fGeo->GetBrickCentreY(BrickID) * mm;
	  if (IsHigh) fHChannelProfile2D_EOB_PM1->Fill(x/m, y/m, count);
	  else        fHChannelProfile2D_EOB_PM0->Fill(x/m, y/m, count);
	}
      }
    }

    //////////////
    // EOB scalers
    /*
    if (EOBTrigger->GetFPGAID()==4) {

      /////////////////////////////////
      // Tight trigger primitive counts

      Int_t TotalPrimitiveCount = 0;
      for (Int_t i=0; i<148; i++) { // number of tiles (this is not fNTiles!)
	Int_t iRO = (fNChannels==288) ? i*2 : i%8 + 16*(i/8); // tile ID mapping, both old and new CFDs
	if (iRO>=fNChannels) continue;
	Int_t chid  = fChannels[iRO]->GetGeoChannelID();
	Int_t Tile  = chid%200;
	Int_t count = EOBTrigger->GetCounter("MUV3_COUNTS")->GetValue(i);
	if (count<0) continue; // no information found in the EOB
	fHTightPrimitiveProfileEOB->Fill(Tile, count);
	TotalPrimitiveCount += count;
	//Double_t x  = fGeo->GetBrickCentreX(Tile) * mm; // @@ to be done.....
	//Double_t y  = fGeo->GetBrickCentreY(Tile) * mm;
	//fHTightPrimitiveProfile2D_EOB->Fill(x/m, y/m, count);
      }
      fHNTightMuonsPerBurstEOB->SetBinContent
	(fRecoEvent->GetBurstID()+1, TotalPrimitiveCount*1e-6); // unit: mln
      
      //////////////////////////////////////////
      // Total primitive counts and error counts

      for (Int_t i=0; i<16; i++) {
	fHTotalPrimitiveCountsEOB->Fill(i, 1e-6*EOBTrigger->GetCounter("MUV3_PRIMI")->GetValue(i)); // [mln]
      }
      for (Int_t i=0; i<12; i++) {
	Double_t count = EOBTrigger->GetCounter("MUV3_ERROR")->GetValue(i);
	if (!i) count *= 1e-6; // Covert N(frames) to millions
	fHErrorCountsEOB->Fill(i, count);
      }
    }
    */
  }

  fHNHitsPerBurstEOB->SetBinContent(fRecoEvent->GetBurstID()+1, TotalHitCount*1e-6); // [mln]
}

/////////////////////////////////////////////////
// Compute the corrected reconstructed Digi times

Double_t NewCHODReconstruction::GetRecoTime (TNewCHODDigi *Digi, Int_t Edge) {
  Int_t    PositionID = Digi->GetChannelID();
  Int_t    ROchannel  = static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRO(PositionID);
  Double_t Time       = (Edge==1) ? Digi->GetLeadingEdge() : Digi->GetTrailingEdge();
  Double_t T0         = (fEnableT0) ? fChannels[ROchannel]->GetT0() : 0.0;
  return   (Time - GetT0Correction(Digi) - T0);
}

Double_t NewCHODReconstruction::GetRecoTimeNoT0 (TNewCHODDigi *Digi, Int_t Edge) {
  return (Edge==1) ?
    Digi->GetLeadingEdge()  - GetT0Correction(Digi) :
    Digi->GetTrailingEdge() - GetT0Correction(Digi);
}

///////////////////////////////////////
// Initialize the monitoring histograms

void NewCHODReconstruction::InitHistograms() {

  TString Name = "NDigis";
  fHNDigis = new TH1D(Name, Name, 20, -0.5, 19.5);
  fHNDigis->GetXaxis()->SetTitle("Number of digis");

  Name = "NRecoHits";
  fHNRecoHits = new TH1D(Name, Name, 20, -0.5, 19.5);
  fHNRecoHits->GetXaxis()->SetTitle("Reconstructed hits");

  Name = "NRecoHitsPerTile";
  fHNRecoHitsPerTile = new TH1D(Name, Name, 350, 100.5, 450.5);
  fHNRecoHitsPerTile->GetXaxis()->SetTitle("Tile ID");

  Name = "NQuadrants";
  fHNQuadrants = new TH1D(Name, Name, 5, -0.5, 4.5);
  fHNQuadrants->GetXaxis()->SetTitle("NewCHOD quandrants hit");

  Name = "QuadrantTrigger";
  fHQuadrantTrigger = new TH1D(Name, Name, 5, -0.5, 4.5);
  fHQuadrantTrigger->GetXaxis()->SetTitle("NewCHOD quandrant trigger decision");
  fHQuadrantTrigger->GetXaxis()->SetBinLabel(1, "None");
  fHQuadrantTrigger->GetXaxis()->SetBinLabel(2, "Q1");
  fHQuadrantTrigger->GetXaxis()->SetBinLabel(3, "Q2");
  fHQuadrantTrigger->GetXaxis()->SetBinLabel(4, "Q3");
  fHQuadrantTrigger->GetXaxis()->SetBinLabel(5, "QX");

  if (fHistosLevel>0) {
    Name = "NHitsPerBurstEOB";
    fHNHitsPerBurstEOB = new TH1D(Name, Name, 3000, -0.5, 2999.5);
    fHNHitsPerBurstEOB->GetXaxis()->SetTitle("Burst ID");

    Name = "NTightPrimitivesPerBurstEOB";
    fHNTightPrimitivesPerBurstEOB = new TH1D(Name, Name, 3000, -0.5, 2999.5);
    fHNTightPrimitivesPerBurstEOB->GetXaxis()->SetTitle("Burst ID");

    Name = "TotalPrimitiveCountsEOB";
    fHTotalPrimitiveCountsEOB = new TH1D(Name, Name, 16, -0.5, 15.5);
    TString PrimitiveNames[16] = // fake names
      {"P0", "P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9",
       "P10", "P11",
       "P12", "P13",
       "Total primitive count", "Calibration"};
    for (Int_t i=0; i<16; i++) {
      fHTotalPrimitiveCountsEOB->GetXaxis()->SetBinLabel(i+1, PrimitiveNames[i]);
    }

    Name = "ErrorCountsEOB"; // newchod uses PPs 1-3, not 0-2 ?
    fHErrorCountsEOB = new TH1D(Name, Name, 12, -0.5, 11.5);
    TString ErrorNames[12] =
      {"Number of frames [mln]",
       "PP0 channel FIFO full",
       "PP1 channel FIFO full",
       "PP2 channel FIFO full",
       "PP0 mismatched frame",
       "PP1 mismatched frame",
       "PP2 mismatched frame",
       "PP0 mismatched data",
       "PP1 mismatched data",
       "PP2 mismatched data",
       "Clustering module overflow",
       "PMT signals in different frames"};
    for (Int_t i=0; i<12; i++) {
      fHErrorCountsEOB->GetXaxis()->SetBinLabel(i+1, ErrorNames[i]);
    }
  }

  Name = "InterRecoHitDistance";
  fHInterRecoHitDistance = new TH1D(Name, Name, 200, 0, 2000);
  fHInterRecoHitDistance->GetXaxis()->SetTitle("Distance [mm]");

  Name = "HitStatus";
  fHHitStatus = new TH1D(Name, Name, 3, 0.5, 3.5);
  fHHitStatus->GetXaxis()->SetTitle("HitStatus: 1=leading edge only, 2=trailing edge only, 3=both");
  
  Name = "RecoHitProfile";
  fHRecoHitProfile = new TH1D(Name, Name, 350, 100.5, 450.5);
  fHRecoHitProfile->GetXaxis()->SetTitle("Tile");

  Name = "TightRecoHitProfile";
  fHTightRecoHitProfile = new TH1D(Name, Name, 350, 100.5, 450.5);
  fHTightRecoHitProfile->GetXaxis()->SetTitle("Tile");

  if (fHistosLevel>0) {
    Name = "TightPrimitiveProfileEOB";
    fHTightPrimitiveProfileEOB = new TH1D(Name, Name, 350, 100.5, 450.5);
    fHTightPrimitiveProfileEOB->GetXaxis()->SetTitle("Tile");
  }

  Name = "LeadingTime";
  fHLeadingTime = new TH1D(Name, Name, 250, -300-fStationsT0[0], +200-fStationsT0[0]);
  fHLeadingTime->GetXaxis()->SetTitle("Leading time (ns)");

  Name = "TrailingTime";
  fHTrailingTime = new TH1D(Name, Name, 250, -300-fStationsT0[0], +200-fStationsT0[0]);
  fHTrailingTime->GetXaxis()->SetTitle("Trailing time (ns)");

  Name = "LeadingTimeRaw_NoTrailing";
  fHLeadingTimeRaw_NoTrailing = new TH1D(Name, Name, 250, -300-fStationsT0[0], +200-fStationsT0[0]);
  fHLeadingTimeRaw_NoTrailing->GetXaxis()->SetTitle("Raw leading time (ns)");

  Name = "TrailingTimeRaw_NoLeading";
  fHTrailingTimeRaw_NoLeading = new TH1D(Name, Name, 250, -300-fStationsT0[0], +200-fStationsT0[0]);
  fHTrailingTimeRaw_NoLeading->GetXaxis()->SetTitle("Raw trailing time (ns)");

  Name = "TimeWrtReference";
  fHTimeWrtReference = new TH1D(Name, Name, 400, -20, 20);
  fHTimeWrtReference->GetXaxis()->SetTitle("Digi time wrt CEDAR (ns)");

  Name = "TimeWrtReferenceNoT0";
  fHTimeWrtReferenceNoT0 = new TH1D(Name, Name, 400, -20, 20);
  fHTimeWrtReferenceNoT0->GetXaxis()->SetTitle("Digi time wrt CEDAR (ns)");

  Name = "TightRecoHitTimeWrtReference";
  fHTightRecoHitTimeWrtReference = new TH1D(Name, Name, 400, -20, 20);
  fHTightRecoHitTimeWrtReference->GetXaxis()->SetTitle("Tight RecoHit time wrt CEDAR (ns)");

  Name = "TightRecoHitTimeWrtReferenceNoT0";
  fHTightRecoHitTimeWrtReferenceNoT0 = new TH1D(Name, Name, 400, -20, 20);
  fHTightRecoHitTimeWrtReferenceNoT0->GetXaxis()->SetTitle("Tight RecoHit time wrt CEDAR (ns)");

  Name = "LooseRecoHitTimeWrtReference";
  fHLooseRecoHitTimeWrtReference = new TH1D(Name, Name, 400, -20, 20);
  fHLooseRecoHitTimeWrtReference->GetXaxis()->SetTitle("Loose RecoHit time wrt CEDAR (ns)");

  Name = "LooseRecoHitTimeWrtReferenceNoT0";
  fHLooseRecoHitTimeWrtReferenceNoT0 = new TH1D(Name, Name, 400, -20, 20);
  fHLooseRecoHitTimeWrtReferenceNoT0->GetXaxis()->SetTitle("Loose RecoHit time wrt CEDAR (ns)");

  Name = "Width";
  fHWidth = new TH1D(Name, Name, 300, 200.5*TdcCalib, 500.5*TdcCalib);
  fHWidth->GetXaxis()->SetTitle("Time width (ns)");

  Name = "NErrorWord";
  fHNErrorWord = new TH1D(Name, Name, 11, -0.5, 10.5);
  fHNErrorWord->GetXaxis()->SetTitle("NErrorWord");
  fHNErrorWord->GetYaxis()->SetTitle("NEvents");

  Name = "ChannelProfile";
  fHChannelProfile = new TH1D(Name, Name, 501, -0.5, 500.5);
  fHChannelProfile->GetXaxis()->SetTitle("NewCHOD channel");

  if (fHistosLevel>0) {
    Name = "ChannelProfileEOB";
    fHChannelProfileEOB = new TH1D(Name, Name, 501, -0.5, 500.5);
    fHChannelProfileEOB->GetXaxis()->SetTitle("NewCHOD channel (EOB scaler)");
  }

  Name = "ReadoutChannelProfile";
  fHROChannelProfile = new TH1D(Name, Name, fNChannels, -0.5, fNChannels-0.5);
  fHROChannelProfile->GetXaxis()->SetTitle("NewCHOD RO channel");

  if (fHistosLevel>0) {
    Name = "ReadoutChannelProfileEOB";
    fHROChannelProfileEOB = new TH1D(Name, Name, fNChannels, -0.5, fNChannels-0.5);
    fHROChannelProfileEOB->GetXaxis()->SetTitle("NewCHOD RO channel (EOB scaler)");
  }

  Name = "RecoHitProfileMb1";
  fHRecoHitProfileMb1 = new TH1D(Name, Name, 350, 100.5, 450.5);
  fHRecoHitProfileMb1->SetTitle("RecoHit profile of Motherboard 1");
  fHRecoHitProfileMb1->GetXaxis()->SetTitle("NewCHOD tile");

  Name = "RecoHitProfileMb2";
  fHRecoHitProfileMb2 = new TH1D(Name, Name, 350, 100.5, 450.5);
  fHRecoHitProfileMb2->SetTitle("RecoHit profile of Motherboard 2");
  fHRecoHitProfileMb2->GetXaxis()->SetTitle("NewCHOD tile");

  Name = "RecoHitProfileMb3";
  fHRecoHitProfileMb3 = new TH1D(Name, Name, 350, 100.5, 450.5);
  fHRecoHitProfileMb3->SetTitle("RecoHit profile of Motherboard 3");
  fHRecoHitProfileMb3->GetXaxis()->SetTitle("NewCHOD tile");

  Name = "RecoHitProfileMb4";
  fHRecoHitProfileMb4 = new TH1D(Name, Name, 350, 100.5, 450.5);
  fHRecoHitProfileMb4->SetTitle("RecoHit profile of Motherboard 4");
  fHRecoHitProfileMb4->GetXaxis()->SetTitle("NewCHOD tile");

  Name = "RecoHitProfileMb5";
  fHRecoHitProfileMb5 = new TH1D(Name, Name, 350, 100.5, 450.5);
  fHRecoHitProfileMb5->SetTitle("RecoHit profile of Motherboard 5");
  fHRecoHitProfileMb5->GetXaxis()->SetTitle("NewCHOD tile");

  Name = "TileAND";
  fHTileAND = new TH1D(Name, Name, 350, 100.5, 450.5);
  fHTileAND->GetXaxis()->SetTitle("NewCHOD tile");

  Name = "TileOR";
  fHTileOR = new TH1D(Name, Name, 350, 100.5, 450.5);
  fHTileOR->GetXaxis()->SetTitle("NewCHOD tile");

  Name = "DeltaTimeDigis";
  fHDeltaTime = new TH1D(Name, Name, 100, -20, 20);
  fHDeltaTime->GetXaxis()->SetTitle("PMT1-PMT0 time difference (ns)");

  Name = "DeltaTimeRecoHit";
  fHDeltaTimeRecoHit = new TH1D(Name, Name, 100, -20, 20);
  fHDeltaTimeRecoHit->GetXaxis()->SetTitle("PMT1-PMT0 time difference (ns)");

  Name = "HitFineTimeBits";
  fHHitFineTimeBits = new TH1D(Name, Name, 8, -7.5, 0.5);
  fHHitFineTimeBits->GetXaxis()->SetTitle("Hit Fine Time Bits");
  fHHitFineTimeBits->Sumw2();

  Name = "HitFineTime256";
  fHHitFineTime256 = new TH1D(Name, Name, 256, -0.5, 255.5);
  fHHitFineTime256->GetXaxis()->SetTitle("Hit Fine Time");

  //////////////////////////////////////////

  if (fHistosLevel>0) {
    Name = "ChannelProfileVsBurst";
    fHChannelProfileVsBurst = new TH2F
      (Name, Name, 3000, -0.5, 2999.5, 400, 100.5, 500.5);
    fHChannelProfileVsBurst->GetXaxis()->SetTitle("Burst ID");
    fHChannelProfileVsBurst->GetYaxis()->SetTitle("NewCHOD channel");

    Name = "ChannelProfileVsBurstEOB";
    fHChannelProfileVsBurstEOB = new TH2F
      (Name, Name, 3000, -0.5, 2999.5, 400, 100.5, 500.5);
    fHChannelProfileVsBurstEOB->GetXaxis()->SetTitle("Burst ID");
    fHChannelProfileVsBurstEOB->GetYaxis()->SetTitle("NewCHOD channel");
  }

  Name = "ChannelProfile2D_PM0";
  fHChannelProfile2D_PM0 = new TH2F(Name, Name, 16, -1.072, 1.072, 20, -1.070, 1.070);
  fHChannelProfile2D_PM0->GetXaxis()->SetTitle("NewCHOD tile X (m)");
  fHChannelProfile2D_PM0->GetYaxis()->SetTitle("NewCHOD tile Y (m)");

  Name = "ChannelProfile2D_PM1";
  fHChannelProfile2D_PM1 = new TH2F(Name, Name, 16, -1.072, 1.072, 20, -1.070, 1.070);
  fHChannelProfile2D_PM1->GetXaxis()->SetTitle("NewCHOD tile X (m)");
  fHChannelProfile2D_PM1->GetYaxis()->SetTitle("NewCHOD tile Y (m)");

  if (fHistosLevel>0) {
    Name = "ChannelProfile2D_PM0 EOB";
    fHChannelProfile2D_EOB_PM0 = new TH2F(Name, Name, 16, -1.072, 1.072, 20, -1.070, 1.070);
    fHChannelProfile2D_EOB_PM0->GetXaxis()->SetTitle("NewCHOD tile X (m)");
    fHChannelProfile2D_EOB_PM0->GetYaxis()->SetTitle("NewCHOD tile Y (m)");

    Name = "ChannelProfile2D_PM1 EOB";
    fHChannelProfile2D_EOB_PM1 = new TH2F(Name, Name, 16, -1.072, 1.072, 20, -1.070, 1.070);
    fHChannelProfile2D_EOB_PM1->GetXaxis()->SetTitle("NewCHOD tile X (m)");
    fHChannelProfile2D_EOB_PM1->GetYaxis()->SetTitle("NewCHOD tile Y (m)");
  }

  Name = "RecoHitProfile2D";
  fHRecoHitProfile2D = new TH2F(Name, Name, 16, -1.072, 1.072, 20, -1.070, 1.070);
  fHRecoHitProfile2D->GetXaxis()->SetTitle("NewCHOD tile X (m)");
  fHRecoHitProfile2D->GetYaxis()->SetTitle("NewCHOD tile Y (m)");

  Name = "TightRecoHitProfile2D";
  fHTightRecoHitProfile2D = new TH2F(Name, Name, 16, -1.072, 1.072, 20, -1.070, 1.070);
  fHTightRecoHitProfile2D->GetXaxis()->SetTitle("NewCHOD tile X (m)");
  fHTightRecoHitProfile2D->GetYaxis()->SetTitle("NewCHOD tile Y (m)");

  if (fHistosLevel>0) {
    Name = "TightPrimitiveProfile2D EOB";
    fHTightPrimitiveProfile2D_EOB = new TH2F(Name, Name, 16, -1.072, 1.072, 20, -1.070, 1.070);
    fHTightPrimitiveProfile2D_EOB->GetXaxis()->SetTitle("NewCHOD tile X (m)");
    fHTightPrimitiveProfile2D_EOB->GetYaxis()->SetTitle("NewCHOD tile Y (m)");

    Name = "NRecoHitsVsL0TriggerBit";
    fHNRecoHitsVsL0TriggerBit = new TH2F(Name, Name, 20, -0.5, 19.5, 8, -0.5, 7.5);
    fHNRecoHitsVsL0TriggerBit->GetXaxis()->SetTitle("Reconstructed hits");
    fHNRecoHitsVsL0TriggerBit->GetYaxis()->SetTitle("L0 trigger bit");

    Name = "NRecoHitsVsNoL0TriggerBit";
    fHNRecoHitsVsNoL0TriggerBit = new TH2F(Name, Name, 20, -0.5, 19.5, 8, -0.5, 7.5);
    fHNRecoHitsVsNoL0TriggerBit->GetXaxis()->SetTitle("Reconstructed hits");
    fHNRecoHitsVsNoL0TriggerBit->GetYaxis()->SetTitle("L0 trigger bit OFF");

    Name = "HitStatusVsChannel";
    fHHitStatusVsChannel = new TH2F(Name, Name, 400, 100.5, 500.5, 3, 0.5, 3.5);
    fHHitStatusVsChannel->GetXaxis()->SetTitle("Channel ID");
    fHHitStatusVsChannel->GetYaxis()->SetTitle("Hit Status");

    Name = "HitStatusVsReadoutChannel";
    fHHitStatusVsROChannel = new TH2F
      (Name, Name, fNChannels, -0.5, fNChannels-0.5, 3, 0.5, 3.5);
    fHHitStatusVsROChannel->GetXaxis()->SetTitle("RO channel ID");
    fHHitStatusVsROChannel->GetYaxis()->SetTitle("Hit Status");

    //Name = "TimeWrtReferenceVsChannel";
    //fHTimeWrtReferenceVsChannel = new TH2F (Name, Name, 400, 100.5, 500.5, 120, -30, 30);
    //fHTimeWrtReferenceVsChannel->GetXaxis()->SetTitle("Channel ID");
    //fHTimeWrtReferenceVsChannel->GetYaxis()->SetTitle("Digi time wrt CEDAR (ns)");

    //Name = "TimeWrtReferenceVsChannelNoT0";
    //fHTimeWrtReferenceVsChannelNoT0 = new TH2F (Name, Name, 400, 100.5, 500.5, 120, -30, 30);
    //fHTimeWrtReferenceVsChannelNoT0->GetXaxis()->SetTitle("Channel ID");
    //fHTimeWrtReferenceVsChannelNoT0->GetYaxis()->SetTitle("Digi time wrt CEDAR (ns)");

    Name = "TimeWrtReferenceVsBurst";
    fHTimeWrtReferenceVsBurst = new TH2F (Name, Name, 3000, -0.5, 2999.5, 160, -40, 40);
    fHTimeWrtReferenceVsBurst->GetXaxis()->SetTitle("Burst ID");
    fHTimeWrtReferenceVsBurst->GetYaxis()->SetTitle("Digi time wrt CEDAR (ns)");

    Name = "TightRecoHitTimeWrtReferenceVsTile";
    fHTightRecoHitTimeWrtReferenceVsTile = new TH2F (Name, Name, 350, 100.5, 450.5, 120, -30, 30);
    fHTightRecoHitTimeWrtReferenceVsTile->GetXaxis()->SetTitle("Tile ID");
    fHTightRecoHitTimeWrtReferenceVsTile->GetYaxis()->SetTitle("RecoHit time wrt CEDAR (ns)");

    Name = "TightRecoHitTimeWrtReferenceVsTileNoT0";
    fHTightRecoHitTimeWrtReferenceVsTileNoT0 = new TH2F (Name, Name, 350, 100.5, 450.5, 120, -30, 30);
    fHTightRecoHitTimeWrtReferenceVsTileNoT0->GetXaxis()->SetTitle("Tile ID");
    fHTightRecoHitTimeWrtReferenceVsTileNoT0->GetYaxis()->SetTitle("RecoHit time wrt CEDAR (ns)");

    Name = "LooseRecoHitTimeWrtReferenceVsTile";
    fHLooseRecoHitTimeWrtReferenceVsTile = new TH2F (Name, Name, 350, 100.5, 450.5, 120, -30, 30);
    fHLooseRecoHitTimeWrtReferenceVsTile->GetXaxis()->SetTitle("Tile ID");
    fHLooseRecoHitTimeWrtReferenceVsTile->GetYaxis()->SetTitle("RecoHit time wrt CEDAR (ns)");

    Name = "LooseRecoHitTimeWrtReferenceVsTileNoT0";
    fHLooseRecoHitTimeWrtReferenceVsTileNoT0 = new TH2F (Name, Name, 350, 100.5, 450.5, 120, -30, 30);
    fHLooseRecoHitTimeWrtReferenceVsTileNoT0->GetXaxis()->SetTitle("Tile ID");
    fHLooseRecoHitTimeWrtReferenceVsTileNoT0->GetYaxis()->SetTitle("RecoHit time wrt CEDAR (ns)");

    Name = "DeltaTimeVsTile";
    fHDeltaTimeVsTile = new TH2F(Name, Name, 350, 100.5, 450.5, 100, -10, 10);
    fHDeltaTimeVsTile->GetXaxis()->SetTitle("Tile ID");
    fHDeltaTimeVsTile->GetYaxis()->SetTitle("t(PMT1)-t(PMT0) (ns)");

    Name = "DeltaTimeVsTileProfile";
    fPDeltaTimeVsTile = new TProfile(Name, Name, 350, 100.5, 450.5);
    fPDeltaTimeVsTile->GetXaxis()->SetTitle("Tile ID");
    fPDeltaTimeVsTile->GetYaxis()->SetTitle("t(PMT1)-t(PMT0) (ns)");

    Name = "TightRecoHitProfileVsL0TriggerBit";
    fHTightRecoHitProfileVsL0TriggerBit = new TH2F(Name, Name, 350, 100.5, 450.5, 8, -0.5, 7.5);
    fHTightRecoHitProfileVsL0TriggerBit->GetXaxis()->SetTitle("RecoHit tile ID");
    fHTightRecoHitProfileVsL0TriggerBit->GetYaxis()->SetTitle("L0 trigger bit");

    Name = "TightRecoHitProfileVsNoL0TriggerBit";
    fHTightRecoHitProfileVsNoL0TriggerBit = new TH2F(Name, Name, 350, 100.5, 450.5, 8, -0.5, 7.5);
    fHTightRecoHitProfileVsNoL0TriggerBit->GetXaxis()->SetTitle("RecoHit tile ID");
    fHTightRecoHitProfileVsNoL0TriggerBit->GetYaxis()->SetTitle("L0 trigger bit OFF");
  }

  /*
  Name = "SlotVsLeadingTime"; // NB: ~1M bins
  fHSlotVsLeadingTime = new TH2F
    (Name, Name, 20001, -10000.5*TdcCalib, 10000.5*TdcCalib, 40, 60.5, 100.5);
  fHSlotVsLeadingTime->GetXaxis()->SetTitle("Leading time (ns)");
  fHSlotVsLeadingTime->GetYaxis()->SetTitle("Slot index");
  */
}

////////////////////////////////////////
// Write histograms into the output file

void NewCHODReconstruction::SaveHistograms() {

  TDirectory *NewCHODDir = GetOrMakeDir(fHistoFile, "NewCHODMonitor");
  if (fChannelHistograms) {
    TDirectory *TilesDir = GetOrMakeDir(NewCHODDir, "NewCHODTiles");
    GetOrMakeDir(TilesDir, "dT");
    GetOrMakeDir(TilesDir, "Time2D");
  }

  fHistoFile->cd("NewCHODMonitor");

  fHNDigis->Write();
  fHNRecoHits->Write();
  if (fHNRecoHitsVsL0TriggerBit)   fHNRecoHitsVsL0TriggerBit->Write();
  if (fHNRecoHitsVsNoL0TriggerBit) fHNRecoHitsVsNoL0TriggerBit->Write();
  fHNRecoHitsPerTile->Write();
  fHNQuadrants->Write();
  fHQuadrantTrigger->Write();
  if (fHNTightPrimitivesPerBurstEOB) fHNTightPrimitivesPerBurstEOB->Write();
  if (fHTotalPrimitiveCountsEOB) fHTotalPrimitiveCountsEOB->Write();
  if (fHErrorCountsEOB) fHErrorCountsEOB->Write();
  if (fHNHitsPerBurstEOB) fHNHitsPerBurstEOB->Write();
  fHInterRecoHitDistance->Write();

  fHHitStatus->Write();
  fHLeadingTime->Write();
  fHTrailingTime->Write();
  fHLeadingTimeRaw_NoTrailing->Write();
  fHTrailingTimeRaw_NoLeading->Write();

  fHTimeWrtReference->Write();
  fHTimeWrtReferenceNoT0->Write();
  fHTightRecoHitTimeWrtReference->Write();
  fHTightRecoHitTimeWrtReferenceNoT0->Write();
  fHLooseRecoHitTimeWrtReference->Write();
  fHLooseRecoHitTimeWrtReferenceNoT0->Write();

  fHWidth->Write();
  fHNErrorWord->Write();
  fHChannelProfile->Write();
  if (fHChannelProfileEOB) fHChannelProfileEOB->Write();
  fHROChannelProfile->Write();
  if (fHROChannelProfileEOB) fHROChannelProfileEOB->Write();
  if (fHChannelProfileVsBurst) fHChannelProfileVsBurst->Write();
  if (fHChannelProfileVsBurstEOB) fHChannelProfileVsBurstEOB->Write();
  fHTileAsymmetry->Write();
  if (fHTileAsymmetryEOB) fHTileAsymmetryEOB->Write();
  fHRecoHitProfile->Write();
  fHTightRecoHitProfile->Write();
  if (fHTightPrimitiveProfileEOB) fHTightPrimitiveProfileEOB->Write();
  fHTileAND->Write();
  fHTileOR->Write();
  fHDeltaTime->Write();
  fHDeltaTimeRecoHit->Write();
  fHHitFineTimeBits->Write();
  fHHitFineTime256->Write();
  fHRecoHitProfileMb1->Write();
  fHRecoHitProfileMb2->Write();
  fHRecoHitProfileMb3->Write();
  fHRecoHitProfileMb4->Write();
  fHRecoHitProfileMb5->Write();

  fHChannelProfile2D_PM0->Write();
  fHChannelProfile2D_PM1->Write();
  if (fHChannelProfile2D_EOB_PM0) fHChannelProfile2D_EOB_PM0->Write();
  if (fHChannelProfile2D_EOB_PM1) fHChannelProfile2D_EOB_PM1->Write();

  fHRecoHitProfile2D->Write();
  fHTightRecoHitProfile2D->Write();
  if (fHTightPrimitiveProfile2D_EOB) fHTightPrimitiveProfile2D_EOB->Write();

  if (fHHitStatusVsChannel) fHHitStatusVsChannel->Write();
  if (fHHitStatusVsROChannel) fHHitStatusVsROChannel->Write();
  if (fHTimeWrtReferenceVsChannel) fHTimeWrtReferenceVsChannel->Write();
  if (fHTimeWrtReferenceVsChannelNoT0) fHTimeWrtReferenceVsChannelNoT0->Write();
  if (fHTimeWrtReferenceVsBurst) fHTimeWrtReferenceVsBurst->Write();
  if (fHTightRecoHitTimeWrtReferenceVsTile)     fHTightRecoHitTimeWrtReferenceVsTile->Write();
  if (fHTightRecoHitTimeWrtReferenceVsTileNoT0) fHTightRecoHitTimeWrtReferenceVsTileNoT0->Write();
  if (fHLooseRecoHitTimeWrtReferenceVsTile)     fHLooseRecoHitTimeWrtReferenceVsTile->Write();
  if (fHLooseRecoHitTimeWrtReferenceVsTileNoT0) fHLooseRecoHitTimeWrtReferenceVsTileNoT0->Write();

  if (fHSlotVsLeadingTime) fHSlotVsLeadingTime->Write();
  if (fHDeltaTimeVsTile) fHDeltaTimeVsTile->Write();
  if (fPDeltaTimeVsTile) fPDeltaTimeVsTile->Write();

  if (fHTightRecoHitProfileVsL0TriggerBit)   fHTightRecoHitProfileVsL0TriggerBit->Write();
  if (fHTightRecoHitProfileVsNoL0TriggerBit) fHTightRecoHitProfileVsNoL0TriggerBit->Write();

  if (fChannelHistograms) {
    for (Int_t i=0; i<fNChannels; i++) static_cast<NewCHODChannel*>(fChannels[i])->Write(fHistoFile);
    for (Int_t i=0; i<fNTiles; i++) fTiles[i]->Write(fHistoFile);
  }

  fHistoFile->cd("/");
}

void NewCHODReconstruction::DeleteHistograms() {
  if (fHNDigis)                       delete fHNDigis;
  if (fHNRecoHits)                    delete fHNRecoHits;
  if (fHNRecoHitsVsL0TriggerBit)      delete fHNRecoHitsVsL0TriggerBit;
  if (fHNRecoHitsVsNoL0TriggerBit)    delete fHNRecoHitsVsNoL0TriggerBit;
  if (fHNRecoHitsPerTile)             delete fHNRecoHitsPerTile;
  if (fHNQuadrants)                   delete fHNQuadrants;
  if (fHQuadrantTrigger)              delete fHQuadrantTrigger;
  if (fHNTightPrimitivesPerBurstEOB)  delete fHNTightPrimitivesPerBurstEOB;
  if (fHTotalPrimitiveCountsEOB)      delete fHTotalPrimitiveCountsEOB;
  if (fHErrorCountsEOB)               delete fHErrorCountsEOB;
  if (fHNHitsPerBurstEOB)             delete fHNHitsPerBurstEOB;
  if (fHInterRecoHitDistance)         delete fHInterRecoHitDistance;

  if (fHHitStatus)                    delete fHHitStatus;
  if (fHLeadingTime)                  delete fHLeadingTime;
  if (fHTrailingTime)                 delete fHTrailingTime;
  if (fHLeadingTimeRaw_NoTrailing)    delete fHLeadingTimeRaw_NoTrailing;
  if (fHTrailingTimeRaw_NoLeading)    delete fHTrailingTimeRaw_NoLeading;

  if (fHTimeWrtReference)                 delete fHTimeWrtReference;
  if (fHTimeWrtReferenceNoT0)             delete fHTimeWrtReferenceNoT0;
  if (fHTightRecoHitTimeWrtReference)     delete fHTightRecoHitTimeWrtReference;
  if (fHTightRecoHitTimeWrtReferenceNoT0) delete fHTightRecoHitTimeWrtReferenceNoT0;
  if (fHLooseRecoHitTimeWrtReference)     delete fHLooseRecoHitTimeWrtReference;
  if (fHLooseRecoHitTimeWrtReferenceNoT0) delete fHLooseRecoHitTimeWrtReferenceNoT0;

  if (fHWidth)                    delete fHWidth;
  if (fHNErrorWord)               delete fHNErrorWord;
  if (fHChannelProfile)           delete fHChannelProfile;
  if (fHChannelProfileEOB)        delete fHChannelProfileEOB;
  if (fHROChannelProfile)         delete fHROChannelProfile;
  if (fHROChannelProfileEOB)      delete fHROChannelProfileEOB;
  if (fHChannelProfileVsBurst)    delete fHChannelProfileVsBurst;
  if (fHChannelProfileVsBurstEOB) delete fHChannelProfileVsBurstEOB;
  if (fHTileAsymmetry)            delete fHTileAsymmetry;
  if (fHTileAsymmetryEOB)         delete fHTileAsymmetryEOB;
  if (fHRecoHitProfile)           delete fHRecoHitProfile;
  if (fHTightRecoHitProfile)      delete fHTightRecoHitProfile;
  if (fHTightPrimitiveProfileEOB) delete fHTightPrimitiveProfileEOB;
  if (fHTileAND)                  delete fHTileAND;
  if (fHTileOR)                   delete fHTileOR;
  if (fHDeltaTime)                delete fHDeltaTime;
  if (fHDeltaTimeRecoHit)         delete fHDeltaTimeRecoHit;
  if (fHHitFineTimeBits)          delete fHHitFineTimeBits;
  if (fHHitFineTime256)           delete fHHitFineTime256;
  if (fHRecoHitProfileMb1)        delete fHRecoHitProfileMb1;
  if (fHRecoHitProfileMb2)        delete fHRecoHitProfileMb2;
  if (fHRecoHitProfileMb3)        delete fHRecoHitProfileMb3;
  if (fHRecoHitProfileMb4)        delete fHRecoHitProfileMb4;
  if (fHRecoHitProfileMb5)        delete fHRecoHitProfileMb5;

  if (fHChannelProfile2D_PM0)          delete fHChannelProfile2D_PM0;
  if (fHChannelProfile2D_PM1)          delete fHChannelProfile2D_PM1;
  if (fHChannelProfile2D_EOB_PM0)      delete fHChannelProfile2D_EOB_PM0;
  if (fHChannelProfile2D_EOB_PM1)      delete fHChannelProfile2D_EOB_PM1;

  if (fHRecoHitProfile2D)              delete fHRecoHitProfile2D;
  if (fHTightRecoHitProfile2D)         delete fHTightRecoHitProfile2D;
  if (fHTightPrimitiveProfile2D_EOB)   delete fHTightPrimitiveProfile2D_EOB;

  if (fHHitStatusVsChannel)            delete fHHitStatusVsChannel;
  if (fHHitStatusVsROChannel)          delete fHHitStatusVsROChannel;
  if (fHTimeWrtReferenceVsChannel)     delete fHTimeWrtReferenceVsChannel;
  if (fHTimeWrtReferenceVsChannelNoT0) delete fHTimeWrtReferenceVsChannelNoT0;
  if (fHTimeWrtReferenceVsBurst)       delete fHTimeWrtReferenceVsBurst;

  if (fHTightRecoHitTimeWrtReferenceVsTile)     delete fHTightRecoHitTimeWrtReferenceVsTile;
  if (fHTightRecoHitTimeWrtReferenceVsTileNoT0) delete fHTightRecoHitTimeWrtReferenceVsTileNoT0;
  if (fHLooseRecoHitTimeWrtReferenceVsTile)     delete fHLooseRecoHitTimeWrtReferenceVsTile;
  if (fHLooseRecoHitTimeWrtReferenceVsTileNoT0) delete fHLooseRecoHitTimeWrtReferenceVsTileNoT0;

  if (fHSlotVsLeadingTime)                   delete fHSlotVsLeadingTime;
  if (fHDeltaTimeVsTile)                     delete fHDeltaTimeVsTile;
  if (fPDeltaTimeVsTile)                     delete fPDeltaTimeVsTile;
  if (fHTightRecoHitProfileVsL0TriggerBit)   delete fHTightRecoHitProfileVsL0TriggerBit;
  if (fHTightRecoHitProfileVsNoL0TriggerBit) delete fHTightRecoHitProfileVsNoL0TriggerBit;

  ResetHistograms();
}

void NewCHODReconstruction::ResetHistograms() {
  fHNDigis = nullptr;
  fHNRecoHits = nullptr;
  fHNRecoHitsVsL0TriggerBit = nullptr;
  fHNRecoHitsVsNoL0TriggerBit = nullptr;
  fHNRecoHitsPerTile = nullptr;
  fHNQuadrants = nullptr;
  fHQuadrantTrigger = nullptr;
  fHNTightPrimitivesPerBurstEOB = nullptr;
  fHTotalPrimitiveCountsEOB = nullptr;
  fHErrorCountsEOB = nullptr;
  fHNHitsPerBurstEOB = nullptr;
  fHInterRecoHitDistance = nullptr;

  fHHitStatus = nullptr;
  fHLeadingTime = nullptr;
  fHTrailingTime = nullptr;
  fHLeadingTimeRaw_NoTrailing = nullptr;
  fHTrailingTimeRaw_NoLeading = nullptr;
  
  fHTimeWrtReference = nullptr;
  fHTimeWrtReferenceNoT0 = nullptr;
  fHTightRecoHitTimeWrtReference = nullptr;
  fHTightRecoHitTimeWrtReferenceNoT0 = nullptr;
  fHLooseRecoHitTimeWrtReference = nullptr;
  fHLooseRecoHitTimeWrtReferenceNoT0 = nullptr;

  fHWidth = nullptr;
  fHNErrorWord = nullptr;
  fHChannelProfile = nullptr;
  fHChannelProfileEOB = nullptr;
  fHROChannelProfile = nullptr;
  fHROChannelProfileEOB = nullptr;
  fHChannelProfileVsBurst = nullptr;
  fHChannelProfileVsBurstEOB = nullptr;
  fHTileAsymmetry = nullptr;
  fHTileAsymmetryEOB = nullptr;
  fHRecoHitProfile = nullptr;
  fHTightRecoHitProfile = nullptr;
  fHTightPrimitiveProfileEOB = nullptr;
  fHTileAND = nullptr;
  fHTileOR = nullptr;
  fHDeltaTime = nullptr;
  fHDeltaTimeRecoHit = nullptr;
  fHHitFineTimeBits = nullptr;
  fHHitFineTime256 = nullptr;
  fHRecoHitProfileMb1 = nullptr;
  fHRecoHitProfileMb2 = nullptr;
  fHRecoHitProfileMb3 = nullptr;
  fHRecoHitProfileMb4 = nullptr;
  fHRecoHitProfileMb5 = nullptr;

  fHChannelProfile2D_PM0 = nullptr;
  fHChannelProfile2D_PM1 = nullptr;
  fHChannelProfile2D_EOB_PM0 = nullptr;
  fHChannelProfile2D_EOB_PM1 = nullptr;

  fHRecoHitProfile2D = nullptr;
  fHTightRecoHitProfile2D = nullptr;
  fHTightPrimitiveProfile2D_EOB = nullptr;

  fHHitStatusVsChannel = nullptr;
  fHHitStatusVsROChannel = nullptr;
  fHTimeWrtReferenceVsChannel = nullptr;
  fHTimeWrtReferenceVsChannelNoT0 = nullptr;
  fHTimeWrtReferenceVsBurst = nullptr;

  fHTightRecoHitTimeWrtReferenceVsTile = nullptr;
  fHTightRecoHitTimeWrtReferenceVsTileNoT0 = nullptr;
  fHLooseRecoHitTimeWrtReferenceVsTile = nullptr;
  fHLooseRecoHitTimeWrtReferenceVsTileNoT0 = nullptr;

  fHSlotVsLeadingTime = nullptr;
  fHDeltaTimeVsTile = nullptr;
  fPDeltaTimeVsTile = nullptr;
  fHTightRecoHitProfileVsL0TriggerBit = nullptr;
  fHTightRecoHitProfileVsNoL0TriggerBit = nullptr;  
}

void NewCHODReconstruction::EndProcessing() {
  NA62VReconstruction::EndProcessing();

  // Build the asymmetries of the PM1 and PM0 rates
  TString Name = "TileAsymmetry";
  fHTileAsymmetry = new TH1D(Name, Name, 350, 100.5, 450.5);
  fHTileAsymmetry->GetXaxis()->SetTitle("NewCHOD tile");
  fHTileAsymmetry->GetYaxis()->SetTitle("Asymmetry N(hits): (PM1-PM0)/(PM1+PM0)");

  for (Int_t i=1; i<=fHChannelProfile->GetNbinsX(); i++) {
    Int_t ChannelID = (Int_t)fHChannelProfile->GetBinCenter(i);
    if (ChannelID%100<1 || ChannelID%100>38) continue;
    Double_t x    = fHChannelProfile->GetBinContent(i+50);
    Double_t y    = fHChannelProfile->GetBinContent(i);
    if (x+y<1.0) continue;
    Double_t dx   = sqrt(x);
    Double_t dy   = sqrt(y);
    Double_t f    = (x-y)/(x+y);
    Double_t dfdx = 2.0*y/(x+y)/(x+y);
    Double_t dfdy = 2.0*x/(x+y)/(x+y);
    Double_t df   = sqrt((dfdx*dx)*(dfdx*dx)+(dfdy*dy)*(dfdy*dy));
    fHTileAsymmetry->SetBinContent(i, f);
    fHTileAsymmetry->SetBinError(i, df);
  }

  if (fHistosLevel>0) {
    Name = "TileAsymmetryEOB";
    fHTileAsymmetryEOB = new TH1D(Name, Name, 350, 100.5, 450.5);
    fHTileAsymmetryEOB->GetXaxis()->SetTitle("NewCHOD tile");
    fHTileAsymmetryEOB->GetYaxis()->SetTitle("Asymmetry N(hits): (PM1-PM0)/(PM1+PM0)");

    for (Int_t i=1; i<=fHChannelProfileEOB->GetNbinsX(); i++) {
      Int_t ChannelID = (Int_t)fHChannelProfile->GetBinCenter(i);
      if (ChannelID%100<1 || ChannelID%100>38) continue;
      Double_t x    = fHChannelProfileEOB->GetBinContent(i+50);
      Double_t y    = fHChannelProfileEOB->GetBinContent(i);
      if (x+y<1.0) continue;
      Double_t dx   = sqrt(x);
      Double_t dy   = sqrt(y);
      Double_t f    = (x-y)/(x+y);
      Double_t dfdx = 2.0*y/(x+y)/(x+y);
      Double_t dfdy = 2.0*x/(x+y)/(x+y);
      Double_t df   = sqrt((dfdx*dx)*(dfdx*dx)+(dfdy*dy)*(dfdy*dy));
      fHTileAsymmetryEOB->SetBinContent(i, f);
      fHTileAsymmetryEOB->SetBinError(i, df);
    }
  }

  SaveHistograms();
}

void NewCHODReconstruction::FillTimes(Double_t ReferenceTime) {

  // @@ can use a trigger mask here, e.g.
  // if (!(fL0TriggerFlags & 5)) return;

  // Common part for all the subdetectors: currently global T0 evaluation only
  NA62VReconstruction::FillTimes(ReferenceTime);

  ////////////////////////////////////
  // Digi times wrt the reference time

  TClonesArray& Digis = (*(fTdcEvent->GetHits()));
  for (Int_t i=0; i<fNDigis; i++) {
    TNewCHODDigi *Digi       = static_cast<TNewCHODDigi*>(Digis[i]);
    Int_t    ch              = Digi->GetChannelID();
    Int_t    ROch            = static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRO(ch);
    Double_t LeadingTime     = GetRecoTime    (Digi, 1); // corrected for global T0 and channel T0s
    Double_t LeadingTimeNoT0 = GetRecoTimeNoT0(Digi, 1); // corrected for global T0 only
    Double_t dT              = LeadingTime     - ReferenceTime;
    Double_t dT_NoT0         = LeadingTimeNoT0 - ReferenceTime;
    if (fHTimeWrtReference)     fHTimeWrtReference->Fill(dT);          // T0-corrected
    if (fHTimeWrtReferenceNoT0) fHTimeWrtReferenceNoT0->Fill(dT_NoT0); // not T0-corrected
    if (fHTimeWrtReferenceVsChannel)     fHTimeWrtReferenceVsChannel->Fill(ch, dT);
    if (fHTimeWrtReferenceVsChannelNoT0) fHTimeWrtReferenceVsChannelNoT0->Fill(ch, dT_NoT0);
    if (fHTimeWrtReferenceVsBurst)       fHTimeWrtReferenceVsBurst->Fill(fRecoEvent->GetBurstID(), dT);

    // The standard input histograms for T0 computation defined in NA62VReconstruction
    if (fHRecoHitTimeWrtReferenceVsROChannel)     fHRecoHitTimeWrtReferenceVsROChannel->Fill(ROch, dT);
    if (fHRecoHitTimeWrtReferenceVsROChannelNoT0) fHRecoHitTimeWrtReferenceVsROChannelNoT0->Fill(ROch, dT_NoT0);
  }

  ////////////////////////////
  // RecoHit timing monitoring

  Int_t NRecoHits = fRecoEvent->GetNHits();
  TClonesArray &Hits = (*(fRecoEvent->GetHits()));
  
  for (Int_t i=0; i<NRecoHits; i++) {
    TRecoNewCHODHit *Hit = static_cast<TRecoNewCHODHit*>( Hits[i]);
    Int_t Tile    = Hit->GetTileID();    // geometric tile ID
    Int_t SeqTile = Hit->GetSeqTileID(); // sequential tile ID
    if (Hit->GetType()==kTightCandidate) {
      Double_t Time1 = Hit->GetTime1(); // low number PMT
      Double_t Time2 = Hit->GetTime2(); // high number PMT
      fTiles[SeqTile]->FillDeltaTime(Time1-ReferenceTime, Time2-ReferenceTime); // corrected for channel T0s
      fHTightRecoHitTimeWrtReference->Fill(Hit->GetTime()-ReferenceTime);
      fHTightRecoHitTimeWrtReferenceNoT0->Fill(Hit->GetTimeNoT0()-ReferenceTime);
      if (fHTightRecoHitTimeWrtReferenceVsTile) {
	fHTightRecoHitTimeWrtReferenceVsTile->Fill(Tile, Hit->GetTime()-ReferenceTime);
	fHTightRecoHitTimeWrtReferenceVsTileNoT0->Fill(Tile, Hit->GetTimeNoT0()-ReferenceTime);
      }
    }
    else {
      fHLooseRecoHitTimeWrtReference->Fill(Hit->GetTime()-ReferenceTime);
      fHLooseRecoHitTimeWrtReferenceNoT0->Fill(Hit->GetTimeNoT0()-ReferenceTime);
      if (fHLooseRecoHitTimeWrtReferenceVsTile) {
	fHLooseRecoHitTimeWrtReferenceVsTile->Fill(Tile, Hit->GetTime()-ReferenceTime);
	fHLooseRecoHitTimeWrtReferenceVsTileNoT0->Fill(Tile, Hit->GetTimeNoT0()-ReferenceTime);
      }
    }
  }
}
