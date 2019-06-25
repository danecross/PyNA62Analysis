// ------------------------------------------------------------------
// History:
//
// Major update: Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-03-05
// Major update: Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2014-10-06
// Created by Antonino Sergi (Antonino.Sergi@cern.ch)      2008-05-05
//
// ------------------------------------------------------------------

/// \class MUV3Reconstruction
/// \Brief
/// MUV3 reconstruction
/// \EndBrief
/// \Detailed
/// Tight MUV3 candidates are defined as coincidences in the two channels in the same tile.
/// If one of the channels in a tile is masked (as specified via the MaskedChannels cards in MUV3.conf),
/// then LooseMasked candidates based on a single hit are produced for that tile.
/// For unmatched hits, Loose candidates based on a single hit are produced.
/// Reconstruction of Loose and LooseMasked candidates can be suppressed in MUV3.conf
/// (not recommended, leads to sigfinicant MUV3 inefficiency).
/// The possible candidate types are defined by an enumerated type:
/// kTightCandidate, kLooseCandidate, kLooseMaskedCandidate, kUndefinedCandidate.
/// Tight candidate time is defined as the latest of the two hit times.
/// This definition is necessitated by the possibility of a muon hitting a PMT window,
/// generating a "Cherenkov" hit that is about 2.5 ns early.
/// Consequently, tight candidate times are T0-corrected
/// (on top of the standard channel T0 corrections) using the Tile T0s,
/// because the tight candidate times are delayed with respect to the channel times.
/// The typical correction to the tight candidate time is -0.2 ns (so the typical tile T0 is +0.2 ns).
/// \EndDetailed

#include "Riostream.h"
#include "MUV3Geometry.hh"
#include "NA62RecoManager.hh"
#include "NA62ConditionsService.hh"
#include "MUV3Reconstruction.hh"
#include "MUV3Channel.hh"
#include "TRecoMUV3Event.hh"
#include "TRecoMUV3Hit.hh"
#include "TMUV3Digi.hh"
#include "TDCBRawDecoder.hh"
#include "TTDCBSpecialTrigger.hh"
#include "NA62Buffer.hh"
#include "NA62BufferProto.hh"
#include "TString.h"
#include "TRegexp.h"

MUV3Reconstruction::MUV3Reconstruction (TFile* HistoFile, TString ConfigFileName) :
  NA62VReconstruction(HistoFile, "MUV3", ConfigFileName) {

  fRecoEvent = new TRecoMUV3Event();
  fGeo = MUV3Geometry::GetInstance();

  // Default parameters
  fRawDecoder                 = 0;
  fTimeWindow                 = -999.;
  fEdgeRequirement            = kBothEdges;
  fNTiles                     = 0;
  fCandidate                  = 0;
  fPrintCandidateInfo         = kFALSE;
  fBuildLooseCandidates       = kTRUE;
  fBuildLooseMaskedCandidates = kTRUE;
  fEnableT0Tiles              = kTRUE;
  fDigitizerMode              = 1;
  fDigiHistoFlag              = 0;
  fChannelTimeResolution      = -999.;
  fT0TilesFileName            = "";
  fMaskedChannelInputFile     = "";

  ResetHistograms();
  ParseConfFile(fConfigFileName);
  fMaskedChannels.clear();
  ParseMaskedChannelInputFile();
  fTiles = new MUV3Tile*[fNTiles];
  for (Int_t i=0; i<fNTiles; i++) fTiles[i] = nullptr;
}

MUV3Reconstruction::~MUV3Reconstruction() {
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

void MUV3Reconstruction::Init(NA62VReconstruction* MainReco) {

  // Common part for all the subdetectors
  NA62VReconstruction::Init(MainReco);

  // Initialize channels
  for (Int_t ich=0; ich<fNChannels; ich++) {
    Int_t PositionID = static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRemap(ich);
    fChannels[ich] = new MUV3Channel(PositionID, ich, kFALSE);
  }

  for (UInt_t i=0; i<fMaskedChannels.size(); i++) { // channels masked from the config file
    Int_t ich  = fMaskedChannels[i];
    Int_t ROch = static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRO(ich);
    if (ROch>=0) fChannels[ROch]->Disable();
  }

  // Initialize tiles
  for (Int_t i=0; i<fNTiles; i++) {
    Int_t Position1 = i;
    Int_t Position2 = i+200;
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
    fTiles[i] = new MUV3Tile
      (i, Position1, Position2, ROch1, ROch2, TileEnabled, fChannelHistograms);
  }

  ReadT0s();     // Load T0 constants (channels)
  ReadTileT0s(); // Load T0 constants (tiles)
  InitHistograms();

  /*
  // Mark masked channels with an X: this had to be done for 2016 only
  Int_t hval = 1, lval = -1;

  // Temporary: the four missing corner tiles
  fHMaskedProfile2D_PM0->SetBinContent( 1, 1,lval);
  fHMaskedProfile2D_PM0->SetBinContent( 1,12,lval);
  fHMaskedProfile2D_PM0->SetBinContent(12, 1,lval);
  fHMaskedProfile2D_PM0->SetBinContent(12,12,lval);
  fHMaskedProfile2D_PM1->SetBinContent( 1, 1,lval);
  fHMaskedProfile2D_PM1->SetBinContent( 1,12,lval);
  fHMaskedProfile2D_PM1->SetBinContent(12, 1,lval);
  fHMaskedProfile2D_PM1->SetBinContent(12,12,lval);

  fHMaskedProfile2DInner_PM0->SetBinContent(2,2,hval);
  fHMaskedProfile2DInner_PM1->SetBinContent(2,2,hval);

  for (Int_t ich=0; ich<fNChannels; ich++) {
    Int_t val   = (fChannels[ich]->GetEnabled()) ? hval : lval;
    Int_t posID = fChannels[ich]->GetGeoChannelID();
    Int_t tile  = posID%200;
    Bool_t PM0  = posID<200;
    Double_t x  = 0.001*fGeo->GetTileCentreX(tile);
    Double_t y  = 0.001*fGeo->GetTileCentreY(tile);

    if (tile<144) { // outer tiles
      Int_t binx = fHMaskedProfile2D_PM0->GetXaxis()->FindBin(x);
      Int_t biny = fHMaskedProfile2D_PM0->GetYaxis()->FindBin(y);
      if (PM0) fHMaskedProfile2D_PM0->SetBinContent(binx, biny, val);
      else     fHMaskedProfile2D_PM1->SetBinContent(binx, biny, val);
    }
    else { // inner tiles
      Int_t binx = fHMaskedProfile2DInner_PM0->GetXaxis()->FindBin(x);
      Int_t biny = fHMaskedProfile2DInner_PM0->GetYaxis()->FindBin(y);
      if (PM0) fHMaskedProfile2DInner_PM0->SetBinContent(binx, biny, val);
      else     fHMaskedProfile2DInner_PM1->SetBinContent(binx, biny, val);      
    }
  }
  */
}

///////////////////////////////////////////////////////////////////
// Read MUV3 reconstruction parameters from the configuration file

void MUV3Reconstruction::ParseConfFile(TString ConfFileName) {

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
    else if (Line.BeginsWith("NTiles")) {
      fNTiles = TString(Line(TRegexp("[0-9]+"))).Atoi();
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
    else if (Line.BeginsWith("EnableT0Tiles")) {
      Line.Remove(7,1);
      fEnableT0Tiles = TString(Line(TRegexp("[0-1]"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("MaskedChannelInputFile")) {
      TObjArray *l = Line.Tokenize(" ");
      fMaskedChannelInputFile = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("T0TilesFileInput")) {
      TObjArray *l = Line.Tokenize(" ");
      fT0TilesFileName = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("DigitizerMode")) { // required to book histograms
      fDigitizerMode = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("DigiHistogramFlag")) { // required to book histograms
      fDigiHistoFlag = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("ChannelTimeResolution")) { // required for MC tile T0s corrections
      fChannelTimeResolution = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof() * ns;
      continue;
    }
  }
  confFile.close();

  // Sanity checks
  if (fNTiles<=0) {
    std::cout << "[MUV3Reconstruction] Error: invalid number of tiles specified"<< std::endl;
    exit(kWrongConfiguration);
  }
  if (!fT0TilesFileName.Length()) {
    std::cout << "[MUV3Reconstruction] Error: T0 file for tiles not defined" << std::endl;
    exit(kWrongConfiguration);
  }
  if (fTimeWindow<=0) {
    std::cout << "[MUV3Reconstruction] Error: time window <= 0" << std::endl;
    exit(kWrongConfiguration);
  }
  if (abs(fEdgeRequirement)>3) {
    std::cout << "[MUV3Reconstruction] Error: invalid edge requirement" << std::endl;
    exit(kWrongConfiguration);
  }
}

void MUV3Reconstruction::ParseMaskedChannelInputFile() {
  if (NA62ConditionsService::GetInstance()->Open(fMaskedChannelInputFile)!=kSuccess) return;
  TString Line;
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fMaskedChannelInputFile))) {
    if (Line.BeginsWith("#")) continue;
    else if (Line.BeginsWith("T0TilesFileInput")) {
      TObjArray *l = Line.Tokenize(" ");
      for (Int_t i=0; i<(l->GetEntries()-1); i++) {
        Int_t id = static_cast<TObjString*>(l->At(i+1))->GetString().Atoi();
        fMaskedChannels.push_back(id);
      }
    }
  }
  NA62ConditionsService::GetInstance()->Close(fMaskedChannelInputFile);
  std::cout << "[MUV3Reconstruction] Masked channels:";
  if (fMaskedChannels.size()) {
    for (UInt_t i=0; i<fMaskedChannels.size(); i++) std::cout << " " << fMaskedChannels[i];
    std::cout << std::endl;
  }
  else {
    std::cout << " none" << std::endl;
  }
}

//////////////////////////////
// Read the channel T0 offsets

void MUV3Reconstruction::ReadTileT0s () {
  TString Line;

  if (fEnableT0Tiles) {
    if (fIsRawData) { // Data: read tile T0s
      if (NA62ConditionsService::GetInstance()->Open(fT0TilesFileName)!=kSuccess) {
	std::cout << "[MUV3Reconstruction] Warning: tile T0 correction file not found, defaults used"<< std::endl;
	for (Int_t i=0; i<fNTiles; i++) fTiles[i]->SetT0(0.2);
      }
      while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fT0TilesFileName))) {
	if (Line.BeginsWith("#")) continue;
	TObjArray *l = Line.Tokenize(" ");
	Int_t     ch = static_cast<TObjString*>(l->At(0))->GetString().Atoi();
	Double_t  t0 = static_cast<TObjString*>(l->At(2))->GetString().Atof();
	if (fabs(t0)>999.0) t0 = +0.2; // -999.999: disabled tile, +999.999: failed to compute T0
	fTiles[ch]->SetT0(t0);
        delete l;
      }
      NA62ConditionsService::GetInstance()->Close(fT0TilesFileName);
    }
    else { // MC: tile T0s are determined by the channel time resolution
      for (Int_t i=0; i<fNTiles; i++) fTiles[i]->SetT0(0.53*fChannelTimeResolution);
    }
  }
  // PrintTileT0s();
}

///////////////////////////////////////////////////////////////////////////
// Print tile T0 constants used in a format readable by the reconstruction:
// +-999.999 (indicating failed T0 fits) will be replaced with 0.200

void MUV3Reconstruction::PrintTileT0s() {
  std::cout <<"# MUV3 tile T0 constants used:" << std::endl;
  for (Int_t it=0; it<fNTiles; it++) {
    std::cout << Form("%4d %4d %8.3f\n", it, 0, fTiles[it]->GetT0());
  }
}

void MUV3Reconstruction::ResetTileT0s() {
  for (Int_t it=0; it<fNTiles; it++) fTiles[it]->SetT0(0.0);
}

//////////////////////////////////////////////////////////

TDetectorVEvent* MUV3Reconstruction::Trigger (TDetectorVEvent* tEvent, Event* /*tGenEvent*/) {
  return tEvent;
}

/////////////////////////////////////////////////////////

TRecoVEvent* MUV3Reconstruction::ProcessEvent (TDetectorVEvent* tEvent, Event* tGenEvent) {
  
  // TMUV3Event* tMUV3Event = static_cast<TMUV3Event*>(tEvent);
  if (fIsRawData && tEvent->IsA() == TSpecialTriggerEvent::Class()) {
    EOBEventMonitor(tEvent);
    return 0;
  }

  // Common part for all the subdetectors
  NA62VReconstruction::ProcessEvent(tEvent, tGenEvent);

  // Data type: 0x1=physics, 0x2=periodics, 0x4=calibration, 0x8=LKr calibration, 0x10=control, ...
  fL0DataType = (!fIsRawData) ? 0x1 :
    NA62RecoManager::GetInstance()->GetEventHeader()->GetL0TPData()->GetDataType();
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

  TDCEvent* TdcEvent = static_cast<TDCEvent*>(tEvent);
  TDCEventMonitor(TdcEvent);

  Int_t NDigis = TdcEvent->GetNHits();

  ///////////////////////////////////
  // Convert MUV3 Digis into RecoHits

  TClonesArray& Digis = (*(TdcEvent->GetHits()));
  for (Int_t iDigi=0; iDigi<NDigis; iDigi++) {

    TMUV3Digi *Digi = static_cast<TMUV3Digi*>( Digis[iDigi]);

    // Status: 1=leading edge only, 2=trailing edge only, 3=both.
    // "Lost" edges is usually due to the hit being close to boundary of the RO window.
    // Digis with lost trailing edge are found to have biased times wrt Cedar time.
    // Digis with lost leading edge are affected by poor resolution on the trailing time.
    // Therefore is it recommended to discard these Digis for 2014-15 data.
    // However trailing edges were disabled in 2016 to increase max rate.
    //
    // Edge requirement for the hit reconstruction:
    //  1 = leading edge exists, 2 = trailing edge exists, 3 = leading+trailing edges exist (default)
    //  0 = any hit, -1 = leading edge only, -2 = trailing edge only

    Bool_t EdgeOK = kTRUE;
    if (fEdgeRequirement>0)
      EdgeOK = (Digi->GetDetectedEdge()==kBothEdges || Digi->GetDetectedEdge()==fEdgeRequirement);
    if (fEdgeRequirement<0)
      EdgeOK = (Digi->GetDetectedEdge()==abs(fEdgeRequirement));
    if (!EdgeOK) continue; // Digis not matching the required edge condition are ignored

    // Discard Digis in masked channels
    Int_t ROch = static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRO(Digi->GetChannelID());
    if (!fChannels[ROch]->GetEnabled()) continue;

    Double_t RecoLeadingTime      = GetRecoTime(Digi, 1); // T0-corrected
    Double_t RecoTrailingTime     = GetRecoTime(Digi, 2);
    Double_t RecoLeadingTimeNoT0  = GetRecoTimeNoT0(Digi, 1);
    Double_t RecoTrailingTimeNoT0 = GetRecoTimeNoT0(Digi, 2);

    TRecoMUV3Hit* RecoHit = static_cast<TRecoMUV3Hit*>(fRecoEvent->AddHit(Digi));
    RecoHit->DecodeChannelID();
    RecoHit->SetDetectedEdge(Digi->GetDetectedEdge());
    RecoHit->SetLeadingTime(RecoLeadingTime);
    RecoHit->SetTrailingTime(RecoTrailingTime);
    RecoHit->SetTime(RecoLeadingTime);
    RecoHit->SetLeadingTimeNoT0(RecoLeadingTimeNoT0);
    RecoHit->SetTrailingTimeNoT0(RecoTrailingTimeNoT0);
    RecoHit->SetTimeNoT0(RecoLeadingTimeNoT0);

    RecoHit->SetROChannelID
      (static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRO(RecoHit->GetChannelID()));

    Int_t Tile = RecoHit->GetTileID();
    Double_t x = fGeo->GetTileCentreX(Tile) * mm;
    Double_t y = fGeo->GetTileCentreY(Tile) * mm;
    RecoHit->SetPosition(TVector3(x, y, 246800*mm));
    fHNRecoHitsPerTile->Fill(Tile);
  }

  Int_t NRecoHits = fRecoEvent->GetNHits();
  fHNRecoHits->Fill(NRecoHits);
  if (fIsRawData && PhysicsData) {
    for (Int_t i=0; i<8; i++) {
      if ((fL0TriggerFlags>>i)&1) fHNRecoHitsVsL0TriggerBit->Fill(NRecoHits, i);
      else                        fHNRecoHitsVsNoL0TriggerBit->Fill(NRecoHits, i);
    }
  }
  fNRecoHitsPerBurst += NRecoHits;

  //////////////////////////////
  // Reconstruct MUV3 candidates

  TClonesArray &Hits = (*(fRecoEvent->GetHits()));

  /////////////////////////////////////////////////////////
  // Build tight candidates (i.e. coincidences of two PMTs)

  Bool_t Done[NRecoHits];
  for (Int_t i=0; i<NRecoHits; i++) Done[i] = kFALSE;

  for (Int_t i=0; i<NRecoHits; i++) { // potential low channels

    if (Done[i]) continue;
    TRecoMUV3Hit *Hit1 = static_cast<TRecoMUV3Hit*>(Hits[i]);
    Int_t ch1 = Hit1->GetChannelID();
    if (ch1>=200) continue;

    for (Int_t j=0; j<NRecoHits; j++) { // potential high channels

      if (Done[j]) continue;
      TRecoMUV3Hit *Hit2 = static_cast<TRecoMUV3Hit*>( Hits[j]);
      Int_t ch2 = Hit2->GetChannelID();
      if (ch2<200) continue;
      if (ch2-ch1 != 200) continue;

      Double_t Time1 = Hit1->GetTime(); // low-number channel
      Double_t Time2 = Hit2->GetTime(); // high-number channel
      if (fabs(Time1-Time2)>fTimeWindow) continue;

      Double_t Time1NoT0 = Hit1->GetTimeNoT0();
      Double_t Time2NoT0 = Hit2->GetTimeNoT0();
      Int_t    Tile      = ch1%200;

      TRecoMUV3Candidate *Candidate = static_cast<TRecoMUV3Candidate*>(fRecoEvent->AddCandidate());
      Candidate->SetType(kTightCandidate);
      Candidate->AddHit(i); // associate a hit to candidate
      Candidate->AddHit(j); // associate a hit to candidate
      Candidate->SetTileID(Tile);
      Candidate->SetChannel1(ch1);
      Candidate->SetChannel2(ch2);
      Candidate->SetROChannel1
	(static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRO(ch1));
      Candidate->SetROChannel2
	(static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRO(ch2));
      Candidate->SetX(fGeo->GetTileCentreX(Tile));
      Candidate->SetY(fGeo->GetTileCentreY(Tile));
      Double_t TileT0 = (fEnableT0Tiles) ? fTiles[ch1]->GetT0() : 0.0;

      Candidate->SetTime1       (Time1);
      Candidate->SetTime2       (Time2);
      Candidate->SetTime1NoT0   (Time1NoT0);
      Candidate->SetTime2NoT0   (Time2NoT0);
      Candidate->SetTime        (TMath::Max(Time1,Time2) - TileT0);
      Candidate->SetTimeNoTileT0(TMath::Max(Time1,Time2));
      Candidate->SetTimeNoT0    (TMath::Max(Time1NoT0,Time2NoT0));

      Done[i] = Done[j] = kTRUE;
      fNTightCandidatesPerBurst++;
    }
  }

  //////////////////////////////////////////////////////////////
  // Build the loose candidates using the remaining hits.
  // There are two types of loose candidates.
  // LooseMasked: partner channel masked in the config file;
  // Loose:       partner channel not masked but no match found.

  if (fBuildLooseCandidates || fBuildLooseMaskedCandidates) {
    for (Int_t i=0; i<NRecoHits; i++) {
      if (Done[i]) continue;

      TRecoMUV3Hit *Hit1 = static_cast<TRecoMUV3Hit*>(Hits[i]);
      Int_t ch1  = Hit1->GetChannelID();
      Int_t ch2  = (ch1<200) ? ch1+200 : ch1-200;
      Int_t Type = ChannelMasked(ch2) ? kLooseMaskedCandidate : kLooseCandidate;

      if (Type==kLooseMaskedCandidate && !fBuildLooseMaskedCandidates) continue;
      if (Type==kLooseCandidate       && !fBuildLooseCandidates)       continue;

      Int_t    Tile      = ch1%200;
      Double_t Time1     = Hit1->GetTime();
      Double_t Time1NoT0 = Hit1->GetTimeNoT0();

      TRecoMUV3Candidate *Candidate = static_cast<TRecoMUV3Candidate*>(fRecoEvent->AddCandidate());
      Candidate->SetType(Type);
      Candidate->AddHit(i); // associate a hit to candidate
      Candidate->SetTileID(Tile);
      Candidate->SetChannel1(ch1);
      Candidate->SetChannel2(-1);
      Candidate->SetROChannel1
	(static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRO(ch1));
      Candidate->SetROChannel2(-1);

      Candidate->SetX(fGeo->GetTileCentreX(Tile));
      Candidate->SetY(fGeo->GetTileCentreY(Tile));
      Candidate->SetTime1       (Time1);
      Candidate->SetTime2       (-999.);
      Candidate->SetTime1NoT0   (Time1NoT0);
      Candidate->SetTime2NoT0   (-999.);
      Candidate->SetTime        (Time1); // no tile T0 correction for loose candidates
      Candidate->SetTimeNoTileT0(Time1);
      Candidate->SetTimeNoT0    (Time1NoT0);

      Done[i] = kTRUE;
      fNLooseCandidatesPerBurst++;
    }
  }

  ///////////////////////////////////////////////////////////
  // RecoHit time differences in tiles: T(PMT 200+N)-T(PMT N)

  Int_t HitsInChannels[360];
  for (Int_t i=0; i<360; i++) HitsInChannels[i] = 0;

  for (Int_t i=0; i<NRecoHits; i++) {
    TRecoMUV3Hit *Hit1 = static_cast<TRecoMUV3Hit*>(Hits[i]);
    Int_t ch1 = Hit1->GetChannelID();
    HitsInChannels[ch1]++;
    if (ch1>=200) continue;

    for (Int_t j=0; j<NRecoHits; j++) {
      TRecoMUV3Hit *Hit2 = static_cast<TRecoMUV3Hit*>(Hits[j]);
      Int_t ch2 = Hit2->GetChannelID();
      if (ch2<200) continue;
      if (ch2-ch1 != 200) continue;

      Int_t    Tile  = ch1%200;
      Double_t Time1 = Hit1->GetTime(); //  low number PMT; T0-corrected
      Double_t Time2 = Hit2->GetTime(); // high number PMT; T0-corrected
      Double_t DeltaTime = Time2 - Time1;
      fHDeltaTime->Fill(DeltaTime);
      if (fHDeltaTimeVsTile) fHDeltaTimeVsTile->Fill(Tile, DeltaTime);
      if (fPDeltaTimeVsTile) fPDeltaTimeVsTile->Fill(Tile, DeltaTime);
    }
  }

  ////////////////////////////////
  // AND and OR rates in each tile

  for (Int_t i=0; i<fNTiles; i++) {
    Bool_t iAND = (HitsInChannels[i] && HitsInChannels[i+200]);
    Bool_t iOR  = (HitsInChannels[i] || HitsInChannels[i+200]);
    if (iAND) fHTileAND->Fill(i);
    if (iOR)  fHTileOR->Fill(i);
  }

  ///////////////////////////////
  // Monitoring of the candidates

  fHNCandidates->Fill(fRecoEvent->GetNCandidates());
  if (fIsRawData && PhysicsData) {
    for (Int_t i=0; i<8; i++) {
      if ((fL0TriggerFlags>>i)&1) fHNCandidatesVsL0TriggerBit->Fill(fRecoEvent->GetNCandidates(), i);
      else                        fHNCandidatesVsNoL0TriggerBit->Fill(fRecoEvent->GetNCandidates(), i);
    }
  }

  for (Int_t iCand=0; iCand<fRecoEvent->GetNCandidates(); iCand++) {
    fCandidate = static_cast<TRecoMUV3Candidate*>(fRecoEvent->GetCandidate(iCand));
    Int_t Tile = fCandidate->GetTileID();

    fHCandidateProfile->Fill(Tile);
    if (fCandidate->GetType()==kTightCandidate) {
      fHTightCandidateProfile->Fill(Tile);
      fHDeltaTimeCandidate->Fill(fCandidate->GetDeltaTime());
      if (fIsRawData && PhysicsData) {
	for (Int_t i=0; i<8; i++) {
	  if ((fL0TriggerFlags>>i)&1) fHTightCandidateProfileVsL0TriggerBit->Fill(Tile, i);
	  else                        fHTightCandidateProfileVsNoL0TriggerBit->Fill(Tile, i);
	}
      }
    }
    Double_t x = fGeo->GetTileCentreX(Tile) * mm;
    Double_t y = fGeo->GetTileCentreY(Tile) * mm;
    if (Tile<144 && Tile!=65 && Tile!=66 && Tile!=77 && Tile!=78) { // outer tiles
      if (fHCandidateProfile2D) {
	fHCandidateProfile2D->Fill(x/m, y/m);
	if (fCandidate->GetType()==kTightCandidate) fHTightCandidateProfile2D->Fill(x/m, y/m);
      }
    }
    else if (Tile>=144) { // inner tiles
      if (fHCandidateProfile2DInner) {
	fHCandidateProfile2DInner->Fill(x/m, y/m);
	if (fCandidate->GetType()==kTightCandidate) fHTightCandidateProfile2DInner->Fill(x/m, y/m);
      }
    }
    if (fPrintCandidateInfo) PrintCandidateInfo();
  }

  // Distance between candidates
  if (fRecoEvent->GetNCandidates()==2) {
    TRecoMUV3Candidate *c1 = static_cast<TRecoMUV3Candidate*>(fRecoEvent->GetCandidate(0));
    TRecoMUV3Candidate *c2 = static_cast<TRecoMUV3Candidate*>(fRecoEvent->GetCandidate(1));
    Double_t Distance = sqrt(pow(c1->GetX()-c2->GetX(),2)+
			     pow(c1->GetY()-c2->GetY(),2))/220.0;
    //Bool_t NeighbourExists = (Distance<1.1);
    fHInterCandidateDistance->Fill(Distance);
  }
  return fRecoEvent;
}

//////////////////////////////////////////////////////////////////////////////////
// Candidate printout for comparison with the MUV3 primitive dumps.
// Format: event time (TS units), muon fine time (FT=TS/256 units), Tile ID, Type.

void MUV3Reconstruction::PrintCandidateInfo() { 
  TString TypeString = "T";
  if      (fCandidate->GetType()==kLooseCandidate)       TypeString = "L";
  else if (fCandidate->GetType()==kLooseMaskedCandidate) TypeString = "LM";
  else if (fCandidate->GetType()==kUndefinedCandidate)   TypeString = "U";
  Int_t Tile = fCandidate->GetTileID();
  Double_t UncorrectedCandidateTime = fCandidate->GetTimeNoT0() + fStationsT0[0];
  UncorrectedCandidateTime /= ClockPeriod; // convert into timestamp units of 24.951059536 ns
  UncorrectedCandidateTime *= 256.0;       // convert into fine time units of 97.465076313 ps
  printf("@@@ %9d %4i %3d %s\n",
	 fTimeStamp, int(UncorrectedCandidateTime), Tile, TypeString.Data());
}

/////////////////////////
// Start of burst actions

void MUV3Reconstruction::StartOfBurst() {
  NA62VReconstruction::StartOfBurst(); // common part for all the subdetectors
  fNRecoHitsPerBurst = 0;
  fNLooseCandidatesPerBurst = 0;
  fNTightCandidatesPerBurst = 0;
}

///////////////////////
// End of burst actions

void MUV3Reconstruction::EndOfBurst() {
  NA62VReconstruction::EndOfBurst(); // common part for all the subdetectors
  fHNRecoHitsPerBurst->SetBinContent(fRecoEvent->GetBurstID()+1, fNRecoHitsPerBurst);
  fHNLooseCandidatesPerBurst->SetBinContent(fRecoEvent->GetBurstID()+1, fNLooseCandidatesPerBurst);
  fHNTightCandidatesPerBurst->SetBinContent(fRecoEvent->GetBurstID()+1, fNTightCandidatesPerBurst);
}

Bool_t MUV3Reconstruction::ChannelMasked (Int_t ch) {
  std::vector<Int_t>::iterator i = find(fMaskedChannels.begin(), fMaskedChannels.end(), ch);
  return (i != fMaskedChannels.end());
}

///////////////////////
// TDC event monitoring

void MUV3Reconstruction::TDCEventMonitor(TDCEvent* TdcEvent) {

  fHNErrorWord->Fill(TdcEvent->GetNErrors());
  Int_t NDigis = TdcEvent->GetNHits();
  fHNDigis->Fill(NDigis);
  TClonesArray& Digis = (*(TdcEvent->GetHits()));

  for (Int_t iDigi=0; iDigi<NDigis; iDigi++) {
    TMUV3Digi *Digi = static_cast<TMUV3Digi*>(Digis[iDigi]);
    Digi->DecodeChannelID();

    Int_t    ChannelID       = Digi->GetChannelID();
    Int_t    ROID            =
      static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRO(ChannelID);
    if (!fChannels[ROID]->GetEnabled()) continue;
    Double_t T0              = (fEnableT0) ? fChannels[ROID]->GetT0() : 0.0;
    Int_t    Tile            = Digi->GetTileID();
    Bool_t   IsHigh          = Digi->IsHigh();
    Bool_t   IsInner         = Digi->IsInnerTile();
    Int_t    Status          = Digi->GetDetectedEdge(); // 1=leading, 2=trailing, 3=both
    Double_t LeadingTimeRaw  = Digi->GetLeadingEdge(); // histogram DigiTimeRaw is filled centrally
    Double_t TrailingTimeRaw = Digi->GetTrailingEdge();
    Double_t LeadingTime     = LeadingTimeRaw  - GetT0Correction(Digi) - T0;
    Double_t TrailingTime    = TrailingTimeRaw - GetT0Correction(Digi) - T0;
    Double_t Width           = TrailingTimeRaw - LeadingTimeRaw;

    fHChannelProfile->Fill(ChannelID);
    fHROChannelProfile->Fill(ROID);
    if (fHChannelProfileVsBurst) fHChannelProfileVsBurst->Fill(fRecoEvent->GetBurstID(), ChannelID);

    // Fill the 2D channel occupancy plots
    if (fHChannelProfile2D_PM1) {
      Double_t x = fGeo->GetTileCentreX(Tile) * mm;
      Double_t y = fGeo->GetTileCentreY(Tile) * mm;
      if (!IsInner && Tile!=65 && Tile!=66 && Tile!=77 && Tile!=78) {
	if (IsHigh) fHChannelProfile2D_PM1->Fill(x/m, y/m);
	else        fHChannelProfile2D_PM0->Fill(x/m, y/m);
      }
      else if (IsInner) {
	if (IsHigh) fHChannelProfile2DInner_PM1->Fill(x/m, y/m);
	else        fHChannelProfile2DInner_PM0->Fill(x/m, y/m);
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
    else if (Status==kBothEdges) {
      fHTrailingTimeRaw->Fill(TrailingTimeRaw);
      fHLeadingTime->Fill(LeadingTime);
      fHTrailingTime->Fill(TrailingTime);
      if (fHLeadingTimeVsChannel) fHLeadingTimeVsChannel->Fill(ChannelID, LeadingTime);
      if (fHLeadingTimeVsROChannel) fHLeadingTimeVsROChannel->Fill(ROID, LeadingTime);
      //fHSlotVsLeadingTime->Fill(LeadingTime, Digi->GetSlot());
      fHWidth->Fill(Width);
      if (fHWidthVsChannel)   fHWidthVsChannel->Fill(ChannelID, Width);
      if (fHWidthVsROChannel) fHWidthVsROChannel->Fill(ROID, Width);

      // Leading Time is in ns units, and relative to event timestamp
      // convert to TS units
      double LeadingTimeInTSUnits = LeadingTimeRaw/ClockPeriod;

      // account for -ve Leading Times with large +ve offset
      double RealFineTimeInTSUnits = 100 + LeadingTimeInTSUnits;

      // remove the large offset, only left with TDC Fine Time in TS units
      RealFineTimeInTSUnits -= int(RealFineTimeInTSUnits);

      // convert to FT units
      RealFineTimeInTSUnits *= 256;

      // Already an integer, in fact, but just to hammer the point home
      Int_t RealFineTime = RealFineTimeInTSUnits;

      fHHitFineTime256->Fill(RealFineTime);
      std::bitset< 8 > FT = (RealFineTime);
      for (Int_t iBit = 0; iBit < 8; ++iBit) {
	size_t index = 7 - iBit;
	if (FT[index]==1) fHHitFineTimeBits->Fill(iBit-7);
      }
    }
  }
}

/////////////////////////////////
// EOB event (scalers) monitoring

void MUV3Reconstruction::EOBEventMonitor(TDetectorVEvent* tEvent) {
  Int_t TotalHitCount = 0;

  TSpecialTriggerEvent *EOB = static_cast<TSpecialTriggerEvent*>(tEvent);
  for (Int_t iTrig=0; iTrig<EOB->GetNSpecialTriggers(); iTrig++) {
    TTDCBSpecialTrigger *EOBTrigger = reinterpret_cast<TTDCBSpecialTrigger*>(tEvent->GetHit(iTrig));
    if (!EOBTrigger) continue;
    if (!isL0EOB(EOBTrigger->GetTriggerType())) continue;

    //////////////////////////////////////////////
    // The standard PP EOB scalers: channel counts

    for (Int_t i=0; i<128; i++) {
      Int_t ROid = 128*EOBTrigger->GetFPGAID() + i;
      if (ROid >= fNChannels) continue;

      Int_t    chid  = fChannels[ROid]->GetGeoChannelID();
      Int_t    count = EOBTrigger->GetCounter("CHANNEL_COUNT_L")->GetValue(i);
      Int_t    Tile  = chid % 200;
      Bool_t   PM0   = (chid<200);
      Double_t x     = fGeo->GetTileCentreX(Tile) * mm;
      Double_t y     = fGeo->GetTileCentreY(Tile) * mm;

      fHChannelProfileEOB->Fill(chid, count);
      fHROChannelProfileEOB->Fill(ROid, count);
      if (fHChannelProfileVsBurstEOB) {
	fHChannelProfileVsBurstEOB->Fill(fRecoEvent->GetBurstID(), chid, count);
      }
      if (Tile<144 && Tile!=65 && Tile!=66 && Tile!=77 && Tile!=78) {
	if (PM0) fHChannelProfile2D_EOB_PM0->Fill(x/m, y/m, count);
	else     fHChannelProfile2D_EOB_PM1->Fill(x/m, y/m, count);
      }
      else if (Tile>=144) {
	if (PM0) fHChannelProfile2DInner_EOB_PM0->Fill(x/m, y/m, count);
	else     fHChannelProfile2DInner_EOB_PM1->Fill(x/m, y/m, count);
      }
      TotalHitCount += count;
    }

    //////////////
    // EOB scalers

    if (EOBTrigger->GetFPGAID()==4) {

      //////////////////////////////////////
      // Tight muon trigger primitive counts

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
	Double_t x  = fGeo->GetTileCentreX(Tile) * mm;
	Double_t y  = fGeo->GetTileCentreY(Tile) * mm;
	if (Tile<144 && Tile!=65 && Tile!=66 && Tile!=77 && Tile!=78) {
	  fHTightPrimitiveProfile2D_EOB->Fill(x/m, y/m, count);
	}
	else if (Tile>=144) {
	  fHTightPrimitiveProfile2DInner_EOB->Fill(x/m, y/m, count);
	}
      }
      fHNTightMuonsPerBurstEOB->SetBinContent
	(fRecoEvent->GetBurstID()+1, TotalPrimitiveCount*1e-6); // unit: mln

      //////////////////////////////////////////
      // Total primitive counts and error counts

      for (Int_t i=0; i<16; i++) {
	if (EOBTrigger->GetCounter("MUV3_PRIMI")) {
	  fHTotalPrimitiveCountsEOB->Fill(i, 1e-6*EOBTrigger->GetCounter("MUV3_PRIMI")->GetValue(i)); // [mln]
	}
      }
      for (Int_t i=0; i<12; i++) {
	if (EOBTrigger->GetCounter("MUV3_ERROR")) {
	  Double_t count = EOBTrigger->GetCounter("MUV3_ERROR")->GetValue(i);
	  if (!i) count *= 1e-6; // Convert N(frames) to millions
	  fHErrorCountsEOB->Fill(i, count);
	}
      }
    }
  } // end of the loop over special triggers

  fHNHitsPerBurstEOB->SetBinContent(fRecoEvent->GetBurstID()+1, TotalHitCount*1e-6); // unit: mln
}

//////////////////////////////////////
// Compute the reconstructed hit times

Double_t MUV3Reconstruction::GetRecoTime (TMUV3Digi *Digi, Int_t mode) {
  Int_t    PositionID = Digi->GetChannelID();
  Int_t    ROchannel  = static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRO(PositionID);
  Double_t Time       = (mode==1) ? Digi->GetLeadingEdge() : Digi->GetTrailingEdge();
  Double_t T0         = (fEnableT0) ? fChannels[ROchannel]->GetT0() : 0.0;
  return (Time - GetT0Correction(Digi) - T0);
}

Double_t MUV3Reconstruction::GetRecoTimeNoT0 (TMUV3Digi *Digi, Int_t mode) {
  return (mode==1) ?
    Digi->GetLeadingEdge()  - GetT0Correction(Digi) :
    Digi->GetTrailingEdge() - GetT0Correction(Digi);
}

///////////////////////////////////////
// Initialize the monitoring histograms

void MUV3Reconstruction::InitHistograms() {

  TString Name = "NDigis";
  fHNDigis = new TH1D(Name, Name, 20, -0.5, 19.5);
  fHNDigis->GetXaxis()->SetTitle("Number of digis");

  Name = "NRecoHits";
  fHNRecoHits = new TH1D(Name, Name, 20, -0.5, 19.5);
  fHNRecoHits->GetXaxis()->SetTitle("Reconstructed hits");

  if (fIsRawData) {
    Name = "NRecoHitsVsL0TriggerBit";
    fHNRecoHitsVsL0TriggerBit = new TH2F(Name, Name, 20, -0.5, 19.5, 8, -0.5, 7.5);
    fHNRecoHitsVsL0TriggerBit->GetXaxis()->SetTitle("Number of MUV3 RecoHits");
    fHNRecoHitsVsL0TriggerBit->GetYaxis()->SetTitle("L0 trigger bit");

    Name = "NRecoHitsVsNoL0TriggerBit";
    fHNRecoHitsVsNoL0TriggerBit = new TH2F(Name, Name, 20, -0.5, 19.5, 8, -0.5, 7.5);
    fHNRecoHitsVsNoL0TriggerBit->GetXaxis()->SetTitle("Number of MUV3 RecoHits");
    fHNRecoHitsVsNoL0TriggerBit->GetYaxis()->SetTitle("L0 trigger bit OFF");
  }

  Name = "NRecoHitsPerTile";
  fHNRecoHitsPerTile = new TH1D(Name, Name, fNTiles, -0.5, fNTiles-0.5);
  fHNRecoHitsPerTile->GetXaxis()->SetTitle("Tile");

  Name = "NCandidates";
  fHNCandidates = new TH1D(Name, Name, 21, -0.5, 20.5);
  fHNCandidates->GetXaxis()->SetTitle("Candidates");

  if (fIsRawData) {
    Name = "NCandidatesVsL0TriggerBit";
    fHNCandidatesVsL0TriggerBit = new TH2F(Name, Name, 20, -0.5, 19.5, 8, -0.5, 7.5);
    fHNCandidatesVsL0TriggerBit->GetXaxis()->SetTitle("Number of MUV3 candidates");
    fHNCandidatesVsL0TriggerBit->GetYaxis()->SetTitle("L0 trigger bit");

    Name = "NCandidatesVsNoL0TriggerBit";
    fHNCandidatesVsNoL0TriggerBit = new TH2F(Name, Name, 20, -0.5, 19.5, 8, -0.5, 7.5);
    fHNCandidatesVsNoL0TriggerBit->GetXaxis()->SetTitle("Number of MUV3 candidates");
    fHNCandidatesVsNoL0TriggerBit->GetYaxis()->SetTitle("L0 trigger bit OFF");

    Name = "NRecoHitsPerBurst";
    fHNRecoHitsPerBurst = new TH1D(Name, Name, 3000, -0.5, 2999.5);
    fHNRecoHitsPerBurst->GetXaxis()->SetTitle("Burst ID");

    Name = "NHitsPerBurstEOB";
    fHNHitsPerBurstEOB = new TH1D(Name, Name, 3000, -0.5, 2999.5);
    fHNHitsPerBurstEOB->GetXaxis()->SetTitle("Burst ID");

    Name = "NLooseCandidatesPerBurst";
    fHNLooseCandidatesPerBurst = new TH1D(Name, Name, 3000, -0.5, 2999.5);
    fHNLooseCandidatesPerBurst->GetXaxis()->SetTitle("Burst ID");

    Name = "NTightCandidatesPerBurst";
    fHNTightCandidatesPerBurst = new TH1D(Name, Name, 3000, -0.5, 2999.5);
    fHNTightCandidatesPerBurst->GetXaxis()->SetTitle("Burst ID");

    Name = "NTightMuonsPerBurstEOB";
    fHNTightMuonsPerBurstEOB = new TH1D(Name, Name, 3000, -0.5, 2999.5);
    fHNTightMuonsPerBurstEOB->GetXaxis()->SetTitle("Burst ID");

    Name = "TotalPrimitiveCountsEOB";
    fHTotalPrimitiveCountsEOB = new TH1D(Name, Name, 16, -0.5, 15.5);
    TString PrimitiveNames[16] =
      {"ML1", "MT1", "MLO1", "MTO1", "MLO2", "MTO2", "MMO2", "ML2", "MT2", "MM2",
       "MO1 = MLO1 or MTO1", "M1 = ML1 or MT1",
       "MO2 = MLO2 or MTO2 or MMO2", "M2 = ML2 or MT2 or MM2",
       "Total primitive count", "Calibration"};
    for (Int_t i=0; i<16; i++) {
      fHTotalPrimitiveCountsEOB->GetXaxis()->SetBinLabel(i+1, PrimitiveNames[i]);
    }

    Name = "ErrorCountsEOB";
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

  Name = "InterCandidateDistance";
  fHInterCandidateDistance = new TH1D(Name, Name, 1550, 0, 15.5);
  fHInterCandidateDistance->GetXaxis()->SetTitle("Distance / 220 mm");

  Name = "CandidateProfile";
  fHCandidateProfile = new TH1D(Name, Name, fNTiles, -0.5, fNTiles-0.5);
  fHCandidateProfile->GetXaxis()->SetTitle("Tile");

  Name = "TightCandidateProfile";
  fHTightCandidateProfile = new TH1D(Name, Name, fNTiles, -0.5, fNTiles-0.5);
  fHTightCandidateProfile->GetXaxis()->SetTitle("Tile");

  if (fIsRawData) {
    Name = "TightPrimitiveProfileEOB";
    fHTightPrimitiveProfileEOB = new TH1D(Name, Name, fNTiles, -0.5, fNTiles-0.5);
    fHTightPrimitiveProfileEOB->GetXaxis()->SetTitle("Tile");
  }

  Name = "HitStatus";
  fHHitStatus = new TH1D(Name, Name, 3, 0.5, 3.5);
  fHHitStatus->GetXaxis()->SetTitle("HitStatus: 1=leading edge only, 2=trailing edge only, 3=both");

  Name = "TrailingTimeRaw";
  fHTrailingTimeRaw = new TH1D(Name, Name, 5000, -5000, +5000);
  fHTrailingTimeRaw->GetXaxis()->SetTitle("Raw lrailing time (ns)");

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

  Name = "RecoHitTimeWrtReference";
  fHRecoHitTimeWrtReference = new TH1D(Name, Name, 400, -20, 20);
  fHRecoHitTimeWrtReference->GetXaxis()->SetTitle("RecoHit time wrt CEDAR (ns)");

  Name = "RecoHitTimeWrtReferenceNoT0";
  fHRecoHitTimeWrtReferenceNoT0 = new TH1D(Name, Name, 400, -20, 20);
  fHRecoHitTimeWrtReferenceNoT0->GetXaxis()->SetTitle("RecoHit time wrt CEDAR (ns)");

  Name = "CandidateTimeWrtReference";
  fHCandidateTimeWrtReference = new TH1D(Name, Name, 400, -10, 10);
  fHCandidateTimeWrtReference->GetXaxis()->SetTitle("Candidate time wrt CEDAR (ns)");

  Name = "CandidateAvgTimeWrtReference";
  fHCandidateAvgTimeWrtReference = new TH1D(Name, Name, 400, -10, 10);
  fHCandidateAvgTimeWrtReference->GetXaxis()->SetTitle("Candidate time wrt CEDAR (ns)");

  Name = "CandidateTimeWrtReferenceNoTileT0";
  fHCandidateTimeWrtReferenceNoTileT0 = new TH1D(Name, Name, 400, -10, 10);
  fHCandidateTimeWrtReferenceNoTileT0->GetXaxis()->SetTitle("Candidate time wrt CEDAR (ns)");

  Name = "CandidateTimeWrtReferenceNoT0";
  fHCandidateTimeWrtReferenceNoT0 = new TH1D(Name, Name, 400, -10, 10);
  fHCandidateTimeWrtReferenceNoT0->GetXaxis()->SetTitle("Candidate time wrt CEDAR (ns)");

  Name = "Width";
  fHWidth = new TH1D(Name, Name, 300, 200.5*TdcCalib, 500.5*TdcCalib);
  fHWidth->GetXaxis()->SetTitle("Time width (ns)");

  Name = "NErrorWord";
  fHNErrorWord = new TH1D(Name, Name, 11, -0.5, 10.5);
  fHNErrorWord->GetXaxis()->SetTitle("NErrorWord");
  fHNErrorWord->GetYaxis()->SetTitle("NEvents");

  Name = "ChannelProfile";
  fHChannelProfile = new TH1D(Name, Name, 360, -0.5, 359.5);
  fHChannelProfile->GetXaxis()->SetTitle("MUV3 channel");

  if (fIsRawData) {
    Name = "ChannelProfileEOB";
    fHChannelProfileEOB = new TH1D(Name, Name, 360, -0.5, 359.5);
    fHChannelProfileEOB->GetXaxis()->SetTitle("MUV3 channel");
  }

  Name = "ReadoutChannelProfile";
  fHROChannelProfile = new TH1D(Name, Name, fNChannels, -0.5, fNChannels-0.5);
  fHROChannelProfile->GetXaxis()->SetTitle("MUV3 RO channel");

  if (fIsRawData) {
    Name = "ReadoutChannelProfileEOB";
    fHROChannelProfileEOB = new TH1D(Name, Name, fNChannels, -0.5, fNChannels-0.5);
    fHROChannelProfileEOB->GetXaxis()->SetTitle("MUV3 RO channel (EOB scaler)");
    Name = "ChannelProfileVsBurst";
    fHChannelProfileVsBurst = new TH2F
      (Name, Name, 3000, -0.5, 2999.5, 360, -0.5, 359.5);
    fHChannelProfileVsBurst->GetXaxis()->SetTitle("Burst ID");
    fHChannelProfileVsBurst->GetYaxis()->SetTitle("MUV3 channel");

    Name = "ChannelProfileVsBurstEOB";
    fHChannelProfileVsBurstEOB = new TH2F
      (Name, Name, 3000, -0.5, 2999.5, 360, -0.5, 359.5);
    fHChannelProfileVsBurstEOB->GetXaxis()->SetTitle("Burst ID");
    fHChannelProfileVsBurstEOB->GetYaxis()->SetTitle("MUV3 channel");
  }

  Name = "TileAND";
  fHTileAND = new TH1D(Name, Name, fNTiles, -0.5, fNTiles-0.5);
  fHTileAND->GetXaxis()->SetTitle("MUV3 tile");

  Name = "TileOR";
  fHTileOR = new TH1D(Name, Name, fNTiles, -0.5, fNTiles-0.5);
  fHTileOR->GetXaxis()->SetTitle("MUV3 tile");

  Name = "DeltaTime";
  fHDeltaTime = new TH1D(Name, Name, 100, -20, 20);
  fHDeltaTime->GetXaxis()->SetTitle("PMT1-PMT0 time difference (ns)");

  Name = "DeltaTimeCandidate";
  fHDeltaTimeCandidate = new TH1D(Name, Name, 100, -20, 20);
  fHDeltaTimeCandidate->GetXaxis()->SetTitle("PMT1-PMT0 time difference (ns)");

  Name = "HitFineTimeBits";
  fHHitFineTimeBits = new TH1D(Name, Name, 8, -7.5, 0.5);
  fHHitFineTimeBits->GetXaxis()->SetTitle("Hit Fine Time Bits");
  fHHitFineTimeBits->Sumw2();

  Name = "HitFineTime (0 to 255)";
  fHHitFineTime256 = new TH1D(Name, Name, 256, -0.5, 255.5);
  fHHitFineTime256->GetXaxis()->SetTitle("Hit Fine Time");

  // 2-dimensional channel maps

  if (fIsRawData) {
    Name = "MaskedProfile2D_PM0";
    fHMaskedProfile2D_PM0 = new TH2F(Name, Name, 12, -1.32, 1.32, 12, -1.32, 1.32);
    Name = "MaskedProfile2D_PM1";
    fHMaskedProfile2D_PM1 = new TH2F(Name, Name, 12, -1.32, 1.32, 12, -1.32, 1.32);
    Name = "MaskedProfile2DInner_PM0";
    fHMaskedProfile2DInner_PM0 = new TH2F(Name, Name,  3, -0.22, 0.22,  3, -0.22, 0.22);
    Name = "MaskedProfile2DInner_PM1";
    fHMaskedProfile2DInner_PM1 = new TH2F(Name, Name,  3, -0.22, 0.22,  3, -0.22, 0.22);

    Name = "ChannelProfile2D_PM0";
    fHChannelProfile2D_PM0 = new TH2F(Name, Name, 12, -1.32, 1.32, 12, -1.32, 1.32);
    fHChannelProfile2D_PM0->GetXaxis()->SetTitle("MUV3 tile X (m)");
    fHChannelProfile2D_PM0->GetYaxis()->SetTitle("MUV3 tile Y (m)");

    Name = "ChannelProfile2D_PM1";
    fHChannelProfile2D_PM1 = new TH2F(Name, Name, 12, -1.32, 1.32, 12, -1.32, 1.32);
    fHChannelProfile2D_PM1->GetXaxis()->SetTitle("MUV3 tile X (m)");
    fHChannelProfile2D_PM1->GetYaxis()->SetTitle("MUV3 tile Y (m)");

    Name = "ChannelProfile2DInner_PM0";
    fHChannelProfile2DInner_PM0 = new TH2F(Name, Name,  3, -0.22, 0.22,  3, -0.22, 0.22);
    fHChannelProfile2DInner_PM0->GetXaxis()->SetTitle("MUV3 tile X (m)");
    fHChannelProfile2DInner_PM0->GetYaxis()->SetTitle("MUV3 tile Y (m)");

    Name = "ChannelProfile2DInner_PM1";
    fHChannelProfile2DInner_PM1 = new TH2F(Name, Name,  3, -0.22, 0.22,  3, -0.22, 0.22);
    fHChannelProfile2DInner_PM1->GetXaxis()->SetTitle("MUV3 tile X (m)");
    fHChannelProfile2DInner_PM1->GetYaxis()->SetTitle("MUV3 tile Y (m)");

    Name = "ChannelProfile2D_PM0 EOB";
    fHChannelProfile2D_EOB_PM0 = new TH2F(Name, Name, 12, -1.32, 1.32, 12, -1.32, 1.32);
    fHChannelProfile2D_EOB_PM0->GetXaxis()->SetTitle("MUV3 tile X (m)");
    fHChannelProfile2D_EOB_PM0->GetYaxis()->SetTitle("MUV3 tile Y (m)");

    Name = "ChannelProfile2D_PM1 EOB";
    fHChannelProfile2D_EOB_PM1 = new TH2F(Name, Name, 12, -1.32, 1.32, 12, -1.32, 1.32);
    fHChannelProfile2D_EOB_PM1->GetXaxis()->SetTitle("MUV3 tile X (m)");
    fHChannelProfile2D_EOB_PM1->GetYaxis()->SetTitle("MUV3 tile Y (m)");

    Name = "ChannelProfile2DInner_PM0 EOB";
    fHChannelProfile2DInner_EOB_PM0 = new TH2F(Name, Name,  3, -0.22, 0.22,  3, -0.22, 0.22);
    fHChannelProfile2DInner_EOB_PM0->GetXaxis()->SetTitle("MUV3 tile X (m)");
    fHChannelProfile2DInner_EOB_PM0->GetYaxis()->SetTitle("MUV3 tile Y (m)");

    Name = "ChannelProfile2DInner_PM1 EOB";
    fHChannelProfile2DInner_EOB_PM1 = new TH2F(Name, Name,  3, -0.22, 0.22,  3, -0.22, 0.22);
    fHChannelProfile2DInner_EOB_PM1->GetXaxis()->SetTitle("MUV3 tile X (m)");
    fHChannelProfile2DInner_EOB_PM1->GetYaxis()->SetTitle("MUV3 tile Y (m)");

    Name = "CandidateProfile2D";
    fHCandidateProfile2D = new TH2F(Name, Name,  12, -1.32, 1.32, 12, -1.32, 1.32);
    fHCandidateProfile2D->GetXaxis()->SetTitle("MUV3 tile X (m)");
    fHCandidateProfile2D->GetYaxis()->SetTitle("MUV3 tile Y (m)");

    Name = "CandidateProfile2DInner";
    fHCandidateProfile2DInner = new TH2F(Name, Name,  3, -0.22, 0.22,  3, -0.22, 0.22);
    fHCandidateProfile2DInner->GetXaxis()->SetTitle("MUV3 tile X (m)");
    fHCandidateProfile2DInner->GetYaxis()->SetTitle("MUV3 tile Y (m)");

    Name = "TightCandidateProfile2D";
    fHTightCandidateProfile2D = new TH2F(Name, Name,  12, -1.32, 1.32, 12, -1.32, 1.32);
    fHTightCandidateProfile2D->GetXaxis()->SetTitle("MUV3 tile X (m)");
    fHTightCandidateProfile2D->GetYaxis()->SetTitle("MUV3 tile Y (m)");

    Name = "TightCandidateProfile2DInner";
    fHTightCandidateProfile2DInner = new TH2F(Name, Name,  3, -0.22, 0.22,  3, -0.22, 0.22);
    fHTightCandidateProfile2DInner->GetXaxis()->SetTitle("MUV3 tile X (m)");
    fHTightCandidateProfile2DInner->GetYaxis()->SetTitle("MUV3 tile Y (m)");

    Name = "TightPrimitiveProfile2D EOB";
    fHTightPrimitiveProfile2D_EOB = new TH2F(Name, Name,  12, -1.32, 1.32, 12, -1.32, 1.32);
    fHTightPrimitiveProfile2D_EOB->GetXaxis()->SetTitle("MUV3 tile X (m)");
    fHTightPrimitiveProfile2D_EOB->GetYaxis()->SetTitle("MUV3 tile Y (m)");

    Name = "TightPrimitiveProfile2DInner EOB";
    fHTightPrimitiveProfile2DInner_EOB = new TH2F(Name, Name,  3, -0.22, 0.22,  3, -0.22, 0.22);
    fHTightPrimitiveProfile2DInner_EOB->GetXaxis()->SetTitle("MUV3 tile X (m)");
    fHTightPrimitiveProfile2DInner_EOB->GetYaxis()->SetTitle("MUV3 tile Y (m)");

    Name = "TightCandidateProfileVsL0TriggerBit";
    fHTightCandidateProfileVsL0TriggerBit = new TH2F(Name, Name, fNTiles, -0.5, fNTiles-0.5, 8, -0.5, 7.5);
    fHTightCandidateProfileVsL0TriggerBit->GetXaxis()->SetTitle("Tight candidate tile ID");
    fHTightCandidateProfileVsL0TriggerBit->GetYaxis()->SetTitle("L0 trigger bit");

    Name = "TightCandidateProfileVsNoL0TriggerBit";
    fHTightCandidateProfileVsNoL0TriggerBit = new TH2F(Name, Name, fNTiles, -0.5, fNTiles-0.5, 8, -0.5, 7.5);
    fHTightCandidateProfileVsNoL0TriggerBit->GetXaxis()->SetTitle("Tight candidate tile ID");
    fHTightCandidateProfileVsNoL0TriggerBit->GetYaxis()->SetTitle("L0 trigger bit OFF");

    Name = "HitStatusVsChannel";
    fHHitStatusVsChannel = new TH2F(Name, Name, 360, -0.5, 359.5, 3, 0.5, 3.5);
    fHHitStatusVsChannel->GetXaxis()->SetTitle("Channel ID");
    fHHitStatusVsChannel->GetYaxis()->SetTitle("Hit Status");

    Name = "HitStatusVsReadoutChannel";
    fHHitStatusVsROChannel = new TH2F
      (Name, Name, fNChannels, -0.5, fNChannels-0.5, 3, 0.5, 3.5);
    fHHitStatusVsROChannel->GetXaxis()->SetTitle("RO channel ID");
    fHHitStatusVsROChannel->GetYaxis()->SetTitle("Hit Status");

    Name = "LeadingTimeVsChannel";
    fHLeadingTimeVsChannel = new TH2F(Name, Name, 360, -0.5, 359.5, 50, -300, +200);
    fHLeadingTimeVsChannel->GetXaxis()->SetTitle("Channel ID");
    fHLeadingTimeVsChannel->GetYaxis()->SetTitle("Leading time (ns)");

    Name = "LeadingTimeVsReadoutChannel";
    fHLeadingTimeVsROChannel = new TH2F
      (Name, Name, fNChannels, -0.5, fNChannels-0.5, 50, -300, +200);
    fHLeadingTimeVsROChannel->GetXaxis()->SetTitle("RO channel ID");
    fHLeadingTimeVsROChannel->GetYaxis()->SetTitle("Leading time (ns)");

    Name = "RecoHitTimeWrtReferenceVsChannel";
    fHRecoHitTimeWrtReferenceVsChannel = new TH2F
      (Name, Name, 360, -0.5, 359.5, 120, -30, 30);
    fHRecoHitTimeWrtReferenceVsChannel->GetXaxis()->SetTitle("Channel ID");
    fHRecoHitTimeWrtReferenceVsChannel->GetYaxis()->SetTitle("RecoHit time wrt CEDAR (ns)");

    Name = "RecoHitTimeWrtReferenceVsBurst";
    fHRecoHitTimeWrtReferenceVsBurst = new TH2F
      (Name, Name, 3000, -0.5, 2999.5, 160, -40, 40);
    fHRecoHitTimeWrtReferenceVsBurst->GetXaxis()->SetTitle("Burst ID");
    fHRecoHitTimeWrtReferenceVsBurst->GetYaxis()->SetTitle("RecoHit time wrt CEDAR (ns)");

    Name = "RecoHitTimeWrtReferenceVsWidth";
    fHRecoHitTimeWrtReferenceVsWidth = new TH2F
      (Name, Name, 100, 25, 45, 50, -5, 5);
    fHRecoHitTimeWrtReferenceVsWidth->GetXaxis()->SetTitle("Width (ns)");
    fHRecoHitTimeWrtReferenceVsWidth->GetYaxis()->SetTitle("RecoHit time wrt CEDAR (ns)");

    Name = "HRecoHitTimeWrtReferenceVsTimeStamp_Channel147";
    fHRecoHitTimeWrtReferenceVsTimeStamp_Channel147 = new TH2F
      (Name, Name, 24, 0, 6, 250, -25, 25);
    fHRecoHitTimeWrtReferenceVsTimeStamp_Channel147->GetXaxis()->SetTitle("Timestamp (s)");
    fHRecoHitTimeWrtReferenceVsTimeStamp_Channel147->GetYaxis()->SetTitle("RecoHit time wrt CEDAR (ns)");

    Name = "HRecoHitTimeWrtReference_Channel147";
    fHRecoHitTimeWrtReference_Channel147 = new TH1D(Name, Name, 250, -25, 25);
    fHRecoHitTimeWrtReference_Channel147->GetXaxis()->SetTitle("RecoHit time wrt CEDAR (ns)");

    Name = "CandidateTimeWrtReferenceVsTile";
    fHCandidateTimeWrtReferenceVsTile = new TH2F
      (Name, Name, fNTiles, -0.5, fNTiles-0.5, 200, -20, 20);
    fHCandidateTimeWrtReferenceVsTile->GetXaxis()->SetTitle("Tile ID");
    fHCandidateTimeWrtReferenceVsTile->GetYaxis()->SetTitle("Candidate time wrt CEDAR (ns)");

    Name = "CandidateAvgTimeWrtReferenceVsTile";
    fHCandidateAvgTimeWrtReferenceVsTile = new TH2F
      (Name, Name, fNTiles, -0.5, fNTiles-0.5, 200, -20, 20);
    fHCandidateAvgTimeWrtReferenceVsTile->GetXaxis()->SetTitle("Tile ID");
    fHCandidateAvgTimeWrtReferenceVsTile->GetYaxis()->SetTitle("Candidate Avg time wrt CEDAR (ns)");

    Name = "CandidateTimeWrtReferenceNoTileT0VsTile";
    fHCandidateTimeWrtReferenceNoTileT0VsTile = new TH2F
      (Name, Name, fNTiles, -0.5, fNTiles-0.5, 200, -20, 20); // 0.2 ns bin for T0 fits
    fHCandidateTimeWrtReferenceNoTileT0VsTile->GetXaxis()->SetTitle("Tile ID");
    fHCandidateTimeWrtReferenceNoTileT0VsTile->GetYaxis()->SetTitle("Candidate time wrt CEDAR (ns)");

    Name = "CandidateTimeWrtReferenceVsBurst";
    fHCandidateTimeWrtReferenceVsBurst = new TH2F
      (Name, Name, 3000, -0.5, 2999.5, 160, -40, 40);
    fHCandidateTimeWrtReferenceVsBurst->GetXaxis()->SetTitle("Burst ID");

    Name = "WidthVsChannel";
    fHWidthVsChannel = new TH2F
      (Name, Name, 360, -0.5, 359.5, 300, 200.5*TdcCalib, 500.5*TdcCalib);
    fHWidthVsChannel->GetXaxis()->SetTitle("Channel ID");
    fHWidthVsChannel->GetYaxis()->SetTitle("Time width (ns)");

    Name = "WidthVsReadoutChannel";
    fHWidthVsROChannel = new TH2F
      (Name, Name, fNChannels, -0.5, fNChannels-0.5, 300, 200.5*TdcCalib, 500.5*TdcCalib);
    fHWidthVsROChannel->GetXaxis()->SetTitle("RO channel ID");
    fHWidthVsROChannel->GetYaxis()->SetTitle("Time width (ns)");

    /*
    Name = "SlotVsLeadingTime"; // NB: ~1M bins
    fHSlotVsLeadingTime = new TH2F
      (Name, Name, 20001, -10000.5*TdcCalib, 10000.5*TdcCalib, 40, 60.5, 100.5);
    fHSlotVsLeadingTime->GetXaxis()->SetTitle("Leading time (ns)");
    fHSlotVsLeadingTime->GetYaxis()->SetTitle("Slot index");
    */    

    Name = "DeltaTimeVsTile";
    fHDeltaTimeVsTile = new TH2F(Name, Name, fNTiles, -0.5, fNTiles-0.5, 100, -10, 10);
    fHDeltaTimeVsTile->GetXaxis()->SetTitle("Tile ID");
    fHDeltaTimeVsTile->GetYaxis()->SetTitle("t(PMT1)-t(PMT0) (ns)");

    Name = "DeltaTimeVsTileProfile";
    fPDeltaTimeVsTile = new TProfile(Name, Name, fNTiles, -0.5, fNTiles-0.5);
    fPDeltaTimeVsTile->GetXaxis()->SetTitle("Tile ID");
    fPDeltaTimeVsTile->GetYaxis()->SetTitle("t(PMT1)-t(PMT0) (ns)");
  }

  // Digitizer monitoring histograms
  if (!fIsRawData && fDigitizerMode==2 && fDigiHistoFlag) {
    Double_t dTmin = 115;
    Double_t dTmax = 125;

    Name = "MUV3EventMinusCFDTime0";
    fHEventMinusCFDTimePM0 = new TH1D(Name, Name, 100, dTmin, dTmax);
    Name = "MUV3EventMinusCFDTime1";
    fHEventMinusCFDTimePM1 = new TH1D(Name, Name, 100, dTmin, dTmax);
    Name = "MUV3EventMinusCFDTime0_All";
    fHEventMinusCFDTimePM0_all = new TH1D(Name, Name, 100, dTmin, dTmax);
    Name = "MUV3EventMinusCFDTime1_All";
    fHEventMinusCFDTimePM1_all = new TH1D(Name, Name, 100, dTmin, dTmax);
    Name = "MUV3CFDTime0";
    fHCFDTimePM0 = new TH1D(Name, Name, 100, 850, 1050);
    Name = "MUV3CFDTime1";
    fHCFDTimePM1 = new TH1D(Name, Name, 100, 850, 1050);
    Name = "MUV3DepositedEnergy";
    fHEnergy = new TH1D(Name, Name, 100, 0, 30);
    Name = "MUV3DepositedEnergyInCell";
    fHEnergyPerCell = new TH1D(Name, Name, 100, 0, 30);
    Name = "MUV3NHits";
    fHNHits = new TH1D(Name, Name, 30, -0.5, 29.5);
    Name = "MUV3NHitChannels";
    fHNChannels = new TH1D(Name, Name, 50, -0.5, 49.5);
    Name = "MUV3EnergyVsTimePM0";
    fHTimevsEnergyPM0 = new TH2F(Name, Name, 100, 0, 30, 100, dTmin, dTmax);
    Name = "MUV3EnergyVsTimePM1";
    fHTimevsEnergyPM1 = new TH2F(Name, Name, 100, 0, 30, 100, dTmin, dTmax);
    Name = "MUV3NHitsVsTimePM0";
    fHTimevsNHitsPM0 = new TH2F(Name, Name, 30, -0.5, 29.5, 100, dTmin, dTmax);
    Name = "MUV3NHitsVsTimePM1";
    fHTimevsNHitsPM1 = new TH2F(Name, Name, 30, -0.5, 29.5, 100, dTmin, dTmax);
    Name = "MUV3CollectioMatrixA";
    fHCollectionMatrixA = new TH2F(Name, Name, 22, 0, 22, 22, 0, 22);
    Name = "MUV3CollectioMatrixB";
    fHCollectionMatrixB = new TH2F(Name, Name, 22, 0, 22, 22, 0, 22);
    Name = "MUV3ReferenceShape";
    fHReferenceShape = new TH1D(Name, Name, 502, 0, 502*0.2);
    fHReferenceShape->GetXaxis()->SetTitle("Time (ns)");
  }
}

////////////////////////////////////////
// Write histograms into the output file

void MUV3Reconstruction::SaveHistograms() {

  TDirectory *MUV3Dir = GetOrMakeDir(fHistoFile, "MUV3Monitor");
  if (!fIsRawData && fDigitizerMode==2 && fDigiHistoFlag) {
    GetOrMakeDir(MUV3Dir, "MUV3Digitizer");
  }
  if (fChannelHistograms) {
    TDirectory *TilesDir = GetOrMakeDir(MUV3Dir, "MUV3Tiles");
    GetOrMakeDir(TilesDir, "dT");
    GetOrMakeDir(TilesDir, "Time2D");
  }
  fHistoFile->cd("MUV3Monitor");

  if (fHNDigis)                       fHNDigis->Write();
  if (fHNRecoHits)                    fHNRecoHits->Write();
  if (fHNRecoHitsVsL0TriggerBit)      fHNRecoHitsVsL0TriggerBit->Write();
  if (fHNRecoHitsVsNoL0TriggerBit)    fHNRecoHitsVsNoL0TriggerBit->Write();
  if (fHNCandidatesVsL0TriggerBit)    fHNCandidatesVsL0TriggerBit->Write();
  if (fHNCandidatesVsNoL0TriggerBit)  fHNCandidatesVsNoL0TriggerBit->Write();
  if (fHNRecoHitsPerTile)             fHNRecoHitsPerTile->Write();
  if (fHNCandidates)                  fHNCandidates->Write();
  if (fHNLooseCandidatesPerBurst)     fHNLooseCandidatesPerBurst->Write();
  if (fHNTightCandidatesPerBurst)     fHNTightCandidatesPerBurst->Write();
  if (fHNTightMuonsPerBurstEOB)       fHNTightMuonsPerBurstEOB->Write();
  if (fHTotalPrimitiveCountsEOB)      fHTotalPrimitiveCountsEOB->Write();
  if (fHErrorCountsEOB)               fHErrorCountsEOB->Write();
  if (fHNRecoHitsPerBurst)            fHNRecoHitsPerBurst->Write();
  if (fHNHitsPerBurstEOB)             fHNHitsPerBurstEOB->Write();
  if (fHInterCandidateDistance)       fHInterCandidateDistance->Write();
  if (fHNCandidatesToRecoHitsPerTile) fHNCandidatesToRecoHitsPerTile->Write();
 
  if (fHHitStatus)                    fHHitStatus->Write();
  if (fHTrailingTimeRaw)              fHTrailingTimeRaw->Write();
  if (fHLeadingTime)                  fHLeadingTime->Write();
  if (fHTrailingTime)                 fHTrailingTime->Write();
  if (fHLeadingTimeRaw_NoTrailing)    fHLeadingTimeRaw_NoTrailing->Write();
  if (fHTrailingTimeRaw_NoLeading)    fHTrailingTimeRaw_NoLeading->Write();

  if (fHRecoHitTimeWrtReference)      fHRecoHitTimeWrtReference->Write();
  if (fHRecoHitTimeWrtReferenceNoT0)  fHRecoHitTimeWrtReferenceNoT0->Write();
  if (fHCandidateTimeWrtReference)    fHCandidateTimeWrtReference->Write();
  if (fHCandidateAvgTimeWrtReference) fHCandidateAvgTimeWrtReference->Write();
  if (fHCandidateTimeWrtReferenceNoTileT0) fHCandidateTimeWrtReferenceNoTileT0->Write();
  if (fHCandidateTimeWrtReferenceNoT0) fHCandidateTimeWrtReferenceNoT0->Write();

  if (fHWidth)          fHWidth->Write();
  if (fHNErrorWord)     fHNErrorWord->Write();
  if (fHChannelProfile) fHChannelProfile->Write();
  if (fHChannelProfileEOB) fHChannelProfileEOB->Write();
  if (fHROChannelProfile)  fHROChannelProfile->Write();
  if (fHROChannelProfileEOB)      fHROChannelProfileEOB->Write();
  if (fHChannelProfileVsBurst)    fHChannelProfileVsBurst->Write();
  if (fHChannelProfileVsBurstEOB) fHChannelProfileVsBurstEOB->Write();
  if (fHTileAsymmetry)            fHTileAsymmetry->Write();
  if (fHTileAsymmetryEOB)         fHTileAsymmetryEOB->Write();
  if (fHCandidateProfile)         fHCandidateProfile->Write();
  if (fHTightCandidateProfile)    fHTightCandidateProfile->Write();
  if (fHTightPrimitiveProfileEOB) fHTightPrimitiveProfileEOB->Write();
  if(fHTileAND)            fHTileAND->Write();
  if(fHTileOR)             fHTileOR->Write();
  if(fHDeltaTime)          fHDeltaTime->Write();
  if(fHDeltaTimeCandidate) fHDeltaTimeCandidate->Write();
  if(fHHitFineTimeBits)    fHHitFineTimeBits->Write();
  if(fHHitFineTime256)     fHHitFineTime256->Write();
  
  if (fHMaskedProfile2D_PM0)      fHMaskedProfile2D_PM0->Write();
  if (fHMaskedProfile2DInner_PM0) fHMaskedProfile2DInner_PM0->Write();
  if (fHMaskedProfile2D_PM1)      fHMaskedProfile2D_PM1->Write();
  if (fHMaskedProfile2DInner_PM1) fHMaskedProfile2DInner_PM1->Write();

  if (fHChannelProfile2D_PM0) fHChannelProfile2D_PM0->Write();
  if (fHChannelProfile2D_PM1) fHChannelProfile2D_PM1->Write();
  if (fHChannelProfile2DInner_PM0) fHChannelProfile2DInner_PM0->Write();
  if (fHChannelProfile2DInner_PM1) fHChannelProfile2DInner_PM1->Write();

  if (fHChannelProfile2D_EOB_PM0) fHChannelProfile2D_EOB_PM0->Write();
  if (fHChannelProfile2D_EOB_PM1) fHChannelProfile2D_EOB_PM1->Write();
  if (fHChannelProfile2DInner_EOB_PM0) fHChannelProfile2DInner_EOB_PM0->Write();
  if (fHChannelProfile2DInner_EOB_PM1) fHChannelProfile2DInner_EOB_PM1->Write();

  if (fHCandidateProfile2D) fHCandidateProfile2D->Write();
  if (fHCandidateProfile2DInner) fHCandidateProfile2DInner->Write();
  if (fHTightCandidateProfile2D) fHTightCandidateProfile2D->Write();
  if (fHTightCandidateProfile2DInner) fHTightCandidateProfile2DInner->Write();

  if (fHTightPrimitiveProfile2D_EOB) fHTightPrimitiveProfile2D_EOB->Write();
  if (fHTightPrimitiveProfile2DInner_EOB) fHTightPrimitiveProfile2DInner_EOB->Write();

  if (fHTightCandidateProfileVsL0TriggerBit) fHTightCandidateProfileVsL0TriggerBit->Write();
  if (fHTightCandidateProfileVsNoL0TriggerBit) fHTightCandidateProfileVsNoL0TriggerBit->Write();

  if (fHHitStatusVsChannel) fHHitStatusVsChannel->Write();
  if (fHHitStatusVsROChannel) fHHitStatusVsROChannel->Write();
  if (fHLeadingTimeVsChannel) fHLeadingTimeVsChannel->Write();
  if (fHLeadingTimeVsROChannel) fHLeadingTimeVsROChannel->Write();
  if (fHRecoHitTimeWrtReferenceVsChannel) fHRecoHitTimeWrtReferenceVsChannel->Write();
  if (fHRecoHitTimeWrtReferenceVsBurst) fHRecoHitTimeWrtReferenceVsBurst->Write();
  if (fHRecoHitTimeWrtReferenceVsWidth) fHRecoHitTimeWrtReferenceVsWidth->Write();
  if (fHRecoHitTimeWrtReferenceVsTimeStamp_Channel147) fHRecoHitTimeWrtReferenceVsTimeStamp_Channel147->Write();
  if (fHRecoHitTimeWrtReference_Channel147) fHRecoHitTimeWrtReference_Channel147->Write();
  if (fHCandidateTimeWrtReferenceVsTile) fHCandidateTimeWrtReferenceVsTile->Write();
  if (fHCandidateAvgTimeWrtReferenceVsTile) fHCandidateAvgTimeWrtReferenceVsTile->Write();
  if (fHCandidateTimeWrtReferenceNoTileT0VsTile) fHCandidateTimeWrtReferenceNoTileT0VsTile->Write();
  if (fHCandidateTimeWrtReferenceVsBurst) fHCandidateTimeWrtReferenceVsBurst->Write();

  if (fHWidthVsChannel)    fHWidthVsChannel->Write();
  if (fHWidthVsROChannel)  fHWidthVsROChannel->Write();
  if (fHSlotVsLeadingTime) fHSlotVsLeadingTime->Write();
  if (fHDeltaTimeVsTile) fHDeltaTimeVsTile->Write();
  if (fPDeltaTimeVsTile) fPDeltaTimeVsTile->Write();

  if (fChannelHistograms) {
    for (Int_t i=0; i<fNChannels; i++) static_cast<MUV3Channel*>(fChannels[i])->Write(fHistoFile);
    for (Int_t i=0; i<fNTiles; i++) fTiles[i]->Write(fHistoFile);
  }

  // Digitizer monitoring histograms

  if (!fIsRawData && fDigitizerMode==2 && fDigiHistoFlag) {
    fHistoFile->cd("MUV3Monitor/MUV3Digitizer");
    if(fHEventMinusCFDTimePM0)     fHEventMinusCFDTimePM0->Write();
    if(fHEventMinusCFDTimePM1)     fHEventMinusCFDTimePM1->Write();
    if(fHEventMinusCFDTimePM0_all) fHEventMinusCFDTimePM0_all->Write();
    if(fHEventMinusCFDTimePM1_all) fHEventMinusCFDTimePM1_all->Write();
    if(fHCFDTimePM0)               fHCFDTimePM0->Write();
    if(fHCFDTimePM1)               fHCFDTimePM1->Write();
    if(fHEnergy)                   fHEnergy->Write();
    if(fHEnergyPerCell)            fHEnergyPerCell->Write();
    if(fHNHits)                    fHNHits->Write();
    if(fHNChannels)                fHNChannels->Write();
    if(fHTimevsEnergyPM0)          fHTimevsEnergyPM0->Write();
    if(fHTimevsEnergyPM1)          fHTimevsEnergyPM1->Write();
    if(fHTimevsNHitsPM0)           fHTimevsNHitsPM0->Write();
    if(fHTimevsNHitsPM1)           fHTimevsNHitsPM1->Write();
    if(fHCollectionMatrixA)        fHCollectionMatrixA->Write();
    if(fHCollectionMatrixB)        fHCollectionMatrixB->Write();
    if(fHReferenceShape)           fHReferenceShape->Write();
  }
  fHistoFile->cd("/");
}

void MUV3Reconstruction::DeleteHistograms() {
  if (fHNDigis) delete fHNDigis;
  if (fHNRecoHits) delete fHNRecoHits;
  if (fHNRecoHitsPerTile) delete fHNRecoHitsPerTile;
  if (fHNCandidates) delete fHNCandidates;
  if (fHNRecoHitsPerBurst) delete fHNRecoHitsPerBurst;
  if (fHNHitsPerBurstEOB) delete fHNHitsPerBurstEOB;
  if (fHNLooseCandidatesPerBurst) delete fHNLooseCandidatesPerBurst;
  if (fHNTightCandidatesPerBurst) delete fHNTightCandidatesPerBurst;
  if (fHNTightMuonsPerBurstEOB) delete fHNTightMuonsPerBurstEOB;
  if (fHTotalPrimitiveCountsEOB) delete fHTotalPrimitiveCountsEOB;
  if (fHErrorCountsEOB) delete fHErrorCountsEOB;
  if (fHInterCandidateDistance) delete fHInterCandidateDistance;
  if (fHNCandidatesToRecoHitsPerTile) delete fHNCandidatesToRecoHitsPerTile;
  if (fHHitStatus) delete fHHitStatus;
  if (fHTrailingTimeRaw) delete fHTrailingTimeRaw;
  if (fHLeadingTime) delete fHLeadingTime;
  if (fHTrailingTime) delete fHTrailingTime;
  if (fHLeadingTimeRaw_NoTrailing) delete fHLeadingTimeRaw_NoTrailing;
  if (fHTrailingTimeRaw_NoLeading) delete fHTrailingTimeRaw_NoLeading;
  if (fHRecoHitTimeWrtReference) delete fHRecoHitTimeWrtReference;
  if (fHRecoHitTimeWrtReferenceNoT0) delete fHRecoHitTimeWrtReferenceNoT0;
  if (fHCandidateTimeWrtReference) delete fHCandidateTimeWrtReference;
  if (fHCandidateAvgTimeWrtReference) delete fHCandidateAvgTimeWrtReference;
  if (fHCandidateTimeWrtReferenceNoTileT0) delete fHCandidateTimeWrtReferenceNoTileT0;
  if (fHCandidateTimeWrtReferenceNoT0) delete fHCandidateTimeWrtReferenceNoT0;
  if (fHWidth) delete fHWidth;
  if (fHNErrorWord) delete fHNErrorWord;
  if (fHChannelProfile) delete fHChannelProfile;
  if (fHChannelProfileEOB) delete fHChannelProfileEOB;
  if (fHROChannelProfile) delete fHROChannelProfile;
  if (fHROChannelProfileEOB) delete fHROChannelProfileEOB;
  if (fHTileAsymmetry) delete fHTileAsymmetry;
  if (fHTileAsymmetryEOB) delete fHTileAsymmetryEOB;
  if (fHCandidateProfile) delete fHCandidateProfile;
  if (fHTightCandidateProfile) delete fHTightCandidateProfile;
  if (fHTightPrimitiveProfileEOB) delete fHTightPrimitiveProfileEOB;
  if (fHTileAND) delete fHTileAND;
  if (fHTileOR) delete fHTileOR;
  if (fHDeltaTime) delete fHDeltaTime;
  if (fHDeltaTimeCandidate) delete fHDeltaTimeCandidate;
  if (fHHitFineTimeBits) delete fHHitFineTimeBits;
  if (fHHitFineTime256) delete fHHitFineTime256;
  if (fHRecoHitTimeWrtReference_Channel147) delete fHRecoHitTimeWrtReference_Channel147;
  if (fHNRecoHitsVsL0TriggerBit) delete fHNRecoHitsVsL0TriggerBit;
  if (fHNRecoHitsVsNoL0TriggerBit) delete fHNRecoHitsVsNoL0TriggerBit;
  if (fHNCandidatesVsL0TriggerBit) delete fHNCandidatesVsL0TriggerBit;
  if (fHNCandidatesVsNoL0TriggerBit) delete fHNCandidatesVsNoL0TriggerBit;
  if (fHChannelProfileVsBurst) delete fHChannelProfileVsBurst;
  if (fHChannelProfileVsBurstEOB) delete fHChannelProfileVsBurstEOB;
  if (fHChannelProfile2D_PM0) delete fHChannelProfile2D_PM0;
  if (fHChannelProfile2D_PM1) delete fHChannelProfile2D_PM1;
  if (fHChannelProfile2DInner_PM0) delete fHChannelProfile2DInner_PM0;
  if (fHChannelProfile2DInner_PM1) delete fHChannelProfile2DInner_PM1;
  if (fHChannelProfile2D_EOB_PM0) delete fHChannelProfile2D_EOB_PM0;
  if (fHChannelProfile2D_EOB_PM1) delete fHChannelProfile2D_EOB_PM1;
  if (fHChannelProfile2DInner_EOB_PM0) delete fHChannelProfile2DInner_EOB_PM0;
  if (fHChannelProfile2DInner_EOB_PM1) delete fHChannelProfile2DInner_EOB_PM1;
  if (fHCandidateProfile2D) delete fHCandidateProfile2D;
  if (fHCandidateProfile2DInner) delete fHCandidateProfile2DInner;
  if (fHTightCandidateProfile2D) delete fHTightCandidateProfile2D;
  if (fHTightCandidateProfile2DInner) delete fHTightCandidateProfile2DInner;
  if (fHTightPrimitiveProfile2D_EOB) delete fHTightPrimitiveProfile2D_EOB;
  if (fHTightPrimitiveProfile2DInner_EOB) delete fHTightPrimitiveProfile2DInner_EOB;
  if (fHTightCandidateProfileVsL0TriggerBit) delete fHTightCandidateProfileVsL0TriggerBit;
  if (fHTightCandidateProfileVsNoL0TriggerBit) delete fHTightCandidateProfileVsNoL0TriggerBit;
  if (fHHitStatusVsChannel) delete fHHitStatusVsChannel;
  if (fHHitStatusVsROChannel) delete fHHitStatusVsROChannel;
  if (fHLeadingTimeVsChannel) delete fHLeadingTimeVsChannel;
  if (fHLeadingTimeVsROChannel) delete fHLeadingTimeVsROChannel;
  if (fHRecoHitTimeWrtReferenceVsChannel) delete fHRecoHitTimeWrtReferenceVsChannel;
  if (fHRecoHitTimeWrtReferenceVsBurst) delete fHRecoHitTimeWrtReferenceVsBurst;
  if (fHRecoHitTimeWrtReferenceVsWidth) delete fHRecoHitTimeWrtReferenceVsWidth;
  if (fHRecoHitTimeWrtReferenceVsTimeStamp_Channel147) delete fHRecoHitTimeWrtReferenceVsTimeStamp_Channel147;
  if (fHCandidateTimeWrtReferenceVsTile) delete fHCandidateTimeWrtReferenceVsTile;
  if (fHCandidateAvgTimeWrtReferenceVsTile) delete fHCandidateAvgTimeWrtReferenceVsTile;
  if (fHCandidateTimeWrtReferenceNoTileT0VsTile) delete fHCandidateTimeWrtReferenceNoTileT0VsTile;
  if (fHCandidateTimeWrtReferenceVsBurst) delete fHCandidateTimeWrtReferenceVsBurst;
  if (fHWidthVsChannel) delete fHWidthVsChannel;
  if (fHWidthVsROChannel) delete fHWidthVsROChannel;
  if (fHSlotVsLeadingTime) delete fHSlotVsLeadingTime;
  if (fHDeltaTimeVsTile) delete fHDeltaTimeVsTile;
  if (fPDeltaTimeVsTile) delete fPDeltaTimeVsTile;
  if (fHMaskedProfile2D_PM0) delete fHMaskedProfile2D_PM0;
  if (fHMaskedProfile2D_PM1) delete fHMaskedProfile2D_PM1;
  if (fHMaskedProfile2DInner_PM0) delete fHMaskedProfile2DInner_PM0;
  if (fHMaskedProfile2DInner_PM1) delete fHMaskedProfile2DInner_PM1;
  if (fHEventMinusCFDTimePM0) delete fHEventMinusCFDTimePM0;
  if (fHEventMinusCFDTimePM1) delete fHEventMinusCFDTimePM1;
  if (fHEventMinusCFDTimePM0_all) delete fHEventMinusCFDTimePM0_all;
  if (fHEventMinusCFDTimePM1_all) delete fHEventMinusCFDTimePM1_all;
  if (fHCFDTimePM0) delete fHCFDTimePM0;
  if (fHCFDTimePM1) delete fHCFDTimePM1;
  if (fHEnergy) delete fHEnergy;
  if (fHEnergyPerCell) delete fHEnergyPerCell;
  if (fHNHits) delete fHNHits;
  if (fHNChannels) delete fHNChannels;
  if (fHTimevsEnergyPM0) delete fHTimevsEnergyPM0;
  if (fHTimevsEnergyPM1) delete fHTimevsEnergyPM1;
  if (fHTimevsNHitsPM0) delete fHTimevsNHitsPM0;
  if (fHTimevsNHitsPM1) delete fHTimevsNHitsPM1;
  if (fHCollectionMatrixA) delete fHCollectionMatrixA;
  if (fHCollectionMatrixB) delete fHCollectionMatrixB;
  if (fHReferenceShape) delete fHReferenceShape;

  ResetHistograms();
}

void MUV3Reconstruction::ResetHistograms() {
  fHNDigis = nullptr;
  fHNRecoHits = nullptr;
  fHNRecoHitsPerTile = nullptr;
  fHNCandidates = nullptr;
  fHNRecoHitsPerBurst = nullptr;
  fHNHitsPerBurstEOB = nullptr;
  fHNLooseCandidatesPerBurst = nullptr;
  fHNTightCandidatesPerBurst = nullptr;
  fHNTightMuonsPerBurstEOB = nullptr;
  fHTotalPrimitiveCountsEOB = nullptr;
  fHErrorCountsEOB = nullptr;
  fHInterCandidateDistance = nullptr;
  fHNCandidatesToRecoHitsPerTile = nullptr;
  fHHitStatus = nullptr;
  fHTrailingTimeRaw = nullptr;
  fHLeadingTime = nullptr;
  fHTrailingTime = nullptr;
  fHLeadingTimeRaw_NoTrailing = nullptr;
  fHTrailingTimeRaw_NoLeading = nullptr;
  fHRecoHitTimeWrtReference = nullptr;
  fHRecoHitTimeWrtReferenceNoT0 = nullptr;
  fHCandidateTimeWrtReference = nullptr;
  fHCandidateAvgTimeWrtReference = nullptr;
  fHCandidateTimeWrtReferenceNoTileT0 = nullptr;
  fHCandidateTimeWrtReferenceNoT0 = nullptr;
  fHWidth = nullptr;
  fHNErrorWord = nullptr;
  fHChannelProfile = nullptr;
  fHChannelProfileEOB = nullptr;
  fHROChannelProfile = nullptr;
  fHROChannelProfileEOB = nullptr;
  fHTileAsymmetry = nullptr;
  fHTileAsymmetryEOB = nullptr;
  fHCandidateProfile = nullptr;
  fHTightCandidateProfile = nullptr;
  fHTightPrimitiveProfileEOB = nullptr;
  fHTileAND = nullptr;
  fHTileOR = nullptr;
  fHDeltaTime = nullptr;
  fHDeltaTimeCandidate = nullptr;
  fHHitFineTimeBits = nullptr;
  fHHitFineTime256 = nullptr;
  fHRecoHitTimeWrtReference_Channel147 = nullptr;
  fHNRecoHitsVsL0TriggerBit = nullptr;
  fHNRecoHitsVsNoL0TriggerBit = nullptr;
  fHNCandidatesVsL0TriggerBit = nullptr;
  fHNCandidatesVsNoL0TriggerBit = nullptr;
  fHChannelProfileVsBurst = nullptr;
  fHChannelProfileVsBurstEOB = nullptr;
  fHChannelProfile2D_PM0 = nullptr;
  fHChannelProfile2D_PM1 = nullptr;
  fHChannelProfile2DInner_PM0 = nullptr;
  fHChannelProfile2DInner_PM1 = nullptr;
  fHChannelProfile2D_EOB_PM0 = nullptr;
  fHChannelProfile2D_EOB_PM1 = nullptr;
  fHChannelProfile2DInner_EOB_PM0 = nullptr;
  fHChannelProfile2DInner_EOB_PM1 = nullptr;
  fHCandidateProfile2D = nullptr;
  fHCandidateProfile2DInner = nullptr;
  fHTightCandidateProfile2D = nullptr;
  fHTightCandidateProfile2DInner = nullptr;
  fHTightPrimitiveProfile2D_EOB = nullptr;
  fHTightPrimitiveProfile2DInner_EOB = nullptr;
  fHTightCandidateProfileVsL0TriggerBit = nullptr;
  fHTightCandidateProfileVsNoL0TriggerBit = nullptr;
  fHHitStatusVsChannel = nullptr;
  fHHitStatusVsROChannel = nullptr;
  fHLeadingTimeVsChannel = nullptr;
  fHLeadingTimeVsROChannel = nullptr;
  fHRecoHitTimeWrtReferenceVsChannel = nullptr;
  fHRecoHitTimeWrtReferenceVsBurst = nullptr;
  fHRecoHitTimeWrtReferenceVsWidth = nullptr;
  fHRecoHitTimeWrtReferenceVsTimeStamp_Channel147 = nullptr;
  fHCandidateTimeWrtReferenceVsTile = nullptr;
  fHCandidateAvgTimeWrtReferenceVsTile = nullptr;
  fHCandidateTimeWrtReferenceNoTileT0VsTile = nullptr;
  fHCandidateTimeWrtReferenceVsBurst = nullptr;
  fHWidthVsChannel = nullptr;
  fHWidthVsROChannel = nullptr;
  fHSlotVsLeadingTime = nullptr;
  fHDeltaTimeVsTile = nullptr;
  fPDeltaTimeVsTile = nullptr;
  fHMaskedProfile2D_PM0 = nullptr;
  fHMaskedProfile2D_PM1 = nullptr;
  fHMaskedProfile2DInner_PM0 = nullptr;
  fHMaskedProfile2DInner_PM1 = nullptr;
  fHEventMinusCFDTimePM0 = nullptr;
  fHEventMinusCFDTimePM1 = nullptr;
  fHCFDTimePM0 = nullptr;
  fHCFDTimePM1 = nullptr;
  fHEventMinusCFDTimePM0_all = nullptr;
  fHEventMinusCFDTimePM1_all = nullptr;
  fHEnergy = nullptr;
  fHEnergyPerCell = nullptr;
  fHNHits = nullptr;
  fHNChannels = nullptr;
  fHTimevsEnergyPM0 = nullptr;
  fHTimevsEnergyPM1 = nullptr;
  fHTimevsNHitsPM0 = nullptr;
  fHTimevsNHitsPM1 = nullptr;
  fHCollectionMatrixA = nullptr;
  fHCollectionMatrixB = nullptr;
  fHReferenceShape = nullptr;
}

////////////////////////////
// End of processing actions

void MUV3Reconstruction::EndProcessing() {
  NA62VReconstruction::EndProcessing();

  // Build the asymmetries of the PM1 and PM0 rates
  TString Name = "TileAsymmetry";
  fHTileAsymmetry = new TH1D(Name, Name, fNTiles, -0.5, fNTiles-0.5);
  fHTileAsymmetry->GetXaxis()->SetTitle("MUV3 tile");
  fHTileAsymmetry->GetYaxis()->SetTitle("Asymmetry N(hits): (PM1-PM0)/(PM1+PM0)");

  for (Int_t i=1; i<=fHChannelProfile->GetNbinsX(); i++) {
    Double_t x    = fHChannelProfile->GetBinContent(i+200);
    Double_t y    = fHChannelProfile->GetBinContent(i);
    if (x+y<1) continue;
    Double_t dx   = sqrt(x);
    Double_t dy   = sqrt(y);
    Double_t f    = (x-y)/(x+y);
    Double_t dfdx = 2.0*y/(x+y)/(x+y);
    Double_t dfdy = 2.0*x/(x+y)/(x+y);
    Double_t df   = sqrt((dfdx*dx)*(dfdx*dx)+(dfdy*dy)*(dfdy*dy));
    fHTileAsymmetry->SetBinContent(i, f);
    fHTileAsymmetry->SetBinError(i, df);
  }

  if (fIsRawData) {
    Name = "TileAsymmetryEOB";
    fHTileAsymmetryEOB = new TH1D(Name, Name, fNTiles, -0.5, fNTiles-0.5);
    fHTileAsymmetryEOB->GetXaxis()->SetTitle("MUV3 tile");
    fHTileAsymmetryEOB->GetYaxis()->SetTitle("Asymmetry N(hits): (PM1-PM0)/(PM1+PM0)");

    for (Int_t i=1; i<=fHChannelProfileEOB->GetNbinsX(); i++) {
      Double_t x    = fHChannelProfileEOB->GetBinContent(i+200);
      Double_t y    = fHChannelProfileEOB->GetBinContent(i);
      if (x+y<1) continue;
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

  // Build the candidates to reconstructed hits ratio
  Name = "NCandidatesToRecoHitsPerTile";
  fHNCandidatesToRecoHitsPerTile = new TH1D(Name, Name, fNTiles, -0.5, fNTiles-0.5);
  fHNCandidatesToRecoHitsPerTile->GetXaxis()->SetTitle("Tile");
  fHNCandidatesToRecoHitsPerTile->GetYaxis()->SetTitle("Candidates / Reconstructed hits");
  fHNCandidatesToRecoHitsPerTile->Divide(fHCandidateProfile, fHNRecoHitsPerTile);

  SaveHistograms();
}

void MUV3Reconstruction::FillTimes(Double_t ReferenceTime) {

  // @@ can use a trigger mask here, e.g.
  // if (!(fL0TriggerFlags & 5)) return;

  // Common part for all the subdetectors: currently global T0 evaluation only
  NA62VReconstruction::FillTimes(ReferenceTime);

  Int_t NRecoHits = fRecoEvent->GetNHits();
  TClonesArray &Hits = (*(fRecoEvent->GetHits()));

  // RecoHit times wrt the reference time
  for (Int_t i=0; i<NRecoHits; i++) {
    TRecoMUV3Hit *Hit        = static_cast<TRecoMUV3Hit*>(Hits[i]);
    Int_t    ch              = Hit->GetChannelID();
    Int_t    ROch            = Hit->GetROChannelID();
    Double_t LeadingTime     = Hit->GetLeadingTime();     // corrected for global T0 and channel T0s
    Double_t LeadingTimeNoT0 = Hit->GetLeadingTimeNoT0(); // corrected for global T0 only
    Double_t TrailingTime    = Hit->GetTrailingTime();
    Double_t Width           = TrailingTime    - LeadingTime;
    Double_t dT              = LeadingTime     - ReferenceTime;
    Double_t dT_NoT0         = LeadingTimeNoT0 - ReferenceTime;
    fHRecoHitTimeWrtReference->Fill(dT);          // T0-corrected
    fHRecoHitTimeWrtReferenceNoT0->Fill(dT_NoT0); // not T0-corrected
    if (fHRecoHitTimeWrtReferenceVsChannel)       fHRecoHitTimeWrtReferenceVsChannel->Fill(ch, dT);
    if (fHRecoHitTimeWrtReferenceVsROChannel)     fHRecoHitTimeWrtReferenceVsROChannel->Fill(ROch, dT);
    if (fHRecoHitTimeWrtReferenceVsROChannelNoT0) fHRecoHitTimeWrtReferenceVsROChannelNoT0->Fill(ROch, dT_NoT0);
    if (fHRecoHitTimeWrtReferenceVsBurst) {
      fHRecoHitTimeWrtReferenceVsBurst->Fill(fRecoEvent->GetBurstID(), dT);
    }
    if (fHRecoHitTimeWrtReferenceVsWidth) {
      fHRecoHitTimeWrtReferenceVsWidth->Fill(Width, dT);
    }

    // Monitor the 200 MHz beam structure in the "hot tile" 147
    if (ch==147) {
      if (fHRecoHitTimeWrtReferenceVsTimeStamp_Channel147) {
	fHRecoHitTimeWrtReferenceVsTimeStamp_Channel147->Fill(fTimeStamp*ClockPeriod*1e-9, dT_NoT0);
      }
      if (fHRecoHitTimeWrtReference_Channel147) {
	fHRecoHitTimeWrtReference_Channel147->Fill(dT_NoT0);
      }
    }
  }

  // RecoHit time correlations in each tile
  for (Int_t i=0; i<NRecoHits; i++) {
    TRecoMUV3Hit *Hit1 = static_cast<TRecoMUV3Hit*>(Hits[i]);
    Int_t ch1 = Hit1->GetChannelID();
    if (ch1>=200) continue;

    for (Int_t j=0; j<NRecoHits; j++) {
      TRecoMUV3Hit *Hit2 = static_cast<TRecoMUV3Hit*>(Hits[j]);
      Int_t ch2 = Hit2->GetChannelID();
      if (ch2<200) continue;
      if (ch2-ch1 != 200) continue;

      Int_t    Tile  = ch1%200;
      Double_t Time1 = Hit1->GetTime(); // low number PMT
      Double_t Time2 = Hit2->GetTime(); // high number PMT
      fTiles[Tile]->FillDeltaTime(Time1-ReferenceTime, Time2-ReferenceTime); // corrected for channel T0s
    }
  }

  // Tight candidate times wrt the reference time
  for (Int_t iC=0; iC<fRecoEvent->GetNCandidates(); iC++) {
    fCandidate = static_cast<TRecoMUV3Candidate*>(fRecoEvent->GetCandidate(iC));
    if (fCandidate->GetType()!=kTightCandidate) continue;

    Int_t    Tile            = fCandidate->GetTileID();
    Double_t dTcand          = fCandidate->GetTime()         - ReferenceTime;
    Double_t dTcand_NoTileT0 = fCandidate->GetTimeNoTileT0() - ReferenceTime;
    Double_t dTcand_NoT0     = fCandidate->GetTimeNoT0()     - ReferenceTime;
    Double_t dTcand_AvgTime  = fCandidate->GetAverageTime()  - ReferenceTime;

    fHCandidateAvgTimeWrtReference->Fill(dTcand_AvgTime);       // average hit time
    fHCandidateTimeWrtReference->Fill(dTcand);                  // latest hit time: standard definition
    fHCandidateTimeWrtReferenceNoTileT0->Fill(dTcand_NoTileT0); // tile T0 corrections absent
    fHCandidateTimeWrtReferenceNoT0->Fill(dTcand_NoT0);         // all T0 corrections absent
    if (fHCandidateTimeWrtReferenceVsTile) fHCandidateTimeWrtReferenceVsTile->Fill(Tile, dTcand);
    if (fHCandidateAvgTimeWrtReferenceVsTile) fHCandidateAvgTimeWrtReferenceVsTile->Fill(Tile, dTcand_AvgTime);
    if (fHCandidateTimeWrtReferenceNoTileT0VsTile) fHCandidateTimeWrtReferenceNoTileT0VsTile->Fill(Tile, dTcand_NoTileT0);
    if (fHCandidateTimeWrtReferenceVsBurst) fHCandidateTimeWrtReferenceVsBurst->Fill(fRecoEvent->GetBurstID(), dTcand);
  }
}
