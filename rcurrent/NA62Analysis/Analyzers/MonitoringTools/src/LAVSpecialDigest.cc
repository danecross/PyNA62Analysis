#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "LAVConst.hh"
#include "LAVSpecialDigest.hh"
#include "LAVStaticGeometry.hh"
#include "LAVBadChannelList.hh"
#include "LAVEffRecoBlock.hh"
#include "LAVRecoHit.hh"
#include "LAVSpecialColumn.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "TDCEvent.hh"
using namespace std;
using namespace NA62Constants;

LAVStaticGeometry *LAVGeometry;
LAVBadChannelList *LAVBadChannels;

/// \class LAVSpecialDigest
/// \Brief 
/// Describe your Analyzer
/// \EndBrief
///
/// \Detailed
/// Describe your Analyzer
/// \EndDetailed

LAVSpecialDigest::LAVSpecialDigest(BaseAnalysis *ba) : Analyzer(ba),
  fUseCedarOnly(kTRUE)
{

  fAnalyzerName = "LAVSpecialDigest";

  LAVGeometry = LAVStaticGeometry::GetInstance();
  LAVBadChannels = new LAVBadChannelList("LAVBadChannels.list");
  RequestTree("LAV", new TDCEvent, "Digis");
  
  RequestTree("Cedar",new TRecoCedarEvent);
  RequestTree("CHOD",new TRecoCHODEvent);

  fGlobalOffsetTrig = 0;
  fGlobalOffsetCHOD = 0;
  fGlobalOffsetCedar = 0;

}


void LAVSpecialDigest::InitOutput(){
  
  AddParam("GlobalOffsetTrig", "double", &fGlobalOffsetTrig, 0.);
  AddParam("GlobalOffsetCHOD", "double", &fGlobalOffsetCHOD, 0.);
  AddParam("GlobalOffsetCedar", "double", &fGlobalOffsetCedar, 0.);
  AddParam("UseCedarOnly", "bool", &fUseCedarOnly, kTRUE);
  
}


void LAVSpecialDigest::InitHist(){

  BookHisto(new TH1F("nCHODCand", "CHOD candidate multiplicity", 12, -0.5, 11.5));
  BookHisto(new TH1F("nCedarCand", "Cedar candidate multiplicity", 12, -0.5, 11.5));
  
  BookHisto(new TH1F("CHODTimeAbs", "Absolute CHOD time", 5000, -5000, +5000));
  BookHisto(new TH1F("CedarTimeAbs", "Absolute Cedar time", 5000, -5000, +5000));
  BookHisto(new TH1F("CHODTime", "Local CHOD time", 800, -200, +200));
  BookHisto(new TH1F("CedarTime", "Local Cedar time", 800, -200, +200));
  BookHisto(new TH1F("dtCedarCHOD", "dt(Cedar - CHOD)", 200, -100, 100));
  
  for (Int_t iStation = 0; iStation < 12; iStation++) {
    
    Int_t nChannels = LAVGeometry->CompactPerStation(iStation + 1);
    //cout << "LAV " << iStation + 1 << " CHANNELS " << nChannels << endl;    

    BookHisto(new TH1F(Form("hEffC_LAV%02d", iStation + 1), Form("LAV %02d Good Columns", iStation + 1),
		       nChannels, -0.5, nChannels - 0.5));
    BookHisto(new TH1F(Form("hEffL_LAV%02d", iStation + 1), Form("LAV %02d Found Leading", iStation + 1),
		       nChannels, -0.5, nChannels - 0.5));    
    BookHisto(new TH1F(Form("hEffH_LAV%02d", iStation + 1), Form("LAV %02d Found Hit", iStation + 1),
		       nChannels, -0.5, nChannels - 0.5));
    
  }
  
}

void LAVSpecialDigest::DefineMCSimple(){
}

void LAVSpecialDigest::StartOfRunUser(){
}

void LAVSpecialDigest::StartOfBurstUser(){
}

void LAVSpecialDigest::EndOfBurstUser(){
}

void LAVSpecialDigest::Process(Int_t){

  //cout << "I'm in process!!" << endl;
  //Event*  MCTruthEvent = NULL;
  //if(GetWithMC())  MCTruthEvent= GetMCEvent();
  
	TRecoCedarEvent *CedarEvent = GetEvent<TRecoCedarEvent>();
	TRecoCHODEvent *CHODEvent = GetEvent<TRecoCHODEvent>();
	EventHeader *RawHeader = GetEventHeader();  
	cout << user_standard() << "ANALYZED BURST = " << RawHeader->GetBurstID() << endl;

	Double_t FineTime = RawHeader->GetFineTime()*ClockPeriod/256;

	Bool_t TimingGood = kFALSE;
	Double_t dtMax = DT_CEDAR_MAX;
    
	Int_t nCedarCandidates = CedarEvent->GetNCandidates();
	fHisto.GetTH1("nCedarCand")->Fill(nCedarCandidates);
	//cout << "N Cedar Candidates  = " << nCedarCandidates << endl; 

	Int_t nCHODCandidates = CHODEvent->GetNCandidates();
	fHisto.GetTH1("nCHODCand")->Fill(nCHODCandidates);
  
	for (Int_t iCedarCandidate = 0; iCedarCandidate < CedarEvent->GetNCandidates(); iCedarCandidate++) {
	  TRecoCedarCandidate* CedarCandidate = static_cast<TRecoCedarCandidate*>(CedarEvent->GetCandidate(iCedarCandidate));
	  if (CedarCandidate->GetNHits() > MIN_CEDAR_HITS) {
	    Double_t CedarTime = CedarCandidate->GetTime() - FineTime;
	    fHisto.GetTH1("CedarTimeAbs")->Fill(CedarTime);
	    CedarTime -= fGlobalOffsetCedar;
	    fHisto.GetTH1("CedarTime")->Fill(CedarTime);
	    
	    if (fUseCedarOnly) {

	      if (abs(CedarTime) < abs(dtMax)) {
		dtMax = CedarTime;
		TimingGood = kTRUE;
	      }
	      
	    } else {
      
	      for (Int_t iCHODCandidate = 0; iCHODCandidate < CHODEvent->GetNCandidates(); iCHODCandidate++) {

		TRecoCHODCandidate* CHODCandidate = static_cast<TRecoCHODCandidate*>(CHODEvent->GetCandidate(iCHODCandidate));
		Double_t CHODTime = CHODCandidate->GetTime() - FineTime;
		fHisto.GetTH1("CHODTimeAbs")->Fill(CHODTime);
		CHODTime -= fGlobalOffsetCHOD;
		fHisto.GetTH1("CHODTime")->Fill(CHODTime);
		
		Double_t dtNow = CedarTime - CHODTime;
		if (abs(dtNow) < abs(dtMax)) {
		  dtMax = dtNow;
		  TimingGood = kTRUE;
		}
	  
	      }
	    }
	    
	  }
	}
  
  
	if (TimingGood) {
	  
	  fHisto.GetTH1("dtCedarCHOD")->Fill(dtMax);  
    
    TDCEvent* LAVDigiEvent = static_cast<TDCEvent*>(GetEvent("LAV", "Digis"));
    
    
    // Get hits and fill edge collection
    
    Int_t nFiredBlocks = 0;
    Int_t BlockIndex[MAX_BLOCK_ID];
    for (Int_t i = 0; i < MAX_BLOCK_ID; i++) BlockIndex[i] = -1;
    
    vector<LAVEffRecoBlock*> LAVRecoBlocks;

    TClonesArray &Digis = *(LAVDigiEvent->GetHits());
    Int_t nDigis = LAVDigiEvent->GetNHits();
    
    for (Int_t iDigi = 0; iDigi < nDigis; iDigi++) {     

      TLAVDigi* Digi = static_cast<TLAVDigi*>(Digis[iDigi]);
      
      Digi->DecodeChannelID();
      Int_t BlockID = Digi->EncodeChannelID();
      Int_t threshold = Digi->GetThresholdType();
      
      if (! LAVBadChannels->IsBad(BlockID)) {
      
	LAVEffRecoBlock* Block;
	
	if (BlockIndex[BlockID] == -1) {
	  Block = new LAVEffRecoBlock(BlockID);
	  LAVRecoBlocks.push_back(Block);
	  BlockIndex[BlockID] = nFiredBlocks++;
	}
	else {
	  Block = LAVRecoBlocks.at(BlockIndex[BlockID]);
	}
	
	if (Digi->GetDetectedEdge() & 1) {
	  Double_t LAVTime = Digi->GetLeadingEdge() - FineTime - fGlobalOffsetTrig;
	  Block->SetEdge(threshold, 0, LAVTime, Digi);
	}     
	if (Digi->GetDetectedEdge() & 2) {
	  Double_t LAVTime = Digi->GetTrailingEdge() - FineTime - fGlobalOffsetTrig;
	  Block->SetEdge(threshold, 1, LAVTime, Digi);
	}

      }

    }

    
    // Set up for efficiency calculation
    
    vector<LAVSpecialColumn*> ColumnList;
    Int_t ColumnMap[MAX_COLUMN];  
    for (Int_t i = 0; i < MAX_COLUMN; i++) ColumnMap[i] = -1; 
    
    
    // Loop over blocks and create hits
    
    for (Int_t iBlock = 0; iBlock < nFiredBlocks; iBlock++) {
      
      LAVRecoBlocks.at(iBlock)->SortEdges();
      LAVRecoBlocks.at(iBlock)->CreateEffHits();
      
      Int_t BlockID = LAVRecoBlocks.at(iBlock)->GetBlockID();
      LAVGeometry->SpecifyChannel(BlockID);
      Int_t Column = LAVGeometry->GetColumn();
      Int_t Station = LAVGeometry->GetStation();
      Int_t Layer = LAVGeometry->GetLayer();
      Int_t Position = LAVGeometry->GetPosition();
      
      Int_t nRecoHits = LAVRecoBlocks.at(iBlock)->GetNRecoHits();
      
      
      // Loop over hits created on block
      
      for (Int_t iHit = 0; iHit<nRecoHits; iHit++) {
	LAVRecoHit* Hit = LAVRecoBlocks.at(iBlock)->GetRecoHit(iHit);
	
	Int_t EdgeMask = Hit->GetEdgeMask();
	
	Int_t Threshold = 0, Type = 0;
	Double_t Lead=0., ToT=0.;
	if (EdgeMask == 1 || EdgeMask == 8 || EdgeMask == 9) { // Low threshold
	  if (EdgeMask & 1) Type |= 1;
	  if (EdgeMask & 8) Type |= 2;
	  Threshold = 0;
	  Lead = Hit->GetLeadingEdgeLow();
	  ToT = Hit->GetTrailingEdgeLow() -  Hit->GetLeadingEdgeLow();
	  
	} else if (EdgeMask == 2 || EdgeMask == 4 || EdgeMask == 6) { // High threshold 
	  if (EdgeMask & 2) Type |= 1;
	  if (EdgeMask & 4) Type |= 2;
	  Threshold = 1;
	  Lead = Hit->GetLeadingEdgeHigh();
	  ToT = Hit->GetTrailingEdgeHigh() - Hit->GetLeadingEdgeHigh();
	  
	} else {
	  cerr << "Oh no! Your hit reconstruction isn't working! EdgeMask = " << EdgeMask << endl;
	}
	
	if (Threshold == TEST_THRESHOLD) { // Efficiency hit
	  
	  if (ColumnMap[Column] < 0) {
	    LAVSpecialColumn *C = new LAVSpecialColumn(Column);
	    ColumnList.push_back(C);
	    ColumnMap[Column] = ColumnList.size() - 1;
	  } 
	  ColumnList[ColumnMap[Column]]->AddHit(0, Layer, 0, Type, Lead, ToT);
	  
	}

	if (Threshold == CONTROL_THRESHOLD) { // Control hit
	  
	  if (ColumnMap[Column] < 0) { // Use this hit as control hit for 1 or 2 other layers
	    LAVSpecialColumn *C = new LAVSpecialColumn(Column);
	    ColumnList.push_back(C);
	    ColumnMap[Column] = ColumnList.size() - 1;
	  } 
	  ColumnList[ColumnMap[Column]]->AddHit(1, Layer, 0, Type, Lead, ToT);
	  
	  if ((Station == 12 && Layer == 0) ||
	      (Station != 12 && Layer == LAVGeometry->LayersPerStation() - 1)) { // Also use as control hit for previous column
	    Int_t LeftPosition = Position - 1;
	    if (LeftPosition < 0) LeftPosition = 4*LAVGeometry->BananasPerLayer() - 1;
	    LAVGeometry->SpecifyColumn(Station, LeftPosition);
	    Int_t LeftColumn = LAVGeometry->GetColumn();
	    if (ColumnMap[LeftColumn] < 0) {
	      LAVSpecialColumn *C = new LAVSpecialColumn(LeftColumn);
	      ColumnList.push_back(C);
	      ColumnMap[LeftColumn] = ColumnList.size() - 1;
	    } 
	    ColumnList[ColumnMap[LeftColumn]]->AddHit(1, Layer, -1, Type, Lead, ToT);
	  }
	  	  
	  if ((Station == 12 && Layer == LAVGeometry->LayersPerStation() - 1) ||
	      (Station != 12 && Layer == 0)) { // Also use as control hit for next column
	    Int_t RightPosition = Position + 1;
	    if (RightPosition > 4*LAVGeometry->BananasPerLayer() - 1) RightPosition = 0;
	    LAVGeometry->SpecifyColumn(Station, RightPosition);
	    Int_t RightColumn = LAVGeometry->GetColumn();
	    if (ColumnMap[RightColumn] < 0) {
	      LAVSpecialColumn *C = new LAVSpecialColumn(RightColumn);
	      ColumnList.push_back(C);
	      ColumnMap[RightColumn] = ColumnList.size() - 1;
	    } 
	    ColumnList[ColumnMap[RightColumn]]->AddHit(1, Layer, +1, Type, Lead, ToT);
	  }
	  
	}
	
      }
    }
    
    
    // Loop over columns, measure efficiencies
    
    for (Int_t iColumn = 0; iColumn < (Int_t)ColumnList.size(); iColumn++) {
      
      Int_t Station = ColumnList[iColumn]->GetStation();
      Int_t Position = ColumnList[iColumn]->GetPosition();
      for (Int_t iLayer = 0; iLayer < LAVGeometry->LayersPerStation(Station); iLayer++) {
	
	LAVGeometry->SpecifyChannel(Station, iLayer, Position);
	Int_t BlockID = LAVGeometry->GetBlockID();
	
	if (!LAVBadChannels->IsBad(BlockID)) {
	  if (ColumnList[iColumn]->IsGood(iLayer)) {
	    
	    Int_t Compact = LAVGeometry->GetCompact();
	    fHisto.GetTH1(Form("hEffC_LAV%02d", Station))->Fill(Compact);
	    
	    if (ColumnList[iColumn]->IsFound(iLayer, 1))
	      fHisto.GetTH1(Form("hEffL_LAV%02d", Station))->Fill(Compact);
	    
	    if (ColumnList[iColumn]->IsFound(iLayer, 3))
	      fHisto.GetTH1(Form("hEffH_LAV%02d", Station))->Fill(Compact);
	    
	  }
	}
      }      
    }


    // Free memory for stored columns
    
    for (Int_t i = 0; i < (Int_t)ColumnList.size(); i++) delete ColumnList[i];
    
  }
  
}


void LAVSpecialDigest::PostProcess(){
	/// \MemberDescr
	/// This function is called after an event has been processed by all analyzers. It could be used to free some memory allocated
	/// during the Process.
	/// \EndMemberDescr

}

void LAVSpecialDigest::EndOfRunUser(){
	/// \MemberDescr
	/// This method is called at the end of the processing (corresponding to a end of run in the normal NA62 data taking)\n
	/// Do here your end of run processing if any
	/// \EndMemberDescr

}

void LAVSpecialDigest::ExportPlot(){
	/// \MemberDescr
	/// This method is called at the end of processing to save plots.\n
	/// If you want to save them all, just call\n
	/// \code
	/// 	SaveAllPlots();
	/// \endcode
	/// Or you can just save the ones you want with\n
	/// \code
	/// 	histogram->Write()
	/// \endcode
	/// \n
	/// To run over a set of histograms you can use Iterators (HistoHandler::IteratorTH1,
	/// HistoHandler::IteratorTH2, HistoHandler::IteratorTGraph). You can use it to run over
	/// all the histograms or only a subset of histogram by using one of the two forms of
	/// GetIterator...  (replace ... by TH1, TH2 or TGraph)\n
	/// \code
	/// 	GetIterator...()
	/// \endcode
	/// will get an Iterator running over all histograms of this type while
	/// \code
	/// 	GetIterator...("baseName")
	/// \endcode
	/// will get an Iterator running only over the histograms of this type whose name starts
	/// with baseName.\n
	/// For more details and examples on how to use the Iterator after getting it, please refer
	/// to the HistoHandler::Iterator documentation.\n
	/// Although this is described here in ExportPlot(), Iterators can be used anywhere after the
	/// histograms have been booked.
	/// \EndMemberDescr

  SaveAllPlots();

}


void LAVSpecialDigest::DrawPlot(){
	/// \MemberDescr
	/// This method is called at the end of processing to draw plots.\n
	/// If you want to draw all the plots, just call\n
	/// \code
	/// 	DrawAllPlots();
	/// \endcode
	/// Or do as usual (TCanvas, Draw, ...)\n
	/// \EndMemberDescr
}

