// --------------------------------------------------------------------
// History:
//
// Swap corrections implemented by Michal Zamkovsky (michal.zamkovsky@cern.ch) 2017-11-10
//
// Major update by Karim Massri (karim.massri@cern.ch) 2016-11-14
//   - data corruption checks added
//   - proper Special trigger handling added
//   - fCrateRemap and fSlotRemap added in preparation for LKr
//   - new association Crate -> Board, Slot -> Mezzanine
//   - new DataBuffer is now allocated for <=2015 data only
//
// Created by Riccardo Aliberti (riccardo.aliberti@cern.ch) 2014-10-06
//
// --------------------------------------------------------------------

#include "Riostream.h"
#include "TRegexp.h"

#include "NA62RecoManager.hh"
#include "NA62Reconstruction.hh"
#include "NA62ConditionsService.hh"
#include "CREAMRawDecoder.hh"

#include "NA62BufferProto.hh"
#include "NA62Buffer.hh"

#include "CREAMBufferProto.hh"
#include "CREAMBuffer.hh"

#include "TLKrDigi.hh"
#include "TMUV1Digi.hh"
#include "TMUV2Digi.hh"
#include "TSAVDigi.hh"
#include <byteswap.h>
#include <bitset>
#include "LKrParameters.hh"
#include "NA62Global.hh"

CREAMRawDecoder::CREAMRawDecoder(NA62VReconstruction* Reco) : NA62VRawDecoder(Reco, "CREAM", 16),fLatency(0),fMaxCrateID(0),fMaxSlotID(0),fCrateRemap(0),fSlotRemap(0), fSwapInputFileName(""), fSwapDetectionEnabled(true), fNEntriesSwapWithCalibTriggers(0), fNTotalEntriesSwapWithCalibTriggers(0), fNDetectedSwapsWithCalibTriggers(0), fNCreamsCheckedWithCalibTriggers(0), fCrocusReferenceFileName(""), fNHitsSwapWithCrocus(0), fNHitsRefSwapWithCrocus(0), fNCreamsCheckedWithCrocus(0), fNTriggersForCrocus(0){

  if (fReco->GetName()=="LKr"){
    fDigiEvent = new FADCEvent(TLKrDigi::Class());
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TSpecialTrigger::Class());
  }
  else if (fReco->GetName()=="MUV1"){
    fDigiEvent = new FADCEvent(TMUV1Digi::Class());
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TSpecialTrigger::Class());
  }
  else if (fReco->GetName()=="MUV2"){
    fDigiEvent = new FADCEvent(TMUV2Digi::Class());
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TSpecialTrigger::Class());
  }
  else if (fReco->GetName()=="SAV"){
    fDigiEvent = new FADCEvent(TSAVDigi::Class());
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TSpecialTrigger::Class());
  }
  else NA62VReconstruction::Exception("CREAMRawDecoder: requested subdetector '"+fReco->GetName()+"' not available");

  // Reset histos
  fHitEnergyThr = 0.;
  fHZSCounterXY = 0;
  fHPedestalXY = 0;
  fHHitEnergy = 0;
  fHZSCounterXYCalib = 0;
  fHPedestalXYCalib = 0;
  fHHitEnergyCalib = 0;
  fHMaxSampleXYCalib = 0;
  fHMaxSampleCrateSlotCalib = 0;
  fHHitMapEnergyAboveZS = 0;
  fHHitMapEnergyAboveThr = 0;
  fHNQualityWarningsVsROChannel = 0;
  fHNQualityWarningsXY = 0;
  fHNQualityWarningsCrateSlot = 0;
  fHPedestals = 0;
  fHPedestalsRMS = 0;
  fHNCriticalErrorsCrateSlot = 0;
  fHPedestalWrtRefVsROChannel = 0;
  fHPedestalBadRefXYNum = 0;
  fHPedestalBadRefXYDen = 0;
  fHPedestalBadRefXYEff = 0;
  for(Int_t i=0; i<3; i++){ 
    for(Int_t j=0; j<3; j++){ 
      fHNEntriesNegTail[i][j] = 0;
      fHNEntriesPosTail[i][j] = 0;
      fHNEntriesNegTailOCTANE[i][j] = 0;
      fHNEntriesPosTailOCTANE[i][j] = 0;
      fHDeltaT[i][j] = 0;
      fHDeltaTOCTANE[i][j] = 0;
      fHNPipEntries[i][j] = 0;
    }
  }
  fHSeedPosition = 0;
  fHFossilCellPosition = 0;
  fHFossilCREAMPosition = 0;
  if (fReco->GetName()=="LKr"){
    fHitEnergyThr = 250.; //MeV (default for LKr)
    fHZSCounterXY  = new TH2F("ZeroSuppressionCounter","Zero-suppressed cell counter",128,-0.5,127.5,128,-0.5,127.5);
    fHPedestalXY = new TH2F("PedestalsXY","Pedestal value",128,-0.5,127.5,128,-0.5,127.5);
    fHHitEnergy = new TH1D("HitEnergy","",1010,-1000,100000);
    fHZSCounterXYCalib  = new TH2F("ZeroSuppressionCounterCalib","Zero-suppressed cell counter",128,-0.5,127.5,128,-0.5,127.5);
    fHPedestalXYCalib = new TH2F("PedestalsXYCalib","Pedestal value",128,-0.5,127.5,128,-0.5,127.5);
    fHHitEnergyCalib = new TH1D("HitEnergyCalib","",1010,-1000,100000);
    fHMaxSampleXYCalib = new TH2F("MaxSampleXYCalib","Average value of the maximum sample (Calibration triggers)",128,-0.5,127.5,128,-0.5,127.5);
    fHMaxSampleCrateSlotCalib = new TH2F("MaxSampleCrateSlotCalib","Average value of the maximum sample (Calibration triggers)",128,-0.5,31.5,192,-0.5,23.5);
    fHHitMapEnergyAboveZS = new TH2F("HitMapEnergyAboveZS","Total energy deposited per cell",128,-0.5,127.5,128,-0.5,127.5);
    fHHitMapEnergyAboveThr = new TH2F("HitMapEnergyAboveThr","Total energy deposited per cell",128,-0.5,127.5,128,-0.5,127.5);
    fHNQualityWarningsXY  = new TH2F("NQualityWarningsXY","NQualityWarnings XY",128,-0.5,127.5,128,-0.5,127.5);
    fHPedestalBadRefXYNum = new TH2F("PedestalBadRefXYNum","",128,-0.5,127.5,128,-0.5,127.5);
    fHPedestalBadRefXYDen = new TH2F("PedestalBadRefXYDen","",128,-0.5,127.5,128,-0.5,127.5);
    fHPedestalBadRefXYEff = new TH2F("PedestalBadRefXYEff","",128,-0.5,127.5,128,-0.5,127.5);

    for(Int_t i=0; i<3; i++){ 
      for(Int_t j=0; j<3; j++){ 
        if(i*j==1) continue;
        fHNEntriesNegTail[i][j]          = new TH2F(Form("HistoNegative_%d_%d", i,j), "", 32, -0.5, 31.5, 16, -0.5, 15.5);   
        fHNEntriesPosTail[i][j]          = new TH2F(Form("HistoPositive_%d_%d", i,j), "", 32, -0.5, 31.5, 16, -0.5, 15.5);
        fHNEntriesNegTailOCTANE[i][j]    = new TH2F(Form("HistoNegativeOCTANE_%d_%d", i,j), "", 32, -0.5, 31.5, 16, -0.5, 15.5);
        fHNEntriesPosTailOCTANE[i][j]    = new TH2F(Form("HistoPositiveOCTANE_%d_%d", i,j), "", 32, -0.5, 31.5, 16, -0.5, 15.5);
        fHDeltaT[i][j]                   = new TH2F(Form("DeltaT_%d_%d",i,j), "", 512, -0.5, 511.5, 600, -200, 200);
        fHDeltaTOCTANE[i][j]                   = new TH2F(Form("DeltaTOCTANE_%d_%d",i,j), "", 512, -0.5, 511.5, 600, -200, 200);
        fHNPipEntries[i][j]              = new TH2F(Form("NPipsPerCREAM_%d_%d_",i,j), "", 32, -0.5, 31.5, 16, -0.5, 15.5);
      }
    }

    fHSeedPosition = new TH2F("SeedPosition","",128,-0.5,127.5,128,-0.5,127.5);
    fHFossilCellPosition = new TH2F("FossilCellPosition","", 128, -0.5, 127.5, 128, -0.5, 127.5);
    fHFossilCREAMPosition = new TH2F("FossilCREAMPosition", "", 32, -0.5, 31.5, 21, -0.5, 20.5);
  }
  //----------- Init common parameters and instances ----------//
  fMaxCrateID = 31;
  fMaxSlotID = 20; //slot numbers arrive up to 20
  fCrateRemap = new Int_t[fMaxCrateID+1];
  fSlotRemap  = new Int_t[fMaxSlotID+1];

  NA62VRawDecoder::CreateObjects();
  ParseRawDecoderSettingsFile(fReco->GetRawDecoderSettingsFileName()); //CREAM-specific parameters

  fNEntriesSwapWithCalibTriggers = new Int_t**[fMaxCrateID+1];
  fNTotalEntriesSwapWithCalibTriggers = new Int_t*[fMaxCrateID+1];
  fNDetectedSwapsWithCalibTriggers = new Int_t*[fMaxCrateID+1];
  fNHitsSwapWithCrocus    = new Int_t**[fMaxCrateID+1];
  fNHitsRefSwapWithCrocus = new Int_t**[fMaxCrateID+1];
  for(UInt_t iCrate=0;iCrate<fMaxCrateID+1;iCrate++){
    fNEntriesSwapWithCalibTriggers[iCrate] = new Int_t*[fMaxSlotID+1];
    fNTotalEntriesSwapWithCalibTriggers[iCrate] = new Int_t[fMaxSlotID+1];
    fNDetectedSwapsWithCalibTriggers[iCrate] = new Int_t[fMaxSlotID+1];
    fNHitsSwapWithCrocus[iCrate]    = new Int_t*[fMaxSlotID+1];
    fNHitsRefSwapWithCrocus[iCrate] = new Int_t*[fMaxSlotID+1];
    for (UInt_t iSlot=0;iSlot<fMaxSlotID+1;iSlot++){
      fNEntriesSwapWithCalibTriggers[iCrate][iSlot] = new Int_t[2];
      fNTotalEntriesSwapWithCalibTriggers[iCrate][iSlot] = 0;
      for(UInt_t iPattern=0;iPattern<2;iPattern++) fNEntriesSwapWithCalibTriggers[iCrate][iSlot][iPattern]=0;
      fNDetectedSwapsWithCalibTriggers[iCrate][iSlot] = 0;
      fNHitsSwapWithCrocus[iCrate][iSlot] = new Int_t[32];
      fNHitsRefSwapWithCrocus[iCrate][iSlot] = new Int_t[32];
      for (UInt_t iCh=0;iCh<32;iCh++) {
        fNHitsRefSwapWithCrocus[iCrate][iSlot][iCh] = -1;
        fNHitsSwapWithCrocus[iCrate][iSlot][iCh] = 0;
      }
    }
  }
  for(UInt_t iPattern=0;iPattern<2;iPattern++) fNEntriesPerPatternSwapWithCalibTriggers[iPattern]=0;
  fSwapInput.clear();
  fSwapsFoundWithCalibTriggers.clear();
  fSwapsFoundWithCrocus.clear();

  Double_t Baseline = 400.;
  if (fReco->GetName()!="LKr") Baseline = 1000.;
  fLKrZSFossils.clear();
  fLKrNeedles.clear();

  //-----------------------------------------------------------//

  // CREAMDecoderErrString - set bin names
  fCREAMDecoderErrString.clear(); 
  fCREAMDecoderErrString.resize(CREAM_OVERFLOWBIN+1,""); // 0->OVERFLOW are needed
  fCREAMDecoderErrString[CREAM_UNDERFLOWBIN] =        "Underflow";             ///< Not currently used
  fCREAMDecoderErrString[CREAM_BLOCKTS_MISMATCH] =    "Block TS mismatch";     ///< Block TimeStamp differs from Trigger TimeStamp 
  fCREAMDecoderErrString[CREAM_EVENTNUMBER_MISMATCH] ="Event Nb mismatch";     ///< Block Event Number differs from Header Event Number
  fCREAMDecoderErrString[CREAM_TRIGWORD_MISMATCH] =   "TrigWord mismatch";     ///< Block TriggerWord differs from Header L0 TriggerType
  fCREAMDecoderErrString[CREAM_BLOCKPAYLOAD_FATAL]=   "Block/PayLoad FATAL";   ///< PayLoad is not consistent with block length [irrecoverable]
  fCREAMDecoderErrString[CREAM_BLOCKPAYLOAD_MISMATCH]="Block/PayLoad mismatch";///< PayLoad is not consistent with block length [recovered]
  fCREAMDecoderErrString[CREAM_PAYLOADCHS_MISMATCH] = "PayLoad/Chs mismatch";  ///< PayLoad is not consistent with size from channel mask
  fCREAMDecoderErrString[CREAM_NSAMPLES_MISMATCH] =   "NSamples mismatch";     ///< NSamples is not the same for all the blocks
  fCREAMDecoderErrString[CREAM_BAD_SPECIAL_TRIGGER] = "Bad Special Trigger";   ///< Not yet supported SpecialTrigger is received
  fCREAMDecoderErrString[CREAM_WRONG_L0RQ] =          "Wrong L0RQ bit";        ///< CREAM L0RQ bit wrongly set to 1
  fCREAMDecoderErrString[CREAM_L1_ERROR] =            "CREAM L1 Error";        ///< CREAM L1 Error detected in data [fHCREAMErrors to be added?]
  fCREAMDecoderErrString[CREAM_REPEATED_WORD] =       "Repeated word";         ///< Repeated word detected in data
  fCREAMDecoderErrString[CREAM_NOT_TIME_ORDER] =      "Not in time order";     ///< Hits from same channel are not time-ordered
  fCREAMDecoderErrString[CREAM_DATA_TYPE] =           "Wrong data type";       ///< TDC Hit is neither a leading, trailing nor an error 
  fCREAMDecoderErrString[CREAM_BAD_DATA_SIZE] =       "Bad data size";         ///< Expected block size differs from the size of the read block
  fCREAMDecoderErrString[CREAM_BAD_TRIGGERTS] =       "Bad trigger TS";        ///< All the slots are inconsistent with the Trigger TimeStamp
  fCREAMDecoderErrString[CREAM_BAD_SLOT] =            "Slot/Trigger mismatch"; ///< Some slots (but not all of them) are inconsistent with the Trigger TimeStamp
  fCREAMDecoderErrString[CREAM_WRONG_CHECKSUM] =      "Wrong CheckSum";        ///< Wrong CheckSum
  fCREAMDecoderErrString[CREAM_MASKED_CH] =           "Hit from Masked Ch";    ///< At least one hit from a masked channel
  fCREAMDecoderErrString[CREAM_BAD_SAMPLES] =         "Bad samples";           ///< Even or odd samples are all 0
  fCREAMDecoderErrString[CREAM_OSCILLATING_CH] =      "Oscillating Ch";        ///< Oscillating channel detected
  fCREAMDecoderErrString[CREAM_OVERFLOWBIN] =         "Overflow";              ///< Used to evaluate the number of warnings implemented 

  fHDecoderErrors = new TH2F("DecoderErrors","Errors from the CREAMDecoder for the "+fReco->GetName(),
      fNROMezzanines,-0.5,fNROMezzanines-0.5,CREAM_OVERFLOWBIN-1, 0.5, (double)CREAM_OVERFLOWBIN - 0.5);
  fHDecoderErrors->GetXaxis()->SetTitle("ROMezzanine ID");
  for(int iBin = 1; iBin < CREAM_OVERFLOWBIN ; ++iBin){
    if(iBin<=GetNCriticalErrorTypes()) fCREAMDecoderErrString[iBin] = Form("#color[2]{%s}",fCREAMDecoderErrString[iBin].Data());  // make label red for critical errors
    fHDecoderErrors->GetYaxis()->SetBinLabel(iBin,fCREAMDecoderErrString[iBin]);
  }

  fHNQualityWarningsVsROChannel = new TH1D("NQualityWarningsVsROChannel", fReco->GetName()+" NQualityWarnings Vs ROChannel",fNROChannels,-0.5,fNROChannels-0.5);
  fHPedestals    = new TH2F("Pedestals",    fReco->GetName()+" Pedestals Vs MezzanineID",fNROMezzanines,-0.5,fNROMezzanines-0.5, 200,Baseline-400,Baseline+600);
  fHPedestalsRMS = new TH2F("PedestalsRMS", fReco->GetName()+" Pedestals RMS Vs MezzanineID",fNROMezzanines,-0.5,fNROMezzanines-0.5, 30,0,300);
  fHPedestalVsROChannel = new TH2F("PedestalVsROChannel","",fNROChannels,-0.5,fNROChannels-0.5,400,Baseline-199.5,Baseline+200.5);
  //if (fReco->GetName()=="LKr") {
  //  fHPedestalWrtRefVsROChannel = new TH2F("PedestalWrtRefVsROChannel","",fNROChannels,-0.5,fNROChannels-0.5,400,-199.5,200.5);
  //}
  fHNCriticalErrorsCrateSlot = new TH2F("NCriticalErrorsCrateSlot","Number of Critical Errors",32,-0.5,31.5,24,-0.5,23.5);
  fHNCriticalErrorsCrateSlot->GetXaxis()->SetTitle("Crate Number");
  fHNCriticalErrorsCrateSlot->GetYaxis()->SetTitle("Slot Number");
  fHNQualityWarningsCrateSlot = new TH2F("NQualityWarningsCrateSlot","Number of Quality Warnings",32,-0.5,31.5,24,-0.5,23.5);
  fHNQualityWarningsCrateSlot->GetXaxis()->SetTitle("Crate Number");
  fHNQualityWarningsCrateSlot->GetYaxis()->SetTitle("Slot Number");
}

CREAMRawDecoder::~CREAMRawDecoder(){
  if(fCrateRemap){
    delete [] fCrateRemap;
    fCrateRemap=0;
  }
  if(fSlotRemap){
    delete [] fSlotRemap;
    fSlotRemap=0;
  }
}

void CREAMRawDecoder::StartOfBurst() { 

  //Reset SwapFinder variables
  for(UInt_t iCrate=0;iCrate<fMaxCrateID+1;iCrate++){
    for (UInt_t iSlot=0;iSlot<fMaxSlotID+1;iSlot++){
      fNDetectedSwapsWithCalibTriggers[iCrate][iSlot] = 0;
      fNTotalEntriesSwapWithCalibTriggers[iCrate][iSlot] = 0;
      for(UInt_t iPattern=0;iPattern<2;iPattern++) fNEntriesSwapWithCalibTriggers[iCrate][iSlot][iPattern] = 0;
      for (UInt_t iCh=0;iCh<32;iCh++) fNHitsSwapWithCrocus[iCrate][iSlot][iCh] = 0;
    }
  }
  for(UInt_t iPattern=0;iPattern<2;iPattern++) fNEntriesPerPatternSwapWithCalibTriggers[iPattern]=0;
  fSwapInput.clear();
  fSwapsFoundWithCalibTriggers.clear();
  fSwapsFoundWithCrocus.clear();
  fNCreamsCheckedWithCalibTriggers=0;
  fNCreamsCheckedWithCrocus=0;
  fNTriggersForCrocus=0;

  UInt_t RunID   = NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID();
  UInt_t BurstID = NA62RecoManager::GetInstance()->GetEventHeader()->GetBurstID();

  //Open and read the swap input file
  if(fSwapInputFileName == "") {
    fSwapInputFileName = Form("%s-Swaps.dat",fReco->GetName().Data()); //default
  }
  if(NA62ConditionsService::GetInstance()->Open(fSwapInputFileName)==kSuccess){
    std::cout << "[CREAMRawDecoder] Reading swap file '" << fSwapInputFileName << "' for " << fReco->GetName() << ".." << std::endl;
    UInt_t ReadNTriggers=0, ReadNSwaps=0;
    Bool_t BurstFound = false; 
    TString Line;
    while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fSwapInputFileName)) && !BurstFound) {
      if (Line.BeginsWith("#")) continue;
      TObjArray * l = Line.Tokenize("[ -]");
      UInt_t ReadRunID=0, ReadBurstID=0;
      if(l->GetEntries()>0) ReadRunID     = static_cast<TObjString*>(l->At(0))->GetString().Atoi();
      if(l->GetEntries()>1) ReadBurstID   = static_cast<TObjString*>(l->At(1))->GetString().Atoi();
      if(ReadRunID==RunID && ReadBurstID==BurstID){
        if(l->GetEntries()>2) ReadNTriggers = static_cast<TObjString*>(l->At(2))->GetString().Atoi();
        if(l->GetEntries()>4) ReadNSwaps    = static_cast<TObjString*>(l->At(4))->GetString().Atoi();
        for (UInt_t iSwap=0;iSwap<ReadNSwaps;iSwap++){
          fSwapInput.push_back(std::make_pair(static_cast<TObjString*>(l->At(5+2*iSwap))->GetString().Atoi(), static_cast<TObjString*>(l->At(5+2*iSwap+1))->GetString().Atoi()));
        }
        BurstFound = true;
      }
      delete l;
    }
    NA62ConditionsService::GetInstance()->Close(fSwapInputFileName);
    if(!BurstFound){
      std::cerr << "[CREAMRawDecoder] WARNING: Burst " << BurstID << " not found for run " << RunID << " in file '"<< fSwapInputFileName << "'!" << std::endl;
    }
    else if(ReadNTriggers){
      std::cout << "[CREAMRawDecoder] Found " << fSwapInput.size() << " swaps for run " << RunID << ", burst " << BurstID << " in file '"<< fSwapInputFileName << "'!" << std::endl;
      for (UInt_t iSwap=0;iSwap<fSwapInput.size();iSwap++){
        std::cout <<"                  --> Swap in Cream " <<setw(2)<<fSwapInput[iSwap].first<<"-"<<setw(2)<<fSwapInput[iSwap].second << std::endl;
      }
    }
    else {
      std::cerr << "[CREAMRawDecoder] WARNING: No calibration triggers found for run " << RunID << ", burst " << BurstID << " in file '"<< fSwapInputFileName << "'!" << std::endl;
    }
  }

  //Open and read the Crocus reference file
  TString Prefix = fReco->GetName();
  if(fReco->GetName()!="LKr") Prefix = "MUVSAV";
  if(fCrocusReferenceFileName == "") fCrocusReferenceFileName = Form("%s-CrocusRef.dat",Prefix.Data()); //default
  TString Line;
  if (NA62ConditionsService::GetInstance()->Open(fCrocusReferenceFileName)==kSuccess) {
    Bool_t FirstLine = true;
    Int_t NCrocusRefFound = 0;
    while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fCrocusReferenceFileName))) {
      if(FirstLine){
        FirstLine = false;
        if (!Line.BeginsWith("CROCUS")) {
          std::cerr << "[CREAMRawDecoder] WARNING: Crocus reference file " << fCrocusReferenceFileName << " has wrong format" << std::endl;
          break;
        }
        else continue;
      }
      if (Line.BeginsWith("#")) continue;
      if (Line.BeginsWith("*")) continue;
      if (Line.Length()<5) continue;
      if (Line.Contains("CROCEND")) break;
      Int_t ipos = 0;
      if (Line.Contains("Cream")) ipos = 1;
      TObjArray *l = Line.Tokenize(" ");
      Int_t Unit = static_cast<TObjString*>(l->At(ipos+1))->GetString().Atoi();
      Int_t iCrate = Unit / 100;
      Int_t iSlot = Unit%100;
      Int_t jx = static_cast<TObjString*>(l->At(ipos+2))->GetString().Atoi();
      Int_t kx = static_cast<TObjString*>(l->At(ipos+3))->GetString().Atoi();
      //printf("Unit %d iCrate %d iSlot %d jx %d kx %d\n",Unit,iCrate,iSlot,jx,kx);
      fNHitsRefSwapWithCrocus[iCrate][iSlot][jx] = static_cast<TObjString*>(l->At(ipos+6))->GetString().Atoi();
      fNHitsRefSwapWithCrocus[iCrate][iSlot][kx] = static_cast<TObjString*>(l->At(ipos+7))->GetString().Atoi();
      NCrocusRefFound++;
      delete l;
    }
    NA62ConditionsService::GetInstance()->Close(fCrocusReferenceFileName);
    std::cout << "[CREAMRawDecoder] Found Reference values for " << NCrocusRefFound << " Channels in file " << fCrocusReferenceFileName << std::endl;
  }
  else  std::cerr << "[CREAMRawDecoder] WARNING: Crocus Reference file " << fCrocusReferenceFileName << " not found " << std::endl;

}

void CREAMRawDecoder::EndOfBurst() { //called for RawData only
  if(!fSwapDetectionEnabled) return;

  UInt_t RunID = NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID();
  UInt_t BurstID = NA62RecoManager::GetInstance()->GetEventHeader()->GetBurstID();

  // ---------------- Crocus additional file ---------------- //
  ofstream CrocusAdditionalOutputFile; //additional output file with some more info
  CrocusAdditionalOutputFile.open(Form("%s-CrocusInfo.run%06d_%04d-run%06d_%04d.dat",fReco->GetName().Data(),RunID,BurstID,RunID,BurstID));
  CrocusAdditionalOutputFile << "Cr Sl   Pairs   A-All  A_Good  A_Swap   B_All Chi2B_Good Chi2B_Swap SWAP TESTED" << std::endl;
  // -------------------------------------------------------- //

  // Detect Swaps with Crocus
  DetectSwapsWithCrocus(CrocusAdditionalOutputFile);

  // Write swap file for Crocus detection
  TString CrocusSuffix = ""; // official swap file for MUV1,2 and SAV
  if(fReco->GetName()=="LKr") CrocusSuffix = ".Crocus";
  ofstream OutputSwapFileCrocus;
  OutputSwapFileCrocus.open(Form("%s-Swaps%s.run%06d_%04d-run%06d_%04d.dat",fReco->GetName().Data(),CrocusSuffix.Data(),RunID,BurstID,RunID,BurstID));
  OutputSwapFileCrocus<<"### " << fReco->GetName() << " Swap file from Crocus for run " << RunID << ", burst " << BurstID << std::endl;
  OutputSwapFileCrocus<<"# Format: RunID  BurstID NTriggersForCrocus NCheckedCreams NSwaps <Crate-Slot>_0 ... <Crate-Slot>_N-1" << std::endl;
  OutputSwapFileCrocus<<"# Swap file Generated on  " << TimeString() << std::endl;
  OutputSwapFileCrocus<<std::endl;
  OutputSwapFileCrocus<<setw(6)<<RunID<<setw(6)<<BurstID<<setw(8)<<fNTriggersForCrocus<<setw(6)<<fNCreamsCheckedWithCrocus<<setw(6)<<fSwapsFoundWithCrocus.size();
  if(fSwapsFoundWithCrocus.size()){
    for(UInt_t iSwap=0;iSwap<fSwapsFoundWithCrocus.size();iSwap++){
      OutputSwapFileCrocus<<setw(5)<<fSwapsFoundWithCrocus[iSwap].first<<"-"<<setw(2)<<fSwapsFoundWithCrocus[iSwap].second;
    }
  }
  OutputSwapFileCrocus<<std::endl;
  OutputSwapFileCrocus.close();

  // Detect Swaps with Calibration Triggers
  if(fReco->GetName()=="LKr") { //no calib triggers for MUV1,2 and SAV
    DetectSwapsWithCalibTriggers();

    // Write swap file for Calib Trigger detection
    ofstream OutputSwapFileCalibTrig;
    OutputSwapFileCalibTrig.open(Form("%s-Swaps.CalibTrig.run%06d_%04d-run%06d_%04d.dat",fReco->GetName().Data(),RunID,BurstID,RunID,BurstID));
    UInt_t NCalibTriggers = static_cast<NA62Reconstruction*>(fReco->GetMainReco())->GetNProcessedCalibrationTriggerEventsInFile();
    OutputSwapFileCalibTrig<<"### "<< fReco->GetName() << " Swap file from CalibTriggers for run " << RunID << ", burst " << BurstID << std::endl;
    OutputSwapFileCalibTrig<<"# Format: RunID  BurstID NCalibTriggers  NCheckedCreams NSwaps <Crate-Slot>_0 ... <Crate-Slot>_N-1" << std::endl;
    OutputSwapFileCalibTrig<<"# Swap file Generated on  " << TimeString() << std::endl;
    OutputSwapFileCalibTrig<<std::endl;
    OutputSwapFileCalibTrig<<setw(6)<<RunID<<setw(6)<<BurstID<<setw(8)<<NCalibTriggers<<setw(6)<<fNCreamsCheckedWithCalibTriggers<<setw(6)<<fSwapsFoundWithCalibTriggers.size();
    if(fSwapsFoundWithCalibTriggers.size()){
      for(UInt_t iSwap=0;iSwap<fSwapsFoundWithCalibTriggers.size();iSwap++){
        OutputSwapFileCalibTrig<<setw(5)<<fSwapsFoundWithCalibTriggers[iSwap].first<<"-"<<setw(2)<<fSwapsFoundWithCalibTriggers[iSwap].second;
      }
    }
    OutputSwapFileCalibTrig<<std::endl;
    OutputSwapFileCalibTrig.close();

    // Copy the outcome of the right method in the official Swap file
    TString SwapMethod = "CalibTrig";
    if(fNCreamsCheckedWithCalibTriggers<fNCreamsCheckedWithCrocus) SwapMethod = "Crocus";
    ifstream  InputSwapFile;
    InputSwapFile.open(Form("%s-Swaps.%s.run%06d_%04d-run%06d_%04d.dat",fReco->GetName().Data(),SwapMethod.Data(),RunID,BurstID,RunID,BurstID));
    ofstream  OutputSwapFile;
    OutputSwapFile.open(Form("%s-Swaps.run%06d_%04d-run%06d_%04d.dat",fReco->GetName().Data(),RunID,BurstID,RunID,BurstID));
    OutputSwapFile << InputSwapFile.rdbuf();
    OutputSwapFile.close();
    InputSwapFile.close();
  }

  // ---------------- Crocus additional file ---------------- //
  Char_t Stex[8]; sprintf(Stex,"SWAN");
  Char_t Serr[8]; sprintf(Serr,"    ");
  if (fSwapsFoundWithCrocus.size()>0)  sprintf(Stex,"SWAB");
  if (fSwapsFoundWithCrocus.size()>20) sprintf(Serr,"Many");
  if (fNCreamsCheckedWithCrocus<431)   sprintf(Serr,"Part");
  if (fNTriggersForCrocus< 50000)      sprintf(Serr,"Low ");
  if (fNTriggersForCrocus>300000)      sprintf(Serr,"High");
  CrocusAdditionalOutputFile << Form("%4s %4s %5d %5d %8d %8d %5d",Stex,Serr,RunID,BurstID,fNTriggersForCrocus,fNCreamsCheckedWithCrocus,(int)fSwapsFoundWithCrocus.size());
  for(UInt_t isw=0;isw<fSwapsFoundWithCrocus.size();isw++) CrocusAdditionalOutputFile << Form(" %2d%2d",fSwapsFoundWithCrocus[isw].first,fSwapsFoundWithCrocus[isw].second);
  CrocusAdditionalOutputFile << std::endl;
  CrocusAdditionalOutputFile.close();
  // -------------------------------------------------------- //

  // Write Octane Jitter file	for LKr
  if(fReco->GetName() == "LKr"){
    ofstream JittFile[3];
    JittFile[kReal].open(Form("LKr-Jitters.run%06d_%04d-run%06d_%04d.dat", RunID,BurstID,RunID,BurstID));
    JittFile[kReal]<<"# Jitter Finder algorithm run "<<RunID<<" burst "<<BurstID<<" created on "<<TimeString()<<endl;
    JittFile[kAnom].open(Form("LKr-Jitters.anomalous.run%06d_%04d-run%06d_%04d.dat", RunID,BurstID,RunID,BurstID));
    JittFile[kAnom]<<"# Anomalous jitters run "<<RunID<<" burst "<<BurstID<<" created on "<<TimeString()<<" (not corrected)"<<endl;
    JittFile[kOCTANE].open(Form("LKr-Jitters.OCTANE.run%06d_%04d-run%06d_%04d.dat", RunID,BurstID,RunID,BurstID));
    JittFile[kOCTANE]<<"# Jitter Finder algorithm for run "<<RunID<<" and burst "<<BurstID<<" created on "<<TimeString()<<endl;
    std::vector<LKrJitter> Jitters[3];
    Int_t NCheckedCREAMs = 0;
    for(Int_t i=0; i<31; i++){
      for(Int_t j=0; j<16;j++){
        LKrJitter tempJitter = InitializeJitter(i,j,true);
        LKrJitter tempJitterOCTANE = InitializeJitter(i,j,false);
        if(tempJitter.NEntries > 50) NCheckedCREAMs++;
        if(CREAMHasJitter(i,j,tempJitterOCTANE.JittTime,false)){
          tempJitterOCTANE.TimeSlotsShift = round(tempJitterOCTANE.JittTime/ClockPeriod);
          Jitters[kOCTANE].push_back(tempJitterOCTANE);
        }
        if(CREAMHasJitter(i,j,tempJitter.JittTime,true)) {
          tempJitter.TimeSlotsShift = round(tempJitter.JittTime/ClockPeriod);
          if(fabs(tempJitter.JittTime-round(tempJitter.JittTime/25)*25) > 3){
            Jitters[kAnom].push_back(tempJitter);
          }
          else{
            Jitters[kReal].push_back(tempJitter);
          }     
        }
      }
    }
    for(Int_t i=0; i<3; i++){
      JittFile[i] <<"# Format: RunID BurstID NCheckedCREAMS NJitters <CrateID-SlotID NEntries MinRatio MaxRatio Amin Amax TShift>_0 ... <CrateID-SlotID ... >_NJitters-1"<<endl;
      JittFile[i] <<Form("%06d %04d %d %d",RunID,BurstID, NCheckedCREAMs, (Int_t) Jitters[i].size());
      for(UInt_t j=0; j<Jitters[i].size();j++){
        JittFile[i]<<" "<<Jitters[i].at(j).Crate <<"-"<<Jitters[i].at(j).Slot;
        JittFile[i]<<" "<<Jitters[i].at(j).NEntries;
        JittFile[i]<<" "<<Jitters[i].at(j).MinRatio <<" "<<Jitters[i].at(j).MaxRatio;
        JittFile[i]<<" "<<Jitters[i].at(j).MinAsym <<" "<<Jitters[i].at(j).MaxAsym;
        JittFile[i]<<" "<<Jitters[i].at(j).JittTime; 
      }
      JittFile[i]<<endl;
      JittFile[i].close();
    }

    // Write FossilChecker file
    ofstream FossilsFile;
    FossilsFile.open(Form("LKr-Fossils.run%06d_%04d-run%06d_%04d.dat", RunID,BurstID,RunID,BurstID));
    FossilsFile<<"#	ZS Fossils Finder algorithm for run "<<RunID<<" and burst "<<BurstID<<" created on "<<TimeString()<<endl;
    FossilsFile<<"# Format: Run Burst NFossils <CellIx CellIy Crate-Slot-Channel Energy>_0 .... <CellIx CellIy Crate-Slot-Channel Energy>_NFossils-1"<<endl;
    FossilsFile<<Form("%06d %04d %d",RunID,BurstID, (int) fLKrZSFossils.size());
    for(UInt_t i=0; i<fLKrZSFossils.size(); i++){
      FossilsFile<<" "<<fLKrZSFossils.at(i).ix<<" "<<fLKrZSFossils.at(i).iy<<" "<<fLKrZSFossils.at(i).Crate<<"-"<<fLKrZSFossils.at(i).Slot<<"-"<<fLKrZSFossils.at(i).Ch<<" "<<fLKrZSFossils.at(i).Energy;
      fHFossilCellPosition->Fill(fLKrZSFossils.at(i).ix,fLKrZSFossils.at(i).iy);
      fHFossilCREAMPosition->Fill(fLKrZSFossils.at(i).Crate, fLKrZSFossils.at(i).Slot);
    }
    FossilsFile<<endl;
    FossilsFile.close();

    // Write NeedleChecker file
    ofstream NeedlesFile;
    NeedlesFile.open(Form("LKr-Needles.run%06d_%04d-run%06d_%04d.dat", RunID,BurstID,RunID,BurstID));
    NeedlesFile<<"#	ZS Fossils Finder algorithm for run "<<RunID<<" and burst "<<BurstID<<" created on "<<TimeString()<<endl;
    NeedlesFile<<"# Format: Run Burst NFossils <CellIx CellIy Crate-Slot-Channel Energy>_0 .... <CellIx CellIy Crate-Slot-Channel Energy>_NFossils-1"<<endl;
    NeedlesFile<<Form("%06d %04d %d",RunID,BurstID, (int) fLKrNeedles.size());
    for(UInt_t i=0; i<fLKrNeedles.size(); i++){
      NeedlesFile<<" "<<fLKrNeedles.at(i).ix<<" "<<fLKrNeedles.at(i).iy<<" "<<fLKrNeedles.at(i).Crate<<"-"<<fLKrNeedles.at(i).Slot<<"-"<<fLKrNeedles.at(i).Ch<<" "<<fLKrNeedles.at(i).Energy;
      fHFossilCellPosition->Fill(fLKrNeedles.at(i).ix,fLKrNeedles.at(i).iy);
      fHFossilCREAMPosition->Fill(fLKrNeedles.at(i).Crate, fLKrNeedles.at(i).Slot);
    }
    NeedlesFile<<endl;
    NeedlesFile.close();
  }
}


TDetectorVEvent * CREAMRawDecoder::DecodeNextEvent(UInt_t * pDataBuffer, EventHeader * pEventHeader, UInt_t * NextOffset){
  Bool_t dbg = 0;
  Bool_t FlagSpecialTrigger=kFALSE;
  FADCEvent *FAdcEvent = static_cast<FADCEvent*>( fDigiEvent);
  //ULong_t FirstWrongSlotTS=0;           //for debug
  //Int_t FirstWrongSlotID=0;             //for debug
  //ULong_t PreviousDatum=0;              //for debug
  TString CurrentFileName = static_cast<NA62Reconstruction*>(fReco->GetMainReco())->GetCurrentFileName(); //for debug
  UInt_t EventFlag = 0;

  //Reset SwapFinder event-by-event variables
  for(UInt_t iPattern=0;iPattern<2;iPattern++) {
    fNEntriesPerPatternSwapWithCalibTriggers[iPattern]=0;
    for(UInt_t iCrate=0;iCrate<fMaxCrateID+1;iCrate++){
      for (UInt_t iSlot=0;iSlot<fMaxSlotID+1;iSlot++){
        fNEntriesSwapWithCalibTriggers[iCrate][iSlot][iPattern] = 0;
      }
    }
  }

  // Event Header
  cout_en(dbg,1) << "Decode CREAM Buffer ---- start -------- " << std::endl;
  cout_en(dbg,1) << "Detector: " << fReco->GetName() << " EventNumber " << pEventHeader->GetEventNumber() << ", EventLength " << std::hex << pEventHeader->GetEventLength() << ", NextOffset = " << NextOffset << ", TimeStamp " << pEventHeader->GetTimeStamp() << std::dec;
  cout_en(dbg,1) << ", BurstID " << pEventHeader->GetBurstID() << ", NumOfDetectors " << pEventHeader->GetNumOfDetectors() << ", FineTime " << pEventHeader->GetFineTime();
  cout_en(dbg,1) << ", TriggerType " << std::hex<< pEventHeader->GetTriggerType() << std::dec << "\n" << std::endl;
  cout_en(dbg,1) << "Decode CREAM Buffer ---- start -------- " << std::endl;

  FAdcEvent->SetTimeStamp(pEventHeader->GetTimeStamp());
  FAdcEvent->SetStartByte(pEventHeader->GetStartByte());
  FAdcEvent->SetIsMC(kFALSE);
  FAdcEvent->SetID(pEventHeader->GetEventNumber());
  FAdcEvent->SetBurstID(pEventHeader->GetBurstID());
  FAdcEvent->SetRunID(pEventHeader->GetRunID());
  FAdcEvent->SetTriggerType(pEventHeader->GetTriggerType());
  if(isL0SpecialFrame(pEventHeader->GetTriggerType()<<2)) {
    FlagSpecialTrigger=kTRUE;
    fSpecialTriggerEvent->SetTimeStamp(pEventHeader->GetTimeStamp());
    fSpecialTriggerEvent->SetStartByte(pEventHeader->GetStartByte());
    fSpecialTriggerEvent->SetIsMC(kFALSE);
    fSpecialTriggerEvent->SetID(pEventHeader->GetEventNumber());
    fSpecialTriggerEvent->SetBurstID(pEventHeader->GetBurstID());
    fSpecialTriggerEvent->SetRunID(pEventHeader->GetRunID());
    fSpecialTriggerEvent->SetTriggerType(pEventHeader->GetTriggerType()<<2);
  }

  //---- Handling old endianity (<=2015 data) ----//
  UInt_t *NewDataBuffer = 0; //used for 2015 only
  if(FAdcEvent->GetRunID()<4300){
    UInt_t NWords = NextOffset-pDataBuffer;
    NewDataBuffer = new UInt_t [NWords];
    UInt_t *NewNextOffset = NewDataBuffer + NWords;
    memcpy(NewDataBuffer,pDataBuffer,NWords*sizeof(UInt_t));
    cout_en(dbg,1) <<"NWords: "<<NWords<<" Next - pData: "<<NextOffset - pDataBuffer<<" DataBuffer: "<<std::hex<<NewDataBuffer<<" NewNext: "<<NewNextOffset<<std::dec<<std::endl;
    UInt_t *PointerTmp = NewDataBuffer;
    while (PointerTmp<NewNextOffset){
      UInt_t swapped = bswap_32(*(PointerTmp));
      *(PointerTmp) = swapped;
      PointerTmp++;
    }
    //overwrite loop pointers
    pDataBuffer = NewDataBuffer;
    NextOffset = NewNextOffset;
  }
  //----------------------------------------------//

  UInt_t Detector = pEventHeader->GetDetectorID();
  cout_en(dbg,1) << "EventID: " << std::hex << FAdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName() << " Detector ID " <<std::hex<< Detector << " Start Byte " << pEventHeader->GetStartByte() << std::endl;
  cout_en(dbg,1) <<"pDataBuffer: " << pDataBuffer << " NextOffset: " << NextOffset <<std::dec<< " NWords: "<< NextOffset - pDataBuffer<<std::endl;

  UInt_t NNonEmptyBlocks=0;
  UInt_t MinNSamples=99999, MaxNSamples=0;
  while(pDataBuffer < NextOffset){ // LOOP ON ALL SUBDETECTOR BLOCKS

    // SDE header
    UInt_t CREAM_EventNb     = *pDataBuffer&M_CREAM_EVENT_NUMBER;
    UInt_t CREAM_BlockLength = CREAMBlockLength(*(pDataBuffer+1)); //CREAMPayLoad + header
    UInt_t CREAM_TS          = *(pDataBuffer+O_CREAM_EVENT_TSTAMP);
    UInt_t CREAM_Crate       = (*(pDataBuffer+O_CREAM_EVENT_CREAMID)&M_CREAM_EVENT_CRATEID)>>S_CREAM_EVENT_CRATEID;
    UInt_t CREAM_Slot        = (*(pDataBuffer+O_CREAM_EVENT_CREAMID)&M_CREAM_EVENT_SLOTID);
    UInt_t CREAM_L0Word      = (*(pDataBuffer+O_CREAM_EVENT_TRIGWORD)&M_CREAM_EVENT_TRIGWORD)>>S_CREAM_EVENT_TRIGWORD;
    Bool_t CREAM_L1_Error    = ((*(pDataBuffer+O_CREAM_EVENT_L1ERROR)&M_CREAM_EVENT_L1ERROR)!=0);

    cout_en(dbg,1) << "EventID: " << std::hex << FAdcEvent->GetID() << " CREAM_L0Word: " << CREAM_L0Word << std::dec << " SubDet: " << fReco->GetName() << " CREAM_Crate: " << CREAM_Crate << " CREAM_Slot: " << CREAM_Slot << std::endl;
    cout_en(dbg,1) << "CREAM_BlockLength: " << CREAM_BlockLength << std::hex << " CREAM_EventNb: " << CREAM_EventNb << " CREAM_TS: " << CREAM_TS << std::dec <<  std::endl;

    Bool_t SwapFound = false;
    for(UInt_t iSwap=0; iSwap<fSwapInput.size();iSwap++){
      if(fSwapInput[iSwap].first == CREAM_Crate && fSwapInput[iSwap].second == CREAM_Slot) SwapFound=true;
    }

    Int_t CREAM_Crate_Remap = -1;
    Int_t CREAM_Slot_Remap  = -1;
    if(CREAM_Crate<=fMaxCrateID) CREAM_Crate_Remap = fCrateRemap[CREAM_Crate];
    if(CREAM_Slot <=fMaxSlotID ) CREAM_Slot_Remap  = fSlotRemap[CREAM_Slot];

    // Special trigger handling
    if(FlagSpecialTrigger) {
      TSpecialTrigger *SpecTrig = static_cast<TSpecialTrigger*>( fSpecialTriggerEvent->AddSpecialTrigger());
      SpecTrig->SetTimeStamp(CREAM_TS); //no latency correction for SpecialTriggers
      SpecTrig->SetTriggerType(pEventHeader->GetTriggerType()<<2);
    }

    if(!CREAM_BlockLength) {
      cerr_en(fWarningsLevel,WARN_DET) << "[CREAMRawDecoder]    WARNING: NextOffset is 0! Forcing the termination of an infinite loop! [File: " << CurrentFileName << " Event: " << FAdcEvent->GetID() << " SubDet: " << fReco->GetName() << " Crate: " << CREAM_Crate << " Slot: " << CREAM_Slot << "]" << std::endl;
      TDigiVError *Error = static_cast<TDigiVError*>(FAdcEvent->AddError(CREAM_BLOCKPAYLOAD_FATAL));
      Error->SetROBoardID(CREAM_Crate_Remap);
      Error->SetFatal(kTRUE);
      if(fNROMezzanines) {
        fNCriticalErrors[0]++; //skip corrupted event
        fHNCriticalErrorsCrateSlot->Fill(CREAM_Crate,CREAM_Slot);
      }
      break;
    }
    else if(CREAM_BlockLength<=7) { //empty SDE data block
      cerr_en(fWarningsLevel,WARN_MAX) << "[CREAMRawDecoder]    WARNING: Empty data block! [File: " << CurrentFileName << " Event: " << FAdcEvent->GetID() << " SubDet: " << fReco->GetName() << " Crate: " << CREAM_Crate << " Slot: " << CREAM_Slot << "]" << std::endl;
      pDataBuffer += CREAM_BlockLength; 
      continue;
    }

    // SDE data block
    UInt_t CREAM_PayLoad  = (*(pDataBuffer+O_CREAM_EVENT_PLOAD_LEN)&M_CREAM_EVENT_PLOAD_LEN);
    UInt_t CREAM_NSamples = (*(pDataBuffer+O_CREAM_EVENT_NSAMPLES)&M_CREAM_EVENT_NSAMPLES)>>S_CREAM_EVENT_NSAMPLES;
    Int_t CREAM_ZSFlag    = (*(pDataBuffer+O_CREAM_EVENT_FLAGS)&M_CREAM_EVENT_FLAG_ZS)!=0;
    Int_t CREAM_L0RQFlag  = (*(pDataBuffer+O_CREAM_EVENT_FLAGS)&M_CREAM_EVENT_FLAG_L0)!=0;
    UInt_t CREAM_ChanMask = *(pDataBuffer+O_CREAM_EVENT_CH_MASK);
    UInt_t *CREAM_DataPnt = pDataBuffer+O_CREAM_EVENT_SAMPLES;

    cout_en(dbg,1) << "CREAM_PayLoad: " << CREAM_PayLoad << " CREAM_NSamples: " << CREAM_NSamples << std::hex << " CREAM_ChanMask: " << CREAM_ChanMask << std::dec << std::endl;

    Int_t NEnabledChannels = 0;
    for (UInt_t iCh=0; iCh<32; iCh++) {
      if (CREAM_ChanMask&(1<<iCh)) NEnabledChannels++;
    }
    UInt_t PayLoadFromChannelMask = NEnabledChannels*CREAM_NSamples/2+3;
    if(FlagSpecialTrigger && CREAM_L0Word!=0x23) PayLoadFromChannelMask = 4; //SOB, choke/error on/off, synchronization 
    if(CREAM_L0Word==0x23) PayLoadFromChannelMask = 8; //EOB
    if(CREAM_BlockLength!=CREAM_PayLoad+4 || CREAM_BlockLength!=PayLoadFromChannelMask+4) {
      if((CREAM_BlockLength == PayLoadFromChannelMask+4 && CREAM_BlockLength<=NextOffset-pDataBuffer) ||
          (CREAM_PayLoad == PayLoadFromChannelMask && CREAM_PayLoad+4<=NextOffset-pDataBuffer)){ // Mismatch is recoverable
        cerr_en(fWarningsLevel,WARN_DET) << "[CREAMRawDecoder]    WARNING: Cream BlockLength/PayLoad mismatch!         [File: " << CurrentFileName << " EventID: " << std::hex << FAdcEvent->GetID() << std::dec;
        cerr_en(fWarningsLevel,WARN_DET) << " SubDet: " << fReco->GetName() << " Crate: " << CREAM_Crate << " Slot: " << CREAM_Slot << " CREAM_L0Word: " <<std::hex<< CREAM_L0Word << std::dec;
        cerr_en(fWarningsLevel,WARN_DET) << " CREAM_BlockLength: " << CREAM_BlockLength << " CREAM_PayLoad: " << CREAM_PayLoad << " PayLoadFromChannelMask: " << PayLoadFromChannelMask << "]" << std::endl;
        UInt_t ErrorCode = CREAM_BLOCKPAYLOAD_MISMATCH;
        if(CREAM_PayLoad == PayLoadFromChannelMask) CREAM_BlockLength = CREAM_PayLoad+4; // wrong CREAM_BlockLength
        else if(CREAM_BlockLength == PayLoadFromChannelMask+4) { // wrong CREAM_PayLoad
          CREAM_PayLoad = PayLoadFromChannelMask;
          ErrorCode = CREAM_PAYLOADCHS_MISMATCH;
        }
        if(CREAM_Crate_Remap>=0 && CREAM_Slot_Remap>=0 && fNROMezzaninesPerFullBoard*CREAM_Crate_Remap+CREAM_Slot_Remap<fNROMezzanines) {
          fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*CREAM_Crate_Remap+CREAM_Slot_Remap, fCREAMDecoderErrString[ErrorCode],1);
          TDigiVError *Error = static_cast<TDigiVError*>(FAdcEvent->AddError(ErrorCode));
          Error->SetROBoardID(CREAM_Crate_Remap);
        }
      }
      else { // All the 3 values are different or unphysical: irrecoverable
        cerr_en(fWarningsLevel,WARN_DET) << "[CREAMRawDecoder]    WARNING: Irrecoverable cream BlockLength/PayLoad mismatch!         [File: " << CurrentFileName << " EventID: " << std::hex << FAdcEvent->GetID() << std::dec;
        cerr_en(fWarningsLevel,WARN_DET) << " SubDet: " << fReco->GetName() << " Crate: " << CREAM_Crate << " Slot: " << CREAM_Slot << " CREAM_L0Word: " <<std::hex<< CREAM_L0Word << std::dec;
        cerr_en(fWarningsLevel,WARN_DET) << " CREAM_BlockLength: " << CREAM_BlockLength << " CREAM_PayLoad: " << CREAM_PayLoad << " PayLoadFromChannelMask: " << PayLoadFromChannelMask << "]" << std::endl;
        if(CREAM_Crate_Remap>=0 && CREAM_Slot_Remap>=0 && fNROMezzaninesPerFullBoard*CREAM_Crate_Remap+CREAM_Slot_Remap<fNROMezzanines) {
          fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*CREAM_Crate_Remap+CREAM_Slot_Remap, fCREAMDecoderErrString[CREAM_BLOCKPAYLOAD_FATAL],1);
          fNCriticalErrors[fNROMezzaninesPerFullBoard*CREAM_Crate_Remap+CREAM_Slot_Remap]++;
        }
        else if(fNROMezzanines) fNCriticalErrors[0]++;
        fHNCriticalErrorsCrateSlot->Fill(CREAM_Crate,CREAM_Slot);
        TDigiVError *Error = static_cast<TDigiVError*>(FAdcEvent->AddError(CREAM_BLOCKPAYLOAD_FATAL));
        Error->SetROBoardID(CREAM_Crate_Remap);
        Error->SetFatal(kTRUE);
        if (FlagSpecialTrigger) return fSpecialTriggerEvent;
        return FAdcEvent;
      }
    }

    if(CREAM_Crate_Remap<0 || CREAM_Slot_Remap<0 || !(fROMezzanineMasksPerBoard[CREAM_Crate_Remap]&(1<<CREAM_Slot_Remap))) { //Skip block
      if(!fIsAGuestOrHostDetector && (fReco->GetName().Contains("LKr") || FAdcEvent->GetRunID()>4300)) {
        // Ignore guest/host detectors [the check is not suitable for them]
        cerr_en(fWarningsLevel,WARN_DET) << "[CREAMRawDecoder]    WARNING: Current Slot is masked!         [File: " << CurrentFileName << " EventID: " << std::hex << FAdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName() << " Crate: " << CREAM_Crate << " Slot: " << CREAM_Slot << "]" << std::endl;
        fHDecoderErrors->Fill(0.,fCREAMDecoderErrString[CREAM_MASKED_CH],1); //mezzanine ID not valid!
        //TDigiVError *Error = static_cast<TDigiVError*>(FAdcEvent->AddError(CREAM_MASKED_CH));
        //Error->SetROBoardID(0.); //mezzanine ID not valid!
        fNHitsFromMaskedChannels[0]++; //mezzanine ID not valid!
      }
      pDataBuffer += CREAM_BlockLength; 
      cout_en(dbg,1) << "Skipping block! SubDet: " << fReco->GetName() << " CREAM_Crate: " << CREAM_Crate << " CREAM_Slot: " << CREAM_Slot << std::endl;
      continue;
    }

    if(fNROMezzaninesPerFullBoard*CREAM_Crate_Remap+CREAM_Slot_Remap>=fNROMezzanines) {
      cerr_en(fWarningsLevel,WARN_DET) << "[CREAMRawDecoder]    WARNING: Wrong number of NROMezzanines   [File: " << CurrentFileName << " EventID: " << std::hex << FAdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName() << " Crate: " << CREAM_Crate << " Slot: " << CREAM_Slot << " MezzanineID: " << fNROMezzaninesPerFullBoard*CREAM_Crate_Remap+CREAM_Slot_Remap << " fNROMezzanines: " << fNROMezzanines << std::endl;
    }

    CREAM_TS-=fLatency; //latency correction
    if (MinNSamples>CREAM_NSamples ) MinNSamples=CREAM_NSamples;
    if (MaxNSamples<CREAM_NSamples ) MaxNSamples=CREAM_NSamples;
    NNonEmptyBlocks++;

    FAdcEvent->SetFADCID(10 * CREAM_ZSFlag); //Set Zero Suppression State

    if (CREAM_L1_Error){ 
      cerr_en(fWarningsLevel,WARN_DET) << "[CREAMRawDecoder]    WARNING: L1 Error!                       [File: " << CurrentFileName << " EventID: " << std::hex << FAdcEvent->GetID() << std::dec;
      cerr_en(fWarningsLevel,WARN_DET) << " SubDet: " << fReco->GetName() << " Crate: " << CREAM_Crate << " Slot: " << CREAM_Slot << "]" << std::endl;
      fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*CREAM_Crate_Remap+CREAM_Slot_Remap, fCREAMDecoderErrString[CREAM_L1_ERROR],1);
      TDigiVError *Error = static_cast<TDigiVError*>(FAdcEvent->AddError(CREAM_L1_ERROR));
      Error->SetROBoardID(CREAM_Crate_Remap);
      Error->SetFatal(kTRUE);
      fNCriticalErrors[fNROMezzaninesPerFullBoard*CREAM_Crate_Remap+CREAM_Slot_Remap]++;
      fHNCriticalErrorsCrateSlot->Fill(CREAM_Crate,CREAM_Slot);
    }
    if (CREAM_EventNb != (UInt_t)FAdcEvent->GetID()){
      cerr_en(fWarningsLevel,WARN_DET) << "[CREAMRawDecoder]    WARNING: EventNumber mismatch!           [File: " << CurrentFileName << " EventID: " << std::hex << FAdcEvent->GetID() << std::dec;
      cerr_en(fWarningsLevel,WARN_DET) << " SubDet: " << fReco->GetName() << " Crate: " << CREAM_Crate << " Slot: " << CREAM_Slot << std::hex << " BlockEvtNb: " << CREAM_EventNb << " EventNb: " << FAdcEvent->GetID() << std::dec << "]" << std::endl;
      fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*CREAM_Crate_Remap+CREAM_Slot_Remap, fCREAMDecoderErrString[CREAM_EVENTNUMBER_MISMATCH],1);
      TDigiVError *Error = static_cast<TDigiVError*>(FAdcEvent->AddError(CREAM_EVENTNUMBER_MISMATCH));
      Error->SetROBoardID(CREAM_Crate_Remap);
      Error->SetFatal(kTRUE);
      fNCriticalErrors[fNROMezzaninesPerFullBoard*CREAM_Crate_Remap+CREAM_Slot_Remap]++;
      fHNCriticalErrorsCrateSlot->Fill(CREAM_Crate,CREAM_Slot);
    }
    if (CREAM_L0Word != (FAdcEvent->GetTriggerType()&0xff)){
      cerr_en(fWarningsLevel,WARN_DET) << "[CREAMRawDecoder]    WARNING: TriggerWord mismatch!           [File: " << CurrentFileName << " EventID: " << std::hex << FAdcEvent->GetID() << std::dec;
      cerr_en(fWarningsLevel,WARN_DET) << " SubDet: " << fReco->GetName() << " Crate: " << CREAM_Crate << " Slot: " << CREAM_Slot << std::hex << " CREAML0Word: " << CREAM_L0Word << " L0TriggerType: " << (FAdcEvent->GetTriggerType()&0xff) << std::dec << "]" << std::endl;
      fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*CREAM_Crate_Remap+CREAM_Slot_Remap, fCREAMDecoderErrString[CREAM_TRIGWORD_MISMATCH],1);
      TDigiVError *Error = static_cast<TDigiVError*>(FAdcEvent->AddError(CREAM_TRIGWORD_MISMATCH));
      Error->SetROBoardID(CREAM_Crate_Remap);
      Error->SetFatal(kTRUE);
      fNCriticalErrors[fNROMezzaninesPerFullBoard*CREAM_Crate_Remap+CREAM_Slot_Remap]++;
      fHNCriticalErrorsCrateSlot->Fill(CREAM_Crate,CREAM_Slot);
    }

    // Data block
    if (CREAM_L0Word<0x20 || CREAM_L0Word>0x28) {

      // Checks for data block only
      if (CREAM_L0RQFlag){ 
        cerr_en(fWarningsLevel,WARN_DET) << "[CREAMRawDecoder]    WARNING: Wrong L0RQ bit!                 [File: " << CurrentFileName << " EventID: " << std::hex << FAdcEvent->GetID() << std::dec;
        cerr_en(fWarningsLevel,WARN_DET) << " SubDet: " << fReco->GetName() << " Crate: " << CREAM_Crate << " Slot: " << CREAM_Slot << "]" << std::endl;
        fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*CREAM_Crate_Remap+CREAM_Slot_Remap, fCREAMDecoderErrString[CREAM_WRONG_L0RQ],1);
        TDigiVError *Error = static_cast<TDigiVError*>(FAdcEvent->AddError(CREAM_WRONG_L0RQ));
        Error->SetROBoardID(CREAM_Crate_Remap);
      }
      if (CREAM_TS != pEventHeader->GetTimeStamp()){
        cerr_en(fWarningsLevel,WARN_DET) << "[CREAMRawDecoder]    WARNING: BlockTS/TriggerTS mismatch!     [File: " << CurrentFileName << " EventID: " << std::hex << FAdcEvent->GetID() << std::dec;
        cerr_en(fWarningsLevel,WARN_DET) << " SubDet: " << fReco->GetName() << " Crate: " << CREAM_Crate << " Slot: " << CREAM_Slot << " BlockTS-TriggerTS: " << CREAM_TS-(Long_t)pEventHeader->GetTimeStamp() << "]" << std::endl;
        fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*CREAM_Crate_Remap+CREAM_Slot_Remap, fCREAMDecoderErrString[CREAM_BLOCKTS_MISMATCH],1);
        TDigiVError *Error = static_cast<TDigiVError*>(FAdcEvent->AddError(CREAM_BLOCKTS_MISMATCH));
        Error->SetROBoardID(CREAM_Crate_Remap);
        Error->SetFatal(kTRUE);
        fNCriticalErrors[fNROMezzaninesPerFullBoard*CREAM_Crate_Remap+CREAM_Slot_Remap]++;
        fHNCriticalErrorsCrateSlot->Fill(CREAM_Crate,CREAM_Slot);
      }

      for (UInt_t iCh=0; iCh<32; iCh++) {
        Int_t iChCorrected = iCh;
        // Swapping the channels
        if(SwapFound) {
          if(iCh<16) iChCorrected=iCh+16;
          else iChCorrected=iCh-16;
        }
        if (CREAM_ChanMask & (1<<iCh)) {
          Int_t nROChannel = 512*CREAM_Crate_Remap+32*CREAM_Slot_Remap+iChCorrected;
          Int_t ChannelID = GetChannelRemap(nROChannel);
          if(fReco->GetName()=="LKr" && ChannelID>=0 && LKrParameters::GetInstance()->IsDeadCell(ChannelID/1000,ChannelID%1000)) {
            ChannelID = -1; //mask dead channels
          }
          if(ChannelID<0){ //skip channel block
            cerr_en(fWarningsLevel,WARN_MAX) << "[CREAMRawDecoder]    WARNING: Hit from masked channel!        [File: " << CurrentFileName << " EventID: " << std::hex << FAdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName() << " Crate: " << CREAM_Crate << " Slot: " << CREAM_Slot;
            cerr_en(fWarningsLevel,WARN_MAX) << " Channel: " << nROChannel << "]" << std::endl;
            fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*CREAM_Crate_Remap+CREAM_Slot_Remap,fCREAMDecoderErrString[CREAM_MASKED_CH],1);
            //TDigiVError *Error = static_cast<TDigiVError*>(FAdcEvent->AddError(CREAM_MASKED_CH));
            //Error->SetROBoardID(CREAM_Crate_Remap);
            //fNHitsFromMaskedChannels[fNROMezzaninesPerFullBoard*CREAM_Crate_Remap+CREAM_Slot_Remap]++; //disabled
            CREAM_DataPnt+=(CREAM_NSamples/2);
            continue;
          }
          //Read channel samples
          FADCVHit * Digi = static_cast<FADCVHit *>(FAdcEvent->AddHit());
          Digi->SetChannelID(ChannelID);
          Digi->DecodeChannelID();
          Double_t MaxCount = 0;
          Double_t MinCount = 16385;
          UInt_t iMaxCount = 0;
          Double_t OddSamples = 0., EvenSamples = 0.;
          for (UInt_t iSample=0; iSample<CREAM_NSamples; iSample++) {
            Double_t Sample = (*CREAM_DataPnt&0x0000ffff);
            if(iSample%2) Sample = (*CREAM_DataPnt&0xffff0000)>>16;
            cout_en(dbg,1) << "Channel: " << ChannelID << " iSample: " << iSample << std::hex << " Sample: " << Sample << " CREAM_DataPnt: " <<  CREAM_DataPnt << std::dec << std::endl;
            if (Sample>MaxCount) {
              MaxCount = Sample;
              iMaxCount = iSample;
            }
            if (Sample<MinCount) MinCount = Sample;
            Digi->AddSample(Sample);
            CREAM_DataPnt+=(iSample%2);
            if(iSample%2) OddSamples +=Sample;
            else          EvenSamples+=Sample;
          } // loop over samples

          //Swap Finder
          if(CREAM_L0Word==0x30) FillSwapsWithCalibTriggersInfo(CREAM_Crate,CREAM_Slot,iCh,MinCount,MaxCount);
          else if (CREAM_L0Word!=0x31 && CREAM_L0Word!=0x32) { // exclude calib trigger and periodics
            FillSwapsWithCrocusInfo(CREAM_Crate,CREAM_Slot,iCh,MinCount,MaxCount);
          }

          if(!OddSamples || !EvenSamples) {
            cerr_en(fWarningsLevel,WARN_DET) << "[CREAMRawDecoder]    WARNING: Bad samples found!     [File: " << CurrentFileName << " EventID: " << std::hex << FAdcEvent->GetID() << std::dec;
            cerr_en(fWarningsLevel,WARN_DET) << " SubDet: " << fReco->GetName() << " Crate: " << CREAM_Crate << " Slot: " << CREAM_Slot << " OddSamples: " << OddSamples << " EvenSamples: " <<  EvenSamples << "]" << std::endl;
            fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*CREAM_Crate_Remap+CREAM_Slot_Remap,fCREAMDecoderErrString[CREAM_BAD_SAMPLES],1);
            TDigiVError *Error = static_cast<TDigiVError*>(FAdcEvent->AddError(CREAM_BAD_SAMPLES));
            Error->SetROBoardID(CREAM_Crate_Remap);
            Error->SetFatal(kTRUE);
            fNCriticalErrors[fNROMezzaninesPerFullBoard*CREAM_Crate_Remap+CREAM_Slot_Remap]++;
            fHNCriticalErrorsCrateSlot->Fill(CREAM_Crate,CREAM_Slot);
          }
          else {
            Double_t OddEvenAsymmetry = (OddSamples-EvenSamples)/((Double_t)TMath::Min(OddSamples,EvenSamples));
            Double_t MinBitDistance = 16384;
            for(UInt_t iBit=8;iBit<14;iBit++){ //from 256 to 16384
              if(fabs(fabs(OddSamples-EvenSamples)-(1<<iBit))<MinBitDistance) MinBitDistance = fabs(fabs(OddSamples-EvenSamples)-(1<<iBit));
            }
            if(fabs(OddEvenAsymmetry)>0.1 && MinBitDistance<=8) {
              cerr_en(fWarningsLevel,WARN_MAX) << "[CREAMRawDecoder]    WARNING: Possible oscillating cell found! [File: " << CurrentFileName << " EventID: " << std::hex << FAdcEvent->GetID() << std::dec;
              cerr_en(fWarningsLevel,WARN_MAX) << " SubDet: " << fReco->GetName() << " Crate: " << CREAM_Crate << " Slot: " << CREAM_Slot << " ChID: " << ChannelID << " OddSamples: " << OddSamples << " EvenSamples: " <<  EvenSamples << " Asymmetry: " << OddEvenAsymmetry << "]" << std::endl;
              fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*CREAM_Crate_Remap+CREAM_Slot_Remap,fCREAMDecoderErrString[CREAM_OSCILLATING_CH],1);
              fHNQualityWarningsVsROChannel->Fill(nROChannel);
              if(fHNQualityWarningsXY) fHNQualityWarningsXY->Fill(static_cast<TLKrDigi*>(Digi)->GetXCellID(),static_cast<TLKrDigi*>(Digi)->GetYCellID());
              if(fHNQualityWarningsCrateSlot) fHNQualityWarningsCrateSlot->Fill(CREAM_Crate,CREAM_Slot);
              fNQualityWarnings[fNROMezzaninesPerFullBoard*CREAM_Crate_Remap+CREAM_Slot_Remap]++;
              //TDigiVError *Error = static_cast<TDigiVError*>(FAdcEvent->AddError(CREAM_OSCILLATING_CH));
              //Error->SetROBoardID(CREAM_Crate_Remap);
              //fNCriticalErrors[fNROMezzaninesPerFullBoard*CREAM_Crate_Remap+CREAM_Slot_Remap]++;
              //fHNCriticalErrorsCrateSlot->Fill(CREAM_Crate,CREAM_Slot);
            }
          }
          cout_en(dbg,1) << "Channel: " << ChannelID << " MinCount:  " << MinCount << " MaxCount:  " << MaxCount << " iMaxCount: " << iMaxCount << std::endl;

          UInt_t Flag = 0;
          if (MaxCount>=16383) Flag |= (1<<kCREAMSaturationBit); // saturations
          if (MinCount<=0)     Flag |= (1<<kCREAMUnderflowBit);  // underflow
          if (CREAM_L1_Error)  Flag |= (1<<kCREAML1ErrorBit);    // L1 errors
          EventFlag |= Flag;
          if (MaxCount-MinCount>30 && iMaxCount>0 && iMaxCount<CREAM_NSamples-1){
            Double_t *Samples = Digi->GetAllSamples();
            Double_t imax= iMaxCount;
            Double_t x[3] ={ imax-1,imax,imax+1 };
            Double_t y[3] ={ Samples[iMaxCount-1],Samples[iMaxCount],Samples[iMaxCount+1] };

            if (y[0]<y[1] || y[2]<y[1]){
              // pol2 fit central 3 points.
              Double_t p2 = (y[2]-y[0])/((x[2]-x[0])*(x[2]-x[1])) - (y[1]-y[0])/((x[1]-x[0])*(x[2]-x[1]));
              Double_t p1 = (y[1]-y[0])/(x[1]-x[0]) -p2*(x[1]+x[0]);
              Double_t p0 = y[0] -p2*x[0]*x[0] -p1*x[0];                                                   
              Double_t TimeSlot= -p1/(2*p2);
              Double_t Time = TimeSlot*ClockPeriod;
              Double_t PeakAmp = p2*TimeSlot*TimeSlot + p1*TimeSlot + p0;
              Digi->SetPeakTime(Time);
              Digi->SetADCPeakTime(TimeSlot);
              Digi->SetADCPeakEnergy(PeakAmp);
            }
            else{
              Digi->SetPeakTime(iMaxCount*ClockPeriod);
              Digi->SetADCPeakTime(iMaxCount);
              Digi->SetADCPeakEnergy(MaxCount);
            }
          }
          else{
            Digi->SetPeakTime(1.e+20);
            Digi->SetADCPeakTime((Double_t)iMaxCount); 
            Digi->SetADCPeakEnergy((Double_t)MaxCount);
          }
          Digi->SetPeakEnergy(0.);
          Digi->SetQuality(1); //for LKr Digi-filter
          Digi->SetFlags(Flag);
          if (Digi->GetNSamples()>=3){
            // Pedestal and dead cells monitor
            Double_t *Samples = Digi->GetAllSamples();
            Double_t Pedestal = (Double_t)(Samples[0]+Samples[1])/2.;
            fHPedestals->Fill(fNROMezzaninesPerFullBoard*CREAM_Crate_Remap+CREAM_Slot_Remap,Pedestal);
            Double_t PedMaxSample = Samples[0];
            if(Samples[1]>PedMaxSample) PedMaxSample = Samples[1];
            if(Samples[2]>PedMaxSample) PedMaxSample = Samples[2];
            Double_t PedMinSample = Samples[0];
            if(Samples[1]<PedMinSample) PedMinSample = Samples[1];
            if(Samples[2]<PedMinSample) PedMinSample = Samples[2];
            if(PedMaxSample-PedMinSample<10.) fHPedestalVsROChannel->Fill(nROChannel,Pedestal);
            if(fReco->GetName()=="LKr"){
              Int_t ix = static_cast<TLKrDigi*>(Digi)->GetXCellID();
              Int_t iy = static_cast<TLKrDigi*>(Digi)->GetYCellID();
              if(PedMaxSample-PedMinSample<10.) {
                Double_t PedRef = LKrParameters::GetInstance()->GetPedRef(ix,iy);
                Double_t MaxDeltaRef = LKrParameters::GetInstance()->GetPedSigma(ix,iy);
                //fHPedestalWrtRefVsROChannel->Fill(nROChannel,Pedestal-PedRef);
                if(fabs(Pedestal-PedRef)>=MaxDeltaRef) fHPedestalBadRefXYNum->Fill(ix,iy);
                fHPedestalBadRefXYDen->Fill(ix,iy);
              }
              Int_t xBin = ix+1;
              Int_t yBin = iy+1;
              Double_t HitEnergy = 1000.*LKrParameters::GetInstance()->GetCalSteig(ix,iy,0)*(Digi->GetADCPeakEnergy()-Pedestal); //rough (MeV)
              if (CREAM_L0Word!=0x30) {
                fHZSCounterXY->Fill(ix,iy);
                fHPedestalXY->SetBinContent(xBin,yBin,(Pedestal+(fHPedestalXY->GetBinContent(xBin,yBin)*(fHZSCounterXY->GetBinContent(xBin,yBin)-1)))/fHZSCounterXY->GetBinContent(xBin,yBin));
                fHHitEnergy->Fill(HitEnergy); //MeV
                fHHitMapEnergyAboveZS->Fill(ix,iy);    
                if(HitEnergy>fHitEnergyThr) fHHitMapEnergyAboveThr->Fill(ix,iy);    
              }
              else { //calibration triggers
                fHZSCounterXYCalib->Fill(ix,iy);
                fHPedestalXYCalib->SetBinContent(xBin,yBin,(Pedestal+(fHPedestalXY->GetBinContent(xBin,yBin)*(fHZSCounterXYCalib->GetBinContent(xBin,yBin)-1)))/fHZSCounterXYCalib->GetBinContent(xBin,yBin));
                fHMaxSampleXYCalib->SetBinContent(xBin,yBin,(Digi->GetADCPeakEnergy()+(fHMaxSampleXYCalib->GetBinContent(xBin,yBin)*(fHZSCounterXYCalib->GetBinContent(xBin,yBin)-1)))/fHZSCounterXYCalib->GetBinContent(xBin,yBin));
                fHHitEnergyCalib->Fill(HitEnergy); //MeV
                Int_t CrateBin = 4*(CREAM_Crate+0.5*1.75-(iChCorrected/8)*0.25)+1;
                Int_t SlotBin  = 8*(CREAM_Slot+0.5*1.875-(iChCorrected%8)*0.125)+1;
                fHMaxSampleCrateSlotCalib->SetBinContent(CrateBin,SlotBin,(Digi->GetADCPeakEnergy()+(fHMaxSampleCrateSlotCalib->GetBinContent(CrateBin,SlotBin)*(fHZSCounterXYCalib->GetBinContent(xBin,yBin)-1)))/fHZSCounterXYCalib->GetBinContent(xBin,yBin));
              }
            }
          }
        }
      }
    }
    else { // Special trigger treatment
      // Add further info to the special trigger
      //SpecTrig->SetSomething();
    }
    // Checksum
    UInt_t SumOfAllWordsInBlock[32] = {0};
    for(UInt_t iWord=0; iWord<CREAM_BlockLength-1;iWord++){ // Check sum word not included!
      std::bitset<32> WordBits(*pDataBuffer);
      for(UInt_t iBit=0;iBit<32;iBit++) {
        SumOfAllWordsInBlock[iBit]+=WordBits.test(iBit);
      }
      pDataBuffer++; 
    }
    UInt_t EvaluatedCheckSum = 0;
    for(UInt_t iBit=0;iBit<32;iBit++) EvaluatedCheckSum+=((SumOfAllWordsInBlock[iBit]%2)<<iBit); //LRC
    UInt_t CREAM_CheckSum = *(pDataBuffer);
    pDataBuffer++;
    cout_en(dbg,1) << std::hex << "CREAM_CheckSum: " << CREAM_CheckSum << " EvaluatedCheckSum: " << EvaluatedCheckSum << std::dec << std::endl;
    if (CREAM_CheckSum != EvaluatedCheckSum){
      cerr_en(fWarningsLevel,WARN_DET) << "[CREAMRawDecoder]    WARNING: Wrong CheckSum!                 [File: " << CurrentFileName << " EventID: " << std::hex << FAdcEvent->GetID() << std::dec;
      cerr_en(fWarningsLevel,WARN_DET) << " SubDet: " << fReco->GetName() << " Crate: " << CREAM_Crate << " Slot: " << CREAM_Slot << std::hex << " CREAM_CheckSum: " << CREAM_CheckSum << " EvaluatedCheckSum: " << EvaluatedCheckSum << std::dec << "]" << std::endl;
      fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*CREAM_Crate_Remap+CREAM_Slot_Remap, fCREAMDecoderErrString[CREAM_WRONG_CHECKSUM],1);
      TDigiVError *Error = static_cast<TDigiVError*>(FAdcEvent->AddError(CREAM_WRONG_CHECKSUM));
      Error->SetROBoardID(CREAM_Crate_Remap);
      Error->SetFatal(kTRUE);
      fNCriticalErrors[fNROMezzaninesPerFullBoard*CREAM_Crate_Remap+CREAM_Slot_Remap]++;
      fHNCriticalErrorsCrateSlot->Fill(CREAM_Crate,CREAM_Slot);
    }
  }
  //End of while loop

  if (NNonEmptyBlocks && MinNSamples != MaxNSamples) {
    cerr_en(fWarningsLevel,WARN_DET) << "[CREAMRawDecoder]    WARNING: Mismatch in number of samples! [File: " << CurrentFileName << " EventID: " << std::hex << FAdcEvent->GetID() << std::dec;
    cerr_en(fWarningsLevel,WARN_DET) << " SubDet: " << fReco->GetName() << " MinNSamples: " << MinNSamples << " MaxNSamples: " << MaxNSamples << "]" << std::endl;
    for(Int_t iMezzanine=0;iMezzanine<fNROMezzanines;iMezzanine++){
      fHDecoderErrors->Fill(iMezzanine, fCREAMDecoderErrString[CREAM_NSAMPLES_MISMATCH],1);
      TDigiVError *Error = static_cast<TDigiVError*>(FAdcEvent->AddError(CREAM_NSAMPLES_MISMATCH));
      Error->SetROBoardID(iMezzanine/fNROMezzaninesPerFullBoard);
      Error->SetFatal(kTRUE);
      fNCriticalErrors[iMezzanine]++;
    }
  }
  FAdcEvent->SetNSamples(MinNSamples);
  FAdcEvent->SetEventFlag(EventFlag);

  // Swap finder event-by-event verdict
  if ((pEventHeader->GetTriggerType()&0xff)==0x30){
    Int_t MajorityPattern = (fNEntriesPerPatternSwapWithCalibTriggers[0]>=fNEntriesPerPatternSwapWithCalibTriggers[1])? 0 : 1;
    Int_t MinorityPattern = (fNEntriesPerPatternSwapWithCalibTriggers[0]>=fNEntriesPerPatternSwapWithCalibTriggers[1])? 1 : 0;
    for(UInt_t iCrate=0;iCrate<fMaxCrateID+1;iCrate++){
      for (UInt_t iSlot=0;iSlot<fMaxSlotID+1;iSlot++){
        if(fNEntriesSwapWithCalibTriggers[iCrate][iSlot][MajorityPattern]<fNEntriesSwapWithCalibTriggers[iCrate][iSlot][MinorityPattern]) {
          fNDetectedSwapsWithCalibTriggers[iCrate][iSlot]++;
        }
      }
    }
  }
  if ((pEventHeader->GetTriggerType()&0xff)<0x20 || (pEventHeader->GetTriggerType()&0xff)>0x32) fNTriggersForCrocus++;

  //Jitter analysis
  if(fReco->GetName() == "LKr" && (pEventHeader->GetTriggerType()&0xff)<0x20)  JitterAnalysis();
  if(fReco->GetName() == "LKr" && (pEventHeader->GetTriggerType()&0xff)==0x31)  FossilChecker(0); // <-- ZS periodics
  if(fReco->GetName() == "LKr" && (pEventHeader->GetTriggerType()&0xff)<0x20)  FossilChecker(1); // <--- Needles search

  if(NewDataBuffer) delete [] NewDataBuffer;

  if (FlagSpecialTrigger) return fSpecialTriggerEvent;
  return FAdcEvent;
}


void CREAMRawDecoder::ParseRawDecoderSettingsFile(TString RawDecFileName){

  TString Line;
  if(NA62ConditionsService::GetInstance()->Open(RawDecFileName)!=kSuccess) return;
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(RawDecFileName))) {
    if (Line.BeginsWith("Latency")){
      Line.Remove(0,6);
      fLatency = strtol(TString(Line(TRegexp("[0-9a-fA-F]+"))),NULL,16);
      continue;
    }
    else if(Line.BeginsWith("CrRemap_")){
      for(UInt_t iCrate=0;iCrate<(fMaxCrateID+1)/16;iCrate++){
        if(Line.BeginsWith(Form("CrRemap_%04d=",iCrate))){
          TObjArray * l = Line.Tokenize(" ");
          for (UInt_t jCrate=0;jCrate<16;jCrate++){
            fCrateRemap[16*iCrate+jCrate] = static_cast<TObjString*>(l->At(jCrate+1))->GetString().Atoi();
          }
          delete l;
        }
      }
    }
    else if(Line.BeginsWith("SlRemap_")){
      for(UInt_t iSlot=0;iSlot<=fMaxSlotID/16;iSlot++){
        if(Line.BeginsWith(Form("SlRemap_%04d=",iSlot))){
          TObjArray * l = Line.Tokenize(" ");
          for (UInt_t jSlot=0;jSlot<16;jSlot++){
            if(16*iSlot+jSlot>fMaxSlotID) break;
            fSlotRemap[16*iSlot+jSlot] = static_cast<TObjString*>(l->At(jSlot+1))->GetString().Atoi();
          }
          delete l;
        }
      }
    }
    else if (Line.BeginsWith("HitEnergyThr")){
      fHitEnergyThr = TString(Line(TRegexp("[0-9]+"))).Atof();  //MeV
      continue;
    }
    else if (Line.BeginsWith("SwapFileInput")){
      TObjArray * l = Line.Tokenize(" ");
      fSwapInputFileName = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
    }
    else if (Line.BeginsWith("CrocusReferenceFileInput")){
      TObjArray * l = Line.Tokenize(" ");
      fCrocusReferenceFileName = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
    }
    else if (Line.BeginsWith("SwapDetectionEnabled")){
      fSwapDetectionEnabled = TString(Line(TRegexp("[0-1]"))).Atoi();
      continue;
    }
  }
  NA62ConditionsService::GetInstance()->Close(RawDecFileName);
}

void CREAMRawDecoder::EndProcessing(){
  NA62VRawDecoder::EndProcessing();
  if(fHPedestalBadRefXYEff)         fHPedestalBadRefXYEff->Divide(fHPedestalBadRefXYNum,fHPedestalBadRefXYDen,1,1,"B");
  if(fHZSCounterXY)                 fHZSCounterXY->Write();
  if(fHZSCounterXYCalib)            fHZSCounterXYCalib->Write();
  if(fHNQualityWarningsXY)          fHNQualityWarningsXY->Write();
  if(fHNQualityWarningsVsROChannel) fHNQualityWarningsVsROChannel->Write();
  if(fHPedestals)                   fHPedestals->Write();
  if(fHPedestalsRMS)                fHPedestalsRMS->Write();
  if(fHPedestalVsROChannel)         fHPedestalVsROChannel->Write();
  if(fHPedestalWrtRefVsROChannel)   fHPedestalWrtRefVsROChannel->Write();
  if(fHPedestalBadRefXYNum)         fHPedestalBadRefXYNum->Write();
  if(fHPedestalBadRefXYDen)         fHPedestalBadRefXYDen->Write();
  if(fHPedestalBadRefXYEff)         fHPedestalBadRefXYEff->Write();
  if(fHNCriticalErrorsCrateSlot)    fHNCriticalErrorsCrateSlot->Write();
  if(fHNQualityWarningsCrateSlot)   fHNQualityWarningsCrateSlot->Write();
  if(fHPedestalXY)                  fHPedestalXY->Write();
  if(fHHitEnergy)                   fHHitEnergy->Write();
  if(fHPedestalXYCalib)             fHPedestalXYCalib->Write();
  if(fHHitEnergyCalib)              fHHitEnergyCalib->Write();
  if(fHMaxSampleXYCalib)            fHMaxSampleXYCalib->Write();
  if(fHMaxSampleCrateSlotCalib)     fHMaxSampleCrateSlotCalib->Write();
  if(fHHitMapEnergyAboveZS)         fHHitMapEnergyAboveZS->Write();
  if(fHHitMapEnergyAboveThr)        fHHitMapEnergyAboveThr->Write();

  if(fHSeedPosition) 	fHSeedPosition->Write();

  for(Int_t i=0; i<3; i++){ 
    for(Int_t j=0; j<3; j++){ 
      if(i*j==1) continue;
      if(fHNEntriesNegTail[i][j])       fHNEntriesNegTail[i][j]->Write();  
      if(fHNEntriesPosTail[i][j])       fHNEntriesPosTail[i][j]->Write(); 
      if(fHNEntriesNegTailOCTANE[i][j]) fHNEntriesNegTailOCTANE[i][j]->Write(); 
      if(fHNEntriesPosTailOCTANE[i][j]) fHNEntriesPosTailOCTANE[i][j]->Write(); 
      if(fHDeltaT[i][j])                fHDeltaT[i][j]->Write(); 
      if(fHDeltaTOCTANE[i][j])          fHDeltaTOCTANE[i][j]->Write(); 
      if(fHNPipEntries[i][j])           fHNPipEntries[i][j]->Write(); 
    }
  }

  if(fHFossilCellPosition) fHFossilCellPosition->Write();
  if(fHFossilCREAMPosition) fHFossilCREAMPosition->Write();
}

void CREAMRawDecoder::FillSwapsWithCalibTriggersInfo(UInt_t CrateID,UInt_t SlotID,UInt_t ChannelID,UInt_t MinCount,UInt_t MaxCount){
  if((MaxCount-MinCount) < 500) return; // no pulse
  fNEntriesPerPatternSwapWithCalibTriggers[ChannelID/16]++;
  fNEntriesSwapWithCalibTriggers[CrateID][SlotID][ChannelID/16]++;
  fNTotalEntriesSwapWithCalibTriggers[CrateID][SlotID]++;
}

void CREAMRawDecoder::FillSwapsWithCrocusInfo(UInt_t CrateID,UInt_t SlotID,UInt_t ChannelID,UInt_t MinCount,UInt_t MaxCount){
  // swap finder with Crocus (Algo by Alan Norton, Coding Jurgen Engelfried)
  const Double_t DCountMin = 20., DCountMax = 100.;
  if ((MaxCount-MinCount)>=DCountMin && (MaxCount-MinCount)<=DCountMax) {
    fNHitsSwapWithCrocus[CrateID][SlotID][ChannelID]++;
  }
}

void CREAMRawDecoder::DetectSwapsWithCalibTriggers(){
  UInt_t NCalibTriggers = static_cast<NA62Reconstruction*>(fReco->GetMainReco())->GetNProcessedCalibrationTriggerEventsInFile();
  for(UInt_t iCrate=0;iCrate<fMaxCrateID+1;iCrate++) {
    if(fCrateRemap[iCrate]<0) continue;
    for(UInt_t iSlot=0;iSlot<fMaxSlotID+1;iSlot++) {

      if(fSlotRemap[iSlot]<0) continue;
      if(iCrate== 4 &&  5<=iSlot && iSlot<= 8) continue; // Slots not instrumented
      if(iCrate== 7 && (iSlot==13 || iSlot==14 || iSlot==19 || iSlot==20)) continue; // Slots not instrumented
      if(iCrate==24 && (iSlot== 3 || iSlot== 4 || iSlot== 9 || iSlot==10)) continue; // Slots not instrumented
      if(iCrate==27 && 15<=iSlot && iSlot<=18) continue; // Slots not instrumented

      if(fNTotalEntriesSwapWithCalibTriggers[iCrate][iSlot]!=0){
        if(fNDetectedSwapsWithCalibTriggers[iCrate][iSlot] > 0.1*NCalibTriggers){
          fSwapsFoundWithCalibTriggers.push_back(std::make_pair(iCrate, iSlot));
        }
        fNCreamsCheckedWithCalibTriggers++;
      }
    }
  }
}

void CREAMRawDecoder::DetectSwapsWithCrocus(std::ofstream& AdditionalOutputFile){
  //swap finder with Crocus (Algo by Alan Norton, Coding Jurgen Engelfried)
  Int_t NCreamSeen = 0;
  for(UInt_t iCrate=0;iCrate<fMaxCrateID+1;iCrate++) {
    if(fCrateRemap[iCrate]<0) continue;
    for(UInt_t iSlot=0;iSlot<fMaxSlotID+1;iSlot++) {

      if(fSlotRemap[iSlot]<0) continue;
      if(iCrate== 4 &&  5<=iSlot && iSlot<= 8) continue; // Slots not instrumented
      if(iCrate== 7 && (iSlot==13 || iSlot==14 || iSlot==19 || iSlot==20)) continue; // Slots not instrumented
      if(iCrate==24 && (iSlot== 3 || iSlot== 4 || iSlot== 9 || iSlot==10)) continue; // Slots not instrumented
      if(iCrate==27 && 15<=iSlot && iSlot<=18) continue; // Slots not instrumented
      Int_t Nseen  = 0;
      Int_t Npairs_all = 0;
      Int_t NtestA_all = 0;
      Int_t NtestA_good = 0;
      Int_t NtestA_swap = 0;
      Int_t NtestB_all = 0;
      Double_t ChisqB_good = 0.; 
      Double_t ChisqB_swap = 0.;     
      Int_t Itest,Iswap;
      for (Int_t jx=0;jx<=15;jx++) {
        Int_t kx = jx + 16;
        Iswap = 0;
        Itest = 0;
        if (fNHitsRefSwapWithCrocus[iCrate][iSlot][jx]>=5 || fNHitsRefSwapWithCrocus[iCrate][iSlot][kx]>=5) {
          Nseen++;
          Npairs_all++;
          //Int_t Nerr     = 0;
          Double_t diff_sig = 0.;
          Double_t dsq_good = 0.;
          Double_t dsq_swap = 0.;
          if (fNHitsSwapWithCrocus[iCrate][iSlot][jx]<5 && fNHitsSwapWithCrocus[iCrate][iSlot][kx]<5) {
            //Nerr = 1;
          }
          else if (fNHitsRefSwapWithCrocus[iCrate][iSlot][jx]<5 || fNHitsRefSwapWithCrocus[iCrate][iSlot][kx]<5) { 
            if (fNHitsSwapWithCrocus[iCrate][iSlot][jx]>=5 && fNHitsSwapWithCrocus[iCrate][iSlot][kx]>=5) {
              //Nerr  = 2;
            }
            else {
              Itest = 1;
              if ( (fNHitsRefSwapWithCrocus[iCrate][iSlot][jx]<5 && fNHitsSwapWithCrocus[iCrate][iSlot][kx]<5) ||  (fNHitsRefSwapWithCrocus[iCrate][iSlot][kx]<5 && fNHitsSwapWithCrocus[iCrate][iSlot][jx]<5) ) Iswap = 1;
            }
          }
          else {          
            Double_t dsum_ref  = fNHitsRefSwapWithCrocus[iCrate][iSlot][jx] + fNHitsRefSwapWithCrocus[iCrate][iSlot][kx];
            Double_t diff_ref  = fNHitsRefSwapWithCrocus[iCrate][iSlot][jx] - fNHitsRefSwapWithCrocus[iCrate][iSlot][kx];
            Double_t dsum      = fNHitsSwapWithCrocus[iCrate][iSlot][jx] + fNHitsSwapWithCrocus[iCrate][iSlot][kx];
            Double_t diff      = fNHitsSwapWithCrocus[iCrate][iSlot][jx] - fNHitsSwapWithCrocus[iCrate][iSlot][kx];
            diff_sig  = diff_ref/sqrt(dsum_ref);
            Double_t diff_rnor = diff_ref * (dsum/dsum_ref);
            dsq_good  = (diff-diff_rnor)*(diff-diff_rnor) / dsum;
            dsq_swap  = (diff+diff_rnor)*(diff+diff_rnor) / dsum;
            Iswap = -1;
            if ( fabs(diff_sig) > 16. ) {
              if (dsq_good< 4. && dsq_swap>25. ) Iswap = 0;
              if (dsq_good<10. && dsq_swap>50. ) Iswap = 0;
              if (dsq_good>25. && dsq_swap< 4. ) Iswap = 1;
              if (dsq_good>50. && dsq_swap<10. ) Iswap = 1;
              if (Iswap>=0 ) Itest = 2;
            }

            if (Iswap==-1 && fabs(diff_sig)>2.5 && (dsq_good<10. || dsq_swap<10.) ) {
              Itest = 3;
              NtestB_all ++;
              ChisqB_good += dsq_good;
              ChisqB_swap += dsq_swap;
            }             
          }         
          if (Itest>0 && Itest<3) {
            if (Iswap>=0) NtestA_all++;
            if (Iswap==0) NtestA_good++;
            if (Iswap>0) NtestA_swap++;
          }

        }
      } // loop over channels
      if ( Itest==3 && NtestB_all>0 ) {
        ChisqB_good = ChisqB_good / NtestB_all;
        ChisqB_swap = ChisqB_swap / NtestB_all;
      }
      if ( Nseen>0 ) NCreamSeen++;
      Bool_t Tested = false;
      Bool_t Swapped  = false;
      if ( Npairs_all>0 ) {
        Int_t NA      = NtestA_all;
        Int_t NB      = NtestB_all;
        Tested  = (NA>1)||(NB>3);

        Double_t DChisqB = ChisqB_good - ChisqB_swap;
        Double_t FChisqB = DChisqB/TMath::Max(ChisqB_swap,3.);
        if (Tested) {
          if ( NA==0 ) Swapped = (FChisqB>1.) && (NB>5);
          if ( NA==1 ) Swapped = (DChisqB>-1.) && (NtestA_swap==1);
          if ( NA>1  ) Swapped = NtestA_swap>=(NA-NA/4);
        }
        Char_t IF_TEST[12];
        if (!Tested) sprintf(IF_TEST," NO TEST");
        else sprintf(IF_TEST,"        ");
        Char_t IF_SWAP[8];
        if (Tested && Swapped) sprintf(IF_SWAP," YES");
        else sprintf(IF_SWAP,"    ");
        if (Swapped) {
          // ---------------- Crocus additional file ---------------- //
          AdditionalOutputFile << Form("%2d %2d %7d %7d %7d %7d %7d %8.2f %8.2f   %4s  %8s\n",iCrate,iSlot,Npairs_all,NA,NtestA_good,NtestA_swap,NB,ChisqB_good,ChisqB_swap,IF_SWAP,IF_TEST);
          // -------------------------------------------------------- //
        }
        if ( Tested  ) fNCreamsCheckedWithCrocus++;
        if ( Swapped ) fSwapsFoundWithCrocus.push_back(std::make_pair(iCrate, iSlot));
      }
    }
  }
}

void CREAMRawDecoder::JitterAnalysis(){    //Original Algo by A. Norton, coding by M. Corvino
  // X AXIS DEFINITION wrt CREAM channels:
  //	 _______________________
  //	|     |24	16  8  0|     |
  //	|     |25	17  9  1|     |
  //	|     |26	18 10  2|     |
  //	|     |27	19 11  3|     |
  //	|     |28	20 12  4|     |
  //	|     |29	21 13  5|	    |
  //	|	    |30	22 14  6|	    |
  //	|     |31	23 15  7|     |	
  //	|_____|___________|_____|	
  //  	iX+1    iX 	    	iX-1
  //	<--------------------------
  //	x
  //
  // 

  TClonesArray &Digis = (*(fDigiEvent->GetHits()));
  Int_t NDigis = fDigiEvent->GetNHits();
  std::vector<Int_t> SeedIDs;
  std::vector<Int_t> GoodPipsIDs;
  for(int iDigi=0; iDigi<NDigis; iDigi++){
    Double_t SeedEnergy = 0;
    FADCVHit *Seed = static_cast<FADCVHit *>(Digis[iDigi]);
    if(Seed->GetPeakTime()>1e10) continue; //remove fake digis
    Double_t *Samples = Seed->GetAllSamples();
    Double_t Pedestal = (Double_t)(Samples[0]+Samples[1])/2.;
    Int_t ix = static_cast<TLKrDigi*>(Seed)->GetXCellID();
    Int_t iy = static_cast<TLKrDigi*>(Seed)->GetYCellID();
    Double_t HitEnergy = 1000.*LKrParameters::GetInstance()->GetCalSteig(ix,iy,0)*(Seed->GetADCPeakEnergy()-Pedestal); //rough (MeV)
    if(HitEnergy > 400){ 
      SeedEnergy = HitEnergy;
    }
    else continue;
    GoodPipsIDs.clear();
    for(int jDigi=0; jDigi<NDigis; jDigi++){
      if(jDigi == iDigi) continue;
      FADCVHit *Pip = static_cast<FADCVHit*>(Digis[jDigi]);
      if(Pip->GetPeakTime()>1e10) continue; //remove fake digis
      Double_t *PipSamples = Pip->GetAllSamples();
      Double_t PipPedestal = (Double_t)(PipSamples[0]+PipSamples[1])/2.;
      Int_t Pipix = static_cast<TLKrDigi*>(Pip)->GetXCellID();
      Int_t Pipiy = static_cast<TLKrDigi*>(Pip)->GetYCellID();
      if(fabs(Pipix-static_cast<TLKrDigi*>(Seed)->GetXCellID())>1 || fabs(Pipiy-static_cast<TLKrDigi*>(Seed)->GetYCellID())>1) continue;
      Double_t PipEnergy = 1000.*LKrParameters::GetInstance()->GetCalSteig(Pipix,Pipiy,0)*(Pip->GetADCPeakEnergy()-PipPedestal); //rough (MeV)  
      if(PipEnergy > SeedEnergy){
        break;
      }
      else if(PipEnergy > 100) {
        GoodPipsIDs.push_back(jDigi);
      }
    }
    if(GoodPipsIDs.size() > 4) {
      for(UInt_t iPip=0; iPip<GoodPipsIDs.size();iPip++) SeedPipDeltaT(Seed, static_cast<FADCVHit*>(Digis[GoodPipsIDs.at(iPip)]));
      fHSeedPosition->Fill(ix,iy);
      SeedIDs.push_back(iDigi);
    }
  }
  return;
}

void CREAMRawDecoder::FossilChecker(Int_t flag){ // Look for Fossils between LKr periodic triggers
  Int_t NDigis = fDigiEvent->GetNHits();
  TClonesArray &Digis = (*(fDigiEvent->GetHits()));
  for(Int_t iDigi=0; iDigi<NDigis; iDigi++){
    Bool_t IsNeedle = true;
    Bool_t FossilAlreadyFound = false;
    FADCVHit *fDigi = static_cast<FADCVHit *>(Digis[iDigi]);
    if(fDigi->GetPeakTime()>1e10) continue; //remove fake digis
    Double_t *Samples = fDigi->GetAllSamples();
    Double_t Pedestal = (Double_t)(Samples[0]+Samples[1])/2.;
    Int_t ix = static_cast<TLKrDigi*>(fDigi)->GetXCellID();
    Int_t iy = static_cast<TLKrDigi*>(fDigi)->GetYCellID();
    Int_t ROChID  = GetChannelRO(fDigi->GetChannelID());
    Int_t ROChIDInCREAM = ROChID%32;
    Int_t Crate = ROChID/512;
    Int_t Slot = (ROChID%512)/32;
    Double_t HitEnergy = 1000.*LKrParameters::GetInstance()->GetCalSteig(ix,iy,0)*(fDigi->GetADCPeakEnergy()-Pedestal); //rough (MeV)
    if( HitEnergy < 1000) continue;
    for (UInt_t i=0; i<fLKrZSFossils.size();i++){
      if(!flag && Crate == fLKrZSFossils.at(i).Crate && GetPhysicalSlot(Slot) == fLKrZSFossils.at(i).Slot && ROChIDInCREAM == fLKrZSFossils.at(i).Ch) FossilAlreadyFound = true;
    }
    for (UInt_t i=0; i<fLKrNeedles.size();i++){
      if(flag && Crate == fLKrNeedles.at(i).Crate && GetPhysicalSlot(Slot) == fLKrNeedles.at(i).Slot && ROChIDInCREAM == fLKrNeedles.at(i).Ch) FossilAlreadyFound = true;
    }
    if (FossilAlreadyFound) continue;
    if(flag){
      // Find the "needles", i.e. cells with E > 1 GeV with no energy deposits in the 8 pips surrounding them
      for(Int_t jDigi=0; jDigi<NDigis; jDigi++){
        if(jDigi==iDigi) continue;        
        FADCVHit *Pip = static_cast<FADCVHit*>(Digis[jDigi]);
        if(Pip->GetPeakTime()>1e10) continue;
        if(fabs(static_cast<TLKrDigi*>(Pip)->GetXCellID()-ix)<=1 || fabs(static_cast<TLKrDigi*>(Pip)->GetYCellID()-iy)<=1) IsNeedle = false;
      }
      if(!IsNeedle) continue;
    }    
    LKrFossil newFossil;
    newFossil.Crate = Crate;
    newFossil.Slot = GetPhysicalSlot(Slot);
    newFossil.Ch = ROChIDInCREAM;
    newFossil.Energy = HitEnergy;
    newFossil.ix = ix;
    newFossil.iy = iy;
    if(!flag) fLKrZSFossils.push_back(newFossil);
    else fLKrNeedles.push_back(newFossil);
  }
}

Double_t CREAMRawDecoder::GetAsymmetry(Int_t Crate, Int_t Slot,Int_t i, Int_t j, Bool_t UseCoarseT0){
  Double_t Asym = -9999.99;
  TH2F *Neg, *Pos; 
  if(UseCoarseT0){
    Neg = fHNEntriesNegTail[i][j];
    Pos = fHNEntriesPosTail[i][j];
  }
  else{
    Neg = fHNEntriesNegTailOCTANE[i][j];
    Pos = fHNEntriesPosTailOCTANE[i][j];
  }
  Double_t n = Neg->GetBinContent(Crate+1,Slot+1);
  Double_t p = Pos->GetBinContent(Crate+1,Slot+1);
  if(n && p) Asym = (p-n)/TMath::Sqrt(n+p);
  else if(n) Asym = (p-n)/TMath::Sqrt(1+n);
  else Asym = (p-n)/TMath::Sqrt(1+p);
  return Asym;
}

void CREAMRawDecoder::SeedPipDeltaT(FADCVHit *seed, FADCVHit *pip){
  Int_t SeedCrateID = GetChannelRO(seed->GetChannelID())/512;
  Int_t SeedSlotID = (GetChannelRO(seed->GetChannelID())%512)/32;
  Int_t SeedChID = (GetChannelRO(seed->GetChannelID()))%32;
  Int_t PipCrateID = GetChannelRO(pip->GetChannelID())/512;
  Int_t PipSlotID = (GetChannelRO(pip->GetChannelID())%512)/32;
  Int_t PipChID = (GetChannelRO(pip->GetChannelID()))%32;
  if(SeedCrateID == PipCrateID && SeedSlotID == PipSlotID) return; // no delta t in the same CREAM
  Int_t SeedxID = ((TLKrDigi*)seed)->GetXCellID(); 
  Int_t SeedyID = ((TLKrDigi*)seed)->GetYCellID(); 
  Int_t PipxID  = ((TLKrDigi*)pip)->GetXCellID(); 
  Int_t PipyID  = ((TLKrDigi*)pip)->GetYCellID(); 
  Int_t iRow = kCREAMSeed;
  Int_t jCol = kCREAMSeed;
  if(SeedChID>0 && SeedChID<7) jCol=kCREAMRight; 
  else if(SeedChID>24 && SeedChID<31) jCol=kCREAMLeft; 
  else if(SeedChID==8 || SeedChID==16) iRow=kCREAMUp; 
  else if(SeedChID==15 || SeedChID==23) iRow=kCREAMDown; 
  else if(SeedChID==0){
    if(PipChID==7 || PipChID==15)  iRow = kCREAMUp;
    else if(PipChID==24 || PipChID==25) jCol = kCREAMRight;
    else if(PipChID==31){
      iRow = kCREAMUp;
      jCol = kCREAMRight;
    }
    else cout<<"this could not happen!!"<<"Seed ch is "<<SeedChID<<" Pip ch is "<<PipChID<<endl;
  }
  else if(SeedChID==7){
    if(PipChID==0 || PipChID==8) iRow = kCREAMDown;
    else if (PipChID==24){
      iRow = kCREAMDown;
      jCol = kCREAMRight;
    }
    else if(PipChID==31 || PipChID==30) jCol = kCREAMRight;
    else cout<<"this could not happen!!"<<"Seed ch is "<<SeedChID<<" Pip ch is "<<PipChID<<endl;

  }
  else if(SeedChID==24){
    if(PipChID==0 || PipChID==1) jCol = kCREAMLeft;
    else if(PipChID==7){
      iRow = kCREAMUp;
      jCol = kCREAMLeft;
    }
    else if(PipChID==31 || PipChID==23) iRow = kCREAMUp;
    else   cout<<"this could not happen!!"<<"Seed ch is "<<SeedChID<<" Pip ch is "<<PipChID<<endl;
  }
  else if(SeedChID==31){
    if(PipChID==0){
      iRow = kCREAMDown;
      jCol = kCREAMLeft;
    }
    else if(PipChID==7 || PipChID==6) jCol = kCREAMLeft;
    else if(PipChID==24 || PipChID == 16)  iRow = kCREAMDown;
    else  cout<<"this could not happen!!"<<"Seed ch is "<<SeedChID<<" Pip ch is "<<PipChID<<" CREAM "<<SeedCrateID<<"-"<<GetPhysicalSlot(SeedSlotID)<<endl;
  }
  else cout<<"Invalid seed channel!"<<endl;

  Double_t *SeedSamples = seed->GetAllSamples();
  Double_t SeedPedestal = (Double_t)(SeedSamples[0]+SeedSamples[1])/2.;
  Double_t SeedEnergy = 1000.*LKrParameters::GetInstance()->GetCalSteig(SeedxID,SeedyID,0)*(seed->GetADCPeakEnergy()-SeedPedestal); //rough (MeV)  
  Double_t *PipSamples = pip->GetAllSamples();
  Double_t PipPedestal = (Double_t)(PipSamples[0]+PipSamples[1])/2.;
  Double_t PipEnergy = 1000.*LKrParameters::GetInstance()->GetCalSteig(PipxID,PipyID,0)*(pip->GetADCPeakEnergy()-PipPedestal); //rough (MeV)  
  Double_t SeedT0 = fReco->GetT0Correction(seed)+LKrParameters::GetInstance()->GetCellT0(SeedxID,SeedyID);
  Double_t PipT0  = fReco->GetT0Correction(pip)+LKrParameters::GetInstance()->GetCellT0(PipxID,PipyID);
  Double_t SeedTime = seed->GetPeakTime()-SeedT0;
  Double_t PipTime = pip->GetPeakTime()-PipT0;
  if(SeedxID==PipxID && SeedyID!=PipyID) PipTime = PipTime + 1.0*SeedEnergy/PipEnergy; // correction for Up and Down pips 
  if(SeedxID!=PipxID && SeedyID!=PipyID) PipTime = PipTime + 0.2*SeedEnergy/PipEnergy; // correction for diagonal pips
  Double_t DeltaT = SeedTime-PipTime;
  fHDeltaT[iRow][jCol]->Fill(SeedCrateID*16+SeedSlotID,DeltaT);
  if(DeltaT > 15) {
    fHNEntriesPosTail[iRow][jCol]->Fill(SeedCrateID,SeedSlotID);
  }
  if(DeltaT < -15) {
    fHNEntriesNegTail[iRow][jCol]->Fill(SeedCrateID,SeedSlotID);
  }
  Double_t DeltaTOCTANE = (seed->GetPeakTime()-LKrParameters::GetInstance()->GetCellT0(SeedxID,SeedyID))-(pip->GetPeakTime()-LKrParameters::GetInstance()->GetCellT0(PipxID,PipyID));
  fHDeltaTOCTANE[iRow][jCol]->Fill(SeedCrateID*16+SeedSlotID,DeltaTOCTANE);
  if(DeltaTOCTANE > 15){
    fHNEntriesPosTailOCTANE[iRow][jCol]->Fill(SeedCrateID,SeedSlotID);
  }
  if(DeltaTOCTANE < -15){
    fHNEntriesNegTailOCTANE[iRow][jCol]->Fill(SeedCrateID,SeedSlotID);
  }
  fHNPipEntries[iRow][jCol]->Fill(SeedCrateID,SeedSlotID);

}

Bool_t CREAMRawDecoder::HasEnoughStatistics(Int_t crate, Int_t slot, Int_t row, Int_t col, Bool_t UseCoarseT0){
  if(UseCoarseT0) return (fHNEntriesNegTail[row][col]->GetBinContent(crate+1,slot+1)+fHNEntriesPosTail[row][col]->GetBinContent(crate,slot+1) > 30);
  else return (fHNEntriesNegTailOCTANE[row][col]->GetBinContent(crate+1,slot+1)+fHNEntriesPosTailOCTANE[row][col]->GetBinContent(crate,slot+1) > 30);
} 

Bool_t CREAMRawDecoder::CREAMHasJitter(Int_t CrateID, Int_t ProgressiveSlotID, Double_t &JittTime, Bool_t UseCoarseT0){
  Int_t NCREAMsUsed = 0;
  Int_t NPos = 0;
  Int_t NNeg = 0;
  Double_t MeanPos =  0;
  Double_t MeanNeg	= 0;
  Double_t MinTailFraction = 0.01;
  for(Int_t i=0; i<3;i++){
    for(Int_t j=0; j<3; j++){
      if(i==kCREAMSeed && j==kCREAMSeed) continue;
      TH1D *histo;
      if(UseCoarseT0) histo = fHDeltaT[i][j]->ProjectionY("_l",CrateID*16+ProgressiveSlotID+1,CrateID*16+ProgressiveSlotID+1,"");
      else histo = fHDeltaTOCTANE[i][j]->ProjectionY("_l",CrateID*16+ProgressiveSlotID+1,CrateID*16+ProgressiveSlotID+1,"");
      Int_t NBins = histo->GetNbinsX();
      Int_t NegTailEnd = histo->FindBin(-15);
      Int_t PosTailStart = histo->FindBin(15);
      Int_t NEventsInCore = histo->Integral(NegTailEnd,PosTailStart);
      Int_t NEventsInTails = histo->Integral(1,NegTailEnd)+histo->Integral(PosTailStart,NBins); 
      if(!HasEnoughStatistics(CrateID,ProgressiveSlotID,i,j,UseCoarseT0)) continue;
      NCREAMsUsed++; 
      if(GetAsymmetry(CrateID,ProgressiveSlotID,i,j,UseCoarseT0) > fJitterThreshold) {
        if(NEventsInTails<MinTailFraction*NEventsInCore) continue;
        NPos++;
        histo->GetXaxis()->SetRange(PosTailStart,NBins);
        MeanPos += histo->GetMean();
      }
      if(GetAsymmetry(CrateID,ProgressiveSlotID,i,j,UseCoarseT0) < -1*fJitterThreshold){
        if(NEventsInTails<MinTailFraction*NEventsInCore) continue;
        NNeg++;
        histo->GetXaxis()->SetRange(1,NegTailEnd);
        MeanNeg += histo->GetMean();
      }
    } 
  }
  if(NPos > NCREAMsUsed/2 ){
    JittTime = MeanPos/NCREAMsUsed;
    return true;
  }
  else if(NNeg > NCREAMsUsed/2){
    JittTime = MeanNeg/NCREAMsUsed;
    return true;
  }
  return false;
}
LKrJitter CREAMRawDecoder::InitializeJitter(Int_t crate, Int_t slot, Bool_t UseCoarseT0){
  LKrJitter tempJitter;
  tempJitter.Crate = crate;
  tempJitter.Slot = GetPhysicalSlot(slot);
  tempJitter.NEntries = 0;
  tempJitter.MaxAsym = 0 ;
  tempJitter.MinAsym = 999 ;
  tempJitter.MaxRatio = 0 ;
  tempJitter.MinRatio = 999 ;
  tempJitter.JittTime= -999 ;
  tempJitter.TimeSlotsShift = 0 ;
  for(Int_t iRow=0; iRow<3; iRow++){
    for(Int_t jCol=0; jCol<3; jCol++){
      if(iRow*jCol==1) continue;
      Int_t N = fHNPipEntries[iRow][jCol]->GetBinContent(crate+1,slot+1);
      Double_t n, p;
      if(UseCoarseT0){
        n = fHNEntriesNegTail[iRow][jCol]->GetBinContent(crate+1,slot+1);
        p = fHNEntriesPosTail[iRow][jCol]->GetBinContent(crate+1,slot+1);
      }
      else{
        n = fHNEntriesNegTailOCTANE[iRow][jCol]->GetBinContent(crate+1,slot+1);
        p = fHNEntriesPosTailOCTANE[iRow][jCol]->GetBinContent(crate+1,slot+1);
      }
      tempJitter.NEntries += N;
      if(HasEnoughStatistics(crate,slot,iRow,jCol,UseCoarseT0)){
        Double_t tempAsym = GetAsymmetry(crate,slot,iRow,jCol,UseCoarseT0);
        Double_t tempRatio = (n+p)/N;
        if(fabs(tempAsym) > fJitterThreshold){
          tempJitter.MinAsym   = fabs(tempAsym)   < fabs(tempJitter.MinAsym)  ? tempAsym   : tempJitter.MinAsym; 
          tempJitter.MinRatio  = tempRatio  < tempJitter.MinRatio ? tempRatio  : tempJitter.MinRatio; 
          tempJitter.MaxAsym   = fabs(tempAsym)   > fabs(tempJitter.MaxAsym)  ? tempAsym   : tempJitter.MaxAsym; 
          tempJitter.MaxRatio  = tempRatio  > tempJitter.MaxRatio ? tempRatio  : tempJitter.MaxRatio; 
        }
      }
    }
  }
  return tempJitter;
}
