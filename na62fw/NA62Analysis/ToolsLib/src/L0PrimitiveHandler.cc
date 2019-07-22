// ---------------------------------------------------------------
// History:
//
// Added fake primitives to solve timing issues, Chris Parkinson, 2019-07-16
// Created by Chris Parkinson (chris.parkinson) 2017-05-31
// With help from Dario Soldi, Francesco Gonnella, Radoslav Marchevski
// ---------------------------------------------------------------

/// \class L0PrimitiveHandler
/// \Brief
/// A user-friendly interface to the L0 primitives contained in the L0TP data packet,
/// designed to facilitate computation of L0 trigger efficiencies.
/// \EndBrief
/// \Detailed
/// This class provides an interface to the L0 primitives contained in the L0TP data packet.
/// The class is designed to facilitate the computation of L0 trigger efficiencies.\n\n
/// THE L0 PRIMITIVE HANDLER\n\n
/// The L0PrimitiveHandler is implemented as a singleton class -- there can be only one instance that the user
/// will interact with via the L0PrimitiveHandler::GetInstance() function.\n\n
/// The run-dependent names of the primtive bits for each L0 detector, and the run-dependent configuration
/// of the L0TP, including the time cut for each detector and the parameter that determines the RAM segmentation,
///  are read from the file NA62Tools/Conditions/Trigger/L0PrimitiveBits.dat and
/// stored in memory as "L0DetectorBits" objects.\n\n
/// A user can check the efficiency of a single primitive bit, for example "MUV",
/// check the bit with a vetoing logic, for example
/// "nMUV", and check several bits at once, for example "QX-MO2-nCalo".
/// The procedure is detailed below.\n\n
/// To use the tool, a user should first instantiate it:
/// \code
/// #include "L0PrimitiveHandler.hh"
/// ...
/// // In the analyzer constructor:
/// RequestL0Data();
/// RequestL0TPSpecialTrigger(); // only needed to access L0 trigger names/downscales!
/// L0PrimitiveHandler* handle = L0PrimitiveHandler::GetInstance();
/// \endcode
/// In the Process() function of their analyzer, the user should supply the tool with a pointer to the
/// L0TPData object and the run number. The names and downscalings of the trigger masks of this burst can also be
/// accessed (on any data reconstructed after NARKD-564 was fixed) at this point:
/// \code
/// handle->SetData(GetL0Data(), GetRunID());
/// std::vector<TString> maskNames;
/// std::vector<Int_t> maskDS;
/// fL0PrimHandle->GetL0Masks(GetL0SpecialTrigger(), maskNames, maskDS);
/// \endcode
/// The names and downscalings of the trigger masks of the burst can be accessed.
/// All the DetectorBits currently known to the tool, and the list of primitive bit-names known for the current
/// run, can be shown with the following functions:
/// \code
/// handle->PrintDetectorBits();
/// handle->PrintKnownBitNames(GetRunID());
/// \endcode
/// The list of L0 Detectors known to the tool, and their accessor names, can be viewed with the command:
/// \code
/// handle->PrintDetectorKey();
/// \endcode
/// To measure the trigger efficiencies, the user should check that the event
/// has passed the control trigger, then check that the conditions for
/// making a L0 trigger efficiency measurement (that there is a primitive from a second L0 detector
/// in the TriggerSlot, and not in the PreviousSlot and NextSlot) are satisfied. The time used as a
/// reference for the efficiency measurement is extracted from the relevant primitive, then the
/// presence of certain primitive bits, or combinations of primitive bits, can be checked.
/// \code
/// if (!fTriggerConditions->IsControlTrigger(GetL0Data())) return; // return if not collected by control trigger
/// // To measure any efficiency other than the RICH
/// Bool_t RICHCondition = handle->CheckCondition(kL0RICH);
/// if (RICHCondition) {
///   // First get the RICH primitive time (L0TP cuts w.r.t. this time)
///   Int_t richtime = handle->GetTriggerTime(kL0RICH);
///   // Check if Calo bit is high:
///   Bool_t HasCALObit = handle->CheckPrimitives("Calo",richtime);
///   // Check if !MUV bit is high:
///   Bool_t HasnMUVbit = handle->CheckPrimitives("nMUV",richtime);
///   // Check for pinunu mask on run 7442 (RICH is 100% efficient by definition)
///   handle->CheckPrimitives("RICH-NewCHOD-nQX-UTMC-nE20-nLAV-nMUV",richtime);
/// }
///
/// // for RICH efficiency measurement
/// Bool_t NewCHODCondition = handle->CheckCondition(kL0NewCHOD);
/// if (NewCHODCondition) {
///   Int_t newchodtime  = handle->GetTriggerTime(kL0NewCHOD);
///   Bool_t RICHEfficiency = handle->CheckPrimitives("RICH",newchodtime);
/// }
/// \endcode
/// Debugging information of the CheckPrimitives function can be turned on, the function will then print
/// whether each component of the trigger mask has passed or failed.
/// \code
/// handle->SetDebug(true);
/// handle->SetDebug(false);
/// \endcode
/// For expert users, the information used internally by the tool can be accessed using the
/// GetPrimitiveInfo() function. It returns a vector of size four, containing:
/// [0] = the primitive ID
/// [1] = the corrected primitive fine time
/// [2] = the signed time difference between the corrected fine time and the reference time
/// [3] = the absolute value of element [2]
/// The information is accessed as follows:
/// \code
/// std::vector<Int_t> primInfo = handle->GetPrimitiveInfo(RichTime, kL0NewCHOD, kL0TriggerSlot);
/// for(int k=0; k<4; ++k) cout << " primInfo[" << k << "] " << primInfo[k] << endl;
/// \endcode
/// \EndDetailed

#include <iostream>
#include <fstream>
#include <string>
#include "L0PrimitiveHandler.hh"
#include "NA62ConditionsService.hh"
#include <bitset>
#include "VL0Emulator.hh"

using namespace std;

static L0PrimitiveHandler* fInstance = 0;

L0PrimitiveHandler* L0PrimitiveHandler::GetInstance(){
  if (!fInstance) fInstance = new L0PrimitiveHandler();
  return fInstance;
}

L0PrimitiveHandler::L0PrimitiveHandler() : fDebug(false), fRunNumber(99999), fL0TPData(nullptr), fBase(nullptr) {
  // initialise detector key
  detkey[0] = "CHOD";
  detkey[1] = "RICH";
  detkey[2] = "LAV";
  detkey[3] = "MUV3";
  detkey[4] = "NewCHOD";
  detkey[5] = "TALK";
  detkey[6] = "Calo";
  detkey[7] = "L0TP";
  detkey[8] = "EMULATOR";

  for(unsigned i=0;i<7;++i) fEnabledEmulators[i] = false;

  // default constructor
  TString defaultname = "L0PrimitiveBits.dat";
  ParseInputFile(defaultname);
}

L0PrimitiveHandler::~L0PrimitiveHandler(){
  // don't want to delete fL0TPData! But set the pointer to nullptr
  fL0TPData = nullptr;
}

void L0PrimitiveHandler::SetDebug(Bool_t input){
  fDebug = input;
}

Bool_t L0PrimitiveHandler::GetDebug(){
  return fDebug;
}

void L0PrimitiveHandler::NewInputFile(TString fname){
  // Overwrite the default file that contains primitive definitions
  fAllDetectorBits.clear();
  fBitNameToDetectorID.clear();
  ParseInputFile(fname);
}

void L0PrimitiveHandler::ParseInputFile(TString fname) {

  NA62ConditionsService::GetInstance()->Open(fname);
  fAllDetectorBits.reserve(20);
  std::string line;
  while (getline(NA62ConditionsService::GetInstance()->Get(fname), line)) {
    if(line.length()==0) continue; // skip blank lines
    if(line.compare(0,1,"#")==0) continue; // skip if line starts with #
    TString item(line.c_str());
    // cout << " reading item: " << item << endl;
    L0DetectorBits dbits(item); // build a new set of detector bits
    fAllDetectorBits.push_back(dbits);
  }
  NA62ConditionsService::GetInstance()->Close(fname);

  std::vector<L0DetectorBits>::iterator it;
  for(it=fAllDetectorBits.begin(); it!=fAllDetectorBits.end(); ++it){
    for(int j=0; j<15; ++j){
      fBitNameToDetectorID[it->GetBit(j)] = it->GetL0Detector();
    }
  }
}

void L0PrimitiveHandler::GetL0Masks(L0TPSpecialTrigger* L0TPST,
				    std::vector<TString>& MaskNames,
				    std::vector<Int_t>& Downscales){
  if(!L0TPST){
    cout << "[L0PrimitiveHandler::GetL0Masks] A NULL pointer was passed" << endl;
    return;
  }

  //outputs
  MaskNames.clear();
  MaskNames.resize(16, "");

  Downscales.clear();
  Downscales.resize(16, 0);

  // inputs
  std::vector<L0Mask> masks = L0TPST->GetL0Masks();

  for(unsigned int mit=0; mit < masks.size(); ++mit){

    Int_t MaskID = masks[mit].GetMaskID();
    Downscales[MaskID] = masks[mit].GetDownscalingFactor();
    if(!Downscales[MaskID]) continue;

    std::vector<UInt_t> DC = masks[mit].GetDontcarePrimBitMask();
    std::vector<UInt_t> RQ = masks[mit].GetRequiredPrimBitMask();

    for(unsigned int d=0; d<DC.size(); ++d){

      UInt_t detDC = DC[d];
      UInt_t detRQ = RQ[d];

      for(int b=0; b<15; ++b){ // Bit15 is reserved

	Bool_t hasDC = (detDC>>b)&0x1;
	Bool_t hasRQ = (detRQ>>b)&0x1;

	if(hasDC && hasRQ){
	  // bit ignored
	}
	else{
	  TString name = "";
	  Bool_t veto = (!hasDC && !hasRQ);
	  if(veto) name += "n";
	  name += GetBitName(d, b);

	  if(MaskNames[MaskID].Length()){
	    MaskNames[MaskID] += "-";
	    MaskNames[MaskID] += name;
	  }
	  else{
	    MaskNames[MaskID] = name;
	  }
	}
      }// end loop on bits
    }// end loop on detectors
  }// end loop on L0 masks

  cout << endl;
  for(int i=0; i<16; ++i){
    cout << " Mask " << i << " " << MaskNames[i] << "\t"
	 << " Downscale " << Downscales[i] << endl;
  }
}

void L0PrimitiveHandler::BuildL0TPInfo(){
  // cache L0TPInfo information (cut window and FineTime bit)
  fL0TPInfo.clear();
  fL0TPInfo.reserve(7);
  for(int i=0; i<7; ++i){
    Int_t bit = -999;
    Int_t cut = -999;
    if(i==5){
      fL0TPInfo.push_back( std::pair<Int_t, Int_t>(bit,cut) );
      continue;
    }
    GetL0TPConfig(i, bit, cut);
    std::pair<Int_t, Int_t> p(bit,cut);
    fL0TPInfo.push_back(p);
    // cout << " Detector " << i << " bit " << bit << " cut " << cut << endl;
  }
}

void L0PrimitiveHandler::SetData(L0TPData* input, Int_t runNumber){
  SetData(input);
  SetRunID(runNumber);
}

void L0PrimitiveHandler::SetRunID(Int_t runNumber){
  if(runNumber<0) cout << "[L0PrimitiveHandler] WARNING: Input run number is negative" << endl;
  fRunNumber = runNumber;
  BuildL0TPInfo();
}

void L0PrimitiveHandler::SetData(L0TPData* input){
  if(!input){
    cout << "[L0PrimitiveHandler] WARNING: Input L0TPData pointer is NULL" << endl;
    return;
  }
  fL0TPData = input;

  // check for fake trigger flags and data type to identify MC events
  // then fill the central trigger slot with primitives that have the reference
  // fine time used in the event simulation.
  if(fL0TPData->GetTriggerFlags()==0xFFFF){ // && unsigned(fL0TPData->GetDataType())==0xFF
    unsigned ref = unsigned(fL0TPData->GetReferenceFineTime());

    std::vector<L0Primitive> fakeprims( 3*L0NMAXDETECTORS );
    for(int i=0; i<L0NMAXDETECTORS; ++i){
      fakeprims[i].SetFineTime(ref);
      fakeprims[i].SetPrimitiveID(0xFFFF);
    }
    fL0TPData->SetPrimitives(fakeprims);
  }
}

// called in the constructor
void L0PrimitiveHandler::DeclareL0Emulators(BaseAnalysis* ba, Int_t A, Int_t B, Int_t C, Int_t D, Int_t E, Int_t F, Int_t G){

  fBase = ba;

  Int_t Dets[7] = {A,B,C,D,E,F,G};
  for(unsigned i=0;i<7;++i) fEnabledEmulators[i] = false;

  for(unsigned i=0; i<7; ++i){
    if(Dets[i]<0)  continue; // too low
    if(Dets[i]==5) continue; // TALK
    if(Dets[i]>6)  continue; // too high
    fEnabledEmulators[Dets[i]] = true;
  }
}

// config enabled emulators according to run number. Called from StartOfRunUser.
void L0PrimitiveHandler::ConfigureL0Emulators(){

  // get emulator setup
  GetEmulatorConfig();

  // cout << " Emulator info: " ;
  // for(unsigned i=0; i<11; ++i) cout << fL0EmulatorConfig[i] << " ";
  // cout << endl << endl ;

  for(unsigned i=0; i<7; ++i){ // loop over detectors
    if(!fEnabledEmulators[i]) continue;
    if(i==5) continue;

    // L0Emulator name. Needed to hide from Regexp parse!
    TString EmulatorName = "L0"+detkey[i]+"Emulator";

    // Set fine time bit of L0TP
    TString FineTimeBit = "";
    FineTimeBit += fL0TPInfo[i].first; // must be a string!!
    fBase->ReconfigureAnalyzer(EmulatorName, "FineTimeBit", FineTimeBit);

    // using fClusteringWindow (new)
    fBase->ReconfigureAnalyzer(EmulatorName, "ClusteringWindow", fL0EmulatorConfig[i]);

    // detector specific parameters
    if(i==0){// L0CHOD
      fBase->ReconfigureAnalyzer(EmulatorName, "PPThreshold", 2); // always
      fBase->ReconfigureAnalyzer(EmulatorName, "SLThreshold", 2); // always
    }
    if(i==1){// L0RICH
      fBase->ReconfigureAnalyzer(EmulatorName, "PPThreshold", fL0EmulatorConfig[7]);
      fBase->ReconfigureAnalyzer(EmulatorName, "SLThreshold", fL0EmulatorConfig[8]);
    }
    if(i==3){// L0MUV3
      fBase->ReconfigureAnalyzer(EmulatorName, "RunPeriod",   fL0EmulatorConfig[9]);
    }
    if(i==4){// L0NewCHOD
      fBase->ReconfigureAnalyzer(EmulatorName, "RunPeriod",   fL0EmulatorConfig[10]);
    }
  } // end loop over detectors
}

// Bool_t L0PrimitiveHandler::CheckPrimitives(TString Name, Double_t reftime, Bool_t isEmulate){
//   // Warning: this function will convert double reference time to int!!
//   Int_t ref = reftime/TdcCalib;
//   return CheckPrimitives(Name, ref, isEmulate);
// }

Bool_t L0PrimitiveHandler::CheckEmulatedPrimitives(TString name, Int_t reftime){
  return CheckPrimitives(name, reftime, true);
}

Bool_t L0PrimitiveHandler::CheckPrimitives(TString name, Int_t reftime, Bool_t isEmulate){

  if(name.Length()==0) return false;

  Bool_t TotalCheck=true;

  // 1. Tokenize list.
  TString item;
  Int_t from=0;
  while(name.Tokenize(item, from, "-")){

    // 2. Check for entry with 'n' at the start
    //    which denotes a 'veto' condition
    TString item2( item(0,1) );
    Bool_t veto = false;
    if(item2.Contains("n") || item2.Contains("!")){
      veto = true;
      item = item(1, item.Length());
    }

    // 2b. Check for LKr or Calo entries and convert to simply "LKr"
    //     since it checks the same bit regardless.
    if(item.Contains("LKr"))  item = "LKr";
    if(item.Contains("Calo")) item = "LKr";

    // 3. Decode the item from name..
    //    into 'bit' and 'l0detector'
    Int_t L0Detector = 9999;
    Int_t Bit = 9999;
    DecodeItem(item, L0Detector, Bit);

    if(L0Detector==9999 || Bit==9999){
      cout << "[L0PrimitiveHandler] Warning: Could not determine";
      if(L0Detector==9999) cout << " L0Detector ";
      if(Bit==9999) cout << " PrimitiveBit " ;
      cout << " from the input word: " << item;
      cout << " Please check input." << endl;
      return false;
    }

    // cout << " From item " << item << " got detector " << L0Detector << endl;

    // 4. Check if the bit is '1'
    //    in the data of the L0TP
    // Bool_t result = HasPrimitiveBit(Bit, L0Detector, reftime);
    Bool_t   result = false;
    if(isEmulate) result = HasEmulatedBit(item, L0Detector, reftime);
    else          result = HasPrimitiveBit(Bit, L0Detector, reftime);

    if(fDebug){
      cout << " Item " << item
	   << " isEmulate " << isEmulate
	   << " L0Detector " << L0Detector
	   << " Bit " << Bit
	   << " Veto " << veto
	   << " result " << result;
    }

    // 5. Switch result if vetoing
    if( veto ) result = not result;

    // 6. Combine result with overall trigger response
    TotalCheck &= result;

    if(fDebug){
      cout << " result after veto " << result
	   << " decision so far " << TotalCheck
	   << endl;
    }
  }

  // 7. Return the result of checking each bit
  //    requested by the user.
  if(fDebug) cout << " final decision: " << TotalCheck << endl;
  return TotalCheck;
}

void L0PrimitiveHandler::DecodeItem(TString item, Int_t &L0Detector, Int_t &Bit){

  // two LUTs: one contains all bit names related to a detector
  // the other contains the bit positions as a function of run number
  L0Detector = GetL0Detector(item);
  if(L0Detector==9999) return;
  Bit        = GetBit(item, fRunNumber);
  return;
}

void L0PrimitiveHandler::PrintDetectorBits(){
  cout << "=====================================================" << endl;
  cout << "========= Printing all known detector bits ==========" << endl;
  cout << "=====================================================" << endl;
  std::vector<L0DetectorBits>::iterator it;
  for(it= fAllDetectorBits.begin(); it != fAllDetectorBits.end(); ++it){
    it->Print();
  }
  cout << "=====================================================" << endl;
}

void L0PrimitiveHandler::PrintKnownBitNames(){
  PrintKnownBitNames(fRunNumber);
}

void L0PrimitiveHandler::PrintKnownBitNames(Int_t runNumber){
  cout << "=====================================================" << endl;
  cout << "===== Printing known detector bits for run "
	    << runNumber << " =====" << endl;
  cout << "=====================================================" << endl;

  std::vector<L0DetectorBits>::iterator it;
  for(it= fAllDetectorBits.begin(); it != fAllDetectorBits.end(); ++it){
    if(runNumber < it->GetFirstRun())     continue ;
    if(runNumber > it->GetLastRun())      continue ;
    cout.width(7);
    cout << detkey[it->GetL0Detector()] << " ->";
    it->PrintBits();
  }
  cout << "=====================================================" << endl;
}

void L0PrimitiveHandler::PrintDetectorKey(){
  cout << "=====================================================" << endl;
  cout << "============ Printing L0 Detector key ===============" << endl;
  cout << "=====================================================" << endl;

  for(int i=0; i<7; ++i){
    cout << " Detector " << i << " is ";
    cout.width(7);
    cout << detkey[i]
	      << " (access as kL0" << detkey[i] << ")" << endl;
  }
  cout << " Detector 7 is    L0TP (for internal use only)" << endl;
}

Int_t L0PrimitiveHandler::GetL0Detector(TString item){
  std::map<TString, Int_t>::iterator result;
  result = fBitNameToDetectorID.find(item);
  if(result == fBitNameToDetectorID.end()){
    cout << " WARNING: Could not find a detector corresponding to bit-name "
	      << item << "." << endl;
    return 9999;
  }
  return result->second;
}

TString L0PrimitiveHandler::GetL0DetectorName(Int_t L0Detector){
  if(!CheckL0Detector(L0Detector)) return TString("");
  return detkey[L0Detector];
}

TString L0PrimitiveHandler::GetBitName(Int_t L0Detector, Int_t Bit){

  std::vector<L0DetectorBits>::iterator it;
  for(it= fAllDetectorBits.begin(); it != fAllDetectorBits.end(); ++it){
    if(L0Detector != it->GetL0Detector()) continue ;
    if(fRunNumber < it->GetFirstRun())    continue ;
    if(fRunNumber > it->GetLastRun())     continue ;
    return it->GetBit(Bit);
  }
  cout << " Could not find entry for run " << fRunNumber
       << " Detector " << L0Detector
       << " and Bit " << Bit << endl;
  return TString("");
}

Int_t L0PrimitiveHandler::GetBit(TString item, Int_t runNumber){
  Int_t L0Detector = GetL0Detector(item);
  std::vector<L0DetectorBits>::iterator it;
  for(it= fAllDetectorBits.begin(); it != fAllDetectorBits.end(); ++it){
    if(L0Detector != it->GetL0Detector()) continue;
    if(runNumber < it->GetFirstRun())     continue;
    if(runNumber > it->GetLastRun())      continue;
    Int_t bit = it->GetBit(item);
    if(bit==9999){
      cout << "[L0PrimitiveHandler] WARNING: Invalid bit name, " << item << " was given" << endl;
      cout << "(it might not be valid for run " << runNumber << ")" << endl;
      return 9999;
    }
    return bit;
  }
  cout << "[L0PrimitiveHandler] WARNING: No entry for this L0Detector/RunNumber was found" << endl;
  return -1;
}

Int_t L0PrimitiveHandler::GetL0TPGranularity(Int_t L0Detector){
  return fL0TPInfo[L0Detector].first;
}

Bool_t L0PrimitiveHandler::CheckCondition(Int_t L0Detector){
  if(!CheckL0Detector(L0Detector)) return false;
  Bool_t Bad1 = IsPrimitiveInSlot(L0Detector, kL0PreviousSlot);
  Bool_t Good = IsPrimitiveInSlot(L0Detector, kL0TriggerSlot);
  Bool_t Bad2 = IsPrimitiveInSlot(L0Detector, kL0NextSlot);
  return (Good && !Bad1 && !Bad2);
}

Bool_t L0PrimitiveHandler::IsPrimitiveInSlot(Int_t L0Detector, Int_t slot){
  if(!CheckL0Detector(L0Detector)) return false;
  if(!CheckL0Slot(slot))           return false;

  L0Primitive Prim = fL0TPData->GetPrimitive(slot, L0Detector);
  return Prim.GetPrimitiveID();
}

Int_t L0PrimitiveHandler::GetTriggerTime(Int_t L0Detector){

  if(!CheckL0Detector(L0Detector)) return -999;

  L0Primitive prim = fL0TPData->GetPrimitive(kL0TriggerSlot, L0Detector);
  Int_t primFT     = prim.GetFineTime();
  if(!CheckRefTime(primFT)) return -999;
  return primFT;
}

Int_t L0PrimitiveHandler::GetL0TriggerTime(){
  // NB does not work for runs where LKr was used as L0 reference detector!
  Int_t DT = fL0TPData->GetDataType();
  if(DT==0x10) return GetTriggerTime(kL0CHOD);
  else         return GetTriggerTime(kL0RICH);
}

void L0PrimitiveHandler::GetL0TPConfig(Int_t L0Detector, Int_t &bit, Int_t &cut){
  bit = -999;
  cut = -999;
  if(!CheckL0Detector(L0Detector)) return;

  // warning: reverse iterator!
  std::vector<L0DetectorBits>::reverse_iterator it;
  it = fAllDetectorBits.rbegin();
  for( ; it!=fAllDetectorBits.rend(); ++it){

    //find the correct entry for the L0TP
    if(7 != it->GetL0Detector())       continue;
    if(fRunNumber < it->GetFirstRun()) continue;
    if(fRunNumber > it->GetLastRun())  continue;

    TString scut = it->GetBit(L0Detector);
    cut = scut.Atoi();
    TString sbit = it->GetBit(7);
    bit = sbit.Atoi();
    return;
  }
  return;
}

void L0PrimitiveHandler::GetEmulatorConfig(){
  fL0EmulatorConfig.clear();

  // warning: reverse iterator!
  std::vector<L0DetectorBits>::reverse_iterator it;
  it = fAllDetectorBits.rbegin();
  for( ; it!=fAllDetectorBits.rend(); ++it){

    //find the correct entry for the EMULATOR
    if(8 != it->GetL0Detector())       continue;
    if(fRunNumber < it->GetFirstRun()) continue;
    if(fRunNumber > it->GetLastRun())  continue;

    // only need the first 10 bits of 16 available
    TString s="";
    for(Int_t j=0; j<11; ++j){
      s = it->GetBit(j);
      if(s.Length()==0){
	cout << "[L0PrimitiveHandler][GetEmulatorConfig]";
	cout << " Empty string returned from GetBit!" << endl;
      }
      fL0EmulatorConfig.push_back(s);
    }
  }
}


PrimInfo L0PrimitiveHandler::GetPrimitiveInfo(Int_t Ref, Int_t L0Detector, Int_t slot){

  // output is a vector of size three.
  PrimInfo primInfo(4,-999);

  if(!CheckRefTime(Ref))           return primInfo;
  if(!CheckL0Detector(L0Detector)) return primInfo;
  if(!CheckL0Slot(slot))           return primInfo;

  //Get L0TP config for this run.
  Int_t FineTimeBit = fL0TPInfo[L0Detector].first;
  Int_t TimeCut     = fL0TPInfo[L0Detector].second;
  if(!CheckL0TPInfo(FineTimeBit, TimeCut)) return primInfo;

  // Get the primitives in the slot
  L0Primitive Prim = fL0TPData->GetPrimitive(slot, L0Detector);

  // Get the primitive bits
  Int_t PrimID = Prim.GetPrimitiveID();

  // Get the primitive time w.r.t ref
  // Int_t PrimFT = PrimID == 0?-9999:fL0TPData->GetPrimitiveCorrectedFineTime(slot, L0Detector, FineTimeBit);
  Int_t PrimFT = PrimID == 0?-9999:Prim.GetCorrectedFineTime(slot, Ref, FineTimeBit);
  Int_t PrimDT = PrimFT-Ref;

  primInfo[0] = PrimID;
  primInfo[1] = PrimFT;
  primInfo[2] = PrimDT;
  primInfo[3] = TMath::Abs(PrimDT);
  return primInfo;
}

Int_t L0PrimitiveHandler::GetPrimitiveID(Int_t L0Detector, Int_t slot){
  if(!CheckL0Detector(L0Detector)) return -999;
  if(!CheckL0Slot(slot))           return -999;
  PrimInfo a = GetPrimitiveInfo(0, L0Detector, slot);
  return a[0];
}

Int_t L0PrimitiveHandler::GetPrimitiveCorrectedFT(Int_t Ref, Int_t L0Detector, Int_t slot){
  if(!CheckRefTime(Ref))           return -999;
  if(!CheckL0Detector(L0Detector)) return -999;
  if(!CheckL0Slot(slot))           return -999;
  PrimInfo a = GetPrimitiveInfo(Ref, L0Detector, slot);
  return a[1];
}

Int_t L0PrimitiveHandler::GetPrimitiveDT(Int_t Ref, Int_t L0Detector, Int_t slot){
  if(!CheckRefTime(Ref))           return -999;
  if(!CheckL0Detector(L0Detector)) return -999;
  if(!CheckL0Slot(slot))           return -999;
  PrimInfo a = GetPrimitiveInfo(Ref, L0Detector, slot);
  return a[2];
}

Int_t L0PrimitiveHandler::GetPrimitiveAbsDT(Int_t Ref, Int_t L0Detector, Int_t slot){
  if(!CheckRefTime(Ref))           return -999;
  if(!CheckL0Detector(L0Detector)) return -999;
  if(!CheckL0Slot(slot))           return -999;
  PrimInfo a = GetPrimitiveInfo(Ref, L0Detector, slot);
  return a[3];
}

Bool_t L0PrimitiveHandler::GetClosestPrimitiveInfo(Int_t Ref, Int_t L0Detector, PrimInfo& a){

  PrimInfo p[3];
  p[0] = GetPrimitiveInfo(Ref, L0Detector, kL0TriggerSlot);
  p[1] = GetPrimitiveInfo(Ref, L0Detector, kL0PreviousSlot);
  p[2] = GetPrimitiveInfo(Ref, L0Detector, kL0NextSlot);

  Int_t lowest = 999999;
  Bool_t found=false;
  for(int i=0;i<3;++i){
    if(!p[i][0])        continue;
    if(p[i][3]>=lowest) continue;
    lowest = p[i][3];
    a      = p[i];
    found  = true;
  }
  return found;
}

Bool_t L0PrimitiveHandler::HasPrimitiveBit(Int_t Bit, Int_t L0Detector, Int_t Ref){

  if(!CheckRefTime(Ref))           return false;
  if(!CheckL0Detector(L0Detector)) return false;
  if(!CheckPrimBit(Bit))           return false;

  //Get L0TP config for this run.
  Int_t FineTimeBit = fL0TPInfo[L0Detector].first;
  Int_t TimeCut     = fL0TPInfo[L0Detector].second;
  if(!CheckL0TPInfo(FineTimeBit, TimeCut)) return false;

  PrimInfo a=GetPrimitiveInfo(Ref, L0Detector, kL0PreviousSlot);
  PrimInfo b=GetPrimitiveInfo(Ref, L0Detector, kL0TriggerSlot);
  PrimInfo c=GetPrimitiveInfo(Ref, L0Detector, kL0NextSlot);

  Int_t Time0 = a[3];
  Int_t Time1 = b[3];
  Int_t Time2 = c[3];

  if(fDebug){
    cout << " From L0PrimitiveHandler... " << endl;

    cout << " A Time " << a[1];
    cout << " A DT " << a[2];
    cout << " A ADT " << a[3];
    cout << " A TimeCut " << (a[3] < TimeCut);
    cout << " A primID " << std::bitset<16>(a[0]);
    cout << endl;

    cout << " B Time " << b[1];
    cout << " B DT " << b[2];
    cout << " B ADT " << b[3];
    cout << " B TimeCut " << (b[3] < TimeCut);
    cout << " B primID " << std::bitset<16>(b[0]);
    cout << endl;

    cout << " C Time " << c[1];
    cout << " C DT " << c[2];
    cout << " C ADT " << c[3];
    cout << " C TimeCut " << (c[3] < TimeCut);
    cout << " C primID " << std::bitset<16>(c[0]);
    cout << endl;
  }

  Int_t PrimID = 0 ;
  // apply timing cut
  if (Time0 < TimeCut) PrimID |= a[0];
  if (Time1 < TimeCut) PrimID |= b[0];
  if (Time2 < TimeCut) PrimID |= c[0];

  // if(fDebug){
  //   cout << " Final ID " << std::bitset<16>(PrimID) << endl;
  //   cout << " Final bit " << Bit << endl;
  //   cout << " Final result " << ((PrimID>>Bit)&0x1) << endl;
  // }

  // return true if the requested bit is '1'
  return (PrimID>>Bit)&0x1 ;
}

Bool_t L0PrimitiveHandler::HasEmulatedBit(TString bit, Int_t L0Detector, Int_t Ref){

  // This function will check emulator outputs for the requested bit.
  // It uses the L0Detector to determine which Emulator to check.
  // The trigger Ref time (integer) is converted to a float for comparison
  // with the emulator outputs.

  // Get reference time.
  if(!CheckRefTime(Ref)) return false;
  Double_t RefTime = Ref*TdcCalib;

  // Get L0 cut
  if(!CheckL0Detector(L0Detector)) return false;
  Double_t L0Cut   = fL0TPInfo[L0Detector].second * TdcCalib;

  // Get name of emulated primitive list.
  TString onamea = "L0"+detkey[L0Detector]+"Emulator";
  TString onameb = ".EmulatedL0"+detkey[L0Detector]+"Primitives";

  // Get emulated primitives
  Analyzer::OutputState a;
  ClusVec* EmulatedPrimitives = (ClusVec*)fBase->GetOutput(onamea+onameb,a);

  // check yourself before you wreck yourself
  if(EmulatedPrimitives==nullptr || a==Analyzer::kOUninit){
    cout << "[L0PrimitiveHandler] No " << detkey[L0Detector] << " emulator!";
    cout << " You must include " << onamea << " as a preanalyzer in your config file.";
    cout << endl;
    return false;
  }

  // loop through emulated primitives and find valid ones.
  ClusVec::iterator Prim = EmulatedPrimitives->begin();
  for(; Prim != EmulatedPrimitives->end(); ++Prim){
    if(fabs(Prim->GetAverageTime()-RefTime)>L0Cut) continue;
    if(Prim->GetPrimID(bit)) return true;
  }
  return false;
}

inline Bool_t L0PrimitiveHandler::CheckL0Detector(Int_t L0Detector){
  if(L0Detector<0 || L0Detector>6 || L0Detector==5){
    cout << "[L0PrimitiveHandler] WARNING: Invalid value of L0Detector " << L0Detector << endl;
    return false;
  }
  return true;
}

inline Bool_t L0PrimitiveHandler::CheckRefTime(Int_t RefTime){
  if(RefTime<0 || RefTime>255){
    cout << "[L0PrimitiveHandler] WARNING: Invalid value of RefTime " << RefTime << endl;
    return false;
  }
  return true;
}

inline Bool_t L0PrimitiveHandler::CheckL0Slot(Int_t L0Slot){
  if(L0Slot<0 || L0Slot>2){
    cout << "[L0PrimitiveHandler] WARNING: Invalid value of L0Slot " << L0Slot << endl;
    return false;
  }
  return true;
}

inline Bool_t L0PrimitiveHandler::CheckPrimBit(Int_t Bit){
  if(Bit<0 || Bit>15){
    cout << "[L0PrimitiveHandler] WARNING: Invalid value of Bit " << Bit << endl;
    return false;
  }
  return true;
}

inline Bool_t L0PrimitiveHandler::CheckL0TPInfo(Int_t FineTimeBit, Int_t TimeCut){
  if(FineTimeBit < 0 || TimeCut < 0){
    cout << "[L0PrimitiveHandler] Warning: no L0TP info was found for this run." <<
      " FineTimeBit=" << FineTimeBit << " TimeCut=" << TimeCut << endl;
    return false;
  }
  return true;
}

