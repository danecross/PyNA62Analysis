#ifndef SRBRawDecoder_H
#define SRBRawDecoder_H 1

#include "NA62VRawDecoder.hh"
#include "NA62VReconstruction.hh"
#include "SRBEvent.hh"
#include "TSpectrometerDigi.hh"
#include "TSpecialTriggerEvent.hh"

#include "TClonesArray.h"
#include "T0Jumps.hh"
#include <vector>
#include <map>

#define M_STRAWID 0xff00000000000000

// Basic mask to extract 32 bit word
#define SRB_32BITWORD_MASK     0xffffffff
// Masks to extract variables from the first word
#define SRB_ID_MASK            0x0000001f     // SRB ID                 (5 bits)
#define SRB_FLAGS_MASK         0x000000e0     // reserved flags         (3 bits)
#define SRB_L0TRG_TYPE_MASK    0x0000ff00     // L0 trigger type        (8 bits)
#define SRB_PACKET_LENGTH_MASK 0xffff0000     // Packet length          (16 bits)
// Shifts to extract variables from the first word
#define SRB_ID_SHIFT             0
#define SRB_FLAGS_SHIFT          5
#define SRB_L0TRG_TYPE_SHIFT     8
#define SRB_PACKET_LENGTH_SHIFT 16
// Masks to extract data words
#define SRB_WORD1_MASK 0x0000FFFF
#define SRB_WORD2_MASK 0xFFFF0000
// Shifts to extract data words
#define SRB_WORD1_SHIFT 0
#define SRB_WORD2_SHIFT 16
// Masks to extract counters
#define SRB_COUNTER1_MASK 0x000000FF
#define SRB_COUNTER2_MASK 0x0000FF00
#define SRB_COUNTER3_MASK 0x00FF0000
#define SRB_COUNTER4_MASK 0xFF000000
// Shifts to extract counters
#define SRB_COUNTER1_SHIFT 0
#define SRB_COUNTER2_SHIFT 8
#define SRB_COUNTER3_SHIFT 16
#define SRB_COUNTER4_SHIFT 24
// Masks to decode hits
#define SRB_HIT_FINETIME_MASK 0x0000001F
#define SRB_HIT_EDGE_MASK     0x00000020
#define SRB_HIT_STRAWID_MASK  0x00003FC0
#define SRB_HIT_ERROR_MASK    0x0000C000
// Shifts to decode hits
#define SRB_HIT_FINETIME_SHIFT 0
#define SRB_HIT_EDGE_SHIFT     5
#define SRB_HIT_STRAWID_SHIFT  6
#define SRB_HIT_ERROR_SHIFT   14

typedef unsigned long int u_int64;

class SRBRawDecoder : public NA62VRawDecoder
{

public:
  explicit SRBRawDecoder(NA62VReconstruction*);
  ~SRBRawDecoder();
  virtual TDetectorVEvent * DecodeNextEvent(UInt_t*, EventHeader*, UInt_t*);
  void ParseRawDecoderSettingsFile(TString);
  void Init();
  void EndProcessing();
  void StartOfBurst();
  void EndOfBurst();
  Int_t GetLastCriticalErrorEnum() {return (Int_t)SRBDecoderErr::SRB_COUNTER_MISMATCH;}

public:
  UInt_t   GetSRBOffset()                       { return fSRBOffset; }
  void     SetSRBOffset(UInt_t value)           { fSRBOffset=value; }
  UInt_t*  GetSRBLatency()                      { return fSRBLatency; }
  void     SetSRBLatency(UInt_t * value)        { fSRBLatency=value; }
  UInt_t   GetSRBLatency(Int_t i)               { if(fSRBLatency && i<fNROMezzanines) return fSRBLatency[i]; else return 0; }
  void     SetSRBLatency(Int_t i, UInt_t value) { if(fSRBLatency && i<fNROMezzanines) fSRBLatency[i]=value; }
  Double_t GetMinRawTime()                      { return fMinTime;}
  Double_t GetMaxRawTime()                      { return fMaxTime;}
  void SetMinRawTime(Double_t value)            { fMinTime = value;}
  void SetMaxRawTime(Double_t value)            { fMaxTime = value;}
  static constexpr Double_t kTIMEBINSIZE = ClockPeriod / 32; // 32 = number of fine time slots

private:
  TString fConfigFileName;
  void AddSpectrometerHit(UInt_t hitWord, Int_t coarseTime, UInt_t srbID, EventHeader * pEventHeader);
  UInt_t   fSRBOffset;                 ///< offset between trigger TS and Block TS, common to all SRB
  UInt_t * fSRBLatency;               ///< array of latencies for each SRB, offset between Block TS and SRB trigger TS
  Double_t fMinTime;                  ///< minimum SRB raw time
  Double_t fMaxTime;                  ///< maximum SRB raw time
  T0Jumps *fT0j; // T0 jumps
  bool fIfToJump; // if to correct the jumps; 

  TH2F * fHFineTime; ///< Histo for monitoring fine time distribution from different SRBs

  enum class SRBDecoderErr : int {
    ///// critical errors
    SRB_HEADER_MISMATCH  = 0x1,
    SRB_OUT_OF_TIME      = 0x2,
    SRB_BLOCKTS_MISMATCH = 0x3,
    SRB_COUNTER_MISMATCH = 0x4,
    ///// non-critical errors
    SRB_PADDING_ERROR    = 0x5,
    SRB_REPEATED_WORD    = 0x6,
    SRB_MASKED_HIT       = 0x7
  };

  std::map<SRBDecoderErr, TString> fMapSRBError = {
    {SRBDecoderErr::SRB_HEADER_MISMATCH,  "Header mismatch"},
    {SRBDecoderErr::SRB_OUT_OF_TIME,      "Out-of-time event"}, ///< SRB data not belonging to the Block TS
    {SRBDecoderErr::SRB_BLOCKTS_MISMATCH, "Block TS mismatch"}, ///< Block TimeStamp minus fixed SRB latency differs from Trigger TimeStamp
    {SRBDecoderErr::SRB_COUNTER_MISMATCH, "Hit / Counter mismatch"},
    /// non-critical errors
    {SRBDecoderErr::SRB_PADDING_ERROR,    "Padding error"},
    {SRBDecoderErr::SRB_REPEATED_WORD,    "Repeated word"},
    {SRBDecoderErr::SRB_MASKED_HIT,       "Masked channel hit"},
  };

};
#endif
