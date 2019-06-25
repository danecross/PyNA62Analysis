#ifndef TDCBRawDecoder_H
#define TDCBRawDecoder_H 1

#include "NA62VRawDecoder.hh"
#include "NA62VReconstruction.hh"

#include "TClonesArray.h"

#include "TH1D.h"

#include <vector>
#include <map>

class TDCBRawDecoder : public NA62VRawDecoder
{

  public:

    explicit TDCBRawDecoder(NA62VReconstruction*);
    ~TDCBRawDecoder();
    void DecodeSpecialTrigger(UInt_t * &, EventHeader *, UInt_t &, Int_t);
    virtual TDetectorVEvent *DecodeNextEvent(UInt_t *, EventHeader *, UInt_t *);
    void PrintASCIIWord(Int_t FPGAid, UInt_t* pBuffer, UInt_t iWord);
    void ParseRawDecoderSettingsFile(TString);
    void EndProcessing();
    void StartOfBurst();
    void EndOfBurst();

    Int_t * GetNFPGAs()                       { return fNFPGAs;                          };
    void    SetNFPGAs(Int_t* buffer)          { fNFPGAs=buffer;                          };
    Int_t   GetNFPGAs(Int_t i)                { if(fNFPGAs && i<fNROBoards) return fNFPGAs[i]; else return 0;  };
    void    SetNFPGAs(Int_t N,Int_t* buffer)  { for(Int_t i=0;i<N;i++) if(fNFPGAs && i<fNROBoards) fNFPGAs[i]=buffer[i]; };
    TH2F *  GetHTEL62Errors()                 { return fHTEL62Errors;                    };
    Int_t   FindTEL62ErrorType(TString);
    Int_t   GetNCriticalErrorTypes()          { return (Int_t)TDCBDecoderErr::TDCB_WRONG_NERRWORDS; } //number of critical errors currently defined

  private:

    Int_t*   fNFPGAs;
    ULong_t ** fDatumBuffer;    //for debug
    Int_t ** fSlotBuffer;       //for debug
    UInt_t * fNRepeatedWords;   //for debug
    UInt_t * fNWordsPerChannel; //for debug

    // TODO use c++11 style enums when available
    enum class TDCBDecoderErr : int {
      ///// critical errors
      TDCB_TEL62_WRONG_ID     =1, //critical
      TDCB_TRIG_TYPE_MISMATCH   , //critical
      TDCB_BAD_SPECIAL_TRIGGER  , //critical
      TDCB_TDC_WRONG_ID         , //critical
      TDCB_FPGA_WRONG_ID        , //critical
      TDCB_FPGA_MISSING         , //critical
      TDCB_BAD_DATA_SIZE        , //critical
      TDCB_DATA_TYPE            , //critical
      TDCB_BLOCKTS_MISMATCH     , //critical
      TDCB_BAD_TRIGGERTS        , //critical
      TDCB_BAD_SLOT             , //critical
      TDCB_LEFT_SLOT_OVERFLOW   , //critical
      TDCB_RIGHT_SLOT_OVERFLOW  , //critical
      TDCB_WRONG_NERRWORDS      , //critical
      ///// non-critical errors
      TDCB_REPEATED_WORD        ,
      TDCB_NOT_TIME_ORDER       ,
      TDCB_WRONG_ERRORFLAG      ,
      TDCB_FAKE_EOB             ,
      TDCB_TEL62_ERROR          ,
      TDCB_TDCB_ERR             , // note TDCB_ERROR is defined in TDCBBuffer.hh
      TDCB_MASKED_CH
    }; 

    std::map<TDCBDecoderErr, TString> fMapTDCBError = {
      {TDCBDecoderErr::TDCB_TEL62_WRONG_ID     ,"Wrong TEL62 ID"},        ///< Wrong TEL62 ID (or mismatch wrt TEL62ID from Header)
      {TDCBDecoderErr::TDCB_TRIG_TYPE_MISMATCH ,"Trig type mismatch"},    ///< Trigger type from Block differs from TriggerType from EventHeader
      {TDCBDecoderErr::TDCB_BAD_SPECIAL_TRIGGER,"Bad Special Trigger"},   ///< Not yet supported SpecialTrigger is received
      {TDCBDecoderErr::TDCB_TEL62_ERROR        ,"TEL62 Error"},           ///< TEL62 Error detected in data [see fHTEL62Errors]
      {TDCBDecoderErr::TDCB_TDCB_ERR           ,"TDBC Error"},            ///< TDCB Error word detected in data
      {TDCBDecoderErr::TDCB_REPEATED_WORD      ,"Repeated word"},         ///< Repeated word detected in data
      {TDCBDecoderErr::TDCB_NOT_TIME_ORDER     ,"Not in time order"},     ///< Hits from same channel are not time-ordered
      {TDCBDecoderErr::TDCB_FPGA_WRONG_ID      ,"Wrong FPGA ID"},         ///< Wrong FPGA ID
      {TDCBDecoderErr::TDCB_TDC_WRONG_ID       ,"TDC/FPGA mismatch"},     ///< ChannelID and FPGAID of the same hit are not consistent
      {TDCBDecoderErr::TDCB_LEFT_SLOT_OVERFLOW ,"Left slot overflow"},    ///< Hit is in the wrong slot [on an earlier slot wrt to correct one]
      {TDCBDecoderErr::TDCB_RIGHT_SLOT_OVERFLOW,"Right slot overflow"},   ///< Hit is in the wrong slot [on an later slot wrt to correct one]
      {TDCBDecoderErr::TDCB_DATA_TYPE          ,"Wrong data type"},       ///< TDC Hit is neither a leading, trailing nor an error 
      {TDCBDecoderErr::TDCB_WRONG_ERRORFLAG    ,"Wrong error flag"},      ///< Wrong error flag [flag = 1 even if no error frame]
      {TDCBDecoderErr::TDCB_WRONG_NERRWORDS    ,"Wrong NErrorWords"},     ///< Inconsistency between number of error words
      {TDCBDecoderErr::TDCB_FPGA_MISSING       ,"Missing FPGA"},          ///< Expected FPGA is not found in data
      {TDCBDecoderErr::TDCB_BAD_DATA_SIZE      ,"Bad data size"},         ///< Expected block size differs from the size of the read block
      {TDCBDecoderErr::TDCB_BLOCKTS_MISMATCH   ,"Block TS mismatch"},     ///< Block TimeStamp differs from Trigger TimeStamp 
      {TDCBDecoderErr::TDCB_BAD_TRIGGERTS      ,"Bad trigger TS"},        ///< All the slots are inconsistent with the Trigger TimeStamp
      {TDCBDecoderErr::TDCB_BAD_SLOT           ,"Slot/Trigger mismatch"}, ///< Some slots (but not all of them) are inconsistent with the Trigger TimeStamp
      {TDCBDecoderErr::TDCB_FAKE_EOB           ,"Fake EOB"},              ///< Fake EOB detected (real EOB is missing)
      {TDCBDecoderErr::TDCB_MASKED_CH          ,"Hit from Masked Ch"},    ///< At least one hit from a masked channel
    };

    enum class TEL62DecoderErr : int {
      TEL62_TDC_FATAL=1,
      TEL62_TDC_ERROR,
      TEL62_IB_ERROR,
      TEL62_OB_ERROR,
      TEL62_ORGANIZER_ERROR,
      TEL62_COMPRESSOR_OVERFLOW,
      TEL62_HITTS_MISMATCH,
      TEL62_INTERNAL_COMPRESSOR,
      TEL62_DDR_ERROR,
      TEL62_TDCB_GLOBAL_ERROR,
      TEL62_FRAMETS_TIMEOUT,
      TEL62_LIMITER_ON,
      TEL62_SUPPRESSOR_ON
    }; 

    std::vector<std::vector<TString> > fTEL62ErrString;
    std::vector<std::vector<Int_t  > > fTEL62ErrType;

    Bool_t fFPGAFlags[5];

    TH2F * fHTEL62Errors;   ///< Histogram with specific-TEL62 errors in [one histo for each detector (MezzanineID on the x)]

};
#endif
