#include <stdlib.h>
#include <stdio.h>
                                                                                                          
#include <math.h>
#include <string.h>
                                                                                                          
#include <stdarg.h>

#include "TDCBBuffer.hh"
#include "NA62Buffer.hh"

// Board Header (TEL62-specific)
int TDCBFormatIdentifier (unsigned long datum) {return (datum & TDCB_FORMAT_IDENTIFIER_MASK) >> TDCB_FORMAT_IDENTIFIER_RSHIFT;}
int TDCBDetectorSubType (unsigned long datum) {return (datum & TDCB_DATA_SOURCE_SUB_ID_MASK)>>TDCB_DATA_SOURCE_SUB_ID_RSHIFT;}
int TDCBL0TriggerType (unsigned long datum) {return (datum & TDCB_L0_TRIGGER_TYPE_MASK)>>TDCB_L0_TRIGGER_TYPE_RSHIFT;}
unsigned long FPGAIDBoardHeader (unsigned long datum) {return (datum & TDCB_FPGA_ID_BOARD_HEADER_MASK)>>TDCB_FPGA_ID_BOARD_HEADER_RSHIFT;}
// FPGA Header
int TDCBFlagError (unsigned long datum) {return (datum & TDCB_FLAG_ERROR_MASK)>>TDCB_FLAG_ERROR_RSHIFT;}
unsigned long FPGAID (unsigned long datum) {return (datum & TDCB_FPGA_ID_MASK)>>TDCB_FPGA_ID_RSHIFT;}
int TDCBNSlots (unsigned long datum) {return ((datum & TDCB_NUMBER_OF_SLOT_MASK)>>TDCB_NUMBER_OF_SLOT_RSHIFT);}
//FPGA Header for Special Triggers
int TDCBNumberOfSpecialWords (unsigned long datum) {return (datum & TDCB_SPECIAL_TRIGGER_NWORDS_MASK)>>TDCB_SPECIAL_TRIGGER_NWORDS_RSHIFT;}
// Slot Header
int TDCBNSlotWords (unsigned long datum) {return ((datum & TDCB_NUMBER_OF_WORDS)>>TDCB_NUMBER_OF_WORDS_RSHIFT);}
int TDCBSlotTimeStamp (unsigned long datum) {return ((datum & TDCB_SLOT_TIME_STAMP)>>TDCB_SLOT_TIME_STAMP_RSHIFT);}
// Slot Data
bool isTDCBLeading (unsigned long datum) {return ((datum & TDCB_TYPE_MASK)==TDCB_LEADING);}
bool isTDCBTrailing (unsigned long datum) {return ((datum & TDCB_TYPE_MASK)==TDCB_TRAILING);}
bool isTDCBError (unsigned long datum) {return ((datum & TDCB_TYPE_MASK)==TDCB_ERROR);}
int TDCID (unsigned long datum) {return (((datum & TDCB_TDC_MASK)>>TDCB_TDC_RSHIFT) );}
unsigned int TDCBChannelNumber (unsigned long datum) {return (((datum & TDCB_CHANNEL_MASK)>>TDCB_CHANNEL_RSHIFT) | ((datum & TDCB_TDC_MASK)>>TDCB_CHANNEL_RSHIFT) );}
unsigned long TDCBChannelValue (unsigned long datum) {return (datum & TDCB_DATA_MASK);}
// FPGA IDs
int FlagPPFPGA0 (unsigned long datum) {return (datum & TDCB_FLAG_PP_FPGA0_MASK)>>TDCB_PP_FPGA0_FLAG_RSHIFT;}
int FlagPPFPGA1 (unsigned long datum) {return (datum & TDCB_FLAG_PP_FPGA1_MASK)>>TDCB_PP_FPGA1_FLAG_RSHIFT;}
int FlagPPFPGA2 (unsigned long datum) {return (datum & TDCB_FLAG_PP_FPGA2_MASK)>>TDCB_PP_FPGA2_FLAG_RSHIFT;}
int FlagPPFPGA3 (unsigned long datum) {return (datum & TDCB_FLAG_PP_FPGA3_MASK)>>TDCB_PP_FPGA3_FLAG_RSHIFT;}
int FlagSLFPGA  (unsigned long datum) {return (datum & TDCB_FLAG_SL_FPGA_MASK) >>TDCB_SL_FPGA_FLAG_RSHIFT; }

// Error frame
int TDCBNErrorWords(unsigned long datum) {return ((datum & TDCB_NUMBER_OF_ERROR_WORDS)>>TDCB_NUMBER_OF_ERROR_WORDS_RSHIFT);}
int TDCBNErrorWordsFrame0(unsigned long datum) {return ((datum & TDCB_NUMBER_OF_ERROR_WORDS_FRAME0)>>TDCB_NUMBER_OF_ERROR_WORDS_FRAME0_RSHIFT);}
int TDCBNErrorWordsFrame1(unsigned long datum) {return ((datum & TDCB_NUMBER_OF_ERROR_WORDS_FRAME1)>>TDCB_NUMBER_OF_ERROR_WORDS_FRAME1_RSHIFT);}

//TEL62 Error flags
int TEL62ErrorWordID(unsigned long datum) {return ((datum & TDCB_TEL62_ERROR_WORD_ID_MASK)>>TDCB_TEL62_ERROR_WORD_ID_RSHIFT);}
