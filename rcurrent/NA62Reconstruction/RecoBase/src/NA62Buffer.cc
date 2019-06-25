#include <stdlib.h>
#include <iostream>
                                                                                                          
#include <math.h>
#include <string.h>
                                                                                                          
#include <stdarg.h>

#include "NA62Buffer.hh"
// Block Header (generic)
int DataBlockFormat (unsigned long datum) {return (datum & DATA_BLOCK_FORMAT_MASK)>>DATA_BLOCK_FORMAT_RSHIFT;}
int DetectorSubType (unsigned long datum) {return (datum & DATA_SOURCE_SUB_ID_MASK)>>DATA_SOURCE_SUB_ID_RSHIFT;}
int DataBlockSize (unsigned long datum) {return (datum & DATA_BLOCK_SIZE_MASK)>>DATA_BLOCK_SIZE_RSHIFT;}
unsigned long DataBlockTimeStamp(unsigned long datum){return (datum & DATA_BLOCK_TIMESTAMP_MASK)>>DATA_BLOCK_TIMESTAMP_RSHIFT;}
// Special triggers
bool isL0SpecialTrigger (int datum) {return ((datum & L0_SPECIAL_TRIGGER_MASK)!=0);}
bool isL0SpecialFrame (int datum) {
  bool isL0SpecialFrame = true;
  uint32_t TriggerType = (datum & L0_TRIGGER_TYPE_MASK)>>L0_TRIGGER_TYPE_RSHIFT;
  if(TriggerType<0x80)                                isL0SpecialFrame = false; //physics trigger
  else if(TriggerType == 0xa0)                        isL0SpecialFrame = false; //monitoring trigger
  else if(TriggerType == 0xb0 || TriggerType == 0xb4) isL0SpecialFrame = false; //random trigger
  else if(0xc0 <= TriggerType && TriggerType <= 0xfc) isL0SpecialFrame = false; //calibration trigger
  return isL0SpecialFrame;
}
bool isL0SOB (int datum) {return (((datum&L0_TRIGGER_TYPE_MASK)>>L0_TRIGGER_TYPE_RSHIFT)==L0_SOB);}
bool isL0EOB (int datum) {return (((datum&L0_TRIGGER_TYPE_MASK)>>L0_TRIGGER_TYPE_RSHIFT)==L0_EOB);}
bool isL0CHOKEON (int datum)  {return (((datum&L0_TRIGGER_TYPE_MASK)>>L0_TRIGGER_TYPE_RSHIFT)==L0_CHOKE_ON);}
bool isL0CHOKEOFF (int datum) {return (((datum&L0_TRIGGER_TYPE_MASK)>>L0_TRIGGER_TYPE_RSHIFT)==L0_CHOKE_OFF);}
//Special trigger Dim EOB header
bool isDIMSpecialTrigger (int datum) {return ((datum & DIM_SPECIAL_TRIGGER_DATABLOCK)==DIM_SPECIAL_TRIGGER_DATABLOCK);}
bool isDIMSOB (int /*datum*/) {return 0;}                          //for the time being only DIM EOB
bool isDIMEOB (int datum) {return isDIMSpecialTrigger(datum);} //for the time being only DIM EOB
int DIMEOBSubDetID (unsigned long datum) {return (datum & DIM_SPECIAL_TRIGGER_SUBID)>>DIM_SPECIAL_TRIGGER_SUBID_RSHIFT;}
int DIMEOBNumberOfWords (unsigned long datum) {return (datum & DIM_SPECIAL_TRIGGER_NUMBER_OF_WORDS)>>DIM_SPECIAL_TRIGGER_NUMBER_OF_WORDS_RSHIFT;}
