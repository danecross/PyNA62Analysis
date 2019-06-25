// Board Header (TEL62-specific)
int TDCBFormatIdentifier (unsigned long); 
int TDCBDetectorSubType (unsigned long); 
int TDCBL0TriggerType (unsigned long); 
unsigned long FPGAIDBoardHeader (unsigned long); 
// FPGA Header
int TDCBFlagError (unsigned long); 
unsigned long FPGAID (unsigned long); 
int TDCBNSlots (unsigned long); 
//FPGA Header for Special Triggers
int TDCBNumberOfSpecialWords (unsigned long datum);
// Slot Header
int TDCBNSlotWords (unsigned long); 
int TDCBSlotTimeStamp (unsigned long); 
// Slot Data
bool isTDCBLeading (unsigned long); 
bool isTDCBTrailing (unsigned long); 
bool isTDCBError (unsigned long); 
int TDCID (unsigned long); 
unsigned int TDCBChannelNumber (unsigned long); 
unsigned long TDCBChannelValue (unsigned long); 

// FPGA IDs
int FlagPPFPGA0 (unsigned long); 
int FlagPPFPGA1 (unsigned long); 
int FlagPPFPGA2 (unsigned long); 
int FlagPPFPGA3 (unsigned long); 
int FlagSLFPGA (unsigned long); 

// Error frame
int TDCBNErrorWords(unsigned long);
int TDCBNErrorWordsFrame0(unsigned long);
int TDCBNErrorWordsFrame1(unsigned long);

//TEL62 Error flags
int TEL62ErrorWordID(unsigned long);
