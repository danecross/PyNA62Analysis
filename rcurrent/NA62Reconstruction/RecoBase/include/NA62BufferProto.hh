// Block Header (generic)
int DataBlockFormat (unsigned long);
int DetectorSubType (unsigned long); 
int DataBlockSize (unsigned long); 
unsigned long DataBlockTimeStamp(unsigned long);
// Special triggers
bool isL0SpecialTrigger (int); //check if the trigger is not a physics trigger
bool isL0SpecialFrame (int);   //check if the frame sent after receiving the trigger is a special frame
bool isL0SOB (int);
bool isL0EOB (int);
bool isL0CHOKEON (int);
bool isL0CHOKEOFF (int);
//Special trigger Dim EOB header
bool isDIMSpecialTrigger (int);
bool isDIMSOB (int);
bool isDIMEOB (int);
int DIMEOBSubDetID (unsigned long);
int DIMEOBNumberOfWords (unsigned long);
