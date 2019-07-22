unsigned short isTELL1Header(unsigned long datum);
unsigned long TELL1EventSize(unsigned long datum);
unsigned short isTELL1DummyWord(unsigned long datum);
unsigned short isTELL1Error(unsigned long datum);
unsigned short isTELL1Measurement(unsigned long data);
unsigned short isTELL1Leading(unsigned long datum);
unsigned short isTELL1Trailing(unsigned long datum);
int TELL1ChannelNumber(unsigned long Datum);
long TELL1ChannelValue(unsigned long datum);
unsigned short isTELL1TimeStamp(unsigned long datum);
unsigned long TELL1TimeStamp(unsigned long datum);
unsigned long TELL1TimeStampBoard(unsigned long datum);

