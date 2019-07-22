#include<Rtypes.h>

#include <stdlib.h>
#include <stdio.h>

#include <math.h>
#include <string.h>

#include <stdarg.h>

#include "NA62Buffer.hh"
#include "CREAMBuffer.hh"

// CREAM-specific functions
UInt_t CREAMBlockLength(UInt_t datum){return (datum&M_CREAM_EVENT_LENGTH);}
