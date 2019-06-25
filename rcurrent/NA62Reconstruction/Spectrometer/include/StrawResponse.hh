#ifndef STRAWRESPONSE_H
#define STRAWRESPONSE_H 1

#include "TFile.h"
#include "TVector3.h"
#include "TH2.h"
#include "TRandom3.h"

#define SMOOTH_EFF 0.985 /* General flat efficiency - must be set */
#define NODELTA /* No delta-electrons tail to avoid double counting of the effect */

class StrawResponse
{

public:

   StrawResponse(TRandom3*);
  ~StrawResponse();

   float RTS(float xh);                           // RT-scaling function for the simple simulation
   Double_t TimeSimulated(Double_t r);            // T simulation
   Double_t RadiusReconstructed(Double_t tt);     // calculation of R from T for digitization
   int StrawIneff(Double_t r);                    // Straw Inefficiency simulation

private:

    TH1F* fHt;    // histogram for the scaling sampling;

// Constants for the scaling shape approximation drawing
// (depend on the gas mixture). Described in the note
// http://na62.web.cern.ch/NA62/Documents/NotesDoc/na62_10_01.pdf
   float fD1,fD2,fD3,fD4,fD5,fD6,fD7,fD8,fD9,fD10;
   float fP1,fP2,fP3,fP4,fP5,fP6,fP7,fP8;
   Double_t fTmin,fTmax;

// Constants for the radius-dependent inefficiency, obtained from 
// GARFIELD simulation
   float fAeff, fBeff; 

   TRandom3* fRandom;
};
#endif
