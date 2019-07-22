#ifndef STRAWRESPONSE_H
#define STRAWRESPONSE_H 1

#include "TH1.h"
#include "TRandom3.h"
#include <memory>

class StrawResponse
{
public:
   explicit StrawResponse(TRandom3*);
  ~StrawResponse() = default;

   Float_t RTS(float xh);      // RT-scaling function for the simple simulation
   Double_t TimeSimulated(Double_t radius);       // Simulation of T based on R
   Double_t RadiusReconstructed(Double_t time);   // Calculation of R from T for digitization
   Bool_t StrawInefficient(Double_t radius);      // Straw inefficiency simulation

private:
   std::unique_ptr<TH1F> fHt;   ///< Histogram for random scaling sampling
   TRandom3* fRandom; ///< Pointer to random number generator from SpectrometerDigitizer
};
#endif
