#include "Riostream.h"
#include "TDirectory.h"
#include "TObjArray.h"
#include "TMath.h"
#include "TString.h"

#include "CREAMRawDecoder.hh"
#include "SAVDigitizer.hh"
#include "SAVReconstruction.hh"

//#include "SAVGeometry.hh"

#include "TSpecialTriggerEvent.hh"
#include "FADCEvent.hh"
#include "TSAVDigi.hh"
//#include "TSAVHit.hh"


/// \class SAVDigitizer
/// \Brief
/// First implementation of the SAV digitizer.
/// \n
/// Takes the MC Hits and convert them into Digi to be readout by the Reconstruction.
/// \n
/// Fixed readout window (16 ADC points * ~25 ns = 400 ns) centered on the time of the first hit in the event.
/// \n
/// The center of the window can be change using the Offset variable at line 57 in ns.
/// \n
/// In the same way the width of the window can be changed using the variable Nsample in a range between 8 and 32 points at line 56.
/// \EndBrief




SAVDigitizer::SAVDigitizer(NA62VReconstruction* Reco) :
  NA62VDigitizer(Reco, "SAV") {

    fDigiEvent = new FADCEvent(TSAVDigi::Class());

  }

SAVDigitizer::~SAVDigitizer() {
}

TDetectorVEvent * SAVDigitizer::ProcessEvent(TDetectorVEvent * tEvent) {

    if (tEvent->GetHits()->GetClass()->InheritsFrom("TVDigi")
        || tEvent->IsA() == TSpecialTriggerEvent::Class())
        return tEvent;

    fDigiEvent->Clear();
    return fDigiEvent;

//    TEvent * SAEvent = (TMUV2Event*) tEvent;
//    
//    
//    (*(TVEvent*) fDigiEvent) = (*(TVEvent*) MUV2Event);    
}
