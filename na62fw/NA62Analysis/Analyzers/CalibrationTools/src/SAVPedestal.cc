// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2017-07-25
//
// ---------------------------------------------------------

#include "SAVPedestal.hh"

SAVPedestal::SAVPedestal(Core::BaseAnalysis *ba) : PedestalEvaluation(ba, "SAV") {
  fEqualisationValue = 1000.;  ///< Pedestal value used for the equalisation (LKr = 400., MUV1/2 = 1000.)
}
