// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2017-07-25
//
// ---------------------------------------------------------

#include "MUV2Pedestal.hh"

MUV2Pedestal::MUV2Pedestal(Core::BaseAnalysis *ba) : PedestalEvaluation(ba, "MUV2") {
  fEqualisationValue = 1000.;  ///< Pedestal value used for the equalisation (LKr = 400., MUV1/2 = 1000.)
}
