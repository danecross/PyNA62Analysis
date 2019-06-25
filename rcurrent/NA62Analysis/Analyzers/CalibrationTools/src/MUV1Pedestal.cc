// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2017-07-25
//
// ---------------------------------------------------------

#include "MUV1Pedestal.hh"

MUV1Pedestal::MUV1Pedestal(Core::BaseAnalysis *ba) : PedestalEvaluation(ba, "MUV1") {
  fEqualisationValue = 1000.;  ///< Pedestal value used for the equalisation (LKr = 400., MUV1/2 = 1000.)
}
