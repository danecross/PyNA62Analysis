/*
 * LKrClusterCorrectionFunctions.hh
 *
 *  Created on: 9 Dec 2016
 *      Author: ncl
 */

#ifndef TOOLSLIB_INCLUDE_LKRCLUSTERCORRECTIONFUNCTIONS_HH_
#define TOOLSLIB_INCLUDE_LKRCLUSTERCORRECTIONFUNCTIONS_HH_

#include <Rtypes.h>

class TRecoLKrCandidate;

namespace NA62Analysis {
namespace LKrClusterCorrectionFunctions {

  TRecoLKrCandidate* CreateCorrectedCandidate(const TRecoLKrCandidate *LCand, Bool_t isMC);
  Double_t CorrectedEnergy(const TRecoLKrCandidate *Lcand, Bool_t isMC);
  Double_t CorrectedEnergyData(Int_t NCells, Double_t E0);
  Double_t CorrectedEnergyMC(Int_t NCells, Double_t E0);

} /* namespace LKrClusterCorrectionFunctions */
} /* namespace NA62Analysis */

#endif /* TOOLSLIB_INCLUDE_LKRCLUSTERCORRECTIONFUNCTIONS_HH_ */
