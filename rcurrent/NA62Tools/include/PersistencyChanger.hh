/*
 * PersistencyChanger.hh
 *
 *  Created on: 8 Apr 2019
 *      Author: nlurkin
 */

#ifndef INCLUDE_PERSISTENCYCHANGER_HH_
#define INCLUDE_PERSISTENCYCHANGER_HH_

#include <TString.h>

class TSlimRecoVEvent;
class TRecoVEvent;

std::pair<TString,TSlimRecoVEvent*> getSlimClassEquivalent(TString className);

std::pair<TSlimRecoVEvent*, TRecoVEvent*> getSlimAndRecoFromName(TString detName, void** objectAddress);
std::pair<TRecoVEvent*, TSlimRecoVEvent*> getRecoAndSlimFromName(TString detName, void** objectAddress);

#endif /* INCLUDE_PERSISTENCYCHANGER_HH_ */
