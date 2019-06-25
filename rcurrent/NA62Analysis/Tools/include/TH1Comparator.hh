/*
 * TH1Comparator.h
 *
 *  Created on: 1 Nov 2016
 *      Author: ncl
 */

#ifndef TOOLS_INCLUDE_TH1COMPARATOR_HH_
#define TOOLS_INCLUDE_TH1COMPARATOR_HH_

#include "BaseComparator.hh"

class TH1Comparator: public BaseComparator {
public:
	TH1Comparator();
	TH1Comparator(TString parent, TObject *obj1);
	TH1Comparator(TString parent, TObject *obj1, TObject *obj2);

	virtual ~TH1Comparator();

	void readLeftStructure();
	void readRightStructure();

	virtual bool compare();

	virtual TString comparatorName() { return "TH1Comparator"; };

	virtual bool checkBins();
	virtual bool checkGeneral();
};

#endif /* TOOLS_INCLUDE_TH1COMPARATOR_HH_ */
