/*
 * TH2Comparator.hh
 *
 *  Created on: 26 Jan 2017
 *      Author: ncl
 */

#ifndef TOOLS_INCLUDE_TH2COMPARATOR_HH_
#define TOOLS_INCLUDE_TH2COMPARATOR_HH_

#include "TH1Comparator.hh"

class TH2Comparator: public TH1Comparator {
public:
	TH2Comparator();
	TH2Comparator(TString parent, TObject *obj1);
	TH2Comparator(TString parent, TObject *obj1, TObject *obj2);

	virtual ~TH2Comparator();

	void readLeftStructure();
	void readRightStructure();

	virtual bool compare();

	virtual TString comparatorName() { return "TH2Comparator"; };

	virtual bool checkBins();
};

#endif /* TOOLS_INCLUDE_TH2COMPARATOR_HH_ */
