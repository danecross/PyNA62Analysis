/*
 * TGraphComparator.h
 *
 *  Created on: 1 Nov 2016
 *      Author: ncl
 */

#ifndef TOOLS_INCLUDE_TGRAPHCOMPARATOR_HH_
#define TOOLS_INCLUDE_TGRAPHCOMPARATOR_HH_

#include "BaseComparator.hh"

class TGraphComparator: public BaseComparator {
public:
	TGraphComparator();
	TGraphComparator(TString parent, TObject *obj1);
	TGraphComparator(TString parent, TObject *obj1, TObject *obj2);

	virtual ~TGraphComparator();

	void readLeftStructure();
	void readRightStructure();

	virtual bool compare();

	virtual TString comparatorName() { return "TGraphComparator"; };

};

#endif /* TOOLS_INCLUDE_TGRAPHCOMPARATOR_HH_ */
