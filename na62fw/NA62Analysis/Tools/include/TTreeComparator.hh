/*
 * TreeComparator.hh
 *
 *  Created on: 2 Nov 2016
 *      Author: ncl
 */

#ifndef TOOLS_INCLUDE_TTREECOMPARATOR_HH_
#define TOOLS_INCLUDE_TTREECOMPARATOR_HH_

#include "TBranchComparator.hh"
#include <TTree.h>

class TTreeComparator: public TBranchComparator {
public:
	TTreeComparator();
	TTreeComparator(TString parent, TObject *obj1);
	TTreeComparator(TString parent, TObject *obj1, TObject *obj2);

	virtual ~TTreeComparator();

	void readLeftStructure();
	void readRightStructure();

	virtual bool compare();
	virtual bool compareEntry();

	virtual TString comparatorName() { return "TTreeComparator"; };
};

#endif /* TOOLS_INCLUDE_TTREECOMPARATOR_HH_ */
