/*
 * TBranchComparator.hh
 *
 *  Created on: 2 Nov 2016
 *      Author: ncl
 */

#ifndef TOOLS_INCLUDE_TBRANCHCOMPARATOR_HH_
#define TOOLS_INCLUDE_TBRANCHCOMPARATOR_HH_

#include "BaseComparator.hh"
#include <TBranchElement.h>

class TBranchComparator: public BaseComparator {
public:
	TBranchComparator();
	TBranchComparator(TString parent, TObject *obj1);
	TBranchComparator(TString parent, TObject *obj1, TObject *obj2);

	virtual ~TBranchComparator();

	void readLeftStructure();
	void readRightStructure();

	template <class T>
	static void readStructure(T* fd, TString parentName, map_comp &objects, int& nObjects);
	static bool isKnownType(TString type);

	virtual bool compare();
	virtual bool compareElement(TLeaf* ll, TLeaf* lr);
	virtual bool compareEntry();
	void printFinal();

	virtual TString comparatorName() { return "TBranchComparator"; };

protected:
	virtual bool compareSub();
	bool fGood;
	TString fError, fWarning;
};

#endif /* TOOLS_INCLUDE_TBRANCHCOMPARATOR_HH_ */
