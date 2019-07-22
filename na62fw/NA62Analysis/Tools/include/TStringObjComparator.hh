/*
 * TStringObjComparator.h
 *
 *  Created on: 1 Nov 2016
 *      Author: ncl
 */

#ifndef TOOLS_INCLUDE_TSTRINGOBJCOMPARATOR_HH_
#define TOOLS_INCLUDE_TSTRINGOBJCOMPARATOR_HH_

#include "BaseComparator.hh"

class TStringObjComparator: public BaseComparator {
public:
	TStringObjComparator();
	TStringObjComparator(TString parent, TObject *obj1);
	TStringObjComparator(TString parent, TObject *obj1, TObject *obj2);

	virtual ~TStringObjComparator();

	void readLeftStructure();
	void readRightStructure();

	virtual bool compare();

	virtual TString comparatorName() { return "TStringObjComparator"; };

};

#endif /* TOOLS_INCLUDE_TSTRINGOBJCOMPARATOR_HH_ */
