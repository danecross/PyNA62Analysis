/*
 * UnknownComparator.h
 *
 *  Created on: 1 Nov 2016
 *      Author: ncl
 */

#ifndef TOOLS_UNKNOWNCOMPARATOR_H_
#define TOOLS_UNKNOWNCOMPARATOR_H_

#include "BaseComparator.hh"

class UnknownComparator: public BaseComparator {
public:
	UnknownComparator();
	UnknownComparator(TString parent, TObject *obj1, TString name, TString className);
	UnknownComparator(TString parent, TObject *obj1, TObject *obj2, TString name, TString className);

	virtual ~UnknownComparator();

	virtual void readLeftStructure();
	virtual void readRightStructure();


	virtual bool compare();

	virtual TString comparatorName() { return "UnknownComparator"; };

private:
	TString fName, fClass;
};

#endif /* TOOLS_SRC_UNKNOWNCOMPARATOR_H_ */
