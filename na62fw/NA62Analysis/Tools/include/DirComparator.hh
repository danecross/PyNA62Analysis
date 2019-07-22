/*
 * DirComparator.h
 *
 *  Created on: 31 Oct 2016
 *      Author: ncl
 */

#ifndef TOOLS_DIRCOMPARATOR_H_
#define TOOLS_DIRCOMPARATOR_H_

#include <TDirectory.h>
#include "BaseComparator.hh"

class DirComparator: public BaseComparator {
public:
	DirComparator();
	DirComparator(TString parent, TObject *obj1);
	DirComparator(TString parent, TObject *obj1, TObject *obj2);

	virtual ~DirComparator();

	virtual void readLeftStructure(){ readStructure(fIsRoot, static_cast<TDirectory*>(f1), fParentName, fSubCompare, fNLeftObjects);}
	virtual void readRightStructure(){ readStructure(fIsRoot, static_cast<TDirectory*>(f2), fParentName, fSubCompare, fNRightObjects);}

	virtual bool compare();

	virtual TString comparatorName() { return "DirComparator"; };
};

#endif /* TOOLS_DIRCOMPARATOR_H_ */
