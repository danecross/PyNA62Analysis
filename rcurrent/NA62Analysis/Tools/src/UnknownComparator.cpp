/*
 * UnknownComparator.cpp
 *
 *  Created on: 1 Nov 2016
 *      Author: ncl
 */

#include "UnknownComparator.hh"

#include <iostream>

UnknownComparator::UnknownComparator() {
}

UnknownComparator::~UnknownComparator() {
}

UnknownComparator::UnknownComparator(TString parent, TObject* obj1,
		TString name, TString className) :
		BaseComparator(parent, obj1), fName(name), fClass(className) {
}

UnknownComparator::UnknownComparator(TString parent, TObject* obj1,
		TObject* obj2, TString name, TString className) :
		BaseComparator(parent, obj1, obj2), fName(name), fClass(className) {
}

void UnknownComparator::readLeftStructure() {
}

void UnknownComparator::readRightStructure() {
}

bool UnknownComparator::compare() {
	BaseComparator::compare();
	if (f1)
		printWarning(TString::Format("Unknown object (%s)", f1->ClassName()));
	else if (f2)
		printWarning(TString::Format("Unknown object (%s)", f2->ClassName()));
	else
		printWarning(TString::Format("Unknown object: %s (%s)", fName.Data(), fClass.Data()));

	return true;
}
