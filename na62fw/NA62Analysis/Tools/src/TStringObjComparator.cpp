/*
 * TStringObjComparator.cpp
 *
 *  Created on: 1 Nov 2016
 *      Author: ncl
 */

#include "TStringObjComparator.hh"

#include <TObjString.h>
#include <iostream>

TStringObjComparator::TStringObjComparator() {
}

TStringObjComparator::TStringObjComparator(TString parent, TObject* obj1) : BaseComparator(parent, obj1){
}

TStringObjComparator::TStringObjComparator(TString parent, TObject* obj1, TObject* obj2) : BaseComparator(parent, obj1, obj2){
}

TStringObjComparator::~TStringObjComparator() {
}

void TStringObjComparator::readLeftStructure() {
}

void TStringObjComparator::readRightStructure() {
}

bool TStringObjComparator::compare() {
	BaseComparator::compare();
	bool sim = static_cast<TObjString*>(f1)->GetString().CompareTo(static_cast<TObjString*>(f2)->GetString())==0;

	if(sim)
		printGood("Identical");
	else
		printBad("Different");
	return sim;
}
