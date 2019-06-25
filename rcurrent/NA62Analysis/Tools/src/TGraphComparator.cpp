/*
 * TGraphComparator.cpp
 *
 *  Created on: 1 Nov 2016
 *      Author: ncl
 */

#include "TGraphComparator.hh"

#include <TGraph.h>

TGraphComparator::TGraphComparator() {
}

TGraphComparator::TGraphComparator(TString parent, TObject* obj1) : BaseComparator(parent, obj1){
}

TGraphComparator::TGraphComparator(TString parent, TObject* obj1, TObject* obj2) : BaseComparator(parent, obj1, obj2){
}

TGraphComparator::~TGraphComparator() {
}

void TGraphComparator::readLeftStructure() {
}

void TGraphComparator::readRightStructure() {
}

bool TGraphComparator::compare() {
	BaseComparator::compare();
	TGraph* left = static_cast<TGraph*>(f1);
	TGraph* right = static_cast<TGraph*>(f2);

	//Compare npoints
	//Compare points
	//Compare title
	//Compare errors
	int nPointsL = left->GetN();
	int nPointsR = right->GetN();
	if(nPointsL!=nPointsR){
		printBad(TString::Format("NPoints (%i,%i)", nPointsL, nPointsR));
		return false;
	}

	double xL,yL;
	double xR,yR;
	for(int iPoint=0; iPoint<nPointsL; ++iPoint){
		left->GetPoint(iPoint, xL, yL);
		right->GetPoint(iPoint, xR, yR);
		if(xL!=xR || yL!=yR){
			printBad(TString::Format("Content (Point=%i, (%f,%f),(%f,%f))", iPoint, xL, yL, xR, yR));
			return false;
		}
		if( (left->GetErrorX(iPoint)!=right->GetErrorX(iPoint)) ||
			(left->GetErrorXhigh(iPoint)!=right->GetErrorXhigh(iPoint)) ||
			(left->GetErrorXlow(iPoint)!=right->GetErrorXlow(iPoint)) ||
			(left->GetErrorY(iPoint)!=right->GetErrorY(iPoint)) ||
			(left->GetErrorYhigh(iPoint)!=right->GetErrorYhigh(iPoint)) ||
			(left->GetErrorYlow(iPoint)!=right->GetErrorYlow(iPoint)) ){
			printBad(TString::Format("Errors (Point=%i)", iPoint));
			return false;
		}
	}

	if(TString(left->GetTitle()).CompareTo(right->GetTitle()) != 0){
		printBad(TString::Format("Title (%s,%s)", left->GetTitle(), right->GetTitle()));
		return false;
	}
	printGood("Identical");

	return true;
}

