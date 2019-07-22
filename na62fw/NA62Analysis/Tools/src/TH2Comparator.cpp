/*
 * TH2Comparator.cpp
 *
 *  Created on: 26 Jan 2017
 *      Author: ncl
 */

#include "TH2Comparator.hh"

#include <TH2.h>
#include <iostream>

TH2Comparator::TH2Comparator() {
}

TH2Comparator::TH2Comparator(TString parent, TObject* obj1) : TH1Comparator(parent, obj1){
}

TH2Comparator::TH2Comparator(TString parent, TObject* obj1, TObject* obj2) : TH1Comparator(parent, obj1, obj2){
}

TH2Comparator::~TH2Comparator() {
}

void TH2Comparator::readLeftStructure() {
}

void TH2Comparator::readRightStructure() {
}

bool TH2Comparator::checkBins() {
	if(!TH1Comparator::checkBins())
		return false;

	TH1* left = static_cast<TH1*>(f1);
	TH1* right = static_cast<TH1*>(f2);

	int nbinsL = left->GetYaxis()->GetNbins();
	int nbinsR = right->GetYaxis()->GetNbins();
	if(nbinsL!=nbinsR){
		printBad(TString::Format("Y-NBins (%i,%i)", nbinsL, nbinsR));
		return false;
	}
	if(left->GetYaxis()->GetBinLowEdge(0)!=right->GetYaxis()->GetBinLowEdge(0)){
		printBad(TString::Format("Y-LowEdge (%f,%f)", left->GetYaxis()->GetBinLowEdge(0), right->GetYaxis()->GetBinLowEdge(0)));
		return false;
	}
	if(left->GetYaxis()->GetBinUpEdge(nbinsL)!=right->GetYaxis()->GetBinUpEdge(nbinsR)){
		printBad(TString::Format("Y-UpEdge (%f,%f)", left->GetYaxis()->GetBinUpEdge(nbinsL), right->GetYaxis()->GetBinUpEdge(nbinsR)));
		return false;
	}
	return true;
}

bool TH2Comparator::compare() {
	BaseComparator::compare();
	TH2* left = static_cast<TH2*>(f1);
	TH2* right = static_cast<TH2*>(f2);
	//Compare nbins
	//Compare histogram edges
	//Compare number of entries
	//Compare number of entries in each bin

	if(!checkGeneral())
		return false;
	if(!checkBins())
		return false;


	for(int iBinx=0; iBinx<left->GetXaxis()->GetNbins(); ++iBinx){
		for(int iBiny=0; iBiny<left->GetYaxis()->GetNbins(); ++iBiny){
			if(left->GetBinContent(iBinx, iBiny)!=right->GetBinContent(iBinx, iBiny)){
				printBad(TString::Format("Content (bin=(%i,%i), %f,%f)", iBinx, iBiny, left->GetBinContent(iBinx,iBiny), right->GetBinContent(iBinx,iBiny)));
				return false;
			}
		}
	}

	printGood("Identical");

	return true;
}
