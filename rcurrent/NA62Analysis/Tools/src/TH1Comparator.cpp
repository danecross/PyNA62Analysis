/*
 * TH1Comparator.cpp
 *
 *  Created on: 1 Nov 2016
 *      Author: ncl
 */

#include "TH1Comparator.hh"

#include <TH1.h>
#include <iostream>

TH1Comparator::TH1Comparator() {
}

TH1Comparator::TH1Comparator(TString parent, TObject* obj1) : BaseComparator(parent, obj1){
}

TH1Comparator::TH1Comparator(TString parent, TObject* obj1, TObject* obj2) : BaseComparator(parent, obj1, obj2){
}

TH1Comparator::~TH1Comparator() {
}

void TH1Comparator::readLeftStructure() {
}

void TH1Comparator::readRightStructure() {
}

bool TH1Comparator::checkBins() {
	TH1* left = static_cast<TH1*>(f1);
	TH1* right = static_cast<TH1*>(f2);

	int nbinsL = left->GetXaxis()->GetNbins();
	int nbinsR = right->GetXaxis()->GetNbins();
	if(nbinsL!=nbinsR){
		printBad(TString::Format("X-NBins (%i,%i)", nbinsL, nbinsR));
		return false;
	}
	if(left->GetXaxis()->GetBinLowEdge(0)!=right->GetXaxis()->GetBinLowEdge(0)){
		printBad(TString::Format("X-LowEdge (%f,%f)", left->GetXaxis()->GetBinLowEdge(0), right->GetXaxis()->GetBinLowEdge(0)));
		return false;
	}
	if(left->GetXaxis()->GetBinUpEdge(nbinsL)!=right->GetXaxis()->GetBinUpEdge(nbinsR)){
		printBad(TString::Format("X-UpEdge (%f,%f)", left->GetXaxis()->GetBinUpEdge(nbinsL), right->GetXaxis()->GetBinUpEdge(nbinsR)));
		return false;
	}
	return true;
}

bool TH1Comparator::checkGeneral() {
	TH1* left = static_cast<TH1*>(f1);
	TH1* right = static_cast<TH1*>(f2);
	if(left->GetEntries()!=right->GetEntries()){
		printBad(TString::Format("NEntries (%f,%f)", left->GetEntries(), right->GetEntries()));
		return false;
	}
	if(TString(left->GetTitle()).CompareTo(right->GetTitle()) != 0){
		printBad(TString::Format("Title (%s,%s)", left->GetTitle(), right->GetTitle()));
		return false;
	}
	return true;
}

bool TH1Comparator::compare() {
	BaseComparator::compare();
	TH1* left = static_cast<TH1*>(f1);
	TH1* right = static_cast<TH1*>(f2);

	//Compare nbins
	//Compare histogram edges
	//Compare number of entries
	//Compare number of entries in each bin

	if(!checkGeneral())
		return false;
	if(!checkBins())
		return false;

	for(int iBin=0; iBin<left->GetXaxis()->GetNbins(); ++iBin){
		if(left->GetBinContent(iBin)!=right->GetBinContent(iBin) && !isnan(left->GetBinContent(iBin)) && !isnan(right->GetBinCenter(iBin))){
			printBad(TString::Format("Content (bin=%i, %f,%f)", iBin, left->GetBinContent(iBin), right->GetBinContent(iBin)));
			return false;
		}
	}

	printGood("Identical");

	return true;
}
