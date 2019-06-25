/*
  Author: Nicolas Lurkin
  Email: nicolas.lurkin@cern.ch

  Fixes Streams tree by merging all the entries into a single one.
*/
#include <iostream>

#include <TFile.h>
#include <TTree.h>
#include "Stream.hh"
#include "FixStreams.hh"

bool fixStreams(TString fileName){
	TFile *outputFile = TFile::Open(fileName, "UPDATE");

	if(!outputFile){
		std::cout << "Unable to read file " << fileName << ". Aborting Streams merge" << std::endl;
		return false;
	}

	TTree* oldStreams = static_cast<TTree*>(outputFile->Get("Streams"));
	if(!oldStreams){
		std::cout << "No Streams tree found. Aborting Streams merge" << std::endl;
		return false;
	}
	else
		std::cout << "Merging Streams entries" << std::endl;

	TTree* newStreams = new TTree("newStreams", "Streams");

	Stream *newEvent = new Stream();
	Stream *oldEvent = nullptr;
	newStreams->Branch("Stream", &newEvent);
	oldStreams->SetBranchAddress("Stream", &oldEvent);

	for(Int_t iEvent=0; iEvent<oldStreams->GetEntries(); ++iEvent){
		oldStreams->GetEntry(iEvent);
		newEvent->UpdateAndMergeAttributes(*oldEvent);
	}

	outputFile->Delete("Streams;*");
	newStreams->Fill();
	newStreams->Write("Streams");
	return true;
}
