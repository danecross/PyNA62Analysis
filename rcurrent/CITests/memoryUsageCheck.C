#include <iostream>

#include <TString.h>
#include <TFile.h>
#include <TGraph.h>
#include <TH1.h>

int doCheck(TString filePath) {
	TFile *fd = TFile::Open(filePath);

	if(!fd || !fd->IsOpen()) {
          std::cout << "Unable to open file " << filePath << std::endl;
          return -1;
	}

	bool fail = false;

	TGraph* vMem = static_cast<TGraph*>(fd->Get("VirtualMemory"));
	std::cout << "=== Virtual memory report ===" << std::endl;
	std::cout << "Maximum: " << vMem->GetHistogram()->GetMaximumStored() << " Average: " << vMem->GetMean(2) << " RMS: " << vMem->GetRMS(2) << std::endl;
	if(vMem->GetHistogram()->GetMaximumStored()>3000.) fail = true;

	TGraph* rMem = static_cast<TGraph*>(fd->Get("ResidentMemory"));
	std::cout << "=== Resident memory report ===" << std::endl;
	std::cout << "Maximum: " << rMem->GetHistogram()->GetMaximumStored() << " Average: " << rMem->GetMean(2) << " RMS: " << rMem->GetRMS(2) << std::endl;
	if(rMem->GetHistogram()->GetMaximumStored()>2000.) fail = true;

	return fail ? -1 : 0;
}

int memoryUsageCheck(TString filePath) {
	exit(doCheck(filePath));
}
