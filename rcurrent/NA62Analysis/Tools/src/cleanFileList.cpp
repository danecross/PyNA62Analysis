#include <fstream>
#include <iostream>
#include <iterator>
#include <map>
#include <string>
#include <utility>
#include <vector>

#include <TFile.h>
#include <TString.h>
#include <TTree.h>

using namespace std;

TString getRootdPath(TString path){
	if (path.Contains("/castor/") && !path.Contains("root://castorpublic.cern.ch//"))
				path = "root://castorpublic.cern.ch//" + path;
	else if (path.Contains("/eos/") && !path.Contains("root://eosna62.cern.ch//"))
		path = "root://eosna62.cern.ch//" + path;

	return path;
}

int countBranches(TFile& fd){
	TTree* t = (TTree*)fd.Get("Reco");
	if(t==nullptr){
		cout << "TTree Reco not found in " << fd.GetName() << endl;
		return 0;
	}
	return t->GetNbranches();
}

bool checkStreamsTree(TFile& fd){
	TTree* t = (TTree*)fd.Get("Streams");
	if(t==nullptr){
		cout << "TTree Streams not found in " << fd.GetName() << endl;
		return false;
	}
	return true;
}

bool checkSpecialTriggerTree(TFile& fd){
	TTree* t = (TTree*)fd.Get("Reco");
	if(t!=nullptr){
		// We have a MC file
		t = (TTree*)fd.Get("SpecialTrigger");
		if(t==nullptr){
			cout << "TTree SpecialTrigger not found in MC file " << fd.GetName() << endl;
			return false;
		}
	}
	return true;
}

void cleanList(TString fileList, TString cleanList){
	string file;

	ifstream fd_list(fileList);
	if(!fd_list.is_open()){
		cout << "Unable to open input list" << endl;
		return;
	}

	map<int, std::vector<TString> > countMap;

	int nBranches;
	TString rootd;
	while(!fd_list.eof()){
		file = "";
		fd_list >> file;
		if(file.length()>0){
			rootd = getRootdPath(file);
			cout << "Reading file " << rootd << endl;
			TFile *fd = TFile::Open(rootd);
			bool badFile = false;

			if(!fd || !fd->IsOpen()){
				cout << "Unable to open ROOT file " << rootd << endl;
				badFile = true;
			}
			else{
				badFile = badFile || !checkStreamsTree(*fd);
				badFile = badFile || !checkSpecialTriggerTree(*fd);
			}
			if(badFile)
				continue;
			nBranches = countBranches(*fd);

			fd->Close();
			delete fd;
			if(countMap.count(nBranches)>0)
				countMap[nBranches].push_back(file);
			else{
				vector<TString> v;
				v.push_back(file);
				countMap.insert(make_pair(nBranches, v));
			}
		}
	}

	map<int, std::vector<TString> >::reverse_iterator it;
	vector<TString> goodList;
	int maxFiles=0, maxIndex=0;
	for(it = countMap.rbegin(); it!=countMap.rend(); ++it){
		int vSize = it->second.size();
		cout << "Found " << vSize << " files with " << it->first << " branches "<< endl;
		if(vSize>maxFiles){
			maxFiles = vSize;
			maxIndex = it->first;
		}
	}
	goodList = countMap[maxIndex];


	vector<TString>::iterator itv;
	ofstream fdOut(cleanList);
	if(!fdOut.is_open()){
		cout << "Unable to open output list" << endl;
		return;
	}
	for(itv = goodList.begin(); itv != goodList.end(); ++itv){
		fdOut << *itv << endl;
	}
	fdOut.close();
}

int main(int argc, char** argv){
	if(argc!=3){
		cout << "Usage:" << endl;
		cout << "cleanFileList <inputList> <outputList>" << endl;
		return 0;
	}

	cleanList(argv[1], argv[2]);
}
