#ifndef __CINT__
#include "TROOT.h"
#include "TFileMerger.h"
#include "TFile.h"
#endif

using namespace std;
#include <iostream>
#include <sstream>
#include <fstream>
#include <istream>
#include <ostream>

#include "FixStreams.hh"

////////////////////////
/*
  Author: Simone Schuchmann
  Email: simone.schuchmannn@cern.ch

  Compile with
  g++ -I `root-config --incdir` -o ROOTFileMerger ROOTFileMerger.cpp `root-config --libs` --std=c++1y

  Execute with
  ./ROOTFileMerger input.list output.root
*/
///////////////////////

int Merger(const Char_t *fileList, const Char_t *outfile);
int Merger(const Char_t *fileList, const Char_t *outfile){

  int return_code = 0;

  //TFileMerger
  TFileMerger *m = new TFileMerger(kFALSE);
  m->OutputFile(outfile,"RECREATE");
  m->SetPrintLevel(1);

  //file list
  std::ifstream infiles(fileList);
  std::string line;

  int countF =0;
  //open file list and loop over it, add files to merger
  while (std::getline(infiles, line, '\n')) {
    if (line.empty())
      continue;
    TString fname = line.data();
    std::cout<<fname<<std::endl;

    TFile *f = TFile::Open(fname,"READ");
    if(f){
      bool checkadd = m->AddFile(f);

      if (!checkadd){
        return_code = 17;
        std::cout<<"ERROR: analyzer output file "<<fname<<" is corrupted."<<std::endl;
        return return_code;
      }
      else std::cout<<"--> adding file to merger"<<std::endl;
    }
    else {
     return_code = 17;
     std::cout<<"ERROR: analyzer output file "<<fname<<" does not exist."<<std::endl;
     return return_code;
    }

    f=NULL;
    delete f;
    countF++;
  }
  infiles.close();

  //Merge
  bool checkMerge = false;
  if(countF >1){
    checkMerge = m->Merge();
  }
  else{
    std::cout<<"Only one input file. Exiting. Change file name by hand if needed."<<std::endl;
    return_code = -18;
    return return_code;
  }

  if(checkMerge)
	  fixStreams(m->GetOutputFileName());
  delete m;
  m = NULL;

  if(checkMerge) {
    std::cout<<"merged single analyzer output files into: "<<outfile<<std::endl;
  }
  else{
    return_code = 18;
    cout<<"ERROR while merging anayzer output files."<<std::endl;
  }

  return return_code;
}


#ifndef __CINT__
int main(int argc, char** argv) {
  int return_code = -1;
  std::cout<<"Merger: input: "<<argv[1]<<" output: "<<argv[2]<<std::endl;
  if(argc != 3){
    std::cout<<"Merger: invalid number of arguments. Has to be 2: input file list and output name."<<std::endl;
    return return_code;
  }
  else return_code = Merger(argv[1],argv[2]);

  return return_code;
}
#endif
