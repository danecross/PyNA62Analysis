#ifndef __CINT__
#include "TROOT.h"
#include "TFileMerger.h"
#include "TList.h"
#include "TTree.h"
#include "TObject.h"
#include "TClass.h"
#include "TTree.h"
#include "TFile.h"
#include "TKey.h"
#endif

#include <iostream>
#include <sstream>
#include <fstream>
#include <istream>
#include <ostream>

////////////////////////
/*
  Author: Simone Schuchmann
  Email:  simone.schuchmannn@cern.ch

  Compile with
  g++ -I `root-config --incdir` -o RmObjFromRootF RmObjFromRootF.cpp `root-config --libs` --std=c++1y

  Execute with
  ./RmObjFromRootF input.root output.root skip.list

  To enable printout of more information run:
  ./RmObjFromRootF input.root output.root skip.list 1

  Task:
  Removes objects defined in skip.list from content list and saves the remaining objects including
  directories to the output file. If the object (e.g. Histo) to be removed is contained in a
  TDirectoryFile (e.g. RICHMonitor), the name of the directory has to be specified as well
  eg. RICHMonitor/Histo .

*/
///////////////////////

int SkipFromList(const Char_t *fInName, const Char_t *fOutName,const Char_t *fskip);
bool RmFromList(TList *keylist, TString nameObj);
bool LoopList(TDirectory *dirUp, TDirectory *dirIn, TList *keylist,Int_t level);
bool SaveObject(TDirectory *dirUp,TDirectory *dirIn,TKey *key);
bool EnterDirectory( TDirectoryFile *objDir,Int_t level,TString upname="");
TString namesObjDir[100];
static  Int_t countDir = 0;
Bool_t printDetails = false;
//___________________________________________________________________
int SkipFromList(const Char_t *fInName, const Char_t *fOutName,const Char_t *fskip){

  //input root file
  TFile *fin  = TFile::Open(fInName);

  //output file
  TFile *fout = TFile::Open(fOutName,"recreate");

  //get key (object) list
  TList *keylistFile = (TList*)fin->GetListOfKeys();


  //-------- remove objects from content list or store names of second level objects -------//

  //open list of objects to be skipped
  std::ifstream infileS(fskip);
  std::string lineS;

  std::cout<<"SkipFromList: --------------- Loop over list of objects/directories to be removed: -----------------"<<std::endl;
  std::cout<<"\n";
  if (infileS.is_open()) {
    while (std::getline(infileS, lineS, '\n')) {
      if (lineS.empty())
        continue;
      TString rlineS = lineS;
      std::cout<<"SkipFromList: To be removed: "<<lineS<<std::endl;
      if(rlineS.Contains("/")){//check for objects in directories (2nd level)
        std::cout<<"SkipFromList: -> contains '/'! ... removal in directory loop!"<<std::endl;
        std::cout<<"\n";
        namesObjDir[countDir] = lineS;
        countDir++;
      }
      else{//remove objects from list in first level
        bool rmOK = RmFromList(keylistFile,rlineS);
        if(!rmOK) std::cout<<"SkipFromList: ERROR: Could not remove "<<lineS<<"."<<std::endl;
        else  std::cout<<"SkipFromList: --> Removed "<<lineS<<"."<<std::endl;
        std::cout<<"\n";
      }
    }
    infileS.close();

    std::cout<<"\n";
    std::cout<<"SkipFromList: Summary: Found "<<countDir<<" objects in directories!"<<std::endl;
    std::cout<<"\n\n";
    std::cout<<"SkipFromList: ----------------------- Start saving the remaining output: ---------------------------"<<std::endl;
    std::cout<<"\n";

    //-------- save the rest to the new file -------//




    //loop over key list from input file and subsequent lists of (sub)directories
    //save the remaining objects to fout
    LoopList(fout, fin, keylistFile, 0);

    keylistFile = NULL;
    delete keylistFile;

    //closing
    fout->Close();

    delete fin;
    delete fout;


    std::cout<<"\n";
    std::cout<<"SkipFromList: ----------------------- Output file "<<fOutName<<" closed! ---------------------------"<<std::endl;
    return 0;
  }
  else{
    std::cout<<"[RmFromList] Error, skip list could not be opened!"<<std::endl;
    return 7;
  }
}
//___________________________________________________________________
bool RmFromList(TList *keylist, TString nameObj){
  //remove objects from key list

  TObject* objF = (TObject*)keylist->FindObject(nameObj.Data());
  if(!objF){
    std::cout<<"SkipFromList: ERROR: Object "<<nameObj.Data()<<" not found."<<std::endl;
    return false;
  }

  TObject* objR = keylist->Remove(objF);
  if(!objR)  return false;

  objF = NULL;
  objR = NULL;

  delete objF;
  delete objR;

  return true;

}

//___________________________________________________________________
bool LoopList(TDirectory *dirUp, TDirectory *dirIn, TList *keylist,Int_t level){
  //loop over key list and check for subdirectories
  //save objects if they not directory directly
  //otherwise to into subidrectory and repeat this procedure

  TKey *key = NULL;
  const Int_t keepLevel = level;

  TString nameDir = dirIn->GetName();

  //remove level>1 objects from subdirectory content list
  for(Int_t t = 0; t< countDir;t++){
    Int_t levelSkip = namesObjDir[t].CountChar('/');
    if(level == levelSkip){
      if(namesObjDir[t].Contains(nameDir)){
	Ssiz_t lastDash = namesObjDir[t].Last('/');
	TString nameOD = namesObjDir[t];
	TString cutnameOD = nameOD;
	cutnameOD.Remove(lastDash+1);
	nameOD.ReplaceAll(cutnameOD,"");

	bool rmOK =RmFromList(keylist,nameOD);
	if(!rmOK) std::cout<<"SkipFromList: ERROR: Could not remove "<<namesObjDir[t]<<"."<<std::endl;
	else std::cout<<"SkipFromList: --> Removed "<<namesObjDir[t]<<" from list."<<std::endl;
      }
    }
  }

  //enter subdirectories or save objects
  Bool_t countObj = false;
  TDirectoryFile *objDir = NULL;

  for(Int_t i =0;i<keylist->GetEntries();i++){
    key = (TKey*)keylist->At(i);
    TString nameObj = key->GetName();
    TString clname = key->GetClassName();

    if(clname.Contains("Directory") ){
      //get subdirectories

      objDir = (TDirectoryFile*)dirIn->Get(nameObj);

      //get content of subdirectories
      EnterDirectory(objDir,level,nameObj);
      dirUp->cd();

    }
    else{ //save objects
      if(!countObj && level <2) std::cout<<"SkipFromList: Saving objects first level: ------"<<std::endl;

      SaveObject(dirUp,dirIn,key);
      dirIn->cd();

      countObj = true;
    }
    level  = keepLevel;
  }


  objDir = NULL;
  delete objDir;

  key = NULL;
  delete key;

  return true;
}

//___________________________________________________________________
bool SaveObject(TDirectory *dirUp,TDirectory *dirIn,TKey *key){
  //save histos, trees etc. to output file/directory dirUp


  TString classname = key->GetClassName();
  TString nameObj = key->GetName();
  if(classname.Contains("TTree")){
    TTree *treeIn = (TTree*)dirIn->Get(nameObj);
    TTree *tree = treeIn->CloneTree();
    dirUp->cd();
    Int_t writeOK = tree->Write(nameObj,TObject::kSingleKey);
    if( writeOK <1) std::cout<<"SkipFromList: ERROR: "<<classname<<" "<<nameObj<<" not found, could not be saved."<<std::endl;
    if(printDetails) std::cout<<"SkipFromList: --> Saved object "<<classname<<" "<<nameObj<<std::endl;
    tree = NULL;
    delete tree;

    treeIn = NULL;
    delete treeIn;
  }
  else{
    TObject* objw = (TObject*)key->ReadObj();

    if(objw && !objw->IsZombie()){
      dirUp->cd();
      Int_t writeOK = objw->Write(nameObj,TObject::kSingleKey);
      if( writeOK <1) std::cout<<"SkipFromList: ERROR: "<<classname<<" "<<nameObj<<" not found, could not be saved."<<std::endl;
      if(printDetails) std::cout<<"SkipFromList: --> Saved object "<<classname<<" "<<nameObj<<std::endl;
    }
    else std::cout<<"SkipFromList: ERROR: "<<classname<<" "<<nameObj<<" not found, could not be saved."<<std::endl;
    objw = NULL;
    delete objw;
  }

  return true;
}

//___________________________________________________________________
bool EnterDirectory(TDirectoryFile *objDir,Int_t level,TString upname){
  //check for further directories or save output: execute LoopList for this
  level++;

  if(level<2) {
    std::cout<<"\n";
    std::cout<<"SkipFromList: Reading directory ************************************* "<<upname<<" ************************************"<<std::endl;
  }
  else  std::cout<<"SkipFromList: Reading subdirectory (level = "<<level<<") ************** "<<upname<<" *************"<<std::endl;

  TString nameDir = objDir->GetName();

  TList *keylist = (TList*)objDir->GetListOfKeys();

  //create subdirectory in outputfile
  TDirectoryFile *dirClone = new TDirectoryFile(nameDir,objDir->GetTitle());

  if(dirClone){
    dirClone->cd();
    //save remaining content of subdirectory in new subdirectory in output file
    LoopList(dirClone,objDir,keylist,level);
    if(level <2)    std::cout<<"SkipFromList: --> Saved directory "<<nameDir<<std::endl;
    else     std::cout<<"SkipFromList: --> Saved subdirectory (level = "<<level<<") "<<nameDir<<std::endl;
  }
  else std::cout<<"SkipFromList: ERROR: TDirectoryFile "<<nameDir<<" dir not created, could not be saved."<<std::endl;
  keylist = NULL;
  delete keylist;

  return true;
}
//___________________________________________________________________


#ifndef __CINT__
int main(int argc, char** argv) {


  if(argc < 4){
    std::cout<<"SkipFromList: ERROR: number of input smaller 3! Exiting!\nSkipFromList: HELP how to run: './RmObjFromRootF input.root output.root skip.list' and optional a flag for detailed printed output."<<std::endl;
    return 7;
  }

  TString arg1 = argv[1];
  if(!arg1.Contains(".root")) {
    std::cout<<"SkipFromList: ERROR: invalid input file format! Has to be .root. Exiting!"<<std::endl;
    return 5;
  }
  TString arg3 = argv[3];
  if(arg3.Contains(".root")) {
    std::cout<<"SkipFromList: ERROR: invalid file format of list of objects! Exiting!"<<std::endl;
    return 5;
  }


  std::cout<<"SkipFromList: input: "<<argv[1]<<" output: "<<argv[2]<<" list of object names to be removed: "<<argv[3]<<std::endl;
  if(argc > 4) printDetails = argv[4];

  int return_code = SkipFromList(argv[1],argv[2],argv[3]);

  return return_code;
}
#endif
