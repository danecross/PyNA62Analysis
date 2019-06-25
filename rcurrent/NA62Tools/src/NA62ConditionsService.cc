// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2017-12-16
//
// ---------------------------------------------------------------

#include "NA62ConditionsService.hh"
#include <iostream>
#include "TObjArray.h"
#include "TObjString.h"
#include "TPRegexp.h"
#include "dirent.h"
#include "NA62Global.hh"
#include "GitRevision.hh"

bool fileIsEmpty(std::ifstream* pFile){
  return pFile->peek() == std::ifstream::traits_type::eof();
}

NA62ConditionsService* NA62ConditionsService::fInstance = nullptr;

NA62ConditionsService* NA62ConditionsService::GetInstance(){
  if(!fInstance) fInstance = new NA62ConditionsService();
  return fInstance;
}

NA62ConditionsService::NA62ConditionsService() :
  fExitLevel(0),
  fExternalCDBDirectoryPath(""), fCDBDirectoryPath(Form("%s",std::getenv("NA62CDBDIR"))), fCDBTag(Form("Data/%s",GetCurrentGitRevision().Data())),
  fMetaDataDirectoryPath(""),
  fCurrentRunID(0), fCurrentBurstID(0)
{
} 

NA62ConditionsService::~NA62ConditionsService(){}

Int_t NA62ConditionsService::Open(TString FileName,TString FileNameWithValidityRange, TString DirectoryPath){
  TString FileNameFullPath = FileNameWithValidityRange;
  if(DirectoryPath!="") FileNameFullPath = DirectoryPath+"/"+FileNameWithValidityRange;
  std::ifstream* pFile = new std::ifstream(FileNameFullPath);
  if(pFile->is_open()){
    if(!fileIsEmpty(pFile)){
      std::cout << "[NA62ConditionsService] Reading " << FileNameFullPath;
      std::cout << Form(" [Current Burst: %06d-%04d]",fCurrentRunID,fCurrentBurstID);
      std::cout << std::endl;
      fMapIfstream.emplace(FileName,pFile); // should not depend on CDBDirectoryPath!
      fMapFullPath.emplace(FileName,DirectoryPath+"/"+FileNameWithValidityRange); // should not depend on CDBDirectoryPath!
    }
    else {
      std::cerr << "[NA62ConditionsService] WARNING: " << FileNameFullPath << " is empty!" << std::endl;
      if(fExitLevel<kConditionFileIsEmpty-kConditionOffset) _Exit(kConditionFileIsEmpty); // unlike exit() does not execute atexit functions
      return kConditionFileIsEmpty;
    }
  }
  else {
    delete pFile;
    std::cerr << "[NA62ConditionsService] WARNING: " << FileNameFullPath << " is not readable!" << std::endl;
    if(fExitLevel<kConditionFileIsNotReadable-kConditionOffset) _Exit(kConditionFileIsNotReadable); // unlike exit() does not execute atexit functions
    return kConditionFileIsNotReadable;
  }
  return kSuccess;
}

Int_t NA62ConditionsService::Open(TString FileName){

  Int_t Status = kConditionFileNotFound;

  TString CDBDir = fCDBDirectoryPath+"/"+fCDBTag;
  if(fExternalCDBDirectoryPath!="") CDBDir = fExternalCDBDirectoryPath;

  std::vector<TString> CheckDirectories;
  CheckDirectories.push_back(".");                                                  // Check in the local dir first
  CheckDirectories.push_back(Form("%s/%06d",CDBDir.Data(),fCurrentRunID));          // Check in the run-related CDB dir
  CheckDirectories.push_back(Form("%s/MultiRun",CDBDir.Data()));                    // Check in the multi-run CDB dir
  CheckDirectories.push_back(Form("%s/Conditions",std::getenv("NA62TOOLSSOURCE"))); // Check in the default condition dir
  if(fMetaDataDirectoryPath!="") CheckDirectories.push_back(fMetaDataDirectoryPath);// Check in metadata as last chance (to be phased out!)

  // Look for any file matching the FileName description [with the correct validity range] also in the subdirs
  for(UInt_t iDir=0;iDir<CheckDirectories.size();iDir++){
    Status = CheckDir(CheckDirectories[iDir],FileName,true); // Check IOV
    if(Status!=kConditionFileNotFound) return Status;
  }

  // Look for FileName with the exact name as specified in the config files
  for(UInt_t iDir=0;iDir<CheckDirectories.size();iDir++){
    Status = CheckDir(CheckDirectories[iDir],FileName,false); // Don't check IOV
    if(Status!=kConditionFileNotFound) return Status;
  }

  if(Status==kConditionFileNotFound){
    for(UInt_t iDir=0;iDir<CheckDirectories.size();iDir++){
      std::cerr << "[NA62ConditionsService] WARNING: '" << FileName << "' not found in '"<<CheckDirectories[iDir]<<"'!" << std::endl;
    }
    if(fExitLevel<kConditionFileNotFound-kConditionOffset) _Exit(kConditionFileNotFound); // unlike exit() does not execute atexit functions
  }

  return Status;
}

void NA62ConditionsService::Close(TString FileName){
  if(IsOpen(FileName)) {
    Get(FileName).close();
    delete fMapIfstream.find(FileName)->second;
    fMapIfstream.erase(fMapIfstream.find(FileName));
  }
}

std::ifstream& NA62ConditionsService::Get(TString FileName){
  return *(fMapIfstream.find(FileName)->second);
}

TString NA62ConditionsService::GetFullPath(TString FileName){
  if(fMapFullPath.find(FileName)!=fMapFullPath.end())
    return fMapFullPath.find(FileName)->second;
  else { // try to open it
    Open(FileName);
    Close(FileName);
    if(fMapFullPath.find(FileName)!=fMapFullPath.end())
      return fMapFullPath.find(FileName)->second;
  }
  return "";
}

Bool_t NA62ConditionsService::IsOpen(TString FileName){
  if(fMapIfstream.find(FileName)!=fMapIfstream.end())
    return kTRUE;
  return kFALSE;
}

Bool_t DirectoryCache::FillCacheForDir(){
  DIR *dir;

  if ((dir = opendir(fDirectoryPath.Data())) != NULL) {
    struct dirent *ent;
    while ((ent = readdir (dir)) != NULL) {
      // Look in subdirectories
      if (ent->d_type == DT_DIR) {
        if (strcmp(ent->d_name, ".") == 0 || strcmp(ent->d_name, "..") == 0) continue;
        if (strcmp(ent->d_name, "build-cc7")==0 || strcmp(ent->d_name, "build-slc6")==0) continue;
        if (strcmp(ent->d_name, "bin-cc7")==0 || strcmp(ent->d_name, "bin-slc6")==0) continue;
        fDirectories.push_back(DirectoryCache(Form("%s/%s", fDirectoryPath.Data(), ent->d_name)));
      }
      else{
        fFiles.push_back(ent->d_name);
      }
    }
    closedir(dir);
  }
  return kTRUE;
}

std::vector<TString> DirectoryCache::GetFiles(){
  if(!fCached)
    fCached = FillCacheForDir();
  return fFiles;
}

std::vector<DirectoryCache> &DirectoryCache::GetDirs(){
  if(!fCached)
    fCached = FillCacheForDir();
  return fDirectories;
}

void DirectoryCache::Reset(){
  fCached = false;
  fFiles.clear();
  fDirectories.clear();
}

Int_t NA62ConditionsService::CheckDir(TString Directory, TString FileName, Bool_t WithIOV){
  if(fDirectoryCache.count(Directory)==0){
    fDirectoryCache.emplace(Directory, DirectoryCache(Directory));
  }

  return CheckDir(fDirectoryCache[Directory], FileName, WithIOV);
}

Int_t NA62ConditionsService::CheckDir(DirectoryCache &Directory, TString FileName, Bool_t WithIOV){
  Int_t Status = kConditionFileNotFound;
  TObjArray *subStr = TPRegexp("^([\\w-.]+?)(.run\\d+_\\d{4}-run\\d+_\\d{4})?.(\\w+)$").MatchS(FileName);
  if(subStr->GetLast()>=2) { // Regexp matched
    TString FileNamePrefix    = ((TObjString *)subStr->At(1))->GetString();
    TString FileNameExtension = ((TObjString *)subStr->At(subStr->GetLast()))->GetString();
    // Look for a file with a name matching the regexp in each directory
    for(auto &ent : Directory.GetDirs()){
      // Look in subdirectories first
      Status = CheckDir(ent,FileName,WithIOV);
      if(Status!=kConditionFileNotFound) {
        delete subStr;
        return Status;
      }
    }
    for(auto FileInDirName : Directory.GetFiles()){
      //Look at files second
      if(WithIOV){
        TObjArray *subStrInDir = TPRegexp(Form("^%s.run(\\d+)_(\\d+)-run(\\d+)_(\\d+).%s$",FileNamePrefix.Data(),FileNameExtension.Data())).MatchS(FileInDirName);
        if(subStrInDir->GetLast()<4) {
          delete subStrInDir;
          continue;
        }
        Int_t RunMin   = ((TObjString *)subStrInDir->At(1))->GetString().Atoi();
        Int_t BurstMin = ((TObjString *)subStrInDir->At(2))->GetString().Atoi();
        Int_t RunMax   = ((TObjString *)subStrInDir->At(3))->GetString().Atoi();
        Int_t BurstMax = ((TObjString *)subStrInDir->At(4))->GetString().Atoi();
        if((RunMin<fCurrentRunID && fCurrentRunID<RunMax) || 
            (RunMin==fCurrentRunID && BurstMin<=fCurrentBurstID && fCurrentRunID<RunMax) ||
            (RunMax==fCurrentRunID && fCurrentBurstID<=BurstMax && RunMin<fCurrentRunID) ||
            (RunMin==fCurrentRunID && BurstMin<=fCurrentBurstID && RunMax==fCurrentRunID && fCurrentBurstID<=BurstMax)){
          // Valid run/burst range
          Status = Open(FileName,FileInDirName,Directory.GetPath());
          if(Status!=kConditionFileNotFound) {
            delete subStr;
            delete subStrInDir;
            return Status;
          }
        }
        delete subStrInDir;
      }
      else { //Look for exact file name (no IOV)
        if(FileInDirName!=FileName) continue;
        // FileName matches
        Status = Open(FileName,FileInDirName,Directory.GetPath());
        if(Status!=kConditionFileNotFound) {
          delete subStr;
          return Status;
        }
      }
    }
  }
  else {
    std::cerr << "[NA62ConditionsService] WARNING: Invalid format name: '" << FileName << "'!" << std::endl;
  }
  delete subStr;
  return Status;
}
