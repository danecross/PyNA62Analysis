// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2017-12-16
//
// ---------------------------------------------------------------

#ifndef NA62ConditionsService_H
#define NA62ConditionsService_H 1

#include "TString.h"
#include <fstream>
#include <map>

class DirectoryCache {
  public:
    DirectoryCache() : fCached(false) {}
    explicit DirectoryCache(TString path) : fCached(false), fDirectoryPath(path) {}
    Bool_t FillCacheForDir();
    std::vector<TString> GetFiles();
    std::vector<DirectoryCache> &GetDirs();
    void Reset();
    TString GetPath() { return fDirectoryPath; }
  private:
    bool fCached;
    TString fDirectoryPath;
    std::vector<TString> fFiles;
    std::vector<DirectoryCache> fDirectories;
};

class NA62ConditionsService {

  public:

    static NA62ConditionsService* GetInstance();

    Int_t  Open(TString FileName);
    void   Close(TString FileName);
    std::ifstream& Get(TString FileName);
    TString GetFullPath(TString FileName);

    void SetExitLevel(Int_t val)                  { fExitLevel                = val; }
    void SetExternalCDBDirectoryPath(TString val) { fExternalCDBDirectoryPath = val; }
    void SetCDBDirectoryPath(TString val)         { fCDBDirectoryPath         = val; }
    void SetCDBTag(TString val)                   { fCDBTag                   = val; }
    void SetAdditionalDirectoryPath(TString val)  { fAdditionalDirectoryPath  = val; }
    void SetCurrentRunID(Int_t val)               { fCurrentRunID             = val; }
    void SetCurrentBurstID(Int_t val)             { fCurrentBurstID           = val; }

    Int_t GetCurrentRunID()                       { return fCurrentRunID;            }
    Int_t GetCurrentBurstID()                     { return fCurrentBurstID;          }
    TString GetCDBTag()                           { return fCDBTag;                  }

  private:

    NA62ConditionsService();
    ~NA62ConditionsService();

    Int_t  Open(TString FileName, TString FileNameWithValidityRange, TString DirectoryPath);
    Bool_t IsOpen(TString FileName);
    Int_t  CheckDir(TString Directory, TString FileName, Bool_t WithIOV);
    Int_t  CheckDir(DirectoryCache &Directory, TString FileName, Bool_t WithIOV);

    static NA62ConditionsService* fInstance;

    Int_t fExitLevel; /// Exit if a error status > fExitLevel is found

    TString fExternalCDBDirectoryPath;
    TString fCDBDirectoryPath;
    TString fCDBTag;
    TString fAdditionalDirectoryPath;
    std::map<TString,std::ifstream*> fMapIfstream;
    std::map<TString,TString> fMapFullPath;
    std::map<TString, DirectoryCache> fDirectoryCache;

    Int_t fCurrentRunID;
    Int_t fCurrentBurstID;
};

#endif
