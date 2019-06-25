//
// ********************************************************************
// * License and Disclaimer                                           *
// *                                                                  *
// * The  Geant4 software  is  copyright of the Copyright Holders  of *
// * the Geant4 Collaboration.  It is provided  under  the terms  and *
// * conditions of the Geant4 Software License,  included in the file *
// * LICENSE and available at  http://cern.ch/geant4/license .  These *
// * include a list of copyright holders.                             *
// *                                                                  *
// * Neither the authors of this software system, nor their employing *
// * institutes,nor the agencies providing financial support for this *
// * work  make  any representation or  warranty, express or implied, *
// * regarding  this  software system or assume any liability for its *
// * use.  Please see the license in the file  LICENSE  and URL above *
// * for the full disclaimer and the limitation of liability.         *
// *                                                                  *
// * This  code  implementation is the result of  the  scientific and *
// * technical work of the GEANT4 collaboration.                      *
// * By using,  copying,  modifying or  distributing the software (or *
// * any work based  on the software)  you  agree  to acknowledge its *
// * use  in  resulting  scientific  publications,  and indicate your *
// * acceptance of all terms of the Geant4 Software license.          *
// ********************************************************************
//
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------
#ifndef RootIOManager_H
#define RootIOManager_H 1

#include "TROOT.h"
#include "TFile.h"
#include "TTree.h"
#include "TBranch.h"
#include "TClonesArray.h"
#include "TGraph.h"
#include "globals.hh"
#include "NA62VRootIO.hh"
#include <list>

class Stream;
class Event;
class G4Event;

class RootIOManager {
public:

  virtual ~RootIOManager() {}

  static RootIOManager* GetInstance();
  void NewRun(G4int);
  void EndRun(G4int);
  void SaveEvent(const G4Event*);
  void Close();
  NA62VRootIO* FindRootIO(G4String);
  std::pair<G4double,G4double> GetMemoryUsage();

private:

  static RootIOManager* fInstance;

protected:

  RootIOManager();

public:

  Int_t                GetBufSize()                                       { return fBufSize;                      };
  void                 SetBufSize(Int_t value)                            { fBufSize = value;                     };
  Int_t                GetBranchStyle()                                   { return fBranchStyle;                  };
  void                 SetBranchStyle(Int_t value)                        { fBranchStyle = value;                 };
  Int_t                GetCompLevel()                                     { return fCompLevel;                    };
  void                 SetCompLevel(Int_t value)                          { fCompLevel = value;                   };
  G4int                GetVerbose()                                       { return fVerbose;                      };
  void                 SetVerbose(G4int value)                            { fVerbose = value;                     };

  G4String             GetFileName()                                      { return fFileName;                     };
  void                 SetFileName(G4String value);
  G4bool               GetFileNameHasChanged()                            { return fFileNameHasChanged;           };
  void                 SetFileNameHasChanged(G4bool value)                { fFileNameHasChanged = value;          };

  TFile *              GetFile()                                          { return fFile;                         };
  void                 SetFile(TFile * value)                             { fFile = value;                        };
  TTree *              GetStreamTree()                                    { return fStreamTree;                   };
  void                 SetStreamTree(TTree * value)                       { fStreamTree = value;                  };
  TTree *              GetEventTree()                                     { return fEventTree;                    };
  void                 SetEventTree(TTree * value)                        { fEventTree = value;                   };
  TBranch *            GetStreamBranch()                                  { return fStreamBranch;                 };
  void                 SetStreamBranch(TBranch * value)                   { fStreamBranch = value;                };
  TBranch *            GetEventBranch()                                   { return fEventBranch;                  };
  void                 SetEventBranch(TBranch * value)                    { fEventBranch = value;                 };
  Stream *             GetStream()                                        { return fStream;                       };
  void                 SetStream(Stream * value)                          { fStream = value;                      };
  Event *              GetEvent()                                         { return fEvent;                        };
  void                 SetEvent(Event * value)                            { fEvent = value;                       };

//   RootIOList           GetRootIOList()                                    { return fRootIOList;                   };
//   void                 SetRootIOList(RootIOList value)                    { fRootIOList = value;                  };

private:

  Int_t fBufSize;
  Int_t fBranchStyle;
  Int_t fCompLevel;
  G4int fVerbose;

  G4String fFileName;
  G4bool fFileNameHasChanged;

  TFile* fFile;
  TTree* fStreamTree; //Tree to hold all runs in one file
  TTree* fEventTree; //Tree to hold all events in one run
  TBranch* fStreamBranch;
  TBranch* fEventBranch;
  Stream* fStream;
  Event* fEvent;
  TGraph * fGVirtMem;
  TGraph * fGResMem;

  typedef std::list<NA62VRootIO*>  RootIOList;

  RootIOList fRootIOList;
};
#endif // RootIOManager_H
