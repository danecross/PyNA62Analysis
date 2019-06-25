// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-02-01
//
// --------------------------------------------------------------

#ifndef STREAM_H
#define STREAM_H 1

#include "TObject.h"
#include "AnalysisInfo.hh"
#include "MCInfo.hh"
#include "RecoInfo.hh"

class Stream : public TObject {

public:
  Stream();
  virtual ~Stream() {}
  void Clear(Option_t* option="");
  void Print(Option_t* option="") const;

  void UpdateUniqueAttributes(Stream &s);
  void MergeJobAttributes(Stream &s);
  void UpdateAndMergeAttributes(Stream &s);

  MCInfo&       GetMCInfo()                        { return fMCInfo;       }
  void          SetMCInfo(MCInfo &val)             { fMCInfo = val;        }
  RecoInfo&     GetRecoInfo()                      { return fRecoInfo;     }
  void          SetRecoInfo(RecoInfo &val)         { fRecoInfo = val;      }
  AnalysisInfo& GetAnalysisInfo()                  { return fAnalysisInfo; }
  void          SetAnalysisInfo(AnalysisInfo &val) { fAnalysisInfo = val;  }

private:
  MCInfo       fMCInfo;
  RecoInfo     fRecoInfo;
  AnalysisInfo fAnalysisInfo;

  ClassDef(Stream,1)
};

#endif
