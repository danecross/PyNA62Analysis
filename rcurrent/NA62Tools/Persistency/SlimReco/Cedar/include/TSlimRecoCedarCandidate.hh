#ifndef TSLIMRECOCEDARCANDIDATE_H
#define TSLIMRECOCEDARCANDIDATE_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <vector>

#include "TSlimRecoVCandidate.hh"

class TRecoCedarCandidate;

class TSlimRecoCedarCandidate : public TSlimRecoVCandidate {

  public:
    TSlimRecoCedarCandidate();
    explicit TSlimRecoCedarCandidate(TRecoCedarCandidate *candReco);
    virtual ~TSlimRecoCedarCandidate() {};

    // setters for members
    void SetNSectors(Short_t val)  { fNSectors = val;                  }
    void SetTime(Float_t val)      { fTime = val;                      }
    void AddHitIndex(Short_t index){ fHitsIndexes.emplace_back(index); }

    // getters for members
    Short_t GetNSectors()                        const { return fNSectors;           }
    Float_t GetTime()                            const { return fTime;               }
    Short_t GetNHits()                           const { return fHitsIndexes.size(); }
    const std::vector<Short_t>& GetHitsIndexes() const { return fHitsIndexes;        }

    // conversion functions
    virtual void FromReco(TRecoVCandidate *candReco);
    virtual void ToReco(TRecoVCandidate *candReco);
  private:
    Short_t fNSectors;
    Float_t fTime;
    std::vector<Short_t> fHitsIndexes;

    ClassDef(TSlimRecoCedarCandidate, 1)
};

#endif /* TSLIMRECOCEDARCANDIDATE_H */
