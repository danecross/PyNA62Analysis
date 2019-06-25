#ifndef TSLIMRECOIRCCANDIDATE_H
#define TSLIMRECOIRCCANDIDATE_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t

#include "TSlimRecoVCandidate.hh"
#include <TVector3.h>

class TRecoIRCHit;
class TRecoVHit;

class TSlimRecoIRCCandidate : public TSlimRecoVCandidate
{
public:
    TSlimRecoIRCCandidate() = default;
    explicit TSlimRecoIRCCandidate(TRecoIRCHit *);
    virtual ~TSlimRecoIRCCandidate() = default;

    // setters for members
    void SetChannelID       (Short_t value)    { fChannelID = value;          }
    void SetTime            (Float_t value)    { fTime = value;               }
    void SetLeadingEdgeLow  (Float_t edgeTime) { fLeadingEdgeLow = edgeTime;  }
    void SetLeadingEdgeHigh (Float_t edgeTime) { fLeadingEdgeHigh = edgeTime; }

    void SetTimeOverThresholdLowThr (Float_t val) { fTimeOvThrLow=val;          }
    void SetTimeOverThresholdHighThr(Float_t val) { fTimeOvThrHigh=val;         }
    void SetLeadingESlewingSlope    (Float_t val) { fLeadingESlewingSlope =val; }
    void SetTrailingESlewingSlope   (Float_t val) { fTrailingESlewingSlope=val; }

    Float_t GetLeadingEdgeLow  () const { return ((fEdgeMask & 0x1)? fLeadingEdgeLow  :0); }
    Float_t GetLeadingEdgeHigh () const { return ((fEdgeMask & 0x2)? fLeadingEdgeHigh :0); }
    Float_t GetTrailingEdgeHigh() const { return ((fEdgeMask & 0x4)? ComputeTrailingEdgeHigh():0); }
    Float_t GetTrailingEdgeLow () const { return ((fEdgeMask & 0x8)? ComputeTrailingEdgeLow() :0); }

    Float_t GetTimeOverThresholdLowThr () const { return fTimeOvThrLow;          }
    Float_t GetTimeOverThresholdHighThr() const { return fTimeOvThrHigh;         }
    Float_t GetLeadingESlewingSlope    () const { return fLeadingESlewingSlope ; }
    Float_t GetTrailingESlewingSlope   () const { return fTrailingESlewingSlope; }
    Short_t GetChannelID               () const { return fChannelID;             }

    Bool_t HasLeadingEdgeLow   () const { return fEdgeMask & 0x1; }
    Bool_t HasLeadingEdgeHigh  () const { return fEdgeMask & 0x2; }
    Bool_t HasTrailingEdgeHigh () const { return fEdgeMask & 0x4; }
    Bool_t HasTrailingEdgeLow  () const { return fEdgeMask & 0x8; }
    Bool_t HasAll4EdgesDetected() const { return fEdgeMask==0xF;  }
    Bool_t HasAllTimesInOrder  () const;

    Short_t GetPMTID()    const;
    Short_t GetEdgeMask() const { return fEdgeMask;       }
    Float_t GetTime()     const { return fTime;           }

    // conversion functions
    virtual void FromReco(TRecoVCandidate *) {}
    virtual void ToReco(TRecoVCandidate *)   {}

    virtual void FromReco(TRecoVHit *hitReco);
    virtual void ToReco(TRecoVHit *hitReco);
private:
    Float_t ComputeTrailingEdgeLow()  const;
    Float_t ComputeTrailingEdgeHigh() const;

    Short_t fChannelID = -1;
    Short_t fEdgeMask = 0; ///< Mask for the edges present: bit 0 --> LeadingLow; 1 --> LeadingHigh; 2-->TrailingHigh; 3-->TrailingLow

    Float_t fTime      = 0.;

    Float_t fTimeOvThrLow          = -999.;
    Float_t fTimeOvThrHigh         = -999.;
    Float_t fLeadingESlewingSlope  = -999.;
    Float_t fTrailingESlewingSlope = -999.;

    Float_t fLeadingEdgeLow   = -999.; ///< Time of leading low, subtracted of the trigger time only
    Float_t fLeadingEdgeHigh  = -999.; ///< Time of trailing high, subtracted of the trigger time only

    ClassDef(TSlimRecoIRCCandidate, 1)
};

#endif /* TSLIMRECOIRCCANDIDATE_H */
