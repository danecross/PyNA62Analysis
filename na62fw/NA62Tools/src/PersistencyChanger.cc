/*
 * PersistencyChanger.cc
 *
 *  Created on: 8 Apr 2019
 *      Author: nlurkin
 */

#include "PersistencyChanger.hh"

#include "TSlimRecoCedarEvent.hh"
#include "TSlimRecoCHANTIEvent.hh"
#include "TSlimRecoCHODEvent.hh"
#include "TSlimRecoGigaTrackerEvent.hh"
#include "TSlimRecoHACEvent.hh"
#include "TSlimRecoIRCEvent.hh"
#include "TSlimRecoLAVEvent.hh"
#include "TSlimRecoLKrEvent.hh"
#include "TSlimRecoMUV0Event.hh"
#include "TSlimRecoMUV1Event.hh"
#include "TSlimRecoMUV2Event.hh"
#include "TSlimRecoMUV3Event.hh"
#include "TSlimRecoNewCHODEvent.hh"
#include "TSlimRecoRICHEvent.hh"
#include "TSlimRecoSACEvent.hh"
#include "TSlimRecoSAVEvent.hh"
#include "TSlimRecoSpectrometerEvent.hh"

#include "TRecoCedarEvent.hh"
#include "TRecoCHANTIEvent.hh"
#include "TRecoCHODEvent.hh"
#include "TRecoGigaTrackerEvent.hh"
#include "TRecoHACEvent.hh"
#include "TRecoIRCEvent.hh"
#include "TRecoLAVEvent.hh"
#include "TRecoLKrEvent.hh"
#include "TRecoMUV0Event.hh"
#include "TRecoMUV1Event.hh"
#include "TRecoMUV2Event.hh"
#include "TRecoMUV3Event.hh"
#include "TRecoNewCHODEvent.hh"
#include "TRecoRICHEvent.hh"
#include "TRecoSACEvent.hh"
#include "TRecoSAVEvent.hh"
#include "TRecoSpectrometerEvent.hh"

#include "BeamData.hh"
#include "L0TPData.hh"
#include "L1TPData.hh"
#include "L2EBData.hh"
#include "HLTEvent.hh"

std::pair<TString,TObject*> getSlimClassEquivalent(TString branchName){
    TString className = "";
    TObject *evt = nullptr;

    if(branchName.EqualTo("TRecoCedarEvent")){
        className = "TSlimRecoCedarEvent";
        evt = new TSlimRecoCedarEvent();
    }
    else if(branchName.EqualTo("TRecoCHANTIEvent")){
        className = "TSlimRecoCHANTIEvent";
        evt = new TSlimRecoCHANTIEvent();
    }
    else if(branchName.EqualTo("TRecoCHODEvent")){
        className = "TSlimRecoCHODEvent";
        evt = new TSlimRecoCHODEvent();
    }
    else if(branchName.EqualTo("TRecoGigaTrackerEvent")){
        className = "TSlimRecoGigaTrackerEvent";
        evt = new TSlimRecoGigaTrackerEvent();
    }
    else if(branchName.EqualTo("TRecoHACEvent")){
        className = "TSlimRecoHACEvent";
        evt = new TSlimRecoHACEvent();
    }
    else if(branchName.EqualTo("TRecoIRCEvent")){
        className = "TSlimRecoIRCEvent";
        evt = new TSlimRecoIRCEvent();
    }
    else if(branchName.EqualTo("TRecoLAVEvent")){
        className = "TSlimRecoLAVEvent";
        evt = new TSlimRecoLAVEvent();
    }
    else if(branchName.EqualTo("TRecoLKrEvent")){
        className = "TSlimRecoLKrEvent";
        evt = new TSlimRecoLKrEvent();
    }
    else if(branchName.EqualTo("TRecoMUV0Event")){
    	className = "TSlimRecoMUV0Event";
    	evt = new TSlimRecoMUV0Event();
    }
    else if(branchName.EqualTo("TRecoMUV1Event")){
    	className = "TSlimRecoMUV1Event";
    	evt = new TSlimRecoMUV1Event();
    }
    else if(branchName.EqualTo("TRecoMUV2Event")){
    	className = "TSlimRecoMUV2Event";
    	evt = new TSlimRecoMUV2Event();
    }
    else if(branchName.EqualTo("TRecoMUV3Event")){
        className = "TSlimRecoMUV3Event";
        evt = new TSlimRecoMUV3Event();
    }
    else if(branchName.EqualTo("TRecoNewCHODEvent")){
        className = "TSlimRecoNewCHODEvent";
        evt = new TSlimRecoNewCHODEvent();
    }
    else if(branchName.EqualTo("TRecoRICHEvent")){
        className = "TSlimRecoRICHEvent";
        evt = new TSlimRecoRICHEvent();
    }
    else if(branchName.EqualTo("TRecoSACEvent")){
        className = "TSlimRecoSACEvent";
        evt = new TSlimRecoSACEvent();
    }
    else if(branchName.EqualTo("TRecoSAVEvent")){
        className = "TSlimRecoSAVEvent";
        evt = new TSlimRecoSAVEvent();
    }
    else if(branchName.EqualTo("TRecoSpectrometerEvent")){
        className = "TSlimRecoSpectrometerEvent";
        evt = new TSlimRecoSpectrometerEvent();
    }
    else if(branchName.EqualTo("BeamData"))
        className = "BeamData";
    else if(branchName.EqualTo("L0TPData"))
        className = "L0TPData";
    else if(branchName.EqualTo("L1TPData"))
        className = "L1TPData";
    else if(branchName.EqualTo("L2EBData"))
        className = "L2EBData";
    else if(branchName.EqualTo("HLTEvent"))
        className = "HLTEvent";

    return std::make_pair(className, evt);
}

std::pair<TSlimRecoVEvent*, TRecoVEvent*> getSlimAndRecoFromName(TString detName, void** objectAddress) {
    if(detName.EqualTo("Cedar")){
        return std::make_pair(static_cast<TSlimRecoVEvent*>(new TSlimRecoCedarEvent()),
                *reinterpret_cast<TRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("CHANTI")){
        return std::make_pair(static_cast<TSlimRecoVEvent*>(new TSlimRecoCHANTIEvent()),
                *reinterpret_cast<TRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("CHOD")){
        return std::make_pair(static_cast<TSlimRecoVEvent*>(new TSlimRecoCHODEvent()),
                *reinterpret_cast<TRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("GigaTracker")){
        return std::make_pair(static_cast<TSlimRecoVEvent*>(new TSlimRecoGigaTrackerEvent()),
                *reinterpret_cast<TRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("HAC")){
        return std::make_pair(static_cast<TSlimRecoVEvent*>(new TSlimRecoHACEvent()),
                *reinterpret_cast<TRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("IRC")){
        return std::make_pair(static_cast<TSlimRecoVEvent*>(new TSlimRecoIRCEvent()),
                *reinterpret_cast<TRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("LAV")){
        return std::make_pair(static_cast<TSlimRecoVEvent*>(new TSlimRecoLAVEvent()),
                *reinterpret_cast<TRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("LKr")){
        return std::make_pair(static_cast<TSlimRecoVEvent*>(new TSlimRecoLKrEvent()),
                *reinterpret_cast<TRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("MUV0")){
        return std::make_pair(static_cast<TSlimRecoVEvent*>(new TSlimRecoMUV0Event()),
                *reinterpret_cast<TRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("MUV1")){
        return std::make_pair(static_cast<TSlimRecoVEvent*>(new TSlimRecoMUV1Event()),
                *reinterpret_cast<TRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("MUV2")){
        return std::make_pair(static_cast<TSlimRecoVEvent*>(new TSlimRecoMUV2Event()),
                *reinterpret_cast<TRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("MUV3")){
        return std::make_pair(static_cast<TSlimRecoVEvent*>(new TSlimRecoMUV3Event()),
                *reinterpret_cast<TRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("NewCHOD")){
        return std::make_pair(static_cast<TSlimRecoVEvent*>(new TSlimRecoNewCHODEvent()),
                *reinterpret_cast<TRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("RICH")){
        return std::make_pair(static_cast<TSlimRecoVEvent*>(new TSlimRecoRICHEvent()),
                *reinterpret_cast<TRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("SAC")){
        return std::make_pair(static_cast<TSlimRecoVEvent*>(new TSlimRecoSACEvent()),
                *reinterpret_cast<TRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("SAV")){
        return std::make_pair(static_cast<TSlimRecoVEvent*>(new TSlimRecoSAVEvent()),
                *reinterpret_cast<TRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("Spectrometer")){
        return std::make_pair(static_cast<TSlimRecoVEvent*>(new TSlimRecoSpectrometerEvent()),
                *reinterpret_cast<TRecoVEvent**>(objectAddress));
    }

    return std::pair<TSlimRecoVEvent*, TRecoVEvent*>(nullptr, *reinterpret_cast<TRecoVEvent**>(objectAddress));
}

std::pair<TRecoVEvent*, TSlimRecoVEvent*> getRecoAndSlimFromName(TString detName, void** objectAddress) {
    if(detName.EqualTo("Cedar")){
        return std::make_pair(static_cast<TRecoVEvent*>(new TRecoCedarEvent()),
                *reinterpret_cast<TSlimRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("CHANTI")){
        return std::make_pair(static_cast<TRecoVEvent*>(new TRecoCHANTIEvent()),
                *reinterpret_cast<TSlimRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("CHOD")){
        return std::make_pair(static_cast<TRecoVEvent*>(new TRecoCHODEvent()),
                *reinterpret_cast<TSlimRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("GigaTracker")){
        return std::make_pair(static_cast<TRecoVEvent*>(new TRecoGigaTrackerEvent()),
                *reinterpret_cast<TSlimRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("HAC")){
        return std::make_pair(static_cast<TRecoVEvent*>(new TRecoHACEvent()),
                *reinterpret_cast<TSlimRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("IRC")){
        return std::make_pair(static_cast<TRecoVEvent*>(new TRecoIRCEvent()),
                *reinterpret_cast<TSlimRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("LAV")){
        return std::make_pair(static_cast<TRecoVEvent*>(new TRecoLAVEvent()),
                *reinterpret_cast<TSlimRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("LKr")){
        return std::make_pair(static_cast<TRecoVEvent*>(new TRecoLKrEvent()),
                *reinterpret_cast<TSlimRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("MUV0")){
        return std::make_pair(static_cast<TRecoVEvent*>(new TRecoMUV0Event()),
                *reinterpret_cast<TSlimRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("MUV1")){
        return std::make_pair(static_cast<TRecoVEvent*>(new TRecoMUV1Event()),
                *reinterpret_cast<TSlimRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("MUV2")){
        return std::make_pair(static_cast<TRecoVEvent*>(new TRecoMUV2Event()),
                *reinterpret_cast<TSlimRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("MUV3")){
        return std::make_pair(static_cast<TRecoVEvent*>(new TRecoMUV3Event()),
                *reinterpret_cast<TSlimRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("NewCHOD")){
        return std::make_pair(static_cast<TRecoVEvent*>(new TRecoNewCHODEvent()),
                *reinterpret_cast<TSlimRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("RICH")){
        return std::make_pair(static_cast<TRecoVEvent*>(new TRecoRICHEvent()),
                *reinterpret_cast<TSlimRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("SAC")){
        return std::make_pair(static_cast<TRecoVEvent*>(new TRecoSACEvent()),
                *reinterpret_cast<TSlimRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("SAV")){
        return std::make_pair(static_cast<TRecoVEvent*>(new TRecoSAVEvent()),
                *reinterpret_cast<TSlimRecoVEvent**>(objectAddress));
    }
    else if(detName.EqualTo("Spectrometer")){
        return std::make_pair(static_cast<TRecoVEvent*>(new TRecoSpectrometerEvent()),
                *reinterpret_cast<TSlimRecoVEvent**>(objectAddress));
    }

    return std::pair<TRecoVEvent*, TSlimRecoVEvent*>(nullptr, *reinterpret_cast<TSlimRecoVEvent**>(objectAddress));
}
