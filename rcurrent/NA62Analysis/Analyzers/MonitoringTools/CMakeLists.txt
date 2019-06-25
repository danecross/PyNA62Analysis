
include_directories(include
                    ../PreAnalyzers/include
                    ../CalibrationTools/include
                    ../DetectorAssociations/include
                    ../PhysicsTools/include)

set(CLASSES CedarEfficiency
	    CHODEfficiency
            CHANTIMuonEfficiency
            CHANTIRandomVeto
            CHANTIRate
            CollectNumbersOfTriggers
            DetectorOnOffMonitor
	    GigaTrackerDataQualityMonitor
	    GigaTrackerTimeResolution
            HACDataQualityMonitor
            JittersCheck
            L0TriggerResponse
            LAVBurstChecker
	    LAVSpecialDigest	
	    LevelOnePerformance
            LKrEopMonitor
	    MUV12SwapCheck
            MUV3DataQualityMonitor
            MUV3Efficiency
	    MUV12DataQuality
            NewCHODDataQualityMonitor
            NewCHODEfficiency
            NumberOfHitsMonitor
            RICHMonitor
	    SAVEfficiency
            SpectrometerDataQualityMonitor
	    SpectrometerEfficiency
            SpectrometerRICHEventMonitor
            ThreeTrackMCEfficiency
	    TriggerEfficiencyMWE
	    TriggerRates
   )

FOREACH (lib ${CLASSES})
        GET_FILENAME_COMPONENT(libName ${lib} NAME_WE)
        add_library(l${libName}${LIBTYPEPOSTFIX} ${LIBTYPE} include/${libName}.hh src/${libName}.cc)
        add_dependencies(NA62Analyzers l${libName}${LIBTYPEPOSTFIX})
    	SET(ALL_ANALYZERS_LISTS_LOCAL ${ALL_ANALYZERS_LISTS_LOCAL} ${lib})
ENDFOREACH(lib)

SET(ALL_ANALYZERS_LISTS ${ALL_ANALYZERS_LISTS} ${ALL_ANALYZERS_LISTS_LOCAL} PARENT_SCOPE)
        
