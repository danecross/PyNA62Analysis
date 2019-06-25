include_directories(include
                    ../PreAnalyzers/include
                    ../PhysicsTools/include)

set(CLASSES AlphaBetaComputation
	    BetaComputation	
	    AlphaComputation	
            CedarCoarseT0
            CedarT0
            CHANTICoarseT0
            CHANTIT0
	    CHANTITimeSlewing	
	    CHODAlignment
            CHODCoarseT0
            CHODT0
	    CHODSlewingCorrectionsCalculation
            GigaTrackerCoarseT0
	    GigaTrackerAlignment
            GigaTrackerT0
	    GigaTrackerPixelT0ChipTW
            HACCoarseT0
            HACT0
            IRCCoarseT0
            IRCT0
	    L0CaloCalibration
            LAVCoarseT0
            LAVT0
            LKrPedestal
            LKrBadCells
	    LKrFineCalibrationSymm
	    LKrFineCalibrationAsymm
            LKrCoarseT0
            LKrT0
            MUV0CoarseT0
            MUV0T0
            MUV1Pedestal
            MUV1CoarseT0
            MUV1T0
            MUV2Pedestal
            MUV2CoarseT0
            MUV2T0
            MUV3CoarseT0
	    MUV3TilesT0
            MUV3T0
            NewCHODCoarseT0
            NewCHODT0
            RICHCoarseT0
	    RICHElectronRadius
            RICHT0
            SACCoarseT0
            SACT0
            SpectrometerCoarseT0
            SpectrometerJumpsCorrection
            SpectrometerMagicT0
            SpectrometerT0
            SAVPedestal
	    SAVCoarseT0
	    SAVT0
)

FOREACH (lib ${CLASSES})
	GET_FILENAME_COMPONENT(libName ${lib} NAME_WE)
	add_library(l${libName}${LIBTYPEPOSTFIX} ${LIBTYPE} include/${libName}.hh src/${libName}.cc)
	add_dependencies(NA62Analyzers l${libName}${LIBTYPEPOSTFIX})
	SET(ALL_ANALYZERS_LISTS_LOCAL ${ALL_ANALYZERS_LISTS_LOCAL} ${lib})
ENDFOREACH(lib)

SET(ALL_ANALYZERS_LISTS ${ALL_ANALYZERS_LISTS} ${ALL_ANALYZERS_LISTS_LOCAL} PARENT_SCOPE)
