include_directories(include)

set(CLASSES ApplyDetectorEfficiency
	    BeamIntensityGenerator
	    DownstreamPileupGenerator
	    FastMCHandler
            GigaTrackerFineCorrections 
	    LKrClusterCorrections
	    MCTimeAlignment
	    MUV3PileupGenerator
	    PileupParticleGenerator
	    RemoveMUV3CornerTiles
            SpectrometerRecoAnalyzer
            SpectrometerTrackCorrections
	    StrawsTracksFilter
	    UpstreamPileupGenerator
            PnnKinematicTails_AdditionalTimeAlignment
)

FOREACH (lib ${CLASSES})
	GET_FILENAME_COMPONENT(libName ${lib} NAME_WE)
	add_library(l${libName}${LIBTYPEPOSTFIX} ${LIBTYPE} include/${libName}.hh src/${libName}.cc)
	add_dependencies(NA62Analyzers l${libName}${LIBTYPEPOSTFIX})
	SET(ALL_ANALYZERS_LISTS_LOCAL ${ALL_ANALYZERS_LISTS_LOCAL} ${lib})
ENDFOREACH(lib)

SET(ALL_ANALYZERS_LISTS ${ALL_ANALYZERS_LISTS} ${ALL_ANALYZERS_LISTS_LOCAL} PARENT_SCOPE)
