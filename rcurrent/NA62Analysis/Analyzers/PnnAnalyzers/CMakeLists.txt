include_directories(include 
                    ../PreAnalyzers/include
                    ../DetectorAssociations/include
                    ../MonitoringTools/include
                    ../../ToolsLib/include
                    ../PhysicsTools/include)

set(CLASSES CheckTrigger
	    Preselection
	    BestTrackSelection
   	    SingleTrackEventSelection
	    FakeTrackSelection
	    MultiplicitySelection
	    PhotonRejection
	    PhotonRejectionLKr
	    K2pi
	    Kmu2
	    KinematicTails
	    SegmentRejection
	    EventCleaning
	    KaonDecaySelection
	    TrackCalorimetricEnergyAssociation	
)

FOREACH (lib ${CLASSES})
	GET_FILENAME_COMPONENT(libName ${lib} NAME_WE)
	add_library(l${libName}${LIBTYPEPOSTFIX} ${LIBTYPE} include/${libName}.hh src/${libName}.cc)
	add_dependencies(NA62Analyzers l${libName}${LIBTYPEPOSTFIX})
	SET(ALL_ANALYZERS_LISTS_LOCAL ${ALL_ANALYZERS_LISTS_LOCAL} ${lib})
ENDFOREACH(lib)

SET(ALL_ANALYZERS_LISTS ${ALL_ANALYZERS_LISTS} ${ALL_ANALYZERS_LISTS_LOCAL} PARENT_SCOPE)
