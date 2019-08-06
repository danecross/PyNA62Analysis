include_directories(include 
                    ../PreAnalyzers/include
                    ../DetectorAssociations/include
                    ../MonitoringTools/include
                    ../../ToolsLib/include
                    ../PhysicsTools/include)

set(CLASSES 
	    BestTrackSelection
	    CheckTrigger
	    EventCleaning
	    K2pi
	    KaonDecaySelection
	    KinematicTails
	    Kmu2
	    MultiplicitySelection
	    PhotonRejection
	    PhotonRejectionLKr
	    Preselection
   	    SingleTrackEventSelection
	    TrackCalorimetricEnergyAssociation	
)

FOREACH (lib ${CLASSES})
	GET_FILENAME_COMPONENT(libName ${lib} NAME_WE)
	add_library(l${libName}${LIBTYPEPOSTFIX} ${LIBTYPE} include/${libName}.hh src/${libName}.cc)
	add_dependencies(NA62Analyzers l${libName}${LIBTYPEPOSTFIX})
	SET(ALL_ANALYZERS_LISTS_LOCAL ${ALL_ANALYZERS_LISTS_LOCAL} ${lib})
ENDFOREACH(lib)

SET(ALL_ANALYZERS_LISTS ${ALL_ANALYZERS_LISTS} ${ALL_ANALYZERS_LISTS_LOCAL} PARENT_SCOPE)
