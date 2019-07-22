include_directories(include)

#List all classes with both header and sources
set(CLASSES BeamMCTester
            CedarMCTester
            GeantinoMCTester
            MCEopDebugger
            NewCHODMCTester
            NewCHODSearchRadiusComputation
            MUV0MCTester
            MUV3MCTester
            PrintKineParts
            SpectrometerMCTester
            SpectrometerSlimPersistencyTester
   )

FOREACH (lib ${CLASSES})
	GET_FILENAME_COMPONENT(libName ${lib} NAME_WE)
	add_library(l${libName}${LIBTYPEPOSTFIX} ${LIBTYPE} include/${libName}.hh src/${libName}.cc)
	add_dependencies(NA62Analyzers l${libName}${LIBTYPEPOSTFIX})
	SET(ALL_ANALYZERS_LISTS_LOCAL ${ALL_ANALYZERS_LISTS_LOCAL} ${lib})
ENDFOREACH(lib)

SET(ALL_ANALYZERS_LISTS ${ALL_ANALYZERS_LISTS} ${ALL_ANALYZERS_LISTS_LOCAL} PARENT_SCOPE)
