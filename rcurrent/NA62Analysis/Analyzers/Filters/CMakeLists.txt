
include_directories(include 
                    ../PreAnalyzers/include
                    ../DetectorAssociations/include
                    ../PhysicsTools/include)

set(CLASSES EventExtractor
            FilterAll
            FilterControlTrigger
            FilterElectron
            FilterEOP
            FilterDimuonThreeTrackVertex
            FilterDimuonTwoTrackVertex
            FilterHNLVertex
            FilterMuonElectronThreeTrackVertex
            FilterMuonNeutrino
            FilterOneTrackPnn
            FilterOneTrackNotMuon
            FilterOneTrackTwoClusters
            FilterPositronThreeTrackVertex
            FilterPnn
            FilterRestrictedMuon
            FilterRestrictedPositron
            FilterRestrictedThreeTrackVertex
            FilterThreeTracks
            FilterThreeTrackVertex
            FilterTwoTrackVertexWithLepton
	    FilterLKrLargePairDeposit
 )

FOREACH (lib ${CLASSES})
	GET_FILENAME_COMPONENT(libName ${lib} NAME_WE)
	add_library(l${libName}${LIBTYPEPOSTFIX} ${LIBTYPE} include/${libName}.hh src/${libName}.cc)
	add_dependencies(NA62Analyzers l${libName}${LIBTYPEPOSTFIX})
	SET(ALL_ANALYZERS_LISTS_LOCAL ${ALL_ANALYZERS_LISTS_LOCAL} ${lib})
ENDFOREACH(lib)

SET(ALL_ANALYZERS_LISTS ${ALL_ANALYZERS_LISTS} ${ALL_ANALYZERS_LISTS_LOCAL} PARENT_SCOPE)
	
