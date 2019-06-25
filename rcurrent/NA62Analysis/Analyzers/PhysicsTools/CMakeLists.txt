include_directories(include 
                    ../PreAnalyzers/include
                    ../DetectorAssociations/include
                    ../MonitoringTools/include)

set(CLASSES BuildNewCHODPileupHisto
            BuildDPGHitLibrary
            DownstreamTrackBuilder
            DownstreamTrackEnergyClusterAssociation
            EnergyClusterBuilder
            K2piSelection
            K2piSelectionNoSpectrometer
	    K2piSelectionWithMissingPhoton
            K3piSelection
	    K3piStrictSelection
            K3piWithMissingPionSelection
            Ke3Selection
	    Ke3SelectionNoSpectrometer            
	    Kmu2Selection
            Kmu3SelectionNoSpectrometer            
	    L0CaloEmulator
	    L0CHODEmulator
	    L0LAVEmulator
	    L0MUV3Emulator
	    L0NewCHODEmulator
	    L0RateComputer
	    L0RICHEmulator
	    L0TriggerEmulator
	    L0TriggerValidation
            ParticleCandidateBuilder
            RequestAllMCTrees
            RequestAllRecoTrees
            PhotonVetoHandler
            Pi0Selection
            RemoveHits
            SpectrometerVertexBuilder
)

FOREACH (lib ${CLASSES})
	GET_FILENAME_COMPONENT(libName ${lib} NAME_WE)
	add_library(l${libName}${LIBTYPEPOSTFIX} ${LIBTYPE} include/${libName}.hh src/${libName}.cc)
	add_dependencies(NA62Analyzers l${libName}${LIBTYPEPOSTFIX})
	SET(ALL_ANALYZERS_LISTS_LOCAL ${ALL_ANALYZERS_LISTS_LOCAL} ${lib})
ENDFOREACH(lib)

SET(ALL_ANALYZERS_LISTS ${ALL_ANALYZERS_LISTS} ${ALL_ANALYZERS_LISTS_LOCAL} PARENT_SCOPE)
