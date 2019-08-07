include_directories(include)

#List all classes requiring a ROOT dictionary
set(DICTCLASSES CalorimeterCluster)

#List all classes with both header and sources
set(CLASSES BeamParameters
            CoarseT0Evaluation
            CoulombCorrection
            ChParticleCandidate
            DiLeptonLNVReweight
            DownstreamTrack
            EmulatedL0Primitive
            EnergyCluster
            EventCandidate
            EventVetoes
            FilterEOPBase
            Fitter
            functions
            GeometricAcceptance
            HNLFunctions
            HNLWeight
            KalmanFilter
            KinFitConstraint
            KinFitEnergyConstraint
            KinFitMomentumConstraint
            KinFitPi0MassConstraint
            KinFitVertexConstraint
            KinFitParticle
            KinFitChargedParticle
            KinFitNeutralParticle
            KinFitUnmeasParticle
            K3piWithMissingPionInfo
            L0DetectorBits
            L0PrimitiveHandler
            LAVMatching
            LAVSpecialColumn	
            LAVStaticGeometry
            LAVBadChannelList
            LAVEffRecoBlock
            LKrAuxClusterReco
            LKrFineCalibration
            LKrClusterCorrectionFunctions
            LKrMatching
            LKrSpectrometerAssociationOutput
            LKrSpectrometerAssociationRecord
            MUV12Corrections
            MultipleLinesCDA
            NeParticleCandidate
            PedestalEvaluation
	    Particle
	    PnnFunctions
	    PointLineDistance
	    ProtoParticle
	    SAVMatching
	    RICHAcceptance	
	    RICHSingleRingTrkSeededFit
	    RICHSingleRingTrkCentredFit
            SpectrometerCHANTIAssociationOutput
            SpectrometerCHANTIAssociationRecord
            SpectrometerCHODAssociationOutput
            SpectrometerCHODAssociationRecord
            SpectrometerGigaTrackerMatchingOutput
            SpectrometerGigaTrackerMatchingTool
            SpectrometerGigaTrackerMVAMatchingTool
            SpectrometerLKrAssociationOutput
            SpectrometerLKrAssociationRecord
            SpectrometerNewCHODAssociationOutput
            SpectrometerNewCHODAssociationRecord
            SpectrometerMUV12AssociationOutput
            SpectrometerMUV3AssociationOutput
            SpectrometerMUV3AssociationRecord
            SpectrometerRICHAssociationOutput
            SpectrometerRICHAssociationOutputSimple
            SpectrometerRICHAssociationOutputSingleRing
            SpectrometerRICHAssociationOutputTrackCentredRing
            SpectrometerTrackVertex
            StrawAddress
            StrawPlace
            T0Evaluation
            TriggerConditions
            TwoLinesCDA
            UpstreamTrack
            VertexLSF
            VL0Emulator)
#Additional headers
set(LIBHEADERS ${CLASSES}
            NA62Constants
            StrawClasses)
#Additional sources
set(LIBSRC ${CLASSES})

SANDWICH_LIST(LIBHEADERS "include/" ".hh")
SANDWICH_LIST(LIBSRC "src/" ".cc")

#Prepare a single ROOT dictionary with all objects requiring it 
set(OBJHEADERS ${DICTCLASSES})
set(OBJSRC ${DICTCLASSES})

APPEND_LIST(OBJHEADERS ".hh")
if(${ROOT_VERSION} STREQUAL "6.18.00" OR ${ROOT_VERSION} VERSION_GREATER "6.18.00")
        ROOT_GENERATE_DICTIONARY(ToolsLibObjectsDICT ${OBJHEADERS} OPTIONS -inlineInputHeader -Iinclude LINKDEF include/LinkDef.h MODULE ToolsLibObjects)
        PREPEND_LIST(OBJHEADERS "include/")
else()
        PREPEND_LIST(OBJHEADERS "include/")
        ROOT_GENERATE_DICTIONARY(ToolsLibObjectsDICT ${OBJHEADERS} OPTIONS -inlineInputHeader LINKDEF include/LinkDef.h MODULE ToolsLibObjects)
endif()

SANDWICH_LIST(OBJSRC "src/" ".cc")

#Create the library for the ROOT objects
add_library(ToolsLibObjects-static STATIC ${OBJHEADERS} ${OBJSRC} ToolsLibObjectsDICT.cxx)
add_library(ToolsLibObjects SHARED ${OBJHEADERS} ${OBJSRC} ToolsLibObjectsDICT.cxx)
add_dependencies(ToolsLibObjects-static ToolsLibObjectsDICT)
add_dependencies(ToolsLibObjects ToolsLibObjectsDICT)

add_library(ToolsLib${LIBTYPEPOSTFIX} ${LIBTYPE} ${LIBSRC} ${LIBHEADERS})
add_dependencies(ToolsLib${LIBTYPEPOSTFIX} ToolsLibObjects${LIBTYPEPOSTFIX})
add_dependencies(NA62Core ToolsLib${LIBTYPEPOSTFIX})
