
include_directories(include)

#List all classes with both header and sources
set(CLASSES StrawsTracksFilterAlgo
	    ThreePiAssociationAlgo
	    GigaTrackerRecoAlgorithm
            SpectrometerRecoAlgorithm
            StrawSegmentAlgorithm
	    MatchingRG
            FakeTrackSelection
   )

#Additional headers
set(LIBHEADERS ${CLASSES})
#Additional sources
set(LIBSRC ${CLASSES})

SANDWICH_LIST(LIBHEADERS "include/" ".hh")
SANDWICH_LIST(LIBSRC "src/" ".cc")

add_library(Algorithms${LIBTYPEPOSTFIX} ${LIBTYPE} ${LIBSRC} ${LIBHEADERS})

#FOREACH (lib ${CLASSES})
#	GET_FILENAME_COMPONENT(libName ${lib} NAME_WE)
#	add_library(l${libName}${LIBTYPEPOSTFIX} ${LIBTYPE} include/${libName}.hh src/${libName}.cc)
#ENDFOREACH(lib)
