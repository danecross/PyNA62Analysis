cmake_minimum_required(VERSION 2.6 FATAL_ERROR)

project(NA62AnalysisUser)

# Get NA62Analysis
set(NA62ANALYSIS $ENV{ANALYSISFW_PATH})
set(NA62USE_USER 1)
set(INSTALLEXTENSION "-$ENV{SYSTEMINSTALL}")
set(BINDIR "bin-$ENV{SYSTEMINSTALL}")

include(${NA62ANALYSIS}/scripts/NA62Analysis.cmake)

# Define NA62Analysis include and lib directories  
include_directories(Analyzers/include)
include_directories(${NA62ANALYSIS_ANALYZER_INCLUDES})
link_directories(${NA62ANALYSIS}/lib-$ENV{SYSTEMINSTALL})
link_directories(${NA62TOOLS_LIBRARY_DIRS})
link_directories(${NA62RECO_LIBRARY_DIRS})

# Get analyzers definition from builder
include(analyzers.cmake)

# Include POs
add_subdirectory(PhysicsObjects)
include_directories(PhysicsObjects/include)

# Include Algos
add_subdirectory(Algorithms)
include_directories(Algorithms/include)

# Extras
include_directories(${EXTRA_INCLUDE_DIRS})
link_directories(${EXTRA_LIBS_DIRS})
link_directories($ENV{SQLITE}/lib)

# Create libraries for user analyzers
FOREACH(usrAna ${USER_ANALYZERS})
	add_library(l${usrAna}${LIBTYPEPOSTFIX} ${LIBTYPE} Analyzers/src/${usrAna}.cc Analyzers/include/${usrAna}.hh)
ENDFOREACH(usrAna)

# Create executable from builder
add_executable(${TARGET_EXEC} main.cc)

# Specify all analyzers libraries
FOREACH(ana ${ANA_LIBS})
	target_link_libraries(${TARGET_EXEC} l${ana}${LIBTYPEPOSTFIX})
ENDFOREACH(ana)

FOREACH(lib ${USERPOLIBS})
        target_link_libraries(${TARGET_EXEC} ${lib})
ENDFOREACH(lib)

FOREACH(lib ${USERALGOLIBS})
        target_link_libraries(${TARGET_EXEC} ${lib})
ENDFOREACH(lib)
target_link_libraries(${TARGET_EXEC} ${USERPOLIBS}) # Because of static libraries

# Specify NA62Reconstruction/NA62Analysis/ROOT libraries
target_link_libraries(${TARGET_EXEC} Algorithms${LIBTYPEPOSTFIX})
target_link_libraries(${TARGET_EXEC} AnalysisFW${LIBTYPEPOSTFIX})
target_link_libraries(${TARGET_EXEC} ToolsLib${LIBTYPEPOSTFIX} ToolsLibObjects${LIBTYPEPOSTFIX} ${USERPOLIBS})
target_link_libraries(${TARGET_EXEC} ${NA62RECO_LIBS_USED})
target_link_libraries(${TARGET_EXEC} ${ROOT_LIBRARIES} Minuit Geom TMVA EG Eve Spectrum Gui)
target_link_libraries(${TARGET_EXEC} ${NA62TOOLS_LIBS_PERS_USED})
target_link_libraries(${TARGET_EXEC} ${NA62TOOLS_LIBS_SLIM_USED})
target_link_libraries(${TARGET_EXEC} ${NA62TOOLS_LIBS_USED})
target_link_libraries(${TARGET_EXEC} ${NA62TOOLS_LIBS_SLIM_USED})
target_link_libraries(${TARGET_EXEC} ${Geant4_LIBRARIES} sqlite3 gfortran ${BOOST_LIB})

# Specify extra libraries
target_link_libraries(${TARGET_EXEC} ${EXTRA_LIBS})
target_link_libraries(${TARGET_EXEC} ${NA62TOOLS_LIBS_USED})

# Move target to user dir
install(TARGETS ${TARGET_EXEC} DESTINATION bin/..)
