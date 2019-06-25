message(STATUS "Configuring HLT external libraries")


###################
## na62-farm-lib
###################
#include_directories("na62-farm-lib/")
#file(GLOB_RECURSE SOURCES_NA62FARMLIB "na62-farm-lib" "*.cpp")
#add_library(na62-farm-lib STATIC ${SOURCES_NA62FARMLIB})
#
####################
### na62-trigger-algorithms
####################
#file(GLOB_RECURSE SOURCES_NA62FARMTRIGGERALGORITHMS "na62-trigger-algorithms" "*.cpp")
##list(REMOVE_ITEM SOURCES_NA62FARMTRIGGERALGORITHMS l1/L1TriggerProcessor.cpp)
#add_library(na62-trigger-algorithms STATIC ${SOURCES_NA62FARMTRIGGERALGORITHMS})






###################
## farm library for offline
###################
add_definitions(-DONLINEHLT)
add_library(offline-na62-farm-lib STATIC
    na62-farm-lib/l0/offline/Subevent.cpp
    na62-farm-lib/l0/offline/MEPFragment.cpp
    na62-farm-lib/l1/L1InfoToStorage.cpp
)

add_library(offline-na62-trigger-algorithms STATIC
    na62-trigger-algorithms/common/ConfFileReader.cpp
    #na62-trigger-algorithms/l1/CHODAlgo.cpp
    #na62-trigger-algorithms/l1/chod_algorithm/ParsConfFile.cpp
    #na62-trigger-algorithms/l1/IRC_SACAlgo.cpp
    na62-trigger-algorithms/l1/KtagAlgo.cpp
    na62-trigger-algorithms/l1/lav_algorithm/LAVParsConfFile.cpp
    na62-trigger-algorithms/l1/LAVAlgo.cpp
    #na62-trigger-algorithms/l1/MUVAlgo.cpp
    #na62-trigger-algorithms/l1/NewCHODAlgo.cpp
    #na62-trigger-algorithms/l1/RICHAlgo.cpp
    #na62-trigger-algorithms/l1/RICHMultiAlgo.cpp
    na62-trigger-algorithms/l1/StrawAlgo.cpp
    na62-trigger-algorithms/l1/straw_algorithm/STRAWParsConfFile.cpp
    na62-trigger-algorithms/l1/straw_algorithm/Point.cpp
    na62-trigger-algorithms/l1/straw_algorithm/Cluster.cpp
    na62-trigger-algorithms/l1/straw_algorithm/Track.cpp
    na62-trigger-algorithms/l1/straw_algorithm/Straw.cpp
    na62-trigger-algorithms/l1/straw_algorithm/DigiManager.cpp
    na62-trigger-algorithms/common/decoding/DecoderRange.cpp
    na62-trigger-algorithms/common/decoding/TrbFragmentDecoder.cpp
    na62-trigger-algorithms/common/decoding/SrbFragmentDecoder.cpp
)


install(TARGETS
    offline-na62-farm-lib
    offline-na62-trigger-algorithms
#    na62-farm-lib
#    na62-trigger-algorithms
    EXPORT na62-farm DESTINATION lib-$ENV{SYSTEMINSTALL}
)
