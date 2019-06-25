include_directories(include)
link_directories(${NA62TOOLS_LIBRARY_DIRS})

set(CLASSES BaseComparator
            DirComparator
            TH1Comparator
            TH2Comparator
            TGraphComparator
            TStringObjComparator
            TTreeComparator
            TBranchComparator
            UnknownComparator
            IgnoreList
   )

SANDWICH_LIST(CLASSES "src/" ".cpp")

add_library(Comparator${LIBTYPEPOSTFIX} ${LIBTYPE} ${CLASSES} ../src/TermManip.cc)
add_executable(ROOTCompare ROOTCompare.cpp)
add_executable(RmObjFromRootF RmObjFromRootF.cpp)
add_executable(ROOTFileMerger ROOTFileMerger.cpp src/FixStreams.cpp)
add_executable(FixStreams FixStreams.cpp src/FixStreams.cpp)

target_link_libraries(ROOTCompare ${ROOT_LIBRARIES} Minuit Geom TMVA EG Eve Spectrum Gui)
target_link_libraries(ROOTCompare Comparator${LIBTYPEPOSTFIX})
target_link_libraries(RmObjFromRootF ${ROOT_LIBRARIES})
target_link_libraries(ROOTFileMerger ${ROOT_LIBRARIES} ${NA62TOOLS_PERS_LIBRARIES} ${NA62TOOLS_SLIM_LIBRARIES} ${NA62TOOLS_LIBS_USED})
target_link_libraries(FixStreams ${ROOT_LIBRARIES} ${NA62TOOLS_PERS_LIBRARIES} ${NA62TOOLS_SLIM_LIBRARIES} ${NA62TOOLS_LIBS_USED})

add_executable(cleanFileList src/cleanFileList.cpp)
target_link_libraries(cleanFileList ${ROOT_LIBRARIES})
add_dependencies(NA62Tools ROOTCompare cleanFileList FixStreams RmObjFromRootF ROOTFileMerger)
