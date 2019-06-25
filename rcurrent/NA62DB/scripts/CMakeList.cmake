
#Require boost
#Get boost path from environment
set(BOOST_ROOT $ENV{BOOST_DIR})
#Automatically find boost and its required components
find_package(Boost 1.53.0 REQUIRED COMPONENTS program_options)
#Include boost header dirs (-I in makefile)
include_directories($ENV{Boost_INCLUDE_DIRS})
link_directories($ENV{Boost_LIBRARY_DIRS})

#require Coral
#Include coral header dirs (-I in makefile)
include_directories($ENV{CORAL}/../include)
include_directories($ENV{CORAL}/../src/CoralCommon)

#Include coral library dirs (-L in makefile)
link_directories($ENV{CORAL}/lib)

#Add GNU CXX compiler specific flags
if(CMAKE_COMPILER_IS_GNUCXX)
	#Assign the string to variable CMAKE_CXX_FLAGS
	#set(CMAKE_CXX_FLAGS "-std=c++0x -pedantic-errors -Wall -Wextra -Wwrite-strings -Woverloaded-virtual -fno-nonansi-builtins -fno-gnu-keywords -fstrict-aliasing -g3")
	#set(CMAKE_CXX_FLAGS "-std=c++0x -Wall -Wextra -Wwrite-strings -Woverloaded-virtual -fno-nonansi-builtins -fno-gnu-keywords -fstrict-aliasing -g3")
	set(CMAKE_CXX_FLAGS "-Wall -Wextra -Wwrite-strings -Woverloaded-virtual -fno-nonansi-builtins -fno-gnu-keywords -fstrict-aliasing -g3")
endif()

#general cxx flags
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -D_GNU_SOURCE -fPIC -pthread -pipe -ansi -g")

#general link flags
SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -luuid -lnsl -lcrypt -ldl")

#
###Include Compact header dir
##include_directories(${PROJECT_SOURCE_DIR}/../userinc)
##
###Include Compact library dir
##link_directories(${PROJECT_SOURCE_DIR}/../)

#Include DBBase includes
include_directories(${PROJECT_SOURCE_DIR}/DBBase/include)
