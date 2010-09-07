cmake_minimum_required (VERSION 2.8)
project (ReckonerCommon)


find_package(ENet REQUIRED)
include_directories(${ENet_INCLUDE_DIR})
set(LIBS ${LIBS} enet)

find_package(Box2D REQUIRED)
include_directories(${Box2D_INCLUDE_DIRS})
set(LIBS ${LIBS} Box2D_shared)


file(GLOB_RECURSE ReckonerCommon_SOURCES "${ReckonerCommon_SOURCE_DIR}/src/*.cpp")
include_directories("src")

add_definitions(-Wall -Wextra -pedantic -Werror)

#add_library(reckoner-common SHARED ${ReckonerCommon_SOURCES})
#target_link_libraries(reckoner-common ${LIBS})

#install(TARGETS reckoner-common LIBRARY DESTINATION "lib")
copy_headers("${ReckonerCommon_SOURCE_DIR}/src/" "reckoner/common")