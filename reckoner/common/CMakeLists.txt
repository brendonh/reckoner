cmake_minimum_required (VERSION 2.8)
project (ReckonerCommon)


add_subdirectory(proto)
set(PROTO_SRCS ${PROTO_SRCS} PARENT_SCOPE)


find_package(ENet REQUIRED)
include_directories(${ENet_INCLUDE_DIR})
set(LIBS ${LIBS} enet)

find_package(Box2D REQUIRED)
include_directories(${Box2D_INCLUDE_DIRS})
set(LIBS ${LIBS} Box2D_shared)

file(GLOB_RECURSE ReckonerCommon_SOURCES "${ReckonerCommon_SOURCE_DIR}/src/*.cpp")
include_directories("${ReckonerCommon_SOURCE_DIR}/src")
include_directories(${CMAKE_BINARY_DIR})

find_package(Protobuf REQUIRED)
include_directories(${PROTOBUF_INCLUDE_DIRS})
set(LIBS ${LIBS} pthread ${PROTOBUF_LIBRARY})
include_directories(${CMAKE_BINARY_DIR})
set_source_files_properties(${PROTO_SRCS} PROPERTIES GENERATED TRUE)

add_definitions(-Wall -Wextra -pedantic)

add_library(reckoner-common STATIC ${ReckonerCommon_SOURCES} ${PROTO_SRCS} ${PROTO_HDRS})
add_dependencies(reckoner-common reckoner-proto)
target_link_libraries(reckoner-common ${PROTOBUF_LIBRARY})

install(TARGETS reckoner-common ARCHIVE DESTINATION "lib")
copy_headers("${ReckonerCommon_SOURCE_DIR}/src/" "reckoner/common")


