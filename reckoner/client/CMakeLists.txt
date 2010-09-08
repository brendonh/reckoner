cmake_minimum_required (VERSION 2.8)
project (ReckonerClient)
include(MacroOutOfSourceBuild)
macro_ensure_out_of_source_build("${PROJECT_NAME} requires an out of source build.")


find_package(ENet REQUIRED)
include_directories(${ENet_INCLUDE_DIR})
set(LIBS ${LIBS} enet)


file(GLOB ReckonerClient_SOURCES "${ReckonerClient_SOURCE_DIR}/src/*.cpp")
include_directories(ReckonerClient "${RECKONER_COMMON_DIR}/src")

set(LIBS ${LIBS} reckoner-common)

find_package(Protobuf REQUIRED)
include_directories(${PROTOBUF_INCLUDE_DIRS})
set(LIBS ${LIBS} pthread ${PROTOBUF_LIBRARY})
include_directories(${CMAKE_BINARY_DIR})
set_source_files_properties(${PROTO_SRCS} PROPERTIES GENERATED TRUE)


add_definitions(-Wall -Wextra -pedantic)

add_executable(reckoner-client ${ReckonerClient_SOURCES} ${PROTO_SRCS})
add_dependencies(reckoner-server reckoner-proto)
target_link_libraries(reckoner-client ${LIBS})

install(TARGETS reckoner-client RUNTIME DESTINATION "bin")
copy_headers("${ReckonerClient_SOURCE_DIR}/src/" common)