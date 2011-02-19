cmake_minimum_required (VERSION 2.8)
project (Reckoner)

set(CMAKE_MODULE_PATH ${Reckoner_BINARY_DIR}/../cmake/modules ${CMAKE_MODULE_PATH})
include(MacroOutOfSourceBuild)
macro_ensure_out_of_source_build("${PROJECT_NAME} requires an out of source build.")

include(CopyHeaders)

set(CMAKE_INSTALL_PREFIX "dist/")

find_package(ENet REQUIRED)
include_directories(${ENet_INCLUDE_DIR})
set(LIBS ${LIBS} enet)

find_package(Protobuf REQUIRED)

add_subdirectory("reckoner/proto")
include_directories(${PROTOBUF_INCLUDE_DIRS} ${CMAKE_BINARY_DIR})
set(LIBS ${LIBS} pthread ${PROTOBUF_LIBRARY})

include_directories(${Reckoner_SOURCE_DIR})
add_definitions(-std=c++0x -Wall -Wextra -pedantic -Winline -U__SSE__)


add_subdirectory("reckoner/common")
add_subdirectory("reckoner/server")
add_subdirectory("reckoner/client")

install(DIRECTORY "${Reckoner_SOURCE_DIR}/../cmake"
  DESTINATION "share")
