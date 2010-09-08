cmake_minimum_required (VERSION 2.8)
project (Reckoner)

set(RECKONER_COMMON_DIR "${Reckoner_SOURCE_DIR}/common")

set(CMAKE_MODULE_PATH ${Reckoner_SOURCE_DIR}/../cmake/modules ${CMAKE_MODULE_PATH})
include(MacroOutOfSourceBuild)
macro_ensure_out_of_source_build("${PROJECT_NAME} requires an out of source build.")

include(CopyHeaders)

set(CMAKE_INSTALL_PREFIX "dist/")


add_subdirectory(common)
add_subdirectory(server)
add_subdirectory(client)


