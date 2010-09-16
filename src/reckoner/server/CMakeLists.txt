cmake_minimum_required (VERSION 2.8)
project (ReckonerServer)
macro_ensure_out_of_source_build("${PROJECT_NAME} requires an out of source build.")

file(GLOB_RECURSE ReckonerServer_SOURCES "${ReckonerServer_SOURCE_DIR}/*.cpp")

add_library(reckoner-server STATIC ${ReckonerServer_SOURCES})
add_dependencies(reckoner-server reckoner-common)

install(TARGETS reckoner-server ARCHIVE DESTINATION "lib")
copy_headers("${ReckonerServer_SOURCE_DIR}/" "reckoner/server")