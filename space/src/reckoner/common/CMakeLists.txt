cmake_minimum_required (VERSION 2.8)
project (ReckonerCommon)
macro_ensure_out_of_source_build("${PROJECT_NAME} requires an out of source build.")

file(GLOB_RECURSE ReckonerCommon_SOURCES "${ReckonerCommon_SOURCE_DIR}/*.cpp")

set_source_files_properties(${PROTO_SRCS} PROPERTIES GENERATED TRUE)
add_library(reckoner-common STATIC ${ReckonerCommon_SOURCES} ${PROTO_SRCS})
add_dependencies(reckoner-common reckoner-proto)

install(TARGETS reckoner-common ARCHIVE DESTINATION "lib")
copy_headers("${ReckonerCommon_SOURCE_DIR}/" "reckoner/common")


