cmake_minimum_required (VERSION 2.8)
project (ReckonerClient)
macro_ensure_out_of_source_build("${PROJECT_NAME} requires an out of source build.")

file(GLOB ReckonerClient_SOURCES "${ReckonerClient_SOURCE_DIR}/*.cpp")

set(LIBS ${LIBS} reckoner-common)

add_executable(reckoner-client ${ReckonerClient_SOURCES})
add_dependencies(reckoner-client reckoner-common)
target_link_libraries(reckoner-client ${LIBS})

install(TARGETS reckoner-client RUNTIME DESTINATION "bin")
copy_headers("${ReckonerClient_SOURCE_DIR}/" "reckoner/client")