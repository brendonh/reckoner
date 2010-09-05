cmake_minimum_required (VERSION 2.8)
project (ReckonerServer)

include(MacroOutOfSourceBuild)
macro_ensure_out_of_source_build("${PROJECT_NAME} requires an out of source build.")


find_package(ENet REQUIRED)
include_directories(${ENet_INCLUDE_DIR})
set(LIBS ${LIBS} enet)


file(GLOB ReckonerServer_SOURCES "${ReckonerServer_SOURCE_DIR}/src/*.cpp")
include_directories("${RECKONER_COMMON_DIR}/src")

add_definitions(-Wall -Wextra -pedantic -Werror)

add_executable("reckoner-server" ${ReckonerServer_SOURCES})
target_link_libraries("reckoner-server" ${LIBS})

install(TARGETS "reckoner-server" RUNTIME DESTINATION "bin")
copy_headers("${ReckonerServer_SOURCE_DIR}/src/" "reckoner/server")