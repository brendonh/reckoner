cmake_minimum_required (VERSION 2.8)
project (ReckonerServer)

include(MacroOutOfSourceBuild)
macro_ensure_out_of_source_build("${PROJECT_NAME} requires an out of source build.")


find_package(ENet REQUIRED)
include_directories(${ENet_INCLUDE_DIR})
set(LIBS ${LIBS} enet)


file(GLOB ReckonerServer_SOURCES "${ReckonerServer_SOURCE_DIR}/src/*.cpp")
include_directories(ReckonerServer "${ReckonerServer_SOURCE_DIR}/include" "${RECKONER_COMMON_DIR}/include")

add_definitions(-Wall -Wextra -pedantic -Werror)


add_executable(server ${ReckonerServer_SOURCES})
target_link_libraries(server ${LIBS})
