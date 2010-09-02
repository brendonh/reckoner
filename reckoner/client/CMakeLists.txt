cmake_minimum_required (VERSION 2.8)
project (ReckonerClient)
include(MacroOutOfSourceBuild)
macro_ensure_out_of_source_build("${PROJECT_NAME} requires an out of source build.")


find_package(ENet REQUIRED)
include_directories(${ENet_INCLUDE_DIR})
set(LIBS ${LIBS} enet)


file(GLOB ReckonerClient_SOURCES "${ReckonerClient_SOURCE_DIR}/src/*.cpp")
include_directories(ReckonerClient "${ReckonerClient}/include" "${RECKONER_COMMON_DIR}/include")

add_definitions(-Wall -Wextra -pedantic -Werror)


add_executable(client ${ReckonerClient_SOURCES})
target_link_libraries(client ${LIBS})
