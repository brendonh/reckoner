find_package(Protobuf REQUIRED)
include_directories(${PROTOBUF_INCLUDE_DIRS})

file(GLOB ReckonerCommon_PROTOS "${CMAKE_CURRENT_SOURCE_DIR}/*.proto")
PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS ${ReckonerCommon_PROTOS})

add_custom_target(reckoner-proto SOURCES ${PROTO_SRCS} ${PROTO_HRDS})

set(PROTO_SRCS ${PROTO_SRCS} PARENT_SCOPE)

