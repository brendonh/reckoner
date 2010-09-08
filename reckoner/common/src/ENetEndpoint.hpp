#pragma once
#ifndef __RECKONER_COMMON_ENETENDPOINT
#define __RECKONER_COMMON_ENETENDPOINT

#include <enet/enet.h>

#include "common/proto/login.pb.h"

#include "Reckoner.hpp"
#include "Messages.hpp"

#define LOG(m) std::cout << mIdentifier << " " << m << std::endl

namespace Reckoner {

  class ENetEndpoint {
  public:

    void send(message_type t, google::protobuf::MessageLite* message, enet_uint32 flags);
    bool extractWireBuf(::google::protobuf::MessageLite* pb, const ENetEvent* event);


    ENetPeer* mPeer;
    std::string mIdentifier;
    int mMessageBufferSize;
    char* mMessageBuffer;

  };

}

#endif
