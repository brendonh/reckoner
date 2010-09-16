#pragma once
#ifndef __RECKONER_COMMON_ENETENDPOINT
#define __RECKONER_COMMON_ENETENDPOINT

#include <enet/enet.h>

#include "reckoner/proto/login.pb.h"

#include "Reckoner.hpp"
#include "Messages.hpp"

#define LOG(m) std::cout << mIdentifier << " " << m << std::endl

namespace Reckoner {

    class ENetEndpoint {
    public:
    
      ENetEndpoint(ENetPeer* peer);
      ~ENetEndpoint();
    
      void startDisconnect();
      virtual void disconnected();
      
      void send(message_type t, google::protobuf::MessageLite* message, enet_uint32 flags);
      
    protected:

      bool extractMessage(google::protobuf::MessageLite& message, const ENetEvent* event);


      ENetPeer* mPeer;
      std::string mIdentifier;
      int mMessageBufferSize;
      char* mMessageBuffer;
      
      bool mDisconnecting;
      bool mDisconnected;
      
      static const uint32_t sDefaultBufferSize = 1024;
      
    };

}

#endif
