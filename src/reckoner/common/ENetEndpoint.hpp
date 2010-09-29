#pragma once
#ifndef __RECKONER_COMMON_ENETENDPOINT
#define __RECKONER_COMMON_ENETENDPOINT

#include <iostream>
#include <unordered_map>
#include <functional>

#include <enet/enet.h>

#include "reckoner/proto/login.pb.h"

#include "Reckoner.hpp"
#include "Messages.hpp"

#define LOG(m) std::cout << mIdentifier << " " << m << std::endl

namespace Reckoner {
  namespace Network {


    class ENetEndpoint {
    public:
    
      typedef std::function< void(ENetEndpoint& endpoint, const google::protobuf::MessageLite& message) > messageCallback_t;

      ENetEndpoint(ENetPeer& peer);
      ~ENetEndpoint();


      void send(google::protobuf::MessageLite* message, enet_uint32 flags);
      void send(uint32_t messageType, google::protobuf::MessageLite* message, enet_uint32 flags);

      void registerHandler(std::string messageName, 
                           messageCallback_t handler);

      void handle(const ENetEvent& event);

      void startDisconnect();
      virtual void disconnected();

      
      const std::string getIdentifier() { return mIdentifier; }


      static void mapMessageClass(google::protobuf::MessageLite* message) {
        sMessageIDMap[message->GetTypeName()] = sDispatchMap.size();
        sDispatchMap.push_back(message);
      }

      static void registerStaticHandler(std::string messageName, 
                                        messageCallback_t handler);

      static void dumpMessageMap();

      static void initialize();
      
    protected:

      ENetPeer& mPeer;
      std::string mIdentifier;
      int mMessageBufferSize;
      char* mMessageBuffer;
      std::vector< messageCallback_t > mMessageHandlers;

      bool mDisconnecting;
      bool mDisconnected;

      static bool sInitialized;
      static const uint32_t sDefaultBufferSize = 1024;
      static std::vector< google::protobuf::MessageLite* > sDispatchMap;
      static std::unordered_map< std::string, int > sMessageIDMap;
      static std::vector< messageCallback_t > sStaticMessageHandlers;
      static void sDefaultMessageHandler(ENetEndpoint& endpoint,
                                         const google::protobuf::MessageLite& message);
      
    };
  }
}

#endif
