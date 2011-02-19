#pragma once
#ifndef __RECKONER_COMMON_ENETENDPOINT
#define __RECKONER_COMMON_ENETENDPOINT

#include <iostream>
#include <unordered_map>
#include <functional>

#include <enet/enet.h>

#include "reckoner/proto/region.pb.h"
#include "reckoner/common/framework/Noncopyable.hpp"

#include "Reckoner.hpp"
#include "Messages.hpp"

#define LOG(m) std::cout << mIdentifier << " " << m << std::endl

namespace Reckoner {
  namespace Network {

    namespace MessageMap {
      extern bool sInitialized;
      extern std::vector< google::protobuf::MessageLite* > sDispatchMap;
      extern std::unordered_map< std::string, int > sMessageIDMap;

      void initialize();
      void mapMessageClass(google::protobuf::MessageLite* message);
      void dumpMessageMap();
    }


    class ENetPacketBuffer : public Reckoner::Framework::noncopyable {
    public:
      ENetPacketBuffer();
      ~ENetPacketBuffer();

      bool buffer(google::protobuf::MessageLite* message, 
                  enet_uint32 flags);

      bool buffer(uint32_t messageType, 
                  google::protobuf::MessageLite* message, 
                  enet_uint32 flags);
      
      void sendTo(ENetPeer& peer) const;

      int mBufferSize;
      char* mBuffer;
      int mMessageSize;
      enet_uint32 mFlags;
      
      static const uint32_t sDefaultBufferSize = 1024;

    };


    class ENetEndpoint {
    public:
    
      typedef std::function< void(ENetEndpoint& endpoint, 
                                  const google::protobuf::MessageLite& message) 
                             > messageCallback_t;

      ENetEndpoint(ENetPeer& peer);
      ~ENetEndpoint();

      void send(const ENetPacketBuffer& buffer) {
        buffer.sendTo(mPeer);
      }

      bool send(google::protobuf::MessageLite* message, 
                enet_uint32 flags);

      bool send(uint32_t messageType, 
                google::protobuf::MessageLite* message, 
                enet_uint32 flags);


      void registerHandler(std::string messageName, 
                           messageCallback_t handler);

      void handle(const ENetEvent& event);

      void startDisconnect();
      virtual void disconnected();

      const std::string getIdentifier() { return mIdentifier; }

      static void registerStaticHandler(std::string messageName, 
                                        messageCallback_t handler);

      // Temporary
      void* peerData() { return mPeer.data; }

    protected:

      ENetPeer& mPeer;
      std::string mIdentifier;
      std::vector< messageCallback_t > mMessageHandlers;

      ENetPacketBuffer mImmediateBuffer;

      bool mDisconnecting;
      bool mDisconnected;

      static std::vector< messageCallback_t > sStaticMessageHandlers;
      static void sDefaultMessageHandler(ENetEndpoint& endpoint,
                                         const google::protobuf::MessageLite& message);
      
    };

  }
}

#endif
