#pragma once
#ifndef __RECKONER_SERVER_UPDATEBUILDER
#define __RECKONER_SERVER_UPDATEBUILDER

#include "reckoner/proto/region.pb.h"
#include "reckoner/common/framework/WorldObject.hpp"
#include "reckoner/common/framework/Listeners.hpp"
#include "reckoner/common/ENetEndpoint.hpp"
#include "Server.hpp"

namespace Reckoner {
  namespace Server {

    class UpdateBuilder {
    public:
      UpdateBuilder(Reckoner::Framework::WorldObject& obj);
     
      void updatePackedObj() { updatePackedObj(mPackedObj); }
      void updatePackedObj(Reckoner::ProtoBufs::WorldObject& obj);
 
      const Reckoner::Network::ENetPacketBuffer& buffer() { return mBuffer; }

    private:
      Reckoner::Framework::WorldObject& mObj;

      Reckoner::ProtoBufs::WorldObject mPackedObj;
      Reckoner::Network::ENetPacketBuffer mBuffer;
    };

  }
}

#endif
