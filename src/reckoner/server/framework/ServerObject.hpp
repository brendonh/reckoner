#pragma once

#include "reckoner/common/framework/Noncopyable.hpp"
#include "reckoner/common/framework/WorldObject.hpp"

namespace Reckoner {
  namespace Framework {

    class ServerObject : public noncopyable {
    public:
      
      ServerObject(WorldObject& worldObj)
        : mWorldObj(worldObj) {}

    private:
      WorldObject& mWorldObj;
      
    };


  }
}
