#pragma once

#include <string>

#include "reckoner/common/framework/Noncopyable.hpp"

namespace Reckoner {
  namespace Framework {

    class WorldObject;

    class Component : public noncopyable {
    public:

      Component(WorldObject& obj, const std::string name)
        : mObj(obj), mName(name) {}

      virtual ~Component() {}

      virtual void tick() = 0;

      const std::string name() const { return mName; }
      


    protected:

      WorldObject& mObj;
      const std::string mName;

    };

  }
}
