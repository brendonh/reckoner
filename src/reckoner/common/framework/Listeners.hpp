#pragma once
#ifndef __MICROCOSM_FRAMEWORK_LISTENERS
#define __MICROCOSM_FRAMEWORK_LISTENERS

#include "./WorldObject.hpp"

namespace Reckoner {
  namespace Framework {

    class WorldObject;

    class TickListener {
    public:

      virtual void tick() = 0;

      std::string name;

    };

  }
}

# endif
