#pragma once
#ifndef __RECKONER_FRAMEWORK_MATH2D
#define __RECKONER_FRAMEWORK_MATH2D

#include "reckoner/common/vectormath.hpp"

namespace Reckoner {
  namespace Framework {

    using namespace Reckoner::vec;

    class PVR {
    public:

      PVR(Vector3 p, Vector3 v, float r) 
        : position(p), velocity(v), rotation(r) {}

      Vector3 position;
      Vector3 velocity;
      float rotation;

    };

    inline Vector3 rad2vec(float r) {
      return Vector3(cos(r), sin(r), 0);
    }

    inline float vec2rad(Vector3 v) {
      return atan2(v.getY(), v.getX());
    }

  }
}
#endif
