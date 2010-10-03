#pragma once
#ifndef __RECKONER_COMMON_REGION
#define __RECKONER_COMMON_REGION

#include <sys/time.h>

#include <unordered_map>

#include "reckoner/common/Reckoner.hpp"
#include "framework/WorldObject.hpp"


namespace Reckoner {

  static const float sDefaultTimeStep = TIMESTEP;

  class Region {
  public:
  
    Region();
    ~Region();

    void addObject(Framework::WorldObject* obj);

    void tick();

  private:
    float mTimeStep;
    float mUnrenderedTime;

    std::unordered_map<uuid_t, Framework::WorldObject*> mObjects;

    double mLastTickTime;

    float getTimeDelta() {
      timeval Time = {0, 0};
      gettimeofday(&Time, NULL);
      double ms = (Time.tv_sec * 1000) + (Time.tv_usec / 1000.);
      float diff = ms - mLastTickTime;
      mLastTickTime = ms;
      return diff;
    }

  };
  
}

# endif
