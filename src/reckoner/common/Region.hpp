#pragma once
#ifndef __RECKONER_COMMON_REGION
#define __RECKONER_COMMON_REGION

#include <sys/time.h>

#include <unordered_map>
#include <set>

#include "reckoner/common/Reckoner.hpp"
#include "framework/WorldObject.hpp"

#include "reckoner/server/PacketQueue.hpp"


namespace Reckoner {

  static const float sDefaultTimeStep = TIMESTEP;
  static const float sDefaultSnapshotTimeStep = TIMESTEP * 10;

  typedef std::unordered_map<uuid_t, Framework::WorldObject*> ObjectMap_t;

  class Region {
  public:
    
    Region();
    ~Region();
    
    void addObject(Framework::WorldObject& obj);
    
    void tick();
    
    // void dirtyObject(Reckoner::Framework::WorldObject* obj) { 
    //   mDirtyObjects.insert(obj); 
    // }

    //void queuePackets(Reckoner::Server::PacketQueue& queue);

    const ObjectMap_t objects() { return mObjects; }

    // void addWatcher(uuid_t uuid, ClientID clientID) {
    //   mObjects[uuid]->addWatcher(clientID);
    // }

  private:
    float mTimeStep;
    float mUnrenderedTime;

    float mSnapshotTimeStep;
    float mTimeSinceSnapshot;
    
    ObjectMap_t mObjects;
    
    double mLastTickTime;
    
    //std::set<Reckoner::Framework::WorldObject*> mDirtyObjects;


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
