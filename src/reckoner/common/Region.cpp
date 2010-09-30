#include "Region.hpp"

#include <iostream>

using namespace Reckoner;

Region::Region() 
  : mWorld(b2World(b2Vec2(0.0f, 0.0f), true)),
    mTimeStep(sDefaultTimeStep),
    mUnrenderedTime(0),
    mObjects(),
    mLastTickTime(0) {

  // Skip huge first value
  getTimeDelta();

}

Region::~Region() {
}


void Region::addObject(Framework::WorldObject* obj) {
  uuid_t uuid = obj->getUUID();
  auto it = mObjects.find(uuid);
  if (it != mObjects.end()) return;
  mObjects[uuid] = obj;
}


void Region::tick() {
  mUnrenderedTime += getTimeDelta();

  while(mUnrenderedTime > mTimeStep) {
    for (auto i = mObjects.begin(); i != mObjects.end(); i++) 
      (*i).second->tick();
    mWorld.Step(mTimeStep, 10, 10);
    mUnrenderedTime -= mTimeStep;
  }

}
