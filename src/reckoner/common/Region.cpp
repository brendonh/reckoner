#include "Region.hpp"

#include <iostream>

using namespace Reckoner;

Region::Region() 
  : mTimeStep(sDefaultTimeStep),
    mUnrenderedTime(0),
    mWorld(b2World(b2Vec2(0.0f, 0.0f), true)),
    mObjects(),
    mLastTickTime(0) {

  // Skip huge first value
  getTimeDelta();

}

Region::~Region() {
}


void Region::tick() {
  mUnrenderedTime += getTimeDelta();

  std::vector<Framework::WorldObject>::iterator i;

  while(mUnrenderedTime > mTimeStep) {
    for (i = mObjects.begin(); i != mObjects.end(); i++) (*i).tick();
    mWorld.Step(mTimeStep, 10, 10);
    mUnrenderedTime -= mTimeStep;
  }

}
