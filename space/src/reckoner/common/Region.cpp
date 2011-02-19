#include "Region.hpp"

#include <iostream>

using namespace Reckoner;

Region::Region() 
  : mTimeStep(sDefaultTimeStep),
    mUnrenderedTime(0),
    mSnapshotTimeStep(sDefaultSnapshotTimeStep),
    mTimeSinceSnapshot(0),
    mObjects(),
    mLastTickTime(0) {
    //mDirtyObjects() {

  // Skip huge first value
  getTimeDelta();

}

Region::~Region() {
}


void Region::addObject(Framework::WorldObject& obj) {
  uuid_t uuid = obj.getUUID();
  auto it = mObjects.find(uuid);
  if (it != mObjects.end()) return;
  mObjects[uuid] = &obj;
}


void Region::tick() {
  float delta = getTimeDelta();
  mUnrenderedTime += delta;
  mTimeSinceSnapshot += delta;

  while(mUnrenderedTime > mTimeStep) {
    for (auto i = mObjects.begin(); i != mObjects.end(); ++i) {
      (*i).second->tick();
    }
    mUnrenderedTime -= mTimeStep;
  }

  while(mTimeSinceSnapshot > mSnapshotTimeStep) {
    //std::cout << "SNAPSHOT" << std::endl;
    mTimeSinceSnapshot -= mSnapshotTimeStep;
  }
}

// void Region::queuePackets(Reckoner::Server::PacketQueue& queue) {
//   for(auto it = mDirtyObjects.begin(); it != mDirtyObjects.end(); ++it) {
//     (*it)->queueUpdate(queue, false);
//   }
//   mDirtyObjects.clear();
// }
