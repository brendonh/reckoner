#include "UpdateBuilder.hpp"
#include "reckoner/common/framework/Math2D.hpp"

using namespace Reckoner::Server;

UpdateBuilder::UpdateBuilder(Reckoner::Framework::WorldObject& obj)
  : mObj(obj), 
    mPackedObj(),
    mBuffer() {
}

void UpdateBuilder::updatePackedObj(Reckoner::ProtoBufs::WorldObject& obj) {
  obj.Clear();
  obj.set_objectid(mObj.getUUID());
  Reckoner::Framework::PVR& pvr = mObj.mPos;
  obj.set_positionx(pvr.position.getX());
  obj.set_positiony(pvr.position.getY());
  obj.set_positionz(pvr.position.getZ());
  obj.set_velocityx(pvr.velocity.getX());
  obj.set_velocityy(pvr.velocity.getY());
  obj.set_velocityz(pvr.velocity.getZ());
  obj.set_rotation(pvr.rotation);

  mBuffer.buffer(&obj, 0);
}

