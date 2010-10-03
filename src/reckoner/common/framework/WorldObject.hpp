#pragma once
#ifndef __MICROCOSM_FRAMEWORK_WORLDOBJECT
#define __MICROCOSM_FRAMEWORK_WORLDOBJECT

#include <iostream>
#include <vector>

#include "Listeners.hpp"
#include "Math2D.hpp"

#include "reckoner/common/ReckonerTypes.hpp"

namespace Reckoner {
  namespace Framework {

    typedef std::vector<TickListener*> ListenerList;

    class WorldObject {
    public:
      
      PVR mPos;

      WorldObject(uuid_t uuid, PVR pos)
        : mPos(pos), mUUID(uuid), mTickListeners() {}

      virtual ~WorldObject() {
        TickListener* tl;
        while(!mTickListeners.empty()) {
          tl = mTickListeners.back();
          mTickListeners.pop_back();
          delete tl;
        }
      }

      void tick() {
        ListenerList::const_iterator i;
        for (i = mTickListeners.begin(); i != mTickListeners.end(); ++i) {
          (*i)->tick();
        }
      }

      uuid_t getUUID() { return mUUID; }

    protected:

      uuid_t mUUID;
      ListenerList mTickListeners;

    };

  }
}

#endif
