#pragma once
#ifndef __MICROCOSM_FRAMEWORK_WORLDOBJECT
#define __MICROCOSM_FRAMEWORK_WORLDOBJECT

#include <vector>

#include <Box2D/Box2D.h>

#include "./Listeners.hpp"
#include "reckoner/common/ReckonerTypes.hpp"


namespace Reckoner {
  namespace Framework {

    typedef std::vector<TickListener*> ListenerList;

    class WorldObject {
    public:
      
      WorldObject(uuid_t uuid, 
                  b2Body& body)
        : mUUID(uuid), mBody(body) {};

      virtual ~WorldObject() {
        TickListener* tl;
        while(!mTickListeners.empty()) {
          tl = mTickListeners.back();
          mTickListeners.pop_back();
          delete tl;
        }
        mBody.GetWorld()->DestroyBody(&mBody);        
      };

      void tick() {
        ListenerList::const_iterator i;
        for (i = mTickListeners.begin(); i != mTickListeners.end(); ++i) {
          (*i)->tick();
        }
      }

    protected:

      uuid_t mUUID;
      ListenerList mTickListeners;
      b2Body&  mBody;

    };

  }
}

#endif
