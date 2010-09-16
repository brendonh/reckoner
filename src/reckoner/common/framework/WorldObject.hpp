#pragma once
#ifndef __MICROCOSM_FRAMEWORK_WORLDOBJECT
#define __MICROCOSM_FRAMEWORK_WORLDOBJECT

#include <vector>

#include <Box2D/Box2D.h>

#include "./Listeners.hpp"

namespace Reckoner {
  namespace Framework {

    typedef std::vector<TickListener*> ListenerList;

    class WorldObject {
    public:
      
      WorldObject() : mBody(NULL) {};

      void setBody(b2Body* body) { mBody = body; }

      void tick() {
        ListenerList::const_iterator i;
        for (i = mTickListeners.begin(); i != mTickListeners.end(); ++i) {
          (*i)->tick();
        }
      }

      ~WorldObject() {
        TickListener* tl;
        while(!mTickListeners.empty()) {
          tl = mTickListeners.back();
          mTickListeners.pop_back();
          delete tl;
        }
      };

      b2Body*  mBody;

    protected:

      ListenerList mTickListeners;

    };

  }
}

#endif
