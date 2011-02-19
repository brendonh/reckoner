#pragma once

#include <map>
#include <exception>

#include "reckoner/common/ReckonerTypes.hpp"
#include "reckoner/common/framework/Noncopyable.hpp"
#include "reckoner/common/framework/Component.hpp"

// XXX TODO: Put PVR in a component instead
#include "reckoner/common/framework/Math2D.hpp"


// XXX TODO: Not this.
//#include "reckoner/server/PacketQueue.hpp"

namespace Reckoner {
  namespace Framework {

    class NoComponent : public std::exception {
    public:
      NoComponent(std::string name) : mName(name) {}
      ~NoComponent() throw() {}

      virtual const char* what() const throw() {
        return ("No such component: " + mName).c_str();
      }

      std::string mName;
    };


    class WorldObject : public noncopyable {
    public:

      PVR mPos;

      WorldObject(uuid_t uuid, PVR pos)
        : mPos(pos), mUUID(uuid), mComponents() {}

      virtual ~WorldObject() {
        // XXX TODO: Component cleanup
      }

      void tick() {
        for (auto it = mComponents.begin(); it != mComponents.end(); ++it) {
          it->second->tick();
        }
      }

      // Only used on the server :(
      // virtual void queueUpdate(Reckoner::Server::PacketQueue& UNUSED(queue), 
      //                          bool UNUSED(force)) {};

      uuid_t getUUID() const { return mUUID; }

      void addComponent(Component& component) {
        mComponents[component.name()] = &component;
      }

      Component& getComponent(std::string name) throw(NoComponent) {
        auto it = mComponents.find(name);
        if (it == mComponents.end()) throw NoComponent(name);
        return *(it->second);
      }

      //const std::vector<ClientID> watchers() { return mWatcherIDs; }
      //void addWatcher(ClientID clientID) { mWatcherIDs.push_back(clientID); }

    private:

      const uuid_t mUUID;

      std::map<std::string, Component*> mComponents;

      //std::vector<ClientID> mWatcherIDs;

    };

  }
}
