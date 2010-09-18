#ifndef __RECKONER_SERVER
#define __RECKONER_SERVER

#include <string>

#include "reckoner/common/ReckonerTypes.hpp"
#include "ClientList.hpp"

namespace Reckoner {
  namespace Server {

    bool initialize();

    class Server {
    public:

      Server();
      virtual ~Server();

      int run();

      bool _shutdown;

    private:
      ENetHost* mHost;
      ClientList mClientList;

    typedef unsigned long ClientID;
    typedef std::string UserID;

    };
  }
}

#endif
