chatterbox
============
Chatterbox is a server that allows users to create up to four different chat rooms.
The server architecture looks like this:
                 _____ 
               |       |      
               | North |      
          ____ | _____ | ____ 
        |      |       |      |
        | West |  CAP  | East |
        | ____ | _____ | ____ |
               |       |      
               | South |      
               | _____ |      


Chatterbox Access Point (CAP) is the engine that is  placed in the middle
and users can give the following commands:
 - Commands          -> Returns the list of permitted commands
 - Available rooms   -> Returns the list of rooms that have not been started yet
 - Occupied rooms    -> Returns the list of rooms that have already been started
 - Move ${DIRECTION} -> Moves you to ${DIRECTION} 
 - ${DIRECTION}      -> North, South, East or WEST
 
Moving to any direction starts a chat room process with the name of the direction by default.
If a chat room is already started by someone else, you may still move to the room and chat.


Build
-----

    $ rebar3 compile
