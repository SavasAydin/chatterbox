# chatterbox
Chatterbox is a server that allows users to create up to four different chat rooms.  
Chatterbox Access Point (CAP) is the engine and users can give the following commands:  
```
Commands        -> [Command]
```
Returns the list of permitted commands

```
Available rooms -> [RoomName]
```
Returns the list of rooms that have not been started yet

```
Occupied room   -> [RoomName]
```
Returns the list of rooms that have already been started

```
create RoomName -> ok | {error, REASON}
```
Creates a chat room with the name ${ROOM_NAME}

```
JOIN RoomName   -> ok | {error, REASON} 
```
Joins you to the chat room ${ROOM_NAME}

If a chat room is already started, 

# phase_2
Chatterbox is launched successfully.
Customers are happy but they would like to have following additional functionalities:

1- Once a chat room is started, the owner may change the room name.
Occupied and available rooms commands still returns the list defaults room names.

2- Owner of the chat room have rights to kick or shut a user in the room.

# phase_3
Chatterbox is growing, more and more people started using it.
Now it is time to start a robust node instead of a process for each room.
CAP should be able to restart the nodes if they die.

# Build
-----

    $ rebar3 compile
    $ rebar3 eunit	
