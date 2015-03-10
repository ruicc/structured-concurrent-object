structured-concurrent-object
============================

Object as a front-end of a thread.


## What I want to do

Separation of concerns by threads.

* Group
    * Group initializes itself when spawned.
    * Group controls its members (add/remove).
    * Group gets message and do some actions.
    * Group is deleted with finalization when all members left.
    * OnRemove event
        * Group executes its finalizer and gets removed when error occured.


## Memo

* Finite State Machine (gen_fsm)
* Dynamic handling of message handlers
    * Is it possible?
* Timeout
    * Some states get changed to next state automatically in some time after.
* Event handler should dispatch actions by fork?
    * No.
* How does object send message to itself?
    * Synchronous sending message to itself via Channel would get deadlock.
* More error handling
    * What does object notify errors of?
    * User's error handling
* Appropriate masking from asynchronous errors
