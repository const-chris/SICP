#lang racket
(require "constraints.rkt")

(define a (make-connector))
#|
           +------------------------------------+
           |                                    |
global --> |  a --+     make-connector --+      |
           |      |                      |      |
           +------|----------------------|------+
                  |         ^            |   ^
                  |         |            v   |
                  |         |           @=@--+
                  |    +---------+      |
                  |    |         |      |
                  |    +---------+      v
                  |         ^         params: _
                  |         |         body: (let ((value ... )))
                  |         |
                  |    +------------------------+
                  |    | value: false           |
                  |    | informant: false       |
                  |    | constraints: '()       |
                  |    | set-my-value: ...      |
                  |    | forget-my-value: ...   |
                  |    | connect: ...           |
                  | +--- me                     |
                  | |  +------------------------+
                  | |       ^
                  | |       |
                  v v       |
                  @=@-------+
                  |
                  |
                  v
                params: request
                body: (cond ((eq? request 'has-value?) ... ))
|#

(define b (make-connector))
#|
           +------------------------------------------------------------------------+
           |                                                                        |
global --> |  a --+     make-connector --+                     b --+                |
           |      |                      |                         |                |
           +------|----------------------|-------------------------|----------------+
                  |         ^            |   ^                     |         ^
                  |         |            v   |                     |         |
                  |         |           @=@--+                     |         |
                  |    +---------+      |                          |    +---------+
                  |    |         |      |                          |    |         |
                  |    +---------+      v                          |    +---------+
                  |         ^         params: _                    |         ^
                  |         |         body: (let ((value ... )))   |         |
                  |         |                                      |         |
                  |    +------------------------+                  |    +------------------------+
                  |    | value: false           |                  |    | value: false           |
                  |    | informant: false       |                  |    | informant: false       |
                  |    | constraints: '()       |                  |    | constraints: '()       |
                  |    | set-my-value: ...      |                  |    | set-my-value: ...      |
                  |    | forget-my-value: ...   |                  |    | forget-my-value: ...   |
                  |    | connect: ...           |                  |    | connect: ...           |
                  | +--- me                     |                  | +--- me                     |
                  | |  +------------------------+                  | |  +------------------------+
                  | |       ^                                      | |       ^
                  | |       |                                      | |       |
                  v v       |                                      v v       |
                  @=@-------+                                      @=@-------+
                  |                                                |
                  |                                                |
                  v                                                v
                params: request                                  params: request
                body: (cond ((eq? request 'has-value?) ... ))    body: (cond ((eq? request 'has-value?) ... ))
|#

(set-value! a 10 'user)
#|
           +---------------------------------------------------------------------------------------------------+
           |                                                                                                   |
global --> |  a --+     make-connector --+                                                b --+                |
           |      |                      |                                                    |                |
           +------|----------------------|----------------------------------------------------|----------------+
                  |         ^            |   ^                                                |         ^
                  |         |            v   |                                                |         |
                  |         |           @=@--+                                                |         |
                  |    +---------+      |                                                     |    +---------+
                  |    |         |      |                                                     |    |         |
                  |    +---------+      v                                                     |    +---------+
                  |         ^         params: _                                               |         ^
                  |         |         body: (let ((value ... )))                              |         |
                  |         |                                                                 |         |
                  |    +------------------------+                                             |    +------------------------+
                  |    | value: false           |                                             |    | value: false           |
                  |    | informant: false       |<----------+                                 |    | informant: false       |
                  |    | constraints: '()       |           |                                 |    | constraints: '()       |
                  |    | set-my-value: -----------------+   |                                 |    | set-my-value: ...      |
                  |    | forget-my-value: ...   |       |   |                                 |    | forget-my-value: ...   |
                  |    | connect: ...           |       |   |                                 |    | connect: ...           |
                  | +--- me                     |       |   |                                 | +--- me                     |
                  | |  +------------------------+       |   |                                 | |  +------------------------+
                  | |       ^          ^                |   |                                 | |       ^
                  | |       |          |                v   |                                 | |       |
                  v v       |          |               @=@--+                                 v v       |
                  @=@-------+   +---------------+      |                                      @=@-------+
                  |             | newval: 10    |      |                                      |
                  |             | setter: 'user |      v                                      |
                  |             +---------------+    params: newval, setter                   |
                  |                                  body: (cond ...                          |
                  v                                              (for-each-except ... ))      v
                params: request                                                             params: request
                body: (cond ((eq? request 'has-value?) ... ))                               body: (cond ((eq? request 'has-value?) ... ))







           +---------------------------------------------------------------------------------------------------------+
           |                                                                                                         |
global --> |  a --+     make-connector --+         for-each-except: ...                         b --+                |
           |      |                      |                                                          |                |
           +------|----------------------|----------------------------------------------------------|----------------+
                  |         ^            |   ^                      ^                               |         ^
                  |         |            v   |                      |                               |         |
                  |         |           @=@--+                      |                               |         |
                  |    +---------+      |                     +--------------------------------+    |    +---------+
                  |    |         |      |                     | exception: 'user               |    |    |         |
                  |    +---------+      |                     | procedure: inform-about-value  |    |    +---------+
                  |         ^           v                     | list: '()                      |    |         ^
                  |         |         params: _               +--------------------------------+    |         |
                  |         |         body: (let ((value ... )))                                    |         |
                  |         |                                                                       |         |
                  |    +------------------------+                                                   |    +------------------------+
                  |    | value: 10              |                                                   |    | value: false           |
                  |    | informant: 'user       |<----------+                                       |    | informant: false       |
                  |    | constraints: '()       |           |                                       |    | constraints: '()       |
                  |    | set-my-value: -----------------+   |                                       |    | set-my-value: ...      |
                  |    | forget-my-value: ...   |       |   |                                       |    | forget-my-value: ...   |
                  |    | connect: ...           |       |   |                                       |    | connect: ...           |
                  | +--- me                     |       |   |                                       | +--- me                     |
                  | |  +------------------------+       |   |                                       | |  +------------------------+
                  | |       ^          ^                |   |                                       | |       ^
                  | |       |          |                v   |                                       | |       |
                  v v       |          |               @=@--+                                       v v       |
                  @=@-------+   +---------------+      |                                            @=@-------+
                  |             | newval: 10    |      |                                            |
                  |             | setter: 'user |      v                                            |
                  |             +---------------+    params: newval, setter                         |
                  |                                  body: (cond ...                                |
                  v                                              (for-each-except ... ))            v
                params: request                                                             params: request
                body: (cond ((eq? request 'has-value?) ... ))                               body: (cond ((eq? request 'has-value?) ... ))
|#
