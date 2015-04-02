# Carotene

Carotene is a real-time PubSub server that complements web applications, although it can be run alone if you don't need any backend (i.e. a simple chat).

It is designed to provide a fast, scalable and simple solution. To do a single thing and to do it well, so developers can focus on the logic of their applications in their preferred technology while relying on Carotene to manage persistent connections and PubSub messaging.

http://carotene-project.com

# Features

* Transport using **websockets**, falling back to regular HTTP **long-polling** when websockets are not available.
* **Authentication**: ability to identify users in the system and group connections opened by the same user.
* **Authorization**: ability to restrict publication and subscription.
* **Presence**: know who is subscribed to a channel.
* Interoperability with **Redis** or **RabbitMq**: messages can be republished to message queues for further processing.
* HTTP based backends can publish and subscribe to channels without implementing websockets or other long-lived connection mechanisms.
* **Clustered**: Carotene nodes can be joined in clusters, in case you need more capacity.

# Client js library

Client side JavaScript library is in its own repository: https://github.com/carotene/carotene-js

# Building

See build instructions: http://carotene-project.com/docs/manual/installation.html

# Platform

Carotene is built with Erlang/OTP, and uses bullet/cowboy to manage the underlying HTTP connections.

# Documentation

Docs are hosted in their own website.

For detailed information and reference, check out [the manual](http://carotene-project.com/docs/manual.html).

If you prefer a more practical approach, we have written a [step-by-step tutorial](http://carotene-project.com/docs/quick_overview/index.html).

# Contributing

Contributors are very welcome! Just open an issue or a pull request.
