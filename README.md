XMPP-Client
===========

The goal is a minimalistic graphical user interface for a secure (fail hard!) and trustworthy XMPP client.

This goal is achieved with clean-slate libraries (OCaml-TLS, OCaml-OTR) and few features: no support for HTML/avatars/which music you're playing, ...

The configuration has to include the trust anchor for the server certificate - otherwise there is no way how a client can ensure to talk to the correct XMPP server.

The features I plan to support:
- single XMPP account
- OTR (built-in and enabled by default)
- strict TLS certificate verification
- plain text (no HTML!)
- XEP-0184 (Message Delivery Receipts)

While it is currently GTK2 code, the interface is strongly inspired by mcabber.
