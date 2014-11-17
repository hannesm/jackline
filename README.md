XMPP-Client
===========

The goal is a minimalistic graphical user interface for a secure (fail hard!) and trustworthy XMPP client.

This goal is achieved with clean-slate libraries (OCaml-TLS, OCaml-OTR) and few features: no support for HTML/avatars/which music you're playing/...

The configuration has to include the trust anchor for the server certificate - otherwise there is no way how a client can ensure to talk to the correct XMPP server.

The features I plan to support:
- single XMPP account
- OTR (built-in and enabled by default)
- strict TLS certificate verification (trust chain, server name)
- plain text (no HTML!)
- XEP-0184 (Message Delivery Receipts)

While it is currently GTK2 code, the interface is strongly inspired by mcabber (and xmpp-client might be rewritten as a console program).

Trusted code base is at the moment:
- [OCaml-OTR](https://github.com/hannesm/ocaml-otr)
- [OCaml-TLS](https://github.com/mirleft/ocaml-tls)
- [XMPP](https://github.com/hannesm/xmpp)
- [XML](https://github.com/ermine/xml)
- [GTK2](http://www.gtk.org/) only some basic elements are used, especially no image rendering or text with fancy attributes
- [GTK2 OCaml bindings](http://lablgtk.forge.ocamlcore.org/)
- [OCaml compiler](http://ocaml.org/)
- [OPAM](http://opam.ocaml.org/)

Security audits are needed, please contact me if you plan to do some. Transitive dependencies are only partially listed. For a more complete list, use ``opam list --required-by=xmpp_client``.

OPAM is the OCaml package manager, and not directly needed, but very convenient. [It lacks package signing](https://github.com/ocaml/opam/issues/423)!
