XMPP-Client
===========

(active: grey bg, encrypted OTR session: green fg, subscription status via `?` (not for specific side), `[`, `]` (subscribed), `{}` (self-contact))
![screenshot](http://berlin.ccc.de/~hannes/cli8b.png)

The goal is a minimalistic graphical user interface for a secure (fail hard!) and trustworthy XMPP client.

This goal is achieved with clean-slate libraries (OCaml-TLS, OCaml-OTR) and few features: no support for HTML/avatars/which music you're playing/...

The configuration has to include the trust anchor for the server certificate - otherwise there is no way how a client can ensure to talk to the correct XMPP server. There won't be any 'ignore ssl warnings' option.

The features I plan to support:
- single XMPP account
- OTR (built-in and enabled by default)
- strict TLS certificate verification (trust chain, server name)
- plain text (no HTML!)
- XEP-0184 (Message Delivery Receipts)
- no OTR or config import

The interface is strongly inspired by mcabber.

Trusted code base is at the moment:
- [OCaml-OTR](https://github.com/hannesm/ocaml-otr)
- [OCaml-TLS](https://github.com/mirleft/ocaml-tls)
- [XMPP](https://github.com/hannesm/xmpp)
- [XML](https://github.com/ermine/xml)
- [OCaml compiler](http://ocaml.org/) (and runtime thereof)

Security audits are needed, please contact me if you plan to do some. Transitive dependencies are only partially listed. For a more complete list, use ``opam list --required-by=xmpp_client``.

Why should you trust this? Well, first of all whom do you trust? And why? Did you read through your libc and malloc implementation? What about OpenSSL? libotr? libpurple (or whatever XMPP implementation you use)?

OCaml is (compared to C) a game changer: no manual memory management, I stick to a pure (immutable and declarative) coding style (as usual, it can be improved). Some auditing and black box testing was done against our TLS stack.

OPAM is the OCaml package manager, and not directly needed, but very convenient for installation and updating. [It lacks package signing](https://github.com/ocaml/opam/issues/423)!

How to build
============

Get OCaml (4.02.1 preferred), get opam (1.2!), gmp is also needed
If you have an older OCaml compiler, get opam and run `opam switch 4.02.1`

Run the following commands:
- `opam repo add xmpp github.com:hannesm/xmpp-opam.git`
- `opam install xmpp_client`

Now you should have a `~/.opam/system/bin/cli_client` (or `~/.opam/4.02.1/bin/cli_client`) living in your `PATH`... interactive configuration will appear if you don't have a `~/.config/ocaml-xmpp-client/config.sexp`
