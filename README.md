[![Build Status](https://travis-ci.org/hannesm/xmpp-client.svg?branch=master)](https://travis-ci.org/hannesm/xmpp-client)


XMPP-Client
===========

This is unreleased alpha software... Testers and feedback welcome!

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

Security audits are needed, please contact me if you plan to do some. Transitive dependencies are only partially listed. For a more complete list, use ``opam list --required-by=xmpp_client --recursive``.

Why should you trust this? Well, first of all whom do you trust? And why? Did you read through your libc and malloc implementation? What about OpenSSL? libotr? libpurple (or whatever XMPP implementation you use)?

OCaml is (compared to C) a game changer: no manual memory management, I stick to a pure (immutable and declarative) coding style (as usual, it can be improved). Some auditing and black box testing was done against our TLS stack.

OPAM is the OCaml package manager, and not directly needed, but very convenient for installation and updating. [It lacks package signing](https://github.com/ocaml/opam/issues/423)!

How to build
============

First be aware that this is unreleased alpha software. Bug reports are welcome (pull requests as well).

Get OCaml (4.02.1 preferred), get opam (1.2!), gmp is also needed
If you have an older OCaml compiler, get opam and run `opam switch 4.02.1`

Run the following commands:
- `opam repo add xmpp-dev git://github.com/hannesm/xmpp-opam`
- `opam update -u`
- `opam install jackline`

Now you should have a `~/.opam/system/bin/jackline` (or `~/.opam/4.02.1/bin/jackline`) in your `PATH`... interactive configuration will appear if you don't have a `~/.config/ocaml-xmpp-client/config.sexp` (and don't pass a `-f` argument to the configuration directory).

Using it
========

Left is the buddy list, in the middle the chat window, below the log buffer. The last line is a prompt.

In the buddy list, presence subscription information is indicated by `[` and `]` (`?` means that no presence subscription exists for that side). The presence is indicated by a single character (o = online, f = free, a = away, d = do not disturb, x = extended away, _ = offline). One buddy is in focus (indicated by a grey background). Red foreground color means no active OTR session, green that an OTR session is active. Blinking indicates that a new message arrived. The self-contact is surrounded by curly braces.

PgUp/PgDown navigates through the buddy list, F5 toggles display of offline buddies.

Tab completion is available for the prompt. Tab completion completes the largest prefix.

`/help` prints the available commands, `/help command` more detailed help of the given command.

Sending a message is done by just typing the message followed by return.

In the chat window, each message is prefixed with 3 characters: `***` is a local message, `<O-` is an incoming message (`O` indicates OTR encryption), `rO>` is an outgoing message (`r` indicates that message delivery request was not yet answered (NYI)).

Depending on the active chat the frame colors switch to red or green - indicating whether this chat is secured with OTR or not.
