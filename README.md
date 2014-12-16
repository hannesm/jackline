[![Build Status](https://travis-ci.org/hannesm/jackline.svg?branch=master)](https://travis-ci.org/hannesm/jackline)

### [Jackline](https://en.wikipedia.org/wiki/Jackline)

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

Security audits are needed, please contact me if you plan to do some. Transitive dependencies are only partially listed. For a more complete list, use ``opam list --required-by=jackline --recursive``.

Why should you trust this? Well, first of all whom do you trust? And why? Did you read through your libc and malloc implementation? What about OpenSSL? libotr? libpurple (or whatever XMPP implementation you use)?

OCaml is (compared to C) a game changer: no manual memory management, I stick to a pure (immutable and declarative) coding style (as usual, it can be improved). Some auditing and black box testing was done against our TLS stack.

OPAM is the OCaml package manager, and not directly needed, but very convenient for installation and updating. [It lacks package signing](https://github.com/ocaml/opam/issues/423)!

### How to build

First be aware that this is unreleased alpha software. Bug reports are welcome (pull requests as well).

Get OCaml (4.02.1 preferred), get opam (1.2), aspcud and gmp are also needed.
If you have an older OCaml compiler, run `opam switch 4.02.1`.

Run the following commands:
- `opam repo add xmpp-dev git://github.com/hannesm/xmpp-opam`
- `opam update -u`
- `opam install jackline`

Now you should have a `~/.opam/system/bin/jackline` (or `~/.opam/4.02.1/bin/jackline`) in your `PATH`... interactive configuration will appear if you don't have a `~/.config/ocaml-xmpp-client/config.sexp` (and don't pass a `-f` argument to the configuration directory).

### Using it

Left is the contact list, in the middle the chat window, below the log buffer. The last line is a prompt.

In the contact list, presence subscription information is indicated by `[` and `]` (if subscription is both, `F` for from (contact is subscribed to your presence updates), `T` for to (you are subscribed to the presence updates of the contact), `?` for no presence subscription). The self-contact is marked by curly braces `{` and `}`.
The presence is indicated by a single character (o = online, f = free, a = away, d = do not disturb, x = extended away, _ = offline).
The focussed contact is marked by a grey background. Blinking of a contact indicates that a new message arrived (additionally, a blue # on the left bottom appears).

Active keys:
- PgUp/PgDown navigates through the contact list
- Ctrl-q jumps to next notification
- Ctrl-x jumps to last active user
- F5 toggles display of offline contacts
- F12 toggles display of contact list
- Ctrl PgUp/PgDown scrolls chat

Tab completion is available for the prompt. Tab completion completes the largest prefix.

`/help` prints the available commands, `/help command` more detailed help of the given command.

A message is sent to the active contact by pressing return.

In the chat window, each message is prefixed with 3 characters:
- `***` is a local message,
- `<O-` is an incoming message (`O` indicates OTR encryption),
- `rO>` is an outgoing message (`r` indicates that message delivery request was not yet answered (NYI)).

Colors:
- green frame and contact: OTR session established
- red frame and contact: no OTR session
- red OTR fingerprint: not verified (use a second channel and `/fingerprint FP`)
- black: no active session exists
- red your jabber id: logging is enabled for this contact
- blue your jabber id: logging is disabled for this contact
