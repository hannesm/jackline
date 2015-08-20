[![Build Status](https://travis-ci.org/hannesm/jackline.svg?branch=master)](https://travis-ci.org/hannesm/jackline)

### [Jackline](https://en.wikipedia.org/wiki/Jackline)

![Screenshot](http://berlin.ccc.de/~hannes/jackline.png)

The goal is a minimalistic graphical user interface for a secure (fail
hard!) and trustworthy XMPP client.

This goal is achieved with clean-slate libraries (OCaml-TLS,
OCaml-OTR) and few features: no support for HTML markup, avatars,
which music you're playing, timezone you're living in, ...

This is beta software... feedback welcome!  Security audits are
needed, please contact me if you plan to do any or already did some.
The OCaml-TLS implementation received some preliminary black box and
white box testing.

The configuration has to include the trust anchor for the server
certificate (or the SHA256 fingerprint of the certificate) - otherwise
there is no way how a client can ensure to talk to the correct XMPP
server.  There won't be any 'ignore ssl warnings' option.

Supported features:
- single XMPP account
- OTR (built-in and enabled by default)
- strict TLS certificate verification
- plain text (no HTML!)
- XEP-0184 (Message Delivery Receipts)
- _no_ import of OTR keys or configuration
- _no_ plaintext connections to XMPP server

The interface is inspired by [mcabber](http://mcabber.com).

Trusted code base is at the moment:
- [OCaml-OTR](https://github.com/hannesm/ocaml-otr)
- [OCaml-TLS](https://github.com/mirleft/ocaml-tls)
- [XMPP](https://github.com/hannesm/xmpp)
- [XML](https://github.com/ermine/xml)
- [OCaml compiler](http://ocaml.org/) (and its runtime)

Transitive dependencies are only partially listed.  For a complete
list, use ``opam list --required-by=jackline --recursive``.

Why should you trust this? Well, first of all whom do you trust? And
why? Did you read through your kernel, libc and malloc implementation?
What about OpenSSL? libotr? libpurple, loudmouth (or whatever XMPP
implementation you use)? Programming language runtime?

OCaml is a game changer compared to C: automtic memory management; I
try to stick to a purely functional (using immutable data and
declarative) coding style (this code here is not there yet).

OPAM is the OCaml package manager, and not directly needed, but very
convenient for installation and updating.  It lacks package signing,
but has a
[proposal](http://opam.ocaml.org/blog/Signing-the-opam-repository/)!

### Installing jackline

First be aware that this is unreleased alpha software.  Bug reports are
welcome (pull requests as well).

Get OCaml (4.02.1 preferred), get opam (1.2),
[aspcud](http://www.cs.uni-potsdam.de/wv/aspcud/) and
[gmp](http://gmplib.org/) are also required.
If you have an older OCaml compiler, run `opam switch 4.02.1`.

Run the following commands:
- `opam repo add xmpp-dev git://github.com/hannesm/xmpp-opam`
- `opam update`
- `opam install jackline`

Now you should have a `~/.opam/system/bin/jackline` (or
`~/.opam/4.02.1/bin/jackline`), which should be in your `PATH`.

To update, simply run `opam update` followed by `opam upgrade`.  This
will get you the latest version (git master).

### Configuration

When you start `jackline` for the first time, it starts an interactive
configuration dialog asking about account details.  There is no need
to provide optional information.  Hostname and which common name
should appear in the certificate is derived from the jabber id.

The configuration file is stored in
`~/.config/ocaml-xmpp-client/*/config.sexp`, you can specify another
directory by using the `-f` command line argument.  Next to the
configuration, there is a file containing your `password` (unless you
decided to enter it on every start of jackline), a `otr_dsa.sexp`
containing your OTR key, a `users.sexp` with buddy list information
(OTR fingerprints).

### Using jackline

Left is the contact list, in the middle the chat window, below the log
buffer.  `F10` and `F11` (and `Shift + F10`, `Shift + F11`) modify
their sizes.  The bottom line is read-line prompt with tab-completion.

In the contact list, mutual presence subscription information is
indicated by `[` and `]` (`F` if contact is only subscribed to your
presence updates, `T` if you are subscribed to the presence updates of
the contact), `?` for no presence subscription).  The self-contact
uses curly braces `{` and `}` and has initial focus.

The presence is indicated by a single character (o = online, f = free,
a = away, d = do not disturb, x = extended away, _ = offline).  The
focussed contact reverses foreground and background color.  Blinking
(and prepended `*`) of a contact indicates that a new message arrived
(also, a blue # on the left bottom appears).

A message is sent to the active contact by typing it followed by `return`.

In the chat window, each message is prefixed with 3 characters:
- `***` is a local message.
- `<--` is an incoming unencrypted message.
- `<O-` is an incoming encrypted message (`O` indicates OTR encryption).
- `-->` is an outgoing unencrypted message, which has been received by the other client.
- `?->` is an outgoing unencrypted message waiting for a message delivery receipt (XEP 184).
- `-O>` is an outgoing encrypted message, which was received.
- `?O>` is an outgoing encrypted message waiting for receipt.

Active keys:
- `PgUp`, `PgDown` navigates through the contact list
- `Ctrl-q` jumps to next notification
- `Ctrl-x` jumps to last active user
- `F5` toggles display of offline contacts
- `F12` toggles between display of contact list, full screen chat, and raw (only received messages)
- `F11` and `Shift-F11` increase and decrease width of buddy list
- `F10` and `Shift-F10` increase and decrease height of log window
- `Ctrl-PgUp`, `Ctrl-PgDown` scrolls chat window
- `<tab>` tab completion (largest prefix)
- `Ctrl-a` (jump to beginning of line), `Ctrl-e` (jump to end of line), `Ctrl-k` (kill text to the right of cursor), `Ctrl-u` (kill text to the left of cursor), `Ctrl-left` (jump word backwards), `Ctrl-right` (jump word forwards), `Ctrl-f` (forward one character), `Ctrl-b` (backward one character)
- `Ctrl-space` (mark, indicated by underline), `Ctrl-w` (cut), `Ctrl-y` (yank)
- `Ctrl-_` undo

`/help` prints the available commands, `/help command` more detailed help of the given command.

#### Colors

contact list:
- green frame and contact: OTR session established
- red frame and contact: no OTR session
- black: no active session exists

horizontal line
- red OTR fingerprint: not verified (use a second channel)
- green OTR: key is verified

status line
- red jabber id: logging to disk is enabled for this contact

### FAQ

- How do I prevent jackline from doing DNS lookups? -- Interactive configuration or specify `(hostname ("bla.com"))` in `config.sexp`.
- The server certificate does not match the server name, how do I fix this? -- Interactive configuration or specify `(cert_hostname ("blabla.com"))` in `config.sexp`.
- Keys do not work on MacOSX -- [This](https://github.com/timothybasanov/terminal-app-function-keys#full-list-of-all-bindings) might be useful.
- `Ctrl-D` terminates a session, this is insane. -- Create a `~/.lambda-term-inputrc` with the following content:

 ````
 [read-line]
 C-d: goto-eol
 ````

- I want to receive notifications. -- There are two ways at the moment: `notification.state` contains the current state (written on every change) and a hook script can be defined during interactive configuration or `(notification_callback (/my/favorite/script.sh))` in `config.sexp`.  It is executed with two arguments: the jabber id and state.
- I want a systray icon. -- you might be interested in [this script](https://github.com/cfcs/misc/blob/f3cb0cf29464d820149b8cf91c6cced893331000/jackline_systray.py)