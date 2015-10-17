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
containing your OTR key, a `users` directory with a file for each
contact (OTR fingerprints, custom OTR policies, ...).

### Using jackline

Left is the contact list, in the middle the chat window, below the log
buffer.  `F10` and `F11` (and `Shift + F10`, `Shift + F11`) modify
their sizes.  The bottom line is read-line prompt with tab-completion.

In the contact list, mutual presence subscription information is
indicated by `[` and `]` (`F` if contact is only subscribed to your
presence updates, `T` if you are subscribed to the presence updates of
the contact), `?` for no presence subscription).  The own contact uses
curly braces `{` and `}`, and certain operations are not available.
The presence is indicated by a single character (o = online, f = free,
a = away, d = do not disturb, x = extended away, _ = offline).

A single contact is active, which can be modified by `PgUp/PdDown`.
The active contact is shown in reversed foreground and background
color.  Its chat content is displayed in the chat window.  Certain
commands and operations (such as sending a message) require an active
contact.

XMPP allows a contact to be logged in several times.  By default, the
resource with the highest priority (and most online status) is used
for communication.  If a contact is logged in multiple times, a `+`
occurs to its left side, and pressing `return` will expand the
contact, displaying all its sessions.  Commmunicating with the
expanded base contact will deliver the message to the bare contact, if
a specific resource is active, messages will be sent there.  The chat
log is filtered by messages to the specific resource, and merged in
the base contact.  An unexpanded contact equals to the resource with
highest priority.

When a new message is received, this is indicated by blinking of the
contact, a prepended `*` (or `☀` in case of collapsed contact), a blue
`#` in the bottom left corner, execution of `notification_callback`,
and modification of `notification.state`.

A message is sent to the active contact by typing it followed by
`return`.

In the chat window, each message is prefixed with 3 characters:
- `***` is a local message.
- `<--` is an incoming unencrypted message.
- `<O-` is an incoming OTR encrypted message
- `-->` is an outgoing unencrypted message, which has been received by the other client (XEP 184).
- `?->` is an outgoing unencrypted message waiting for a message delivery receipt (XEP 184).
- `-O>` is an outgoing OTR encrypted message, which has been received by the other client (XEP 184).
- `?O>` is an outgoing OTR encrypted message waiting for receipt (XEP 184).

Active keys:
- `PgUp`, `PgDown` navigates through the contact list
- `Ctrl-q` jumps to next notification
- `Ctrl-x` jumps to last active user
- `F5` toggles display of offline contacts
- `F12` toggles between display of contact list, full screen chat, and raw (only received messages)
- `F11` and `Shift-F11` increases and decreases width of contact list
- `F10` and `Shift-F10` increases and decreases height of log window
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
- own jabber id in reverse colors: logging to disk is enabled for this contact
- own jabber id in usual colors: no logging to disk for this contact

### FAQ

- How do I update the fingerprint of the server certificate (getting authentication failure messages)? -- Currently you have to edit `config.sexp`:  find the `(Fingerprint XXX)` data, and replace XXX with the new fingerprint (`openssl s_client -connect SERVER:5222 -starttls xmpp | openssl x509 -fingerprint -sha256 -noout` might be useful (or [tlsclient](https://github.com/hannesm/tlsclient)).
- How do I prevent jackline from doing DNS lookups? -- Interactive configuration or specify `(hostname ("146.255.57.229"))` in `config.sexp`.
- The server certificate does not match the server name, how do I fix this? -- Interactive configuration or specify `(cert_hostname ("blabla.com"))` in `config.sexp`.
- Keys do not work on MacOSX -- [This](https://github.com/timothybasanov/terminal-app-function-keys#full-list-of-all-bindings) might be useful.
- `Ctrl-D` terminates a session, this is insane. -- Create a `~/.lambda-term-inputrc` with the following content:

 ````
 [read-line]
 C-d: goto-eol
 ````

- I want to receive notifications. -- There are two ways at the moment: `notification.state` contains the current state (written on every change) and a hook script can be defined during interactive configuration or `(notification_callback (/my/favorite/script.sh))` in `config.sexp`.  It is executed with two arguments: the jabber id and state.
- I want a systray icon. -- there are two projects, [posiputt/jackification](https://github.com/posiputt/jackification), and [cfcs/misc](https://github.com/cfcs/misc/blob/master/jackline_systray.py)
- I want to have notifications on MacOSX. - Andrej wrote [a script](https://github.com/schoeke/notline) using terminal notifier; otherwise [this guide](https://gist.github.com/prebenlm/5562656) might help.