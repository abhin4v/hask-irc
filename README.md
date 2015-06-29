#Hircarra

> hircarra (plural hircarras)
> 
> (historical, India) A messenger, especially one who delivers a personal message.

Hircarra is a Haskell library to write [IRC][1] bots. It is meant to be very easy to use and completely extensible. It provides a core on which users can add support for their own IRC messages and replies and handlers. It also comes with a set of handlers (in the hircarra-handlers package) which provide a varied set of functionalities.


## Example Usage

To start an IRC with no handlers:

```
$ cabal repl
*Network.IRC> let botConfig = newBotConfig "irc.freenode.net" 6667 "#hircarra" (Nick "hibot") 130 DEBUG
*Network.IRC> runBot botConfig
[2015-06-29 17:37:33] Network.IRC.Client DEBUG Connecting ...
[2015-06-29 17:37:33] Network.IRC.Client DEBUG Connected
[2015-06-29 17:37:33] Network.IRC.Client DEBUG Loading msg handler: help
[2015-06-29 17:37:33] Network.IRC.Client DEBUG Loading msg handler: pingpong
[2015-06-29 17:37:33] Network.IRC.Client DEBUG Running with config:
BotConfig {
server = "irc.freenode.net"
port = 6667
channel = "#hircarra"
nick = hibot
timeout = 130
handlers = ["help","pingpong"] }
[2015-06-29 17:37:33] Network.IRC.Bot INFO > NICK hibot
[2015-06-29 17:37:33] Network.IRC.Bot INFO > USER hibot 0 * :hibot
[2015-06-29 17:37:33] Network.IRC.Bot INFO < :sinisalo.freenode.net NOTICE * :*** Looking up your hostname...
[2015-06-29 17:37:33] Network.IRC.Bot INFO < :sinisalo.freenode.net NOTICE * :*** Checking Ident
[2015-06-29 17:37:33] Network.IRC.Bot INFO < :sinisalo.freenode.net NOTICE * :*** Couldn't look up your hostname
[2015-06-29 17:37:39] Network.IRC.Bot INFO < :sinisalo.freenode.net NOTICE * :*** No Ident response
[2015-06-29 17:37:39] Network.IRC.Bot INFO < :sinisalo.freenode.net 001 hibot :Welcome to the freenode Internet Relay Chat Network hibot
[2015-06-29 17:37:39] Network.IRC.Bot INFO < :hibot MODE hibot :+i
[2015-06-29 17:37:39] Network.IRC.Bot INFO > JOIN #hircarra
[2015-06-29 17:37:45] Network.IRC.Bot INFO < :hibot!~hibot@106.51.139.38 JOIN #hircarra
[2015-06-29 17:37:45] Network.IRC.Bot INFO Joined
[2015-06-29 17:37:45] Network.IRC.Bot INFO < :sinisalo.freenode.net 353 hibot @ #hircarra :hibot @abh
[2015-06-29 17:37:45] Network.IRC.Bot INFO < :sinisalo.freenode.net 366 hibot #hircarra :End of /NAMES list.
[2015-06-29 17:38:40] Network.IRC.Bot INFO > PING :1435599520
[2015-06-29 17:38:40] Network.IRC.Bot INFO < :sinisalo.freenode.net PONG sinisalo.freenode.net :1435599520
[2015-06-29 17:39:46] Network.IRC.Bot INFO > PING :1435599586
[2015-06-29 17:39:47] Network.IRC.Bot INFO < :sinisalo.freenode.net PONG sinisalo.freenode.net :1435599586
[2015-06-29 17:40:08] Network.IRC.Bot INFO < :abh!~znc@128.199.183.32 PRIVMSG #hircarra :hi hibot
^C[2015-06-29 17:40:13] Network.IRC.Client DEBUG User interrupt
[2015-06-29 17:40:13] Network.IRC.Client DEBUG Disconnecting ...
[2015-06-29 17:40:13] Network.IRC.Client DEBUG Stopping msg handler: pingpong
[2015-06-29 17:40:13] Network.IRC.Client DEBUG Stopping msg handler: help
[2015-06-29 17:40:13] Network.IRC.Bot INFO > QUIT
[2015-06-29 17:40:14] Network.IRC.Bot INFO < :hibot!~hibot@106.51.139.38 QUIT :Client Quit
[2015-06-29 17:40:14] Network.IRC.Client DEBUG Disconnected
```

## Features

Core features:

1. Very simple interface. Just create a `BotConfig` and call `runBot` with it.
2. Automatically handles disconnetions and reconnections.
3. Automatically changes nick if not available and recovers the original nick when it becomes available.
4. Comes with standard IRC command support built in but users can extend it to add support for custom IRC commands.
5. Users can add handlers which can react to IRC commands from channels and from private messages.

## Writing Handlers

Let's write a simple echo handler:

```
$ ghci -package-db=./.cabal-sandbox/x86_64-osx-ghc-7.8.3-packages.conf.d            
GHCi, version 7.8.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude> :set prompt  "λ: "
λ> :m +Data.Map
λ> :m +Network.IRC
λ> :set +m
λ> :set -XOverloadedStrings
λ> let echoHandler = MsgHandlerMaker "echo" $ \ _ _ -> return $ newMsgHandler {
λ|   onMessage = \ Message { message = message } ->
λ|     case fromMessage message of
λ|       Just (ChannelMsg _ msg) -> do
λ|         reply <- newMessage (ChannelMsgReply msg)
λ|         return [reply]
λ|       _ -> return []
λ| }
λ|
λ> let botConfig = newBotConfig "irc.freenode.net" 6667 "#hircarra" (Nick "hibot") 130 DEBUG
λ> let botConfigWithHandler = botConfig { msgHandlerMakers = singleton "echo" echoHandler, msgHandlerInfo = singleton "echo" empty }
λ|
λ> runBot botConfigWithHandler
[2015-06-29 18:28:43] Network.IRC.Client DEBUG Connecting ...
[2015-06-29 18:28:43] Network.IRC.Client DEBUG Connected
[2015-06-29 18:28:43] Network.IRC.Client DEBUG Loading msg handler: echo
[2015-06-29 18:28:43] Network.IRC.Client DEBUG Loading msg handler: help
[2015-06-29 18:28:43] Network.IRC.Client DEBUG Loading msg handler: pingpong
[2015-06-29 18:28:43] Network.IRC.Client DEBUG Running with config:
BotConfig {
server = "irc.freenode.net"
port = 6667
channel = "#hircarra"
nick = hibot
timeout = 130
handlers = ["echo","help","pingpong"] }
[2015-06-29 18:28:43] Network.IRC.Bot INFO > NICK hibot
[2015-06-29 18:28:43] Network.IRC.Bot INFO > USER hibot 0 * :hibot
[2015-06-29 18:28:44] Network.IRC.Bot INFO < :wilhelm.freenode.net NOTICE * :*** Looking up your hostname...
[2015-06-29 18:28:44] Network.IRC.Bot INFO < :wilhelm.freenode.net NOTICE * :*** Checking Ident
[2015-06-29 18:28:44] Network.IRC.Bot INFO < :wilhelm.freenode.net NOTICE * :*** Couldn't look up your hostname
[2015-06-29 18:28:50] Network.IRC.Bot INFO < :wilhelm.freenode.net NOTICE * :*** No Ident response
[2015-06-29 18:28:50] Network.IRC.Bot INFO < :wilhelm.freenode.net 001 hibot :Welcome to the freenode Internet Relay Chat Network hibot
[2015-06-29 18:28:50] Network.IRC.Bot INFO < :hibot MODE hibot :+i
[2015-06-29 18:28:50] Network.IRC.Bot INFO > JOIN #hircarra
[2015-06-29 18:28:56] Network.IRC.Bot INFO < :hibot!~hibot@106.51.139.38 JOIN #hircarra
[2015-06-29 18:28:56] Network.IRC.Bot INFO Joined
[2015-06-29 18:28:56] Network.IRC.Bot INFO < :wilhelm.freenode.net 353 hibot @ #hircarra :hibot @abh
[2015-06-29 18:28:56] Network.IRC.Bot INFO < :wilhelm.freenode.net 366 hibot #hircarra :End of /NAMES list.
[2015-06-29 18:29:00] Network.IRC.Bot INFO < :abh!~znc@128.199.183.32 PRIVMSG #hircarra :test test
[2015-06-29 18:29:00] Network.IRC.Bot INFO > PRIVMSG #hircarra :test test
[2015-06-29 18:29:06] Network.IRC.Bot INFO < :abh!~znc@128.199.183.32 PRIVMSG #hircarra :repeater
[2015-06-29 18:29:06] Network.IRC.Bot INFO > PRIVMSG #hircarra :repeater
^C[2015-06-29 18:29:10] Network.IRC.Client DEBUG User interrupt
[2015-06-29 18:29:10] Network.IRC.Client DEBUG Disconnecting ...
[2015-06-29 18:29:10] Network.IRC.Client DEBUG Stopping msg handler: echo
[2015-06-29 18:29:10] Network.IRC.Client DEBUG Stopping msg handler: help
[2015-06-29 18:29:10] Network.IRC.Client DEBUG Stopping msg handler: pingpong
[2015-06-29 18:29:10] Network.IRC.Bot INFO > QUIT
[2015-06-29 18:29:11] Network.IRC.Client DEBUG Disconnected
```

Here is how the conversation looked in an IRC client:

```
[23:58:56] 	hibot (~hibot@106.51.139.38) joined the channel
[23:58:59]  <@abh>	test test
[23:59:00]  <hibot>	test test
[23:59:05]  <@abh>	repeater
[23:59:06]  <hibot>	repeater
[23:59:11] 	hibot (~hibot@106.51.139.38) left IRC (Client Quit)
```

[1]: https://en.wikipedia.org/wiki/Irc