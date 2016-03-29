{application, chat_room,
 [{description, "A simple chat room"},
  {vsn, "0.0.1"},
  {modules, [
             chat_app,
             participant,
             participant_sup
            ]},
  {registered, [chat_room]},
  {application, [kernel, stdlib]},
  {mod, {chat_app, []}}
 ]}.
