% This record defines the structure of the client process.
% It contains the following fields:
%   gui: the name (or Pid) of the GUI process.
-record(cl_st, {gui, username, server, channels}).

% This record defines the structure of the server process.
% It contains the following fields:
%
-record(server_st, {serverName, users, channels}).

% This record defines the structure for a channel.
% It contains the name/Pid of the channel and its users.
-record(channel_st, {channelName, users}).