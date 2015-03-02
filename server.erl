-module(server).
-export([main/1, initial_state/1]).
-include_lib("./defs.hrl").

%% Handle incoming requests and keep the 
%% server online by looping.
main(State) ->
    
    receive
    	{request, From, Ref, Request} ->
    		{Response, NextState} = loop(State, Request),
    		From ! {result, Ref, Response},
    		main(NextState)
    end.

%% Set the initial state of the server.
initial_state(ServerName) ->
    #server_st{serverName = ServerName, users=[], channels=[]}.

%% Connect to the server.
loop(State, {connect, Username, Pid}) ->
	
	%% Check if the there exist a user with name of the new user.
	case lists:keymember(Username, 1, State#server_st.users) of

		%% Return an error if the username is already taken.
		true ->
			{{error, user_already_connected}, State};

		%% Add the new user to the server and reference the Pid of the user.
		false ->
			NewUsersList = State#server_st.users ++ [{Username, Pid}],
			NewState = State#server_st{users = NewUsersList},
			{ok, NewState}
	end;

%% Disconnect from the server. 
loop(State, {disconnect, Username, Pid}) ->
	
	%% Remove the user tuple from the users list.
	NewList = State#server_st.users -- [{Username, Pid}],
	NewState = State#server_st{users = NewList},
	{ok, NewState};


%% Join channel if it exist otherwise create a new one and join.
loop(State, {join_channel, Channel, Username, Pid}) ->
	
	%% Check if the channel already exist.
	case lists:member(Channel, State#server_st.channels) of 
		%% If it exist just send a request to join it.
		true ->
			spawn(fun() -> helper:requestAsync(list_to_atom(Channel), {join_channel, Username, Pid}) end),
			{ok, State}; %% Todo: return whatever the channel returns for this request.
			
		%% Create a new channel with the requested name.
		false ->
			helper:start(list_to_atom(Channel), channel:initial_state(Channel), fun channel:main/1),
			spawn(fun() ->helper:requestAsync(list_to_atom(Channel), {join_channel, Username, Pid}) end),
			NewChannelsList = State#server_st.channels ++ [Channel],
			NewState = State#server_st{channels = NewChannelsList},
			{ok, NewState}
	end;

%% Leave channel.
loop(State, {leave_channel, Channel, Username, Pid}) ->
	
	%% Check if the channel exist.
	case lists:member(Channel, State#server_st.channels) of 
		%% Do a request to the channel to remove the user from it.
		true ->
			spawn(fun() -> helper:requestAsync(list_to_atom(Channel), {leave_channel, Username, Pid}) end),
			{ok, State};

		false ->
			{{error, unable_to_find_channel}, State}
	end;

%% Send message to channel.
loop(State, {write_message, Channel, Message, Username, Pid}) ->

	%% Check if the channel exist.
	case lists:member(Channel, State#server_st.channels) of 
		%% Do a request to the channel to send a message to the users in it.
		true ->
			spawn(fun() -> helper:requestAsync(list_to_atom(Channel), {write_message, Message, Username, Pid}) end),
			{ok, State};

		false ->
			{{error, unable_to_find_channel}, State}
	end;

%% Ping a user.
loop(State, {ping, SendToUsername, FromPid, TimeStamp}) ->

	%% Check if the user is connected to the server.
	case lists:keymember(SendToUsername, 1, State#server_st.users) of
		%% Send a ping to the user.
		true ->
			helper:requestAsync(element(2, lists:keyfind(SendToUsername, 1, State#server_st.users)), {pong, FromPid, TimeStamp}),
			{ok, State};

		false ->
			{{error, user_not_found}, State}
	end.












