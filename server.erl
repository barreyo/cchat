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
	InUse = lists:keymember(Username, 1, State#server_st.users),

	if 
		%% Add the new user to the server and reference the Pid of the user.
		InUse == false ->
			NewUsersList = State#server_st.users ++ [{Username, Pid}],
			NewState = State#server_st{users = NewUsersList},
			{ok, NewState};

		%% Return an error if the username is already taken.
		InUse == true ->
			{{error, username_already_taken}, State}

	end;

%% Disconnect from the server. 
loop(State, {disconnect, Username, Pid}) ->
	
	%% Remove the user tuple from the users list.
	NewList = State#server_st.users -- [{Username, Pid}],
	NewState = State#server_st{users = NewList},
	{ok, NewState};


%% Join channel if it exist otherwise create a new one and join.
loop(State, {join_channel, Channel, Username, Pid}) ->
	
	Result = lists:keymember(Channel, 1, State#server_st.channels),

	case Result of 
		
		false ->
			helper:start(list_to_atom(Channel), channel:initial_state(Channel), fun channel:main/1),
			helper:requestAsync(list_to_atom(Channel), {join_channel, Username, Pid}),
			NewChannelsList = State#server_st.channels ++ [Channel],
			NewState = State#server_st{channels = NewChannelsList},
			{ok, NewState};

		true ->
			helper:requestAsync(list_to_atom(Channel), {join_channel, Username, Pid})

	end;

%% Leave channel.
loop(State, {leave_channel, Channel, Username, Pid}) ->
	
	Result = lists:keymember(Channel, 1, State#server_st.channels),

	case Result of 

		true ->
			helper:requestAsync(list_to_atom(Channel), {leave_channel, Username, Pid}),
			{ok, State};

		false ->
			{{error, unable_to_find_channel}, State}

	end.
















