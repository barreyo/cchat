-module(channel).
-export([main/1, initial_state/1]).
-include_lib("./defs.hrl").

%% Handle incoming requests and keep the 
%% channel online by looping.
main(State) ->
    
    receive	
    	{request, From, Ref, Request} ->
    		{Response, NextState} = loop(State, Request),
    		From ! {result, Ref, Response},
    		main(NextState)
    end.

%% Set the initial state of the channel.
initial_state(ChannelName) ->
    #channel_st{channelName = ChannelName, users = []}.

%% Join channel
loop(State, {join_channel, Username, Pid}) ->
	
	%% Check if the there exist a user with name of the new user.
	case lists:keymember(Username, 1, State#channel_st.users) of

		%% Return an error if the user already in the channel.
		true ->
			{{error, user_already_joined}, State};

		%% Add the new user to the channel and reference the Pid of the user.
		false ->
			NewUsersList = State#channel_st.users ++ [{Username, Pid}],
			NewState = State#channel_st{users = NewUsersList},
			{ok, NewState}
	end;

%% Leave channel
loop(State, {leave_channel, Username, Pid}) ->
	
	%% Check if the user is in the channel.
	case lists:keymember(Username, 1, State#channel_st.users) of 

		%% Remove from channel.
		true ->
			NewUsersList = State#channel_st.users -- [{Username, Pid}],
			NewState = State#channel_st{users = NewUsersList},
			{ok, NewState};

		%% Return error if user not in channel.
		false ->
			{{error, user_not_joined}, State}

	end;

%% Send message to all users in this channel
loop(State, {write_message, Message, Username, Pid}) ->
	
	%% Check if the user is in this channel.
	case lists:keymember(Username, 1, State#channel_st.users) of

		%% Send out the message to all connected users in this channel.
		true ->
			lists:foreach(fun(E) -> 
				spawn(fun() -> helper:requestAsync(element(2, E), {incoming_msg, State#channel_st.channelName, Username, Message}) end) end,
				State#channel_st.users -- [{Username, Pid}]),
			{ok, State};

		%% Return an error if the user is not in the channel.
		false ->
			{{error, user_not_joined}, State}

	end.

	
	








