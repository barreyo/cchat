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
	AlreadyInChannel = lists:keymember(Username, 1, State#channel_st.users),

	if 
		%% Add the new user to the channel and reference the Pid of the user.
		AlreadyInChannel == false ->
			NewUsersList = State#channel_st.users ++ [{Username, Pid}],
			NewState = State#channel_st{users = NewUsersList},
			{ok, NewState};

		%% Return an error if the user already in the channel.
		AlreadyInChannel == true ->
			{{error, user_already_joined}, State}

	end;

%% Leave channel
loop(State, {leave_channel, Username, Pid}) ->
	
	%% Check if the user is in the channel
	UserInChannel = lists:keymember(Username, 1, State#channel_st.users),

	case UserInChannel of 

		%% Remove from channel
		true ->
			NewUsersList = State#channel_st.users -- [{Username, Pid}],
			NewState = State#channel_st{users = NewUsersList},
			{ok, NewState}

		%% Return error if user not in channel
		false ->
			{{error, user_not_joined}, State};

	end.






