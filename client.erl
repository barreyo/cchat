-module(client).
-export([main/1, initial_state/2]).
-include_lib("./defs.hrl").

%% Receive messages from GUI and handle them accordingly
main(State) ->

    receive

        {request, From, Ref, Request} ->
            {Response, NextState} = loop(State, Request),
            From ! {result, Ref, Response},
            main(NextState)

    end.

%% Produce initial state
initial_state(Nick, GUIName) ->
    #cl_st {gui = GUIName, username = Nick, server = disconnected, channels = []}.

%% ---------------------------------------------------------------------------

%% loop handles each kind of request from GUI

%% Connect to server
loop(St, {connect, Server}) ->

    if 
        %% Check if the user is already connected to this server.
        St#cl_st.server == Server ->
            {{error, user_already_connected, "Already connected to this server."}, St};

        %% Connect to the server.
        true ->
            case lists:member(list_to_atom(Server), registered()) of

                true ->
                    case helper:request(list_to_atom(Server), {connect, St#cl_st.username, self()}) of
                        ok ->
                            NewState = St#cl_st{server = Server},
                            {ok, NewState};

                        {error, user_already_connected} ->
                            {{error, user_already_connected, "Username is already taken on server."}, St};
                        
                        _Error ->
                            {{error, generic_error, "Errorororors."}, St}

                    end;
                false ->
                    {{error, server_not_reached, "Server not found."}, St}
            end
    end; 

%% Disconnect from server
loop(St, disconnect) ->
    
    if 
        St#cl_st.server == disconnected ->
            {{error, user_not_connected, "Not connected to any server."}, St};

        length(St#cl_st.channels) /= 0 ->
            {{error, leave_channels_first, "Leave the channels before disconnecting"}, St};

        true ->
            Request = helper:request(list_to_atom(St#cl_st.server), {disconnect, St#cl_st.username, self()}),
            NewState = St#cl_st{server = disconnected},
            {ok, NewState}
    end;

%% Join channel
loop(St, {join, Channel}) ->
    
    if 
        St#cl_st.server == disconnected ->
            {{error, user_not_connected, "Not connected to any server."}, St};
        
        true ->
            case lists:member(Channel, St#cl_st.channels) of

                true ->
                    {{error, user_already_joined, "Already in this channel."}, St};

                false ->                    
                    case helper:request(list_to_atom(St#cl_st.server), {join_channel, Channel, St#cl_st.username, self()}) of
                        ok ->
                            NewChannelList = St#cl_st.channels ++ [Channel],
                            NewState = St#cl_st{channels = NewChannelList},
                            {ok, NewState};

                        {error, user_already_joined} ->
                            {{error, user_already_joined, "Already in this channel."}, St};

                        _Error ->
                            {{error, generic_error, "Channel error."}, St}

                    end
            end
    end;


%% Leave channel
loop(St, {leave, Channel}) ->

    %% Check if the user is even connected to any server.
    if 
        St#cl_st.server == disconnected ->
            {{error, not_connected, "Not connected to any server."}, St};
        
        true ->
            case lists:member(Channel, St#cl_st.channels) of

                %% Request to leave the channel.
                true ->                   
                    case helper:request(list_to_atom(St#cl_st.server), {leave_channel, Channel, St#cl_st.username, self()}) of
                        ok ->
                            NewChannelList = St#cl_st.channels -- [Channel],
                            NewState = St#cl_st{channels = NewChannelList},
                            {ok, NewState};

                        {error, user_not_joined} ->
                            {{error, user_not_joined, "Not in this channel."}, St};

                        _Error ->
                            {{error, generic_error, "Channel error."}, St}
                    end;
                
                %% Return error if the user is not in the channel
                false ->
                    {{error, user_not_joined, "Not in this channel."}, St}
            end
    end;

%% Sending messages
loop(St, {msg_from_GUI, Channel, Msg}) ->
    
    %% Check if the user is even connected to any server.
    if 
        St#cl_st.server == disconnected ->
            {{error, not_connected, "Not connected to any server."}, St};
        
        true ->
            case lists:member(Channel, St#cl_st.channels) of

                %% Do a send a message to the channel.
                true ->                    
                    case helper:request(list_to_atom(St#cl_st.server), {write_message, Channel, Msg, St#cl_st.username, self()}) of 
                        ok ->
                            {ok, St};

                        {error, user_not_joined} ->
                            {{error, user_not_joined, "Not in this channel."}, St};

                        _Error ->
                            {{error, generic_error, "Channel error."}, St}
                    end;
                
                %% Return a error if the user is not in the channel.
                false ->
                    {{error, user_not_joined, "Not in this channel."}, St}
            end
    end;

%% Get current nick
loop(St, whoami) ->
    {St#cl_st.username, St};

%% Change nick
loop(St, {nick, Nick}) ->
    if 
        St#cl_st.server == disconnected ->
            NewState = St#cl_st{username = Nick},
            {ok, NewState}; 
        true ->
            {{error, user_already_connected, "Cannot change name while connected to server."}, St} 
    end;

%% Incoming message
loop(St = #cl_st { gui = GUIName }, MsgFromClient) ->
    {incoming_msg, Channel, Name, Msg} = MsgFromClient,
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.



