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
                    Request = helper:request(list_to_atom(Server), {connect, St#cl_st.username, self()}),

                    case Request of
                        ok ->
                            NewState = St#cl_st{server = Server},
                            {ok, NewState};
                        {error, username_already_taken} ->
                            {{error, username_already_taken, "Username is already taken on server."}, St};
                        Error ->
                            {{error, yoyoyo, "Errorororors."}, St}

                    end;
                false ->
                    {{error, server_not_reached, "Server not found."}, St}
            end
    end; 

%% Disconnect from server
loop(St, disconnect) ->
    
    if 
        St#cl_st.server == disconnected ->
            {{error, not_connected, "Not connected to any server."}, St};

        true ->
            Request = helper:request(list_to_atom(St#cl_st.server), {disconnect, St#cl_st.username, self()}),
            NewState = St#cl_st{server = disconnected},
            {ok, NewState}
    end;

%% Join channel
loop(St, {join, Channel}) ->
    
    if 
        St#cl_st.server == disconnected ->
            {{error, not_connected, "Not connected to any server."}, St};
        
        true ->
            case lists:member(Channel, St#cl_st.channels) of

                true ->
                    {{error, user_already_joined, "Already in this channel."}, St};

                false ->                    
                    Request = helper:request(list_to_atom(St#cl_st.server), {join_channel, Channel, St#cl_st.username, self()}),

                    case Request of
                        ok ->
                            NewChannelList = St#cl_st.channels ++ [Channel],
                            NewState = St#cl_st{channels = NewChannelList},
                            {ok, NewState};

                        {{error, user_already_joined}, State} ->
                            {{error, user_already_joined, "Already in this channel."}, St};

                        Error ->
                            {{error, generic_error, "Channel error."}, St}

                    end
            end
    end;


%% Leave channel
loop(St, {leave, Channel}) ->

    if 
        St#cl_st.server == disconnected ->
            {{error, not_connected, "Not connected to any server."}, St};
        
        true ->
            case lists:member(Channel, St#cl_st.channels) of

                true ->                    
                    Request = helper:request(list_to_atom(St#cl_st.server), {leave_channel, Channel, St#cl_st.username, self()}),

                    case Request of
                        ok ->
                            NewChannelList = St#cl_st.channels -- [Channel],
                            NewState = St#cl_st{channels = NewChannelList},
                            {ok, NewState};

                        {{error, user_not_joined}, State} ->
                            {{error, user_not_joined, "Not in this channel."}, State};

                        Error ->
                            {{error, generic_error, "Channel error."}, St}
                    end;
                
                false ->
                    {{error, user_already_joined, "Not in this channel."}, St}
            end
    end;

% Sending messages
loop(St, {msg_from_GUI, Channel, Msg}) ->
    % {ok, St} ;
    {{error, not_implemented, "Not implemented"}, St} ;

%% Get current nick
loop(St, whoami) ->
    {St#cl_st.username, St};

%% Change nick
loop(St, {nick, Nick}) ->
    NewState = St#cl_st{username = Nick},
    {ok, NewState};

%% Incoming message
loop(St = #cl_st { gui = GUIName }, MsgFromClient) ->
    {incoming_msg, Channel, Name, Msg} = MsgFromClient,
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.
