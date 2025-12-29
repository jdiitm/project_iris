-module(iris_rocksdb).
-export([init/0, store/2, retrieve/1]).

%% In a real production system, this would wrap 'erlang-rocksdb' nif calls.
%% For this local simulation without C-compilation dependencies, we use DETS or just simple file append.
%% We will use DETS for simplicity of key-value storage.

-define(DB_FILE, "offline_msgs.dets").

init() ->
    %% Open or create the DETS file
    dets:open_file(?DB_FILE, [{type, bag}]).

store(User, Msg) ->
    %% Store message for user. 'bag' type allows multiple messages per user.
    dets:insert(?DB_FILE, {User, Msg}).

retrieve(User) ->
    %% Get all messages for user
    Msgs = dets:lookup(?DB_FILE, User),
    %% Remove them after retrieval (pop behavior)
    dets:delete_object(?DB_FILE, User), 
    [Msg || {_, Msg} <- Msgs].
