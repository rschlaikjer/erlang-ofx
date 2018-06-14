-module(ofx).

-export([
    new_client/5
]).

new_client(Username, Password, Org, Fid, Url) ->
    {ok, Pid} = ofx_sup:new_client(Username, Password, Org, Fid, Url),
    link(Pid),
    {ok, Pid}.
