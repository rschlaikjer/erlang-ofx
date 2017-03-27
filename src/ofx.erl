-module(ofx).

-export([
    new_client/5
]).

new_client(Username, Password, Org, Fid, Url) ->
    ofx_sup:new_client(Username, Password, Org, Fid, Url).

