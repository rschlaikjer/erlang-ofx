-module(ofxconnect).

-export([
    new_client/5
]).

new_client(Username, Password, Org, Fid, Url) ->
    ofxconnect_sup:new_client(Username, Password, Org, Fid, Url).

