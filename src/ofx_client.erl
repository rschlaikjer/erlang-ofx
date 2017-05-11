-module(ofx_client).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).

-include_lib("ofx/include/records.hrl").
-define(TIMEOUT, infinity).
-define(CLIENTUID, "b2f82e64-ca94-45e1-b947-b9007cd3cd08").

%% Supervisor init method
-export([start_link/5]).

%% Public methos
-export([
    get_transactions_checking/5,
    get_transactions_credit/4,
    list_accounts/1
]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Internal state
-record(state, {
    username, password, org, fid, ofx_url
}).

%% Public methods
start_link(Username, Password, Org, Fid, Url)
    when is_list(Username)
       and is_list(Password)
       and is_list(Org)
       and is_list(Fid)
       and is_list(Url) ->
    gen_server:start_link(?MODULE, [Username, Password, Org, Fid, Url], []);
start_link(Username, Password, Org, Fid, Url) ->
    start_link(
        case is_binary(Username) of true -> binary_to_list(Username); false -> Username end,
        case is_binary(Password) of true -> binary_to_list(Password); false -> Username end,
        case is_binary(Org) of true -> binary_to_list(Org); false -> Username end,
        case is_binary(Fid) of true -> binary_to_list(Fid); false -> Username end,
        case is_binary(Url) of true -> binary_to_list(Url); false -> Username end
    ).

get_transactions_checking(Client, BankId, AccountId, TimeStart, TimeEnd) ->
    gen_server:call(
        Client,
        {transactions_checking, BankId, AccountId, TimeStart, TimeEnd},
        ?TIMEOUT).

get_transactions_credit(Client, AccountId, TimeStart, TimeEnd) ->
    gen_server:call(
        Client,
        {transactions_credit, AccountId, TimeStart, TimeEnd},
        ?TIMEOUT).

list_accounts(Client) ->
    gen_server:call(Client, list_accounts, ?TIMEOUT).

%% Callbacks
init([Username, Password, Org, Fid, Url]) ->
    {ok, #state{
        username=Username,
        password=Password,
        org=Org,
        fid=Fid,
        ofx_url=Url
    }}.

handle_call({transactions_checking, BankId, AccountId, TimeStart, TimeEnd}, _From, State) ->
    {reply, do_transactions_checking(
              State, BankId, AccountId, TimeStart, TimeEnd), State};
handle_call({transactions_credit, AccountId, TimeStart, TimeEnd}, _From, State) ->
    {reply, do_transactions_credit(
              State, AccountId, TimeStart, TimeEnd), State};
handle_call(list_accounts, _From, State) ->
    {reply, do_list_accounts(State), State};
handle_call(Request, From, State) ->
    lager:info("Call ~p From ~p", [Request, From]),
    {reply, ignored, State}.

handle_cast(Msg, State) ->
    lager:info("Cast ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    lager:info("Info ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(OldVsn, State, _Extra) ->
    lager:info("~p updated from vsn ~p", [?MODULE, OldVsn]),
    {ok, State}.

%% Private methods

ensure_list(BinOrList) when is_list(BinOrList) ->
    BinOrList;
ensure_list(BinOrList) when is_binary(BinOrList) ->
    binary_to_list(BinOrList).

do_transactions_checking(State=#state{}, BankId, AccountId, TimeStart, TimeEnd) ->
    req_and_parse_resp(
        State#state.ofx_url,
        [
            get_signon(State),
            bank_message(
                ensure_list(BankId),
                ensure_list(AccountId),
                "CHECKING",
                ensure_list(TimeStart),
                ensure_list(TimeEnd))
    ]).

do_transactions_credit(State=#state{}, AccountId, TimeStart, TimeEnd) ->
    req_and_parse_resp(
        State#state.ofx_url,
        [
            get_signon(State),
            ccard_message(
                ensure_list(AccountId),
                ensure_list(TimeStart),
                ensure_list(TimeEnd))
    ]).

do_list_accounts(State=#state{}) ->
    req_and_parse_resp(
        State#state.ofx_url,
        [
            get_signon(State),
            list_accounts()
    ]).

req_and_parse_resp(Url, OfxStanzas) ->
    % Combine the OFX request stanzas into a document
    OfxDocument = ofx_parser:marshal(
         #ofx_node{
            name="OFX",
            children=OfxStanzas
           }),

    % Add the OFX header to the OFX doc
    Body = [get_header(), "\n\n", OfxDocument],

    % Send the request, read the raw response
    {ok, Response} = ofx_request(Url, Body),

    % Remove the header from the response and unmarshal
    case extract_ofx_from_response(Response) of
        bad_ofx -> {error, bad_ofx};
        Binary ->
            {ok, ofx_parser:unmarshal(Binary)}
    end.

extract_ofx_from_response(Response) ->
    % Find the index of the <OFX> start tag
    case binary:match(Response, <<"<OFX>">>) of
        nomatch -> bad_ofx;
        {StartPos, _StartLen} ->
            binary:part(Response, StartPos, byte_size(Response) - StartPos)
    end.

get_header() ->
    Headers =  [
        {"OFXHEADER", "100"},
        {"DATA", "OFXSGML"},
        {"VERSION", "103"},
        {"SECURITY", "NONE"},
        {"ENCODING", "USASCII"},
        {"CHARSET", "1252"},
        {"COMPRESSION", "NONE"},
        {"OLDFILEUID", "NONE"},
        {"NEWFILEUID", uuid:to_string(uuid:uuid4())}
    ],
    string:join([[K, ":", V] || {K, V} <- Headers], "\r\n").

get_fi(Org, Fid) ->
    #ofx_node{name="FI", children=[
        #ofx_leaf{name="ORG", value=Org},
        #ofx_leaf{name="FID", value=Fid}
    ]}.

get_dtclient() ->
    {Y, Mo, D} = erlang:date(),
    {H, Mi, S} = erlang:time(),
    [io_lib:format("~4..0B", [Y]) |
     [io_lib:format("~2..0B", [Digit]) || Digit <- [Mo,D,H,Mi,S]]].

get_signon(State=#state{}) ->
    get_signon(
      State#state.username,
      State#state.password,
      State#state.org,
      State#state.fid
    ).

get_signon(Username, Pass, Org, Fid) ->
    #ofx_node{name="SIGNONMSGSRQV1", children=[
        #ofx_node{name="SONRQ", children=[
            #ofx_leaf{name="DTCLIENT", value=get_dtclient()},
            #ofx_leaf{name="USERID", value=Username},
            #ofx_leaf{name="USERPASS", value=Pass},
            #ofx_leaf{name="LANGUAGE", value="ENG"},
            get_fi(Org, Fid),
            #ofx_leaf{name="APPID", value="QWIN"},
            #ofx_leaf{name="APPVER", value="2200"},
            #ofx_leaf{name="CLIENTUID", value=?CLIENTUID}
        ]}
    ]}.

list_accounts() ->
    #ofx_node{name="SIGNUPMSGSRQV1", children=[
        #ofx_node{name="ACCTINFOTRNRQ", children=[
            #ofx_leaf{name="TRNUID", value=uuid:to_string(uuid:uuid4())},
            #ofx_leaf{name="CLTCOOKIE", value="1"},
            #ofx_node{name="ACCTINFORQ", children=[
                #ofx_leaf{name="DTACCTUP", value="19691231"}
            ]}
        ]}
    ]}.

bank_message(BankId, AccountId, AccountType, TimeStart, TimeEnd) ->
    #ofx_node{name="BANKMSGSRQV1", children=[
        #ofx_node{name="STMTTRNRQ", children=[
            #ofx_leaf{name="TRNUID", value=uuid:to_string(uuid:uuid4())},
            #ofx_leaf{name="CLTCOOKIE", value="1"},
            #ofx_node{name="STMTRQ", children=[
                #ofx_node{name="BANKACCTFROM", children=[
                    #ofx_leaf{name="BANKID", value=BankId},
                    #ofx_leaf{name="ACCTID", value=AccountId},
                    #ofx_leaf{name="ACCTTYPE", value=AccountType}
                ]},
                #ofx_node{name="INCTRAN", children=[
                    #ofx_leaf{name="DTSTART", value=TimeStart},
                    #ofx_leaf{name="DTEND", value=TimeEnd},
                    #ofx_leaf{name="INCLUDE", value="Y"}
                ]}
            ]}
        ]}
    ]}.

ccard_message(AccountId, TimeStart, TimeEnd) ->
    #ofx_node{name="CREDITCARDMSGSRQV1", children=[
        #ofx_node{name="CCSTMTTRNRQ", children=[
            #ofx_leaf{name="TRNUID", value=uuid:to_string(uuid:uuid4())},
            #ofx_leaf{name="CLTCOOKIE", value="1"},
            #ofx_node{name="CCSTMTRQ", children=[
                #ofx_node{name="CCACCTFROM", children=[
                    #ofx_leaf{name="ACCTID", value=AccountId}
                ]},
                #ofx_node{name="INCTRAN", children=[
                    #ofx_leaf{name="DTSTART", value=TimeStart},
                    #ofx_leaf{name="DTEND", value=TimeEnd},
                    #ofx_leaf{name="INCLUDE", value="Y"}
                ]}
            ]}
        ]}
    ]}.

ofx_request(Url, Body) ->
    Headers = [
        {"user-agent", "InetClntApp/3.0"},
        {"connection", "close"},
        {"accept", "*/*, application/x-ofx"}
    ],
    ContentType = "application/x-ofx",
    {ok, {_, _, Resp}} = httpc:request(
        post,
        {Url, Headers, ContentType, lists:flatten(Body)},
        [],
        [{body_format, binary}]
    ),
    {ok, Resp}.
