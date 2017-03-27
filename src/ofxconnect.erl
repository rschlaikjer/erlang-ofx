-module(ofxconnect).
-include("include/records.hrl").
-define(CLIENTUID, "b2f82e64-ca94-45e1-b947-b9007cd3cd08").

-export([
    get_header/0,
    get_bank/9,
    get_ccard/7,
    ofx_request/2
]).


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

get_bank(Username, Pass, Org, Fid, BankId, AccountId, AccountType, TimeStart, TimeEnd) ->
    OfxData = ofxconnect_parser:marshal(
         #ofx_node{
            name="OFX",
            children=[
               get_signon(Username, Pass, Org, Fid),
               bank_message(BankId, AccountId, AccountType, TimeStart, TimeEnd)
            ]
           }),
    [get_header(), "\n\n", OfxData].

get_ccard(Username, Pass, Org, Fid, AccountId, TimeStart, TimeEnd) ->
    OfxData = ofxconnect_parser:marshal(
         #ofx_node{
            name="OFX",
            children=[
               get_signon(Username, Pass, Org, Fid),
               ccard_message(AccountId, TimeStart, TimeEnd)
            ]
           }),
    [get_header(), "\n\n", OfxData].

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
        [], []
    ),
    Resp.
