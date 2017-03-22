-module(ofxconnect).
-include("include/records.hrl").
-compile(export_all).

get_header() ->
    Headers =  [
        "OFXHEADER:100",
        "DATA:OFXSGML",
        "VERSION:102",
        "SECURITY:NONE",
        "ENCODING:USASCII",
        "CHARSET:1252",
        "COMPRESSION:NONE",
        "OLDFILEUID:NONE",
        io_lib:format("NEWFILEUID:~s", [uuid:to_string(uuid:uuid4())])
    ],
    string:join(Headers, "\n").

get_fi(Org, Fid) ->
    #ofx_node{name="FI", value=[
        #ofx_leaf{name="ORG", value=Org},
        #ofx_leaf{name="FID", value=Fid}
    ]}.

get_dtclient() ->
    {Y, Mo, D} = erlang:date(),
    {H, Mi, S} = erlang:time(),
    [io_lib:format("~4..0B", [Y]) |
     [io_lib:format("~2..0B", [Digit]) || Digit <- [Mo,D,H,Mi,S]]].

get_signon(Username, Pass, Org, Fid) ->
    #ofx_node{name="SIGNONMSGSRQV1", value=[
        #ofx_node{name="SONRQ", value=[
            #ofx_leaf{name="DTCLIENT", value=get_dtclient()},
            #ofx_leaf{name="USERID", value=Username},
            #ofx_leaf{name="USERPASS", value=Pass},
            #ofx_leaf{name="LANGUAGE", value="ENG"},
            get_fi(Org, Fid),
            #ofx_leaf{name="APPID", value="QWIN"},
            #ofx_leaf{name="APPVER", value="2200"}
        ]}
    ]}.

get_bankmessage_request(BankId, AccountId, AccountType, TimeStart, TimeEnd) ->
    #ofx_node{name="BANKMSGSRQV1", value=[
        #ofx_node{name="STMTTRNRQ", value=[
            #ofx_leaf{name="TRNUID", value=uuid:to_string(uuid:uuid4())},
            #ofx_leaf{name="CLTCOOKIE", value="1"},
            #ofx_node{name="STMTRQ", value=[
                #ofx_node{name="BANKACCTFROM", value=[
                    #ofx_leaf{name="BANKID", value=BankId},
                    #ofx_leaf{name="ACCTID", value=AccountId},
                    #ofx_leaf{name="ACCTTYPE", value=AccountType}
                ]},
                #ofx_node{name="INCTRAN", value=[
                    #ofx_leaf{name="DTSTART", value=TimeStart},
                    #ofx_leaf{name="DTEND", value=TimeEnd},
                    #ofx_leaf{name="INCLUDE", value="Y"}
                ]}
            ]}
        ]}
    ]}.

generate_request(Username, Pass, Org, Fid, BankId, AccountId, AccountType, TimeStart, TimeEnd) ->
    OfxData = ofxconnect_parser:marshal(
         #ofx_node{
            name="OFX",
            value=[
               get_signon(Username, Pass, Org, Fid),
               get_bankmessage_request(BankId, AccountId, AccountType, TimeStart, TimeEnd)
            ]
           }),
    Body = lists:flatten(
      [
       get_header(),
       "\n\n",
       OfxData
      ]),
    io:format("Body: ~s~n", [Body]),
    Body.

ofx_request(Url, Body) ->
    Headers = [],
    ContentType = "application/x-ofx",
    httpc:request(
        post,
        {Url, Headers, ContentType, Body},
        [], []
    ).

get_transactions() ->
    Header = get_header(),

    ok.

