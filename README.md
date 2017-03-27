# Erlang OFX Client

Small library for making OFX requests with Erlang.

For example, to list all your accounts:

    {ok, Client} = ofxconnect:new_client(
        "banking username", "banking password",
        "HAN", % Financial ORG
        "5959", % Financial ID
        "https://eftx.bankofamerica.com/eftxweb/access.ofx" % OFX Url
     ).

    ofxconnect_client:list_accounts(Client).

Or, to parse an OFX file you have already downloaded:

    {ok, Data} = file:read_file("my_ofx_file.ofx"),
    ofxconnect_parser:unmarshal(Data).

Calls return a tree of OFX nodes, along the lines of the following:

```
#ofx_node{name = "OFX",
  children = [#ofx_node{name = "SIGNUPMSGSRSV1",
    children = [#ofx_node{name = "ACCTINFOTRNRS",
      children = [#ofx_node{name = "ACCTINFORS",
        children = [#ofx_node{name = "ACCTINFO",
          children = [#ofx_node{name = "BPACCTINFO",
            children = [#ofx_leaf{name = "SVCSTATUS",value = "ACTIVE"},
                        #ofx_node{name = "BANKACCTFROM",children = [{...}|...]}]},
                        #ofx_node{name = "BANKACCTINFO",
                                children = [#ofx_leaf{name = "SVCSTATUS",value = "ACTIVE"},
                                            #ofx_leaf{name = "HAN.ISPRMRACCT",value = [...]},
                                            #ofx_leaf{name = [...],...},
                                            #ofx_leaf{...},
                                            {...}|...]},
                      #ofx_leaf{name = "DESC",value = "EBANKING"}]},
                    #ofx_node{name = "ACCTINFO", children = [#ofx_node{name = "BANKACCTINFO",
                                children = [#ofx_leaf{name = "SVCSTATUS",value = "ACTIVE"},
                                            #ofx_leaf{name = "XFERDEST",value = [...]},
                                            #ofx_leaf{name = [...],...},
                                            #ofx_leaf{...},
                                            {...}]},
                      #ofx_leaf{name = "DESC",value = "REGULAR SAVINGS"}]},
                    #ofx_node{name = "ACCTINFO",  children = [#ofx_node{name = "CCACCTINFO",
                                children = [#ofx_leaf{name = "SVCSTATUS",value = [...]},
                                            #ofx_leaf{name = [...],...},
                                            #ofx_leaf{...},
                                            {...}|...]},
                      #ofx_leaf{name = "DESC",
                                value = "BankAmericard Cash Rewards Visa Platinum"}]},

                    [etc.]
```
