Definitions.

U = [A-Z]
L = [a-z]
D = [0-9]
SYM = [_\-.:/*+]
WHITESPACE = [\s\t\n\r]
ALPHA = ({U}|{L})
ALNUM = ({ALPHA}|{D})
ALSYM = ({ALNUM}|{SYM}|{WHITESPACE})

Rules.

<({ALNUM})+>    : {token, {opentag, lists:sublist(TokenChars, 2, TokenLen-2)}}.
</({ALNUM})+>   : {token, {closetag, lists:sublist(TokenChars, 3, TokenLen-3)}}.
{WHITESPACE}+ : skip_token.
{ALSYM}+        : {token, {string, string:strip(TokenChars)}}.


Erlang code.

