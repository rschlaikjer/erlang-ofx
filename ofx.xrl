Definitions.

U = [A-Z]
L = [a-z]
D = [0-9]
SYM = [_\-.:/*+]
ALPHA = ({U}|{L})
ALNUM = ({ALPHA}|{D})
ALSYM = ({ALNUM}|{SYM})
WHITESPACE = [\s\t\n\r]

Rules.

<({ALNUM})+>    : {token, {opentag, lists:sublist(TokenChars, 2, TokenLen-2)}}.
</({ALNUM})+>   : {token, {closetag, lists:sublist(TokenChars, 3, TokenLen-3)}}.
{ALSYM}+        : {token, {string, TokenChars}}.

{WHITESPACE}+ : skip_token.

Erlang code.

