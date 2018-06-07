Definitions.

U = [A-Z]
L = [a-z]
D = [0-9]
SAFESYM = [_\-.:+]
SYM = [_\-.:/*+\[\]'#,()&;@$=?]
WHITESPACE = [\s\t\n\r]
ALPHA = ({U}|{L})
ALNUM = ({ALPHA}|{D})
ALSYM = ({ALNUM}|{SYM}|{WHITESPACE})
TAGCHAR = ({ALNUM}|{SAFESYM})

Rules.

<({TAGCHAR})+>    : {token, {opentag, lists:sublist(TokenChars, 2, TokenLen-2)}}.
</({TAGCHAR})+>   : {token, {closetag, lists:sublist(TokenChars, 3, TokenLen-3)}}.
{WHITESPACE}+ : skip_token.
{ALSYM}+        : {token, {string, string:strip(TokenChars)}}.


Erlang code.

