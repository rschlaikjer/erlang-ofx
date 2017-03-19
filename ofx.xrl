Definitions.

U = [A-Z]
L = [a-z]
D = [0-9]
ALPHA = ({U}|{L})
ALNUM = ({ALPHA}|{D})
WHITESPACE = [\s\t\n\r]

Rules.

<({ALNUM})+>    : {token, {opentag, lists:sublist(TokenChars, 2, TokenLen-2)}}.
</({ALNUM})+>   : {token, {closetag, lists:sublist(TokenChars, 3, TokenLen-3)}}.
{D}+\.{D}+      : {token, {float, list_to_float(TokenChars)}}.
{D}+            : {token, {integer, list_to_integer(TokenChars)}}.
{ALPHA}+        : {token, {string, TokenChars}}.

{WHITESPACE}+ : skip_token.

Erlang code.

