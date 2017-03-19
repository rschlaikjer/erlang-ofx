Definitions.

U = [A-Z]
L = [a-z]
D = [0-9]
ALPHA = ({U}|{L})
ALNUM = ({ALPHA}|{D})
WHITESPACE = [\s\t\n\r]

Rules.

<{ALNUM}+>         : {token, {opentag,  TokenLine, TokenChars}}.
</{ALNUM}+>         : {token, {closetag,  TokenLine, TokenChars}}.
{D}+\.{D}+        : {token, {float, TokenLine, list_to_float(TokenChars)}}.
{D}+        : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{ALPHA}+   : {token, {string, TokenLine, TokenChars}}.

{WHITESPACE}+ : skip_token.

Erlang code.

