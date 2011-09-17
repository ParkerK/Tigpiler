type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end


%% 
letter=[a-zA-Z]
digit=[0-9]
%%
\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
","	=> (Tokens.COMMA(yypos,yypos+1));
var  	=> (Tokens.VAR(yypos,yypos+3));
"123"	=> (Tokens.INT(123,yypos,yypos+3));
.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

<INITIAL>"type"         => (Tokens.TYPE(yypos,yypos+4));
<INITIAL>"var"          => (Tokens.TYPE(yypos,yypos+3));
<INITIAL>"function"     => (Tokens.TYPE(yypos,yypos+8));
<INITIAL>"break"        => (Tokens.TYPE(yypos,yypos+5));
<INITIAL>"of"           => (Tokens.TYPE(yypos,yypos+2));
<INITIAL>"end"          => (Tokens.TYPE(yypos,yypos+3));
<INITIAL>"in"           => (Tokens.TYPE(yypos,yypos+2));
<INITIAL>"nil"          => (Tokens.TYPE(yypos,yypos+3));
<INITIAL>"let"          => (Tokens.TYPE(yypos,yypos+3));
<INITIAL>"do"           => (Tokens.TYPE(yypos,yypos+2));
<INITIAL>"to"           => (Tokens.TYPE(yypos,yypos+2));
<INITIAL>"for"          => (Tokens.TYPE(yypos,yypos+3));
<INITIAL>"while"        => (Tokens.TYPE(yypos,yypos+5));
<INITIAL>"else"         => (Tokens.TYPE(yypos,yypos+4));
<INITIAL>"if"           => (Tokens.TYPE(yypos,yypos+2));
<INITIAL>"array"        => (Tokens.TYPE(yypos,yypos+5));

<INITIAL>":="           => (Tokens.TYPE(yypos,yypos+2));
<INITIAL>"|"            => (Tokens.TYPE(yypos,yypos+1));
<INITIAL>"&"            => (Tokens.TYPE(yypos,yypos+1));
<INITIAL>">="           => (Tokens.TYPE(yypos,yypos+2));
<INITIAL>">"            => (Tokens.TYPE(yypos,yypos+2));
<INITIAL>"<="           => (Tokens.TYPE(yypos,yypos+2));
<INITIAL>"<"            => (Tokens.TYPE(yypos,yypos+1));
<INITIAL>"<>"           => (Tokens.TYPE(yypos,yypos+2));
<INITIAL>"="            => (Tokens.TYPE(yypos,yypos+1));

<INITIAL>"/"            => (Tokens.TYPE(yypos,yypos+1));
<INITIAL>"*"            => (Tokens.TYPE(yypos,yypos+1));
<INITIAL>"+"            => (Tokens.TYPE(yypos,yypos+1));
<INITIAL>"-"            => (Tokens.TYPE(yypos,yypos+1));
<INITIAL>"."            => (Tokens.TYPE(yypos,yypos+1));
            
<INITIAL>"{"            => (Tokens.TYPE(yypos,yypos+1));
<INITIAL>"}"            => (Tokens.TYPE(yypos,yypos+1));
<INITIAL>"["            => (Tokens.TYPE(yypos,yypos+1));
<INITIAL>"]"            => (Tokens.TYPE(yypos,yypos+1));
<INITIAL>"("            => (Tokens.TYPE(yypos,yypos+1));
<INITIAL>")"            => (Tokens.TYPE(yypos,yypos+1));
                    
<INITIAL>";"            => (Tokens.TYPE(yypos,yypos+1));
<INITIAL>":"            => (Tokens.TYPE(yypos,yypos+1));
<INITIAL>","            => (Tokens.TYPE(yypos,yypos+1));
        
<INITIAL>"string"       => (Tokens.TYPE(yypos,yypos+6));
<INITIAL>"int"          => (Tokens.TYPE(yypos,yypos+3));
    
<INITIAL>{letter}({letter} | {digit} | "_")* => (Tokens.TYPE(yytest,yypos,yypos+size (yytext)));

val EOF:  linenum * linenum -> token

<INITIAL>"/*"
<INITIAL, COMMENT>"*/"
<COMMENT> . (continue())
