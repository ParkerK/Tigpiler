type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val numComment = ref 0;
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

(* Parse Predefined Tokens *)
<INITIAL>"type"         => (Tokens.TYPE(yypos,yypos+4));
<INITIAL>"var"          => (Tokens.VAR(yypos,yypos+3));
<INITIAL>"function"     => (Tokens.FUNCTION(yypos,yypos+8));
<INITIAL>"break"        => (Tokens.BREAK(yypos,yypos+5));
<INITIAL>"of"           => (Tokens.OF(yypos,yypos+2));
<INITIAL>"end"          => (Tokens.END(yypos,yypos+3));
<INITIAL>"in"           => (Tokens.IN(yypos,yypos+2));
<INITIAL>"nil"          => (Tokens.NIL(yypos,yypos+3));
<INITIAL>"let"          => (Tokens.LET(yypos,yypos+3));
<INITIAL>"do"           => (Tokens.DO(yypos,yypos+2));
<INITIAL>"to"           => (Tokens.TO(yypos,yypos+2));
<INITIAL>"for"          => (Tokens.FOR(yypos,yypos+3));
<INITIAL>"while"        => (Tokens.WHILE(yypos,yypos+5));
<INITIAL>"else"         => (Tokens.ELSE(yypos,yypos+4));
<INITIAL>"if"           => (Tokens.IF(yypos,yypos+2));
<INITIAL>"array"        => (Tokens.ARRAY(yypos,yypos+5));

<INITIAL>":="           => (Tokens.ASSIGN(yypos,yypos+2));
<INITIAL>"|"            => (Tokens.OR(yypos,yypos+1));
<INITIAL>"&"            => (Tokens.AND(yypos,yypos+1));
<INITIAL>">="           => (Tokens.GE(yypos,yypos+2));
<INITIAL>">"            => (Tokens.GT(yypos,yypos+2));
<INITIAL>"<="           => (Tokens.LE(yypos,yypos+2));
<INITIAL>"<"            => (Tokens.LT(yypos,yypos+1));
<INITIAL>"<>"           => (Tokens.NEQ(yypos,yypos+2));
<INITIAL>"="            => (Tokens.EQ(yypos,yypos+1));

<INITIAL>"/"            => (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL>"*"            => (Tokens.TIMES(yypos,yypos+1));
<INITIAL>"+"            => (Tokens.PLUS(yypos,yypos+1));
<INITIAL>"-"            => (Tokens.MINUS(yypos,yypos+1));
<INITIAL>"."            => (Tokens.DOT(yypos,yypos+1));
            
<INITIAL>"{"            => (Tokens.LBRACE(yypos,yypos+1));
<INITIAL>"}"            => (Tokens.RBRACE(yypos,yypos+1));
<INITIAL>"["            => (Tokens.LBRACK(yypos,yypos+1));
<INITIAL>"]"            => (Tokens.RBRACK(yypos,yypos+1));
<INITIAL>"("            => (Tokens.LPAREN(yypos,yypos+1));
<INITIAL>")"            => (Tokens.RPAREN(yypos,yypos+1));
                    
<INITIAL>";"            => (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL>":"            => (Tokens.COLON(yypos,yypos+1));
<INITIAL>","            => (Tokens.COMMA(yypos,yypos+1));
        
<INITIAL>"string"       => (Tokens.STRING(yypos,yypos+6));
<INITIAL>"int"          => (Tokens.INT(yypos,yypos+3));

(* Parse Identifiers *)    
<INITIAL>{letter}({letter}|{digit}|"_")* => (Tokens.ID(yytest,yypos,yypos+size (yytext)));

(* Todo: EOF Handing *)
val EOF:  linenum * linenum -> token

(* Lex comments, and allow for nested comments *)
<INITIAL, COMMENT>"/*" => (YYBEGIN COMMENT; numComment := !numComment+1; continue());
<COMMENT>"*/" => (numComment := !numComment-1;
                    if (!numComment=0)
                    then (YYINITIAL; continue())
                    else (continue()));
                    
<COMMENT> . (continue())

(* If not in any other regex *)
<INITIAL>.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
