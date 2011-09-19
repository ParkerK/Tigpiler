type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val numComment = ref 0
val stringToken = ref ""

fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end


%% 
letter=[a-zA-Z];
digit=[0-9];
quote=[\"];
notquote=[^\"];
ascii={digit}{3};
escapechar=[nt\"\\]|{ascii};
formatchar=[\ \f\t\n\r]+;
whitespace=[\t\r\ ]+;

%s COMMENT;
%s STRING;
%s ESCAPE;
%s FORMAT;
%%
<INITIAL,COMMENT,ESCAPE,FORMAT>\n    => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL,COMMENT>{whitespace}  => (continue());

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
        
<INITIAL>{quote}        => (stringToken:="";YYBEGIN STRING;continue());

<STRING>{quote}         => (YYBEGIN INITIAL;Tokens.STRING(!stringToken,yypos,yypos+size (!stringToken)));

<STRING>"\\n"           => (stringToken := !stringToken ^ "\n"; continue());
<STRING>"\\t"           => (stringToken := !stringToken ^ "\t"; continue());
<STRING>"\\f"           => (stringToken := !stringToken ^ "\f"; continue());
<STRING>"\\[a-zA-Z]+"   => (ErrorMsg.error yypos ("illegal escape character " ^ yytext); continue());
<STRING>"\\"            => (YYBEGIN FORMAT; continue());  
<STRING>\n|\r        => (ErrorMsg.error yypos ("cannot have new line in string " ^ yytext); continue());
<STRING>.               => (stringToken := !stringToken ^ yytext; continue());

<FORMAT>{formatchar}    => (continue()); 
<FORMAT>"\\"            => (YYBEGIN STRING; continue());
<FORMAT>{ascii}         => (stringToken := !stringToken ^ String.str(Char.chr(valOf(Int.fromString yytext))); YYBEGIN STRING; continue());
<FORMAT>.               => (ErrorMsg.error yypos ("illegal format character " ^ yytext); continue());

<INITIAL>{digit}+          => (Tokens.INT(valOf(Int.fromString yytext),yypos,yypos+size yytext));

<INITIAL>{letter}({letter}|{digit}|"_")* => (Tokens.ID(yytext,yypos,yypos+size(yytext)));

<INITIAL,COMMENT>"/*" => (YYBEGIN COMMENT; numComment := !numComment+1; continue());
<COMMENT>"*/" => (numComment := !numComment-1;
                    if (!numComment=0)
                    then (YYBEGIN INITIAL; continue())
                    else (continue()));
                    
<COMMENT>. => (continue());

<INITIAL>.       => (ErrorMsg.error yypos ("illegal character:" ^ "|"^yytext^"|"); continue());