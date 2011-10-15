type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val numComment = ref 0
val stringToken = ref ""

fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in if !numComment <> 0 then (ErrorMsg.error pos "unclosed comment") else (); Tokens.EOF(pos,pos) end

%%
letter=[a-zA-Z];
digit=[0-9];
quote=[\"];
ascii={digit}{3};
escapechar=[nt\"\\]|{ascii};
whitespace=[\t\ ]+;
newline=[\n\r]+;
formatchar={whitespace}|{newline};

%s COMMENT STRING ESCAPE FORMAT;

%%
<INITIAL,COMMENT>{newline}     => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
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
<INITIAL>"then"         => (Tokens.THEN(yypos,yypos+4));
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
        
<INITIAL>{quote}        => (stringToken:=""; YYBEGIN STRING; continue());

<STRING>{quote}         => (YYBEGIN INITIAL;Tokens.STRING(!stringToken, yypos, yypos + size(!stringToken)));
<STRING>"\\"            => (YYBEGIN ESCAPE; continue());  
<STRING>{newline}       => (ErrorMsg.error yypos ("cannot have new line in string " ^ yytext); continue());
<STRING>.               => (stringToken := !stringToken ^ yytext; continue());

<ESCAPE>"\\"             => (stringToken := !stringToken ^ "\\"; YYBEGIN STRING; continue());
<ESCAPE>"n"             => (stringToken := !stringToken ^ "\n"; YYBEGIN STRING; continue());
<ESCAPE>"r"             => (stringToken := !stringToken ^ "\r"; YYBEGIN STRING; continue());
<ESCAPE>"t"             => (stringToken := !stringToken ^ "\t"; YYBEGIN STRING; continue());
<ESCAPE>"f"             => (stringToken := !stringToken ^ "\f"; YYBEGIN STRING; continue());
<ESCAPE>{ascii}         => (stringToken := !stringToken ^ String.str(Char.chr(valOf(Int.fromString yytext))); 
                            YYBEGIN STRING; 
                            continue());
<ESCAPE>{formatchar}    => (YYBEGIN FORMAT; continue());
<ESCAPE>.               => (ErrorMsg.error yypos ("illegal escape character " ^ yytext); continue());

<FORMAT>{formatchar}    => (continue()); 
<FORMAT>"\\"            => (YYBEGIN STRING; continue());
<FORMAT>.               => (ErrorMsg.error yypos ("illegal format character " ^ yytext); continue());

<INITIAL>{digit}+       => (Tokens.INT(valOf(Int.fromString yytext),yypos,yypos+size yytext));

<INITIAL>{letter}({letter}|{digit}|"_")* => (Tokens.ID(yytext,yypos,yypos+size(yytext)));

<INITIAL,COMMENT>"/*"   => ( numComment := !numComment+1; YYBEGIN COMMENT; continue());
<COMMENT>"*/"           => (numComment := !numComment-1;
                            if (!numComment=0)
                            then (YYBEGIN INITIAL; continue())
                            else (continue()));                   
<COMMENT>.              => (continue());

<INITIAL>.       => (ErrorMsg.error yypos ("illegal character:" ^ "|" ^ yytext ^ "|"); continue());