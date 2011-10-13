functor TigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tiger_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* User Declarations *)


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\179\000\005\000\179\000\007\000\179\000\009\000\179\000\
\\011\000\179\000\015\000\038\000\016\000\037\000\017\000\036\000\
\\018\000\035\000\025\000\179\000\026\000\179\000\030\000\179\000\
\\031\000\179\000\034\000\179\000\035\000\179\000\037\000\179\000\
\\038\000\179\000\042\000\179\000\043\000\179\000\044\000\179\000\000\000\
\\001\000\001\000\180\000\005\000\180\000\007\000\180\000\009\000\180\000\
\\011\000\180\000\015\000\038\000\016\000\037\000\017\000\036\000\
\\018\000\035\000\025\000\180\000\026\000\180\000\030\000\180\000\
\\031\000\180\000\034\000\180\000\035\000\180\000\037\000\180\000\
\\038\000\180\000\042\000\180\000\043\000\180\000\044\000\180\000\000\000\
\\001\000\001\000\181\000\005\000\181\000\007\000\181\000\009\000\181\000\
\\011\000\181\000\015\000\038\000\016\000\037\000\017\000\036\000\
\\018\000\035\000\025\000\181\000\026\000\181\000\030\000\181\000\
\\031\000\181\000\034\000\181\000\035\000\181\000\037\000\181\000\
\\038\000\181\000\042\000\181\000\043\000\181\000\044\000\181\000\000\000\
\\001\000\001\000\182\000\005\000\182\000\007\000\182\000\009\000\182\000\
\\011\000\182\000\015\000\038\000\016\000\037\000\017\000\036\000\
\\018\000\035\000\025\000\182\000\026\000\182\000\030\000\182\000\
\\031\000\182\000\034\000\182\000\035\000\182\000\037\000\182\000\
\\038\000\182\000\042\000\182\000\043\000\182\000\044\000\182\000\000\000\
\\001\000\001\000\183\000\005\000\183\000\007\000\183\000\009\000\183\000\
\\011\000\183\000\015\000\038\000\016\000\037\000\017\000\036\000\
\\018\000\035\000\025\000\183\000\026\000\183\000\030\000\183\000\
\\031\000\183\000\034\000\183\000\035\000\183\000\037\000\183\000\
\\038\000\183\000\042\000\183\000\043\000\183\000\044\000\183\000\000\000\
\\001\000\001\000\184\000\005\000\184\000\007\000\184\000\009\000\184\000\
\\011\000\184\000\015\000\038\000\016\000\037\000\017\000\036\000\
\\018\000\035\000\025\000\184\000\026\000\184\000\030\000\184\000\
\\031\000\184\000\034\000\184\000\035\000\184\000\037\000\184\000\
\\038\000\184\000\042\000\184\000\043\000\184\000\044\000\184\000\000\000\
\\001\000\002\000\023\000\003\000\022\000\004\000\021\000\008\000\020\000\
\\016\000\019\000\029\000\018\000\032\000\017\000\033\000\016\000\
\\036\000\015\000\040\000\014\000\041\000\013\000\000\000\
\\001\000\002\000\047\000\000\000\
\\001\000\002\000\056\000\000\000\
\\001\000\002\000\072\000\000\000\
\\001\000\002\000\073\000\000\000\
\\001\000\002\000\074\000\000\000\
\\001\000\002\000\102\000\012\000\101\000\028\000\100\000\000\000\
\\001\000\002\000\104\000\000\000\
\\001\000\002\000\121\000\000\000\
\\001\000\002\000\126\000\000\000\
\\001\000\002\000\129\000\000\000\
\\001\000\002\000\136\000\000\000\
\\001\000\002\000\138\000\000\000\
\\001\000\006\000\088\000\027\000\087\000\000\000\
\\001\000\006\000\116\000\000\000\
\\001\000\006\000\125\000\019\000\124\000\000\000\
\\001\000\006\000\137\000\000\000\
\\001\000\008\000\089\000\000\000\
\\001\000\009\000\078\000\000\000\
\\001\000\009\000\095\000\000\000\
\\001\000\009\000\115\000\000\000\
\\001\000\011\000\084\000\015\000\038\000\016\000\037\000\017\000\036\000\
\\018\000\035\000\019\000\034\000\020\000\033\000\021\000\032\000\
\\022\000\031\000\023\000\030\000\024\000\029\000\025\000\028\000\
\\026\000\027\000\000\000\
\\001\000\011\000\094\000\015\000\038\000\016\000\037\000\017\000\036\000\
\\018\000\035\000\019\000\034\000\020\000\033\000\021\000\032\000\
\\022\000\031\000\023\000\030\000\024\000\029\000\025\000\028\000\
\\026\000\027\000\000\000\
\\001\000\012\000\130\000\000\000\
\\001\000\013\000\122\000\000\000\
\\001\000\013\000\139\000\000\000\
\\001\000\015\000\038\000\016\000\037\000\017\000\036\000\018\000\035\000\
\\019\000\034\000\020\000\033\000\021\000\032\000\022\000\031\000\
\\023\000\030\000\024\000\029\000\025\000\028\000\026\000\027\000\
\\030\000\077\000\000\000\
\\001\000\015\000\038\000\016\000\037\000\017\000\036\000\018\000\035\000\
\\019\000\034\000\020\000\033\000\021\000\032\000\022\000\031\000\
\\023\000\030\000\024\000\029\000\025\000\028\000\026\000\027\000\
\\034\000\107\000\000\000\
\\001\000\015\000\038\000\016\000\037\000\017\000\036\000\018\000\035\000\
\\019\000\034\000\020\000\033\000\021\000\032\000\022\000\031\000\
\\023\000\030\000\024\000\029\000\025\000\028\000\026\000\027\000\
\\035\000\076\000\000\000\
\\001\000\015\000\038\000\016\000\037\000\017\000\036\000\018\000\035\000\
\\019\000\034\000\020\000\033\000\021\000\032\000\022\000\031\000\
\\023\000\030\000\024\000\029\000\025\000\028\000\026\000\027\000\
\\035\000\127\000\000\000\
\\001\000\019\000\075\000\000\000\
\\001\000\019\000\086\000\000\000\
\\001\000\019\000\132\000\000\000\
\\001\000\027\000\114\000\000\000\
\\001\000\037\000\071\000\000\000\
\\001\000\038\000\098\000\000\000\
\\001\000\039\000\110\000\000\000\
\\001\000\039\000\112\000\000\000\
\\142\000\015\000\038\000\016\000\037\000\017\000\036\000\018\000\035\000\
\\019\000\034\000\020\000\033\000\021\000\032\000\022\000\031\000\
\\023\000\030\000\024\000\029\000\025\000\028\000\026\000\027\000\000\000\
\\144\000\000\000\
\\145\000\010\000\026\000\014\000\025\000\027\000\024\000\000\000\
\\146\000\000\000\
\\147\000\000\000\
\\148\000\000\000\
\\149\000\000\000\
\\150\000\000\000\
\\151\000\000\000\
\\152\000\000\000\
\\153\000\000\000\
\\154\000\000\000\
\\155\000\000\000\
\\156\000\000\000\
\\157\000\015\000\038\000\016\000\037\000\017\000\036\000\018\000\035\000\
\\019\000\034\000\020\000\033\000\021\000\032\000\022\000\031\000\
\\023\000\030\000\024\000\029\000\025\000\028\000\026\000\027\000\000\000\
\\158\000\000\000\
\\159\000\000\000\
\\160\000\002\000\023\000\003\000\022\000\004\000\021\000\008\000\020\000\
\\016\000\019\000\029\000\018\000\032\000\017\000\033\000\016\000\
\\036\000\015\000\040\000\014\000\041\000\013\000\000\000\
\\161\000\000\000\
\\162\000\007\000\080\000\015\000\038\000\016\000\037\000\017\000\036\000\
\\018\000\035\000\019\000\034\000\020\000\033\000\021\000\032\000\
\\022\000\031\000\023\000\030\000\024\000\029\000\025\000\028\000\
\\026\000\027\000\000\000\
\\163\000\000\000\
\\164\000\000\000\
\\165\000\002\000\023\000\003\000\022\000\004\000\021\000\008\000\020\000\
\\016\000\019\000\029\000\018\000\032\000\017\000\033\000\016\000\
\\036\000\015\000\040\000\014\000\041\000\013\000\000\000\
\\166\000\000\000\
\\167\000\005\000\097\000\015\000\038\000\016\000\037\000\017\000\036\000\
\\018\000\035\000\019\000\034\000\020\000\033\000\021\000\032\000\
\\022\000\031\000\023\000\030\000\024\000\029\000\025\000\028\000\
\\026\000\027\000\000\000\
\\168\000\015\000\038\000\016\000\037\000\017\000\036\000\018\000\035\000\
\\019\000\034\000\020\000\033\000\021\000\032\000\022\000\031\000\
\\023\000\030\000\024\000\029\000\025\000\028\000\026\000\027\000\000\000\
\\169\000\015\000\038\000\016\000\037\000\017\000\036\000\018\000\035\000\
\\019\000\034\000\020\000\033\000\021\000\032\000\022\000\031\000\
\\023\000\030\000\024\000\029\000\025\000\028\000\026\000\027\000\
\\031\000\108\000\000\000\
\\170\000\015\000\038\000\016\000\037\000\017\000\036\000\018\000\035\000\
\\019\000\034\000\020\000\033\000\021\000\032\000\022\000\031\000\
\\023\000\030\000\024\000\029\000\025\000\028\000\026\000\027\000\000\000\
\\171\000\015\000\038\000\016\000\037\000\017\000\036\000\018\000\035\000\
\\019\000\034\000\020\000\033\000\021\000\032\000\022\000\031\000\
\\023\000\030\000\024\000\029\000\025\000\028\000\026\000\027\000\000\000\
\\172\000\015\000\038\000\016\000\037\000\017\000\036\000\018\000\035\000\
\\019\000\034\000\020\000\033\000\021\000\032\000\022\000\031\000\
\\023\000\030\000\024\000\029\000\025\000\028\000\026\000\027\000\000\000\
\\173\000\000\000\
\\174\000\000\000\
\\175\000\017\000\036\000\018\000\035\000\000\000\
\\176\000\017\000\036\000\018\000\035\000\000\000\
\\177\000\000\000\
\\178\000\000\000\
\\185\000\015\000\038\000\016\000\037\000\017\000\036\000\018\000\035\000\
\\019\000\034\000\020\000\033\000\021\000\032\000\022\000\031\000\
\\023\000\030\000\024\000\029\000\000\000\
\\186\000\015\000\038\000\016\000\037\000\017\000\036\000\018\000\035\000\
\\019\000\034\000\020\000\033\000\021\000\032\000\022\000\031\000\
\\023\000\030\000\024\000\029\000\025\000\028\000\000\000\
\\187\000\000\000\
\\188\000\042\000\046\000\043\000\045\000\044\000\044\000\000\000\
\\189\000\000\000\
\\190\000\000\000\
\\191\000\000\000\
\\192\000\000\000\
\\193\000\000\000\
\\194\000\000\000\
\\195\000\000\000\
\\196\000\000\000\
\\197\000\002\000\106\000\000\000\
\\198\000\000\000\
\\199\000\005\000\134\000\000\000\
\\200\000\015\000\038\000\016\000\037\000\017\000\036\000\018\000\035\000\
\\019\000\034\000\020\000\033\000\021\000\032\000\022\000\031\000\
\\023\000\030\000\024\000\029\000\025\000\028\000\026\000\027\000\000\000\
\\201\000\015\000\038\000\016\000\037\000\017\000\036\000\018\000\035\000\
\\019\000\034\000\020\000\033\000\021\000\032\000\022\000\031\000\
\\023\000\030\000\024\000\029\000\025\000\028\000\026\000\027\000\000\000\
\\202\000\015\000\038\000\016\000\037\000\017\000\036\000\018\000\035\000\
\\019\000\034\000\020\000\033\000\021\000\032\000\022\000\031\000\
\\023\000\030\000\024\000\029\000\025\000\028\000\026\000\027\000\000\000\
\\203\000\015\000\038\000\016\000\037\000\017\000\036\000\018\000\035\000\
\\019\000\034\000\020\000\033\000\021\000\032\000\022\000\031\000\
\\023\000\030\000\024\000\029\000\025\000\028\000\026\000\027\000\000\000\
\\204\000\008\000\054\000\010\000\053\000\000\000\
\\205\000\000\000\
\\206\000\000\000\
\"
val actionRowNumbers =
"\007\000\054\000\053\000\051\000\
\\052\000\046\000\048\000\049\000\
\\050\000\047\000\045\000\056\000\
\\075\000\084\000\008\000\007\000\
\\007\000\007\000\062\000\058\000\
\\057\000\100\000\007\000\009\000\
\\007\000\007\000\007\000\007\000\
\\007\000\007\000\007\000\007\000\
\\007\000\007\000\007\000\007\000\
\\007\000\085\000\087\000\086\000\
\\084\000\041\000\010\000\011\000\
\\012\000\037\000\035\000\033\000\
\\055\000\025\000\064\000\007\000\
\\067\000\070\000\101\000\028\000\
\\082\000\081\000\006\000\005\000\
\\004\000\003\000\002\000\001\000\
\\080\000\079\000\078\000\077\000\
\\083\000\062\000\038\000\020\000\
\\024\000\007\000\007\000\007\000\
\\060\000\061\000\007\000\029\000\
\\026\000\069\000\102\000\042\000\
\\013\000\007\000\014\000\093\000\
\\034\000\073\000\071\000\064\000\
\\043\000\065\000\066\000\007\000\
\\076\000\088\000\044\000\093\000\
\\089\000\096\000\040\000\027\000\
\\021\000\007\000\007\000\063\000\
\\007\000\069\000\015\000\031\000\
\\007\000\022\000\016\000\036\000\
\\072\000\059\000\068\000\091\000\
\\090\000\097\000\007\000\017\000\
\\030\000\007\000\098\000\039\000\
\\095\000\074\000\007\000\092\000\
\\018\000\099\000\023\000\019\000\
\\032\000\095\000\094\000\000\000"
val gotoT =
"\
\\001\000\010\000\002\000\139\000\007\000\009\000\013\000\008\000\
\\014\000\007\000\015\000\006\000\016\000\005\000\019\000\004\000\
\\020\000\003\000\024\000\002\000\025\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\041\000\004\000\040\000\005\000\039\000\006\000\038\000\
\\009\000\037\000\000\000\
\\000\000\
\\001\000\046\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\001\000\047\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\001\000\048\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\001\000\050\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\023\000\049\000\024\000\002\000\025\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\053\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\000\000\
\\001\000\055\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\001\000\056\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\001\000\057\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\001\000\058\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\001\000\059\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\001\000\060\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\001\000\061\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\001\000\062\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\001\000\063\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\001\000\064\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\001\000\065\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\001\000\066\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\001\000\067\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\068\000\004\000\040\000\005\000\039\000\006\000\038\000\
\\009\000\037\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\022\000\077\000\000\000\
\\001\000\079\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\001\000\081\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\017\000\080\000\019\000\004\000\
\\020\000\003\000\024\000\002\000\025\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\050\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\023\000\083\000\024\000\002\000\025\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\088\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\001\000\089\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\001\000\090\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\000\000\
\\000\000\
\\001\000\091\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\000\000\
\\000\000\
\\018\000\094\000\000\000\
\\000\000\
\\000\000\
\\010\000\097\000\000\000\
\\001\000\101\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\000\000\
\\011\000\103\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\022\000\107\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\109\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\111\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\115\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\001\000\116\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\000\000\
\\001\000\117\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\018\000\118\000\000\000\
\\000\000\
\\000\000\
\\001\000\121\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\126\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\000\000\
\\000\000\
\\001\000\129\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\000\000\
\\000\000\
\\012\000\131\000\000\000\
\\000\000\
\\001\000\133\000\007\000\009\000\013\000\008\000\014\000\007\000\
\\015\000\006\000\016\000\005\000\019\000\004\000\020\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\138\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 140
val numrules = 65
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | STRING of unit ->  (string) | INT of unit ->  (int)
 | ID of unit ->  (string)
end
type svalue = MlyValue.svalue
type result = unit
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 31) => true | (T 32) => true | (T 33) => true | (T 39) => true
 | (T 35) => true | (T 36) => true | (T 37) => true | (T 41) => true
 | (T 42) => true | (T 43) => true | (T 27) => true | (T 28) => true
 | (T 29) => true | (T 30) => true | (T 34) => true | (T 38) => true
 | (T 40) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 29))::
(nil
,nil
 $$ (T 30))::
(nil
,nil
 $$ (T 7))::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "INT"
  | (T 3) => "STRING"
  | (T 4) => "COMMA"
  | (T 5) => "COLON"
  | (T 6) => "SEMICOLON"
  | (T 7) => "LPAREN"
  | (T 8) => "RPAREN"
  | (T 9) => "LBRACK"
  | (T 10) => "RBRACK"
  | (T 11) => "LBRACE"
  | (T 12) => "RBRACE"
  | (T 13) => "DOT"
  | (T 14) => "PLUS"
  | (T 15) => "MINUS"
  | (T 16) => "TIMES"
  | (T 17) => "DIVIDE"
  | (T 18) => "EQ"
  | (T 19) => "NEQ"
  | (T 20) => "LT"
  | (T 21) => "LE"
  | (T 22) => "GT"
  | (T 23) => "GE"
  | (T 24) => "AND"
  | (T 25) => "OR"
  | (T 26) => "ASSIGN"
  | (T 27) => "ARRAY"
  | (T 28) => "IF"
  | (T 29) => "THEN"
  | (T 30) => "ELSE"
  | (T 31) => "WHILE"
  | (T 32) => "FOR"
  | (T 33) => "TO"
  | (T 34) => "DO"
  | (T 35) => "LET"
  | (T 36) => "IN"
  | (T 37) => "END"
  | (T 38) => "OF"
  | (T 39) => "BREAK"
  | (T 40) => "NIL"
  | (T 41) => "FUNCTION"
  | (T 42) => "VAR"
  | (T 43) => "TYPE"
  | (T 44) => "UMINUS"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 1) => MlyValue.ID(fn () => ("bogus")) | 
(T 2) => MlyValue.INT(fn () => (1)) | 
(T 3) => MlyValue.STRING(fn () => ("")) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38)
 $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31)
 $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24)
 $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10)
 $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.ntVOID exp1, exp1left, exp1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, exp1left, exp1right), rest671)
end
|  ( 1, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.ntVOID functionCall1, functionCall1left, 
functionCall1right)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  functionCall1 = functionCall1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, functionCall1left, functionCall1right), 
rest671)
end
|  ( 3, ( ( _, ( MlyValue.ntVOID lvalue1, lvalue1left, lvalue1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 lvalue1 = lvalue1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, lvalue1left, lvalue1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.ntVOID mathexp1, mathexp1left, mathexp1right
)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let
 val  mathexp1 = mathexp1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, mathexp1left, mathexp1right), rest671)

end
|  ( 5, ( ( _, ( MlyValue.ntVOID compexp1, compexp1left, compexp1right
)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let
 val  compexp1 = compexp1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, compexp1left, compexp1right), rest671)

end
|  ( 6, ( ( _, ( MlyValue.ntVOID boolexp1, boolexp1left, boolexp1right
)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let
 val  boolexp1 = boolexp1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, boolexp1left, boolexp1right), rest671)

end
|  ( 7, ( ( _, ( MlyValue.ntVOID assignment1, assignment1left, 
assignment1right)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  assignment1 = assignment1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, assignment1left, assignment1right), 
rest671)
end
|  ( 8, ( ( _, ( MlyValue.ntVOID control1, control1left, control1right
)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let
 val  control1 = control1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, control1left, control1right), rest671)

end
|  ( 9, ( ( _, ( MlyValue.ntVOID sequence1, sequence1left, 
sequence1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  sequence1 = sequence1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, sequence1left, sequence1right), rest671)

end
|  ( 10, ( ( _, ( MlyValue.ntVOID array1, array1left, array1right)) ::
 rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
array1 = array1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, array1left, array1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: ( _, ( _, 
MINUS1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, MINUS1left, exp1right), rest671)
end
|  ( 12, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 0, ( result, NIL1left, NIL1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)
) => let val  result = MlyValue.ntVOID (fn _ => ( let val  INT1 = INT1
 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, INT1left, INT1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.STRING STRING1, STRING1left, STRING1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 STRING1 = STRING1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, STRING1left, STRING1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: _ :: (
 _, ( MlyValue.ntVOID exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, 
ID1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  ID1 = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 24, ( result, ID1left, exp2right), rest671)
end
|  ( 16, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ntVOID 
sequenceExp1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ( let val  sequenceExp1 = 
sequenceExp1 ()
 in ()
end; ()))
 in ( LrTable.NT 23, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 17, ( ( _, ( MlyValue.ntVOID sequenceTail1, _, sequenceTail1right
)) :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  sequenceTail1 = sequenceTail1 ()
 in ()
end; ()))
 in ( LrTable.NT 22, ( result, exp1left, sequenceTail1right), rest671)

end
|  ( 18, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 22, ( result, defaultPos, defaultPos), rest671)
end
|  ( 19, ( ( _, ( MlyValue.ntVOID sequenceTail1, _, sequenceTail1right
)) :: ( _, ( MlyValue.ntVOID exp1, _, _)) :: ( _, ( _, SEMICOLON1left,
 _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let
 val  exp1 = exp1 ()
 val  sequenceTail1 = sequenceTail1 ()
 in ()
end; ()))
 in ( LrTable.NT 21, ( result, SEMICOLON1left, sequenceTail1right), 
rest671)
end
|  ( 20, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 21, ( result, defaultPos, defaultPos), rest671)
end
|  ( 21, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ntVOID 
functionParam1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) ::
 rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
ID1 = ID1 ()
 val  functionParam1 = functionParam1 ()
 in ()
end; ()))
 in ( LrTable.NT 15, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.ntVOID functionParamTail1, _, 
functionParamTail1right)) :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)
) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let
 val  exp1 = exp1 ()
 val  functionParamTail1 = functionParamTail1 ()
 in ()
end; ()))
 in ( LrTable.NT 16, ( result, exp1left, functionParamTail1right), 
rest671)
end
|  ( 23, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 16, ( result, defaultPos, defaultPos), rest671)
end
|  ( 24, ( ( _, ( MlyValue.ntVOID functionParamTail1, _, 
functionParamTail1right)) :: ( _, ( MlyValue.ntVOID exp1, _, _)) :: (
 _, ( _, COMMA1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  functionParamTail1 = functionParamTail1 ()
 in ()
end; ()))
 in ( LrTable.NT 17, ( result, COMMA1left, functionParamTail1right), 
rest671)
end
|  ( 25, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 17, ( result, defaultPos, defaultPos), rest671)
end
|  ( 26, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ntVOID lvalue1, lvalue1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  lvalue1 = lvalue1 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 19, ( result, lvalue1left, exp1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 18, ( result, IF1left, exp2right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.ntVOID exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp2, _, _)) :: _ :: ( _, ( MlyValue.ntVOID exp1, _, _
)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in ()
end; ()))
 in ( LrTable.NT 18, ( result, IF1left, exp3right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, _, _)) :: ( _, ( _, WHILE1left, _)) :: rest671))
 => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1
 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 18, ( result, WHILE1left, exp2right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.ntVOID exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp2, _, _)) :: _ :: ( _, ( MlyValue.ntVOID exp1, _, _
)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, FOR1left, _))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 ID1 = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in ()
end; ()))
 in ( LrTable.NT 18, ( result, FOR1left, exp3right), rest671)
end
|  ( 31, ( ( _, ( _, BREAK1left, BREAK1right)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 18, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 32, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.ntVOID 
sequenceExp1, _, _)) :: _ :: ( _, ( MlyValue.ntVOID decs1, _, _)) :: (
 _, ( _, LET1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  decs1 = decs1 ()
 val  sequenceExp1 = sequenceExp1 ()
 in ()
end; ()))
 in ( LrTable.NT 18, ( result, LET1left, END1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 14, ( result, exp1left, exp2right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 14, ( result, exp1left, exp2right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 14, ( result, exp1left, exp2right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 14, ( result, exp1left, exp2right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 13, ( result, exp1left, exp2right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 13, ( result, exp1left, exp2right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 13, ( result, exp1left, exp2right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 13, ( result, exp1left, exp2right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 13, ( result, exp1left, exp2right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 13, ( result, exp1left, exp2right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 12, ( result, exp1left, exp2right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 12, ( result, exp1left, exp2right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.ntVOID decs1, _, decs1right)) :: ( _, ( 
MlyValue.ntVOID dec1, dec1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  dec1 = dec1 ()
 val  decs1 = decs1 ()
 in ()
end; ()))
 in ( LrTable.NT 2, ( result, dec1left, decs1right), rest671)
end
|  ( 46, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 47, ( ( _, ( MlyValue.ntVOID tydec1, tydec1left, tydec1right)) ::
 rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
tydec1 = tydec1 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, tydec1left, tydec1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.ntVOID vardec1, vardec1left, vardec1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 vardec1 = vardec1 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, vardec1left, vardec1right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.ntVOID fundec1, fundec1left, fundec1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 fundec1 = fundec1 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, fundec1left, fundec1right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.ntVOID ty1, _, ty1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, TYPE1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  ty1 = ty1 ()
 in ()
end; ()))
 in ( LrTable.NT 8, ( result, TYPE1left, ty1right), rest671)
end
|  ( 51, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 in ()
end; ()))
 in ( LrTable.NT 9, ( result, ID1left, ID1right), rest671)
end
|  ( 52, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.ntVOID 
tyfields1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ( let val  tyfields1 = 
tyfields1 ()
 in ()
end; ()))
 in ( LrTable.NT 9, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( _, 
ARRAY1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  ID1 = ID1 ()
 in ()
end; ()))
 in ( LrTable.NT 9, ( result, ARRAY1left, ID1right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.ntVOID tyfieldsTail1, _, tyfieldsTail1right
)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID 
ID1, ID1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  tyfieldsTail1 = tyfieldsTail1 ()
 in ()
end; ()))
 in ( LrTable.NT 10, ( result, ID1left, tyfieldsTail1right), rest671)

end
|  ( 55, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 10, ( result, defaultPos, defaultPos), rest671)
end
|  ( 56, ( ( _, ( MlyValue.ntVOID tyfieldsTail1, _, tyfieldsTail1right
)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID 
ID1, _, _)) :: ( _, ( _, COMMA1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  tyfieldsTail1 = tyfieldsTail1 ()
 in ()
end; ()))
 in ( LrTable.NT 11, ( result, COMMA1left, tyfieldsTail1right), 
rest671)
end
|  ( 57, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 11, ( result, defaultPos, defaultPos), rest671)
end
|  ( 58, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 4, ( result, VAR1left, exp1right), rest671)
end
|  ( 59, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _,
 ( _, VAR1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 4, ( result, VAR1left, exp1right), rest671)
end
|  ( 60, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: _ :: (
 _, ( MlyValue.ntVOID tyfields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1
, _, _)) :: ( _, ( _, FUNCTION1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  tyfields1 = tyfields1 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 5, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 61, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, _, _)) :: _ :: _ :: ( _, ( MlyValue.ntVOID tyfields1,
 _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, 
FUNCTION1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  ID1 = ID1 ()
 val  tyfields1 = tyfields1 ()
 val  ID2 = ID2 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 5, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 62, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 in ()
end; ()))
 in ( LrTable.NT 6, ( result, ID1left, ID1right), rest671)
end
|  ( 63, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( 
MlyValue.ntVOID lvalue1, lvalue1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  lvalue1 = lvalue1 ()
 val  ID1 = ID1 ()
 in ()
end; ()))
 in ( LrTable.NT 6, ( result, lvalue1left, ID1right), rest671)
end
|  ( 64, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.ntVOID exp1,
 _, _)) :: _ :: ( _, ( MlyValue.ntVOID lvalue1, lvalue1left, _)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
lvalue1 = lvalue1 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 6, ( result, lvalue1left, RBRACK1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.ntVOID x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun ARRAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
end
end
