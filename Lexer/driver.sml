structure Parse =
struct 
  fun parse (filename,outstream) =
    let 
      val file = TextIO.openIn filename
      fun get _ = TextIO.input file
      fun say s =  TextIO.output(outstream,s)
      fun sayln s= (say s; say "\n")
      val lexer = Mlex.makeLexer get
      fun do_it() =
        let val t = lexer()
        in 
          say t; sayln "";
          if substring(t,0,3)="EOF" then () 
          else do_it()
        end
       in 
         do_it();
         TextIO.closeIn file;
         sayln ""; 
         TextIO.flushOut outstream
      end

end

