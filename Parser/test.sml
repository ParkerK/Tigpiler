CM.make "sources.cm";
fun test (dir,outdir) = 
  let
    fun listDir (s) = 
      let
        val ds = OS.FileSys.openDir (s)
        fun loop (ds) = (
          case OS.FileSys.readDir (ds) of 
            NONE => [] before OS.FileSys.closeDir (ds)
            | file => file::loop (ds)
          )
      in
        loop (ds) handle e => (OS.FileSys.closeDir (ds); raise (e))
      end
    fun checkDir (path) =
      OS.FileSys.openDir path handle e => (
        OS.FileSys.mkDir (path);
        OS.FileSys.openDir (path)
      )
    fun testList ([]) = print "\n"
    | testList (head::rest) = 
      let 
        val infile = dir ^ "/" ^ (valOf(head))
        val outfile = TextIO.openOut (outdir ^ "/" ^ (valOf(head)) ^ ".out")
        val absyn = Parse.parse infile
      in
        (
          print (infile ^ "\n");
          PrintAbsyn.print(outfile, absyn);
          testList(rest)
        )
      end
  in
    (checkDir(outdir); (* to make sure that out exists or create an empty dir *)
    testList(listDir(dir)))
  end
;
test ("../TestPrograms","../Parser");