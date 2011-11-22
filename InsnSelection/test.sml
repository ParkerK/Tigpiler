CM.make "sources.cm";
let 
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
          val outfile = outdir ^ "/" ^ (valOf(head)) ^ ".s"
        in
          (
            print (infile ^ "\n");
            Main.compile(infile, outfile);
            testList(rest)
          )
        end
    in
      (checkDir(outdir); (* to make sure that out exists or create an empty dir *)
      testList(listDir(dir)))
    end
in
  test ("../TestPrograms","../InsnSelection-Out")
end