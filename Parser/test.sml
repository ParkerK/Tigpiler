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
        fun testDir (path) =
            OS.FileSys.openDir path handle e => (
                OS.FileSys.mkDir (path);
                OS.FileSys.openDir (path)
            )
        fun printTree ([]) = print "\n"
        |   printTree (a::l) = 
            let 
                val indir = dir ^ "/" ^ (valOf(a))
                val out = outdir ^ "/" ^ (valOf(a)) ^ ".out"
                val outfile = TextIO.openOut out
            in
                (
                    print (indir ^ "\n");
                    PrintAbsyn.print(outfile, Parse.parse indir);
                    printTree(l)
                )
            end
    in
        (
        testDir(outdir);
        printTree(listDir(dir))
        )
    end
;
test ("../TestPrograms","../out");