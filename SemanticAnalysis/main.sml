structure Main =
struct
  fun main =
    let
      fun test dir = 
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
          fun testList ([]) = print "\n"
            | testList (head::rest) = 
            let 
              val infile = dir ^ "/" ^ (valOf(head))
              val absyn = Parse.parse infile
            in
              (
                print (infile ^ "\n");
                Semant.transProg(absyn);
                testList(rest)
              )
            end
        in
          testList(listDir(dir))
        end
    in
      test "../TestPrograms"
    end
end