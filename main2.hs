import Data.List
import System.IO
import Parser
main::IO()
main = do --main
    let program = "program caio ; if a > 2 then a := 1 end"
    let arvore = prog ( words program)
    print arvore