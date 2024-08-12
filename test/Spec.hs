-- empty leaf
_l1 = leaf 1
_la = leaf 'a'

-- degree
deg _l1 == 1

-- append

_t2 = Node 0 [(Node 1 [(Node 2 []), leaf 2])]
_ports_t2 = listGraftPath _t2



main :: IO ()
main = putStrLn "Test suite not yet implemented"
