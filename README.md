A tiny language called Z
===========================

A strict, impure, curried, partially applied language with rather
peculiar syntax:

Examples

    fn x y
       fn p
          do print p
             print if = p
                        42
                      1
                      0
             - * x
                 y
               p
       5
       6
       42

→

    42
    1
    Right (Right -12)
