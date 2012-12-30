A tiny language called Z
===========================

A strict, impure, curried, partially applied language with rather
peculiar syntax:

Examples

    defun add x y
          + x
            y

    print add 5
              2

    defun mult x y acc
          if = x
               0
             acc
             mult - x
                    1
                  y
                  + y
                    y

    print mult 3
               6
               0

    defun ap x y
          ++ x
             y

    defmacro when input
             fn ls
                ap "if "
                   ++ car ls
                      ++ "\n   "
                         ++ car cdr ls
                            ++ "\n   "
                               ++ car cdr cdr ls
                                  "\n   unit"
                lines input

    when = 1
           1
         print 123
