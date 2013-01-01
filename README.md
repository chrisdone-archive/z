A tiny language called Z
===========================

A strict, impure, curried, partially applied language with rather
peculiar syntax:

Examples

    defmacro -- _
             "unit"

    -- This is the first macro. A comment macro.

    -- A simple function used in the next macro.
    defun ap x y
          ++ x
             y

    -- A messy macro (because it uses string manipulation),
    -- but demonstrates the idea well enough.
    defmacro when input
             fn blocks
                ap "if"
                   ++ z:indent-before 3
                                      car blocks
                      ++ "\n"
                         ++ z:indent 3
                                     car cdr blocks
                            ++ "\n"
                               z:indent 3
                                        "unit"
                z:blocks input

    -- Demo use of the macro. See how it looks native.
    -- Macros within macros are fine.
    when = 1
           1
         print ++ "The number is: "
                  when true
                       show 123

    -- This is the normal way to use strings.
    print "Hai, guys!"

    -- Here we define a macro to make writing strings easier,
    -- called “:”, it's meant to read like typical English,
    -- and lets you write arbitrary text as long as it's
    -- indented to the offside column.
    defmacro : input
             z:string input

    -- Example with print:
    print : Hello, World!
            What's going on in here?

    -- But also it works just fine as normal syntax within function applications:
    defun message msg
          do print : Here's a message
             print msg
             print : End of message.

    message ap : Hello,
               ++ " World! "
                  : Love ya!

    -- Expect you wouldn't write it like that, you'd just write:
    message : Everybody dance now! "Alright?"

    -- Map function.
    defun map f xs
          if unit? xs
             unit
             cons f car xs
                  map f
                      cdr xs

    -- ["foo","bar"] → foo\nbar\n
    defun unlines xs
          if unit? xs
             ""
             ++ car xs
                ++ "\n"
                   unlines cdr xs

    -- Simple id, helpful for testing.
    defun id x
          x

    -- Take the first n elements of list xs.
    defun take n xs
          if = n
               0
             unit
             if unit? xs
                unit
                cons car xs
                     take - n
                            1
                          cdr xs

    -- Take all but the last element of a list.
    defun init xs
          if unit? xs
             unit
             if unit? cdr xs
                unit
                cons car xs
                     init cdr xs

    defun last def xs
          if unit? xs
             def
             if unit? cdr xs
                car xs
                last def
                     cdr xs

    -- Print the blocks of foo and bar with ! on the end.
    print unlines map fn x
                         ++ x
                            "!"
                      z:blocks : foo
                                 bar

    -- Use of take function.
    print unlines take 3
                       z:blocks : foo
                                  bar
                                  mu
                                  zot

    -- Regexes:
    print regex:match regex:new "(abc)"
                      "abc"

    -- Bit nicer syntax:
    defun ~~ regex string
          regex:match regex
                      string

    -- Not bad.
    print ~~ regex:new "(def)"
             "defghi"

    -- Some more nicer syntax:
    defmacro rx input
             ++ "regex:new "
                z:string input

    -- Bit nicer, but not amazing.
    print ~~ rx Age: (.*)
             "Age: 123"


    -- Maybe skip the whole composing part:
    defmacro ~ input
             fn blocks
                ++ "~~ rx"
                   ++ z:indent-before 6
                                      unlines init blocks
                      ++ "\n"
                         z:indent 3
                                  last ""
                                       blocks
                z:blocks input

    print ~ Age: (.*)
            "Age: 666"

    -- Multi-line regex!
    print ~ Age: (.*)
            ([a-z]+)
            "Age: 777\nlalala"

    -- Just works with other macros =)
    print ~ Age: (.*)
            ([a-z]+)
            : Age: 999
              beep!
