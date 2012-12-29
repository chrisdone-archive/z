A little language called Z
===========================

Examples:

    λ> parseAndRun "(cons 1 (cons 2 unit))"
    Right (Right (cons 1 (cons 2 <unit>)))
    λ> parseAndRun "((fn x y (fn p (do (print p) (print (if (= p 42) 1 0)) (- (* x y) p)))) 5 6 42)"
    42
    1
    Right (Right -12)

Strict, impure, curried and partially applied.
