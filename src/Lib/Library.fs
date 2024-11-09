namespace FactorialLib

open System

module Factorial =
    let rec Factorial x : int = 
        if x < 0 
        then 0 
        else if x = 0 
        then 1
        else if x < 2
        then x
        else Factorial (x - 1) * x
