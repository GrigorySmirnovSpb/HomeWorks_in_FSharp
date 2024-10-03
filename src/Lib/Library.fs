namespace FactorLib

open System

module Factor =
    let rec factor x : int = if x < 2 then x else factor (x - 1) * x
