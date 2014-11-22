Locally Relax the Value Restriction by Control Flow Analysis
=====

# Info
- Team 17

    * b00902064 宋昊恩
    * b00902107 游書泓

- Online document:

    ```
    https://docs.google.com/document/d/1JHhpFXR2ksnhukImQUVuqVkoD65kIZMADmn3mGt7mq8/edit
    ```

# Main()
1. **(purpose)** In this study, we propose an extension to the Hindley-Milner type system that loosens the value restriction.

1. **(motivation)** Since Wright suggested in [WR95], the value restriction has been the standard way to type a language with both parametric polymorphism and imperative features. The value restriction is simple and effective, but rejects many semantically correct programs.

1. **(scope)** Garrigue [GAR04] relaxes the value restriction by generalizing all covariant type variables. The modification later became part of the OCaml language.

1. **(gap indication)** However, both approaches are only ad-hoc approximations to the pureness of the input expression.

1. **(solution)** Thus we incorporate a baseline control flow analysis into the type system in this project.

1. **(results)**The control flow analysis gives a finer approximation of the pureness information. Hence we can generalize more cases than the existing systems.
