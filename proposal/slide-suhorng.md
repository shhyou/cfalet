Locally Relax the Value Restriction by Control Flow Analysis
=====

# Abstract

This project is aiming at relaxing the value restriction locally via flow analysis, where the value striction has long been the standard yet too restrictive solution to integrating Hindler-Milner style polymorphism with imperative features.

<!--
DELETED

The value restriction has long been adopted as the standard solution to integrating Hindley-Milner style polymorphism with imperative features, yet the value restriction is too conservative in rejecting to generalize all non-value terms. In this project
we shall aim at relaxing the value restriction locally via control flow analysis.
-->

PICTURE: ./slide-suhorng-demo/expressiveness.png

# Motivation
- The value restriction refuses to generalize all non-value terms, hence rejecting procedures that compute polymorphic functions, e.g. the combinators.

    * Also rejects polymorphic data structures

- Use of imperative features are rare; most of the computation are functionally pure.

# Challenges

- The value restriction is actually at a balance point that any extension could probably be unwillingly complex and break the module abstraction.

- Polymorphism has a bad interaction with imperative features such as mutable variable.

    ```javascript
    var mem = (function unsound() {
      var mem = null;
      return function(x) {
        if (mem === null) {
          mem = x;
          return x;
        } else {
          var y = mem;
          mem = x;
          return y;
        }
      };
    })();

    var mem = unsound();  /* mem : forall a. a -> a */
    var s = mem("hello"); /* mem "hello"  :  string */
    var n = 3 + mem(5);   /* disaster: adding number and string */
    ```

<!--
    STANDARD ML SAMPLE CODE

    ```ocaml
    fun unsound () =
      let val mem = ref NONE
      in  fn x => case !mem of
            NONE   => (mem := SOME x; x)
          | SOME y => (mem := SOME x; y)
      end

    val mem = unsound ()
    val s = mem "hello"
    val n = 3 + mem 5
    ```
-->

# Potential Solutions
