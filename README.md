# Linear F

This is a prototype intended to test the limits of a Linear System F.

## Affine F

This example shows that you can have an explicit / monadic form of weakening. Allowing to model an Affine System F. This shows that Linear F can at least describe PLUS `⊕`.

```shell
cat examples/weak.linf | dune exec check
```
