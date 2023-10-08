# Notes

Just ignore

## Ast reference

```rust
let a = fn x y => + (x) (y);
let c = a 1 2 3 (b 1 2);

print c
```

```hs
(
    Assignment "a" (
        Abs ["x","y"] (
            Call "+" [
                Call "x" [],
                Call "y" []
            ]
        )
    ) (
        Assignment "c" (
            Call "a" [
                Numeric 1,
                Numeric 2,
                Numeric 3,
                Call "b" [
                    Numeric 1, Numeric 2
                ]
            ]
        ) (
            Print [
                Call "c" []
            ]
        )
    ), []
)
```