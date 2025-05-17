# Gran Language Syntax Guide

This document describes the syntax and features currently supported by your Gran language compiler.

---

## 1. **Comments**
- Single-line comments start with `//` and continue to the end of the line.

```gran
// This is a comment
```

---

## 2. **Variable Declaration**
- Use `var` to declare a variable. You can initialize it with an expression.

```gran
var x = 10;
var y = 20;
var name = "Gran";
```

---

## 3. **Expressions**
- Supported: integer literals, float literals, string literals, boolean literals (`true`, `false`), variables, arithmetic, grouping, assignment.

```gran
var z = x + y * 2;
var a = (x + y) / 2;
var b = -x;
```

---

## 4. **Screenit Statement**
- Use `screenit` to print a value (string or integer) to the console.

```gran
screenit "Hello Gran";
screenit x;
```

---

## 5. **Blocks**
- Use `{ ... }` to group statements into a block.

```gran
{
    var temp = 5;
    screenit temp;
}
```

---

## 6. **If Statement**
- Conditional execution.

```gran
if (x > 0) {
    screenit "Positive";
} else {
    screenit "Non-positive";
}
```

---

## 7. **While Loop**
- Repeats a block while a condition is true.

```gran
while (x < 10) {
    screenit x;
    x = x + 1;
}
```

---

## 8. **For Loop**
- C-style for loop.

```gran
for (var i = 0; i < 5; i = i + 1) {
    screenit i;
}
```

---

## 9. **Functions**
- Define functions with `func`.

```gran
func add(a, b) {
    return a + b;
}

func main() {
    screenit add(2, 3);
}
```

---

## 10. **Return Statement**
- Return a value from a function.

```gran
return x;
```

---

## 11. **Break Statement**
- Exit a loop early.

```gran
while (true) {
    break;
}
```

---

## 12. **Supported Operators**
- Arithmetic: `+`, `-`, `*`, `/`
- Comparison: `==`, `!=`, `<`, `<=`, `>`, `>=`
- Assignment: `=`
- Logical: (not yet implemented)

---

## 13. **Literals**
- Integer: `123`
- Float: `3.14`
- String: `"hello"`
- Boolean: `true`, `false`

---

## 14. **Example Program**

```gran
// Factorial example
func factorial(n) {
    var result = 1;
    while (n > 1) {
        result = result * n;
        n = n - 1;
    }
    return result;
}

func main() {
    screenit "Factorial of 5:";
    screenit factorial(5);
}
```

---

## 15. **Notes**
- Only `screenit` for strings and integers is currently supported.
- All variables are currently treated as integers unless assigned a string literal.
- Function calls and user-defined functions are parsed, but code generation for them may be incomplete.
- No arrays, objects, or advanced types yet.
- No logical operators (`&&`, `||`, `!`) yet.

---

**Update this file as you add more features!**
