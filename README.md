# Types

## basic types
- uintN fixed
- uint variable (up to 64)
- intN fixed
- int variable (up to 64)
- float32 fixed
- float64 fixed
- float variable (up to 64)
- bool fixed
- byte fixed

## composite types
- struct name name => type
- array: [N]type
- repeated: []type
- enum: name => type

# Syntax

```
package name

import [ident] "paths" // just relative to current file

type Y enum {

}

type X struct {
    type Z struct {

    }

    z Z
    y []Y

    foo enum {
        a int
        b bool
    }
}

type F struct {
    z X.Z
}
```

# Data layout

1. varints uses utf-8 style
2. enums have at most 256 variants, 1 byte for selecting
3. variable vs fixed with values are important
4. put all the fixed with into the front
5. this means you can never remove a fixed width entry
6. add a varint offset at the front of structs to the variable table
7. add a varint size (same as table size) containing num elements
8. first table entry can be used to tell size of the rest of the table entries
9. every table entry is an offset from the start of the table to where the entry starts

## general idea

- structs have a fixed and variable section
    - the first thing is a pointer to the variable section
    - the widths for the variable section are hoisted to a table at the front of it
    - the number of entries in the table is always written

- slices are either fixed or variable
    - if fixed, just number of elements and then the elements
    - if variable, number of elements, offsets of each element, then elements

- arrays are either fixed or variable
    - if fixed, just the elements
    - if variable, offsets of each element, then elements

- enums are variable
    - just with a selector byte, then the element

## For example

```
struct {}                 => 0100
struct {x byte}           => 02xx00
struct {x [5]byte}        => 06xxxxxxxxxx00
struct {x byte; y []byte} => 02xx0105yyyyyyyyyy

[]byte    => 05xxxxxxxxxx
[][]byte  => 020102xxyyyy
[3][]byte => 010201xxyyyyzz

enum { x; y; z }          => ss
enum { x byte; y uint32 } => ssxx | ssyyyyyyyy
```

## limitations

- you cannot reorder or remove fields
- you can add fields to structs or enums
- you must add to the end of structs
