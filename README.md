# clom - Component Object Model (COM) library for Common Lisp

This is an pre-alpha library for interacting with ***COM** in Common Lisp.

As such I'll be changing around functionality pretty wildly to come up with a sane interface.

## Declaring COM interfaces in CL

We can declare interfaces like so:

``` common-lisp
(define-com-interface i-unknown "{00000000-0000-0000-C000-000000000046}" ()
  (query-interface () win32:hresult
    (riid win32:refiid)
    (ppv-obj (:pointer (:pointer :void)) :out :retval))

  (add-ref () win32:ulong)

  (release () win32:ulong))
```

Defining an interface in this way shall create a [cffi][cffi] struct for
`i-unknown`, as well as its virtual function table.

Additionally, it will create Common Lisp functions for calling each method.
The names of these functions will be constructed by prefixing the method name
with the type name.

For example, to call the `add-ref` method on `ptr`, we'd call it like this:

``` common-lisp
(i-unknown-add-ref ptr)
```

### Syntax

The (current) syntax is:

``` common-lisp
(define-com-interface name iid (base-interface+)
  method-form*)
```
- *name*: A symbol - The name of the interface
- *iid*: A string in 'braces and hyphens' form
- *base-interface*: The base interface, if any. (most commonly `i-unknown`)

Each method is of the form:

``` common-lisp
(method-name (method-flag*) method-return-type
  param-form*)
```

- *method-name* A symbol - The name of the method

- *method-flag*

    Any supported method flag.

    Currently supported method flags:
    - *:convention calling-convention*

        The calling convention of the method. One of `:stdcall` (default) or `:cdecl`
- *method-return-type*: The return type of the method as a cffi foreign type


Each parameter is of the form:

``` common-lisp
(param-name param-type param-flag*)
```

- *param-name* A symbol - The name of the parameter. Not important
- *param-type* The type of the parameter as a cffi foreign type

- *param-flag*
    A parameter flag. See [Parameter Flags](#ParameterFlags)

### Parameter flags

#### :in
Marks an input parameter. Parameters are all implicitly `:in` unless marked with `:out`
On its own, doesn't do anything. But when combined with `:out` the parameter is marked as an *inout* parameter.

For *inout* parameters, the parameter must be provided as normal, but is included in the return values of the function.

**Note**: When the parameter is *inout*, the actual value  provided to the
function should be the actual value type, not the pointer type.

For example, say we have the following method definition in an interface `i-foo`:

``` common-lisp
(set-and-return () win32:hresult
  (value (:pointer :int) :in :out))
```
It would be called as:

``` common-lisp
(i-foo-set-and-return 5)
; =>
5
```

#### :out
Marks the parameter as an output parameter. Unless marked with [:in][#:in], the
parameter isn't included as an argument to the function call, but is instead
returned as a return value.

**Note**: The parameter must be a suitable type (typically a pointer type)

#### :opt
Marks the parameter as optional. If the parameter is in an optional position
(no required parameters follow it), then it will be `&optional` and not required
at all.
If required parameters follow it, then it will still be required, but `nil`
can be used to default its value.

#### :default
Provides the default value form for the parameter.
Only used on a parameter marked with `:opt`.

**Note**: This form is evaluated when calling the method.

### :lcid
Currently unused.

### :retval
Currently unused.

Only one parameter may be marked with `:retval`

### Method Return Values
Methods may return zero or more values, depending on their return type and parameter specifications.

If the return type is anything besides `:void` or `win32:hresult` then it will
be the primary return value of the method.

When the return value of the method is `win32:hresult`, it is checked, possibly
signalling a `com-error`, but not returned as part of the return values.

Each `:out` parameter then signals an additional return value.

As an example, look at `i-unknown`:

``` common-lisp
(define-com-interface i-unknown "{00000000-0000-0000-C000-000000000046}" ()
  (query-interface () win32:hresult
    (riid win32:refiid)
    (ppv-obj (:pointer (:pointer :void)) :out :retval))

  (add-ref () win32:ulong)

  (release () win32:ulong))
```

A call to `query-interface` looks like this:

``` common-lisp
(i-unknown-query-interface ptr guid)
; =>
#.(SB-SYS:INT-SAP #XDEADBEEF)
```
Note that we don't supply a value for `ppv-obj`; it is instead returned.
Also note that the `win32:hresult` return value of the function  is not returned.

`i-type-info` has a method as follows:

``` common-lisp
(get-documentation () win32:hresult
  (memid memberid :in)
  (p-bstr-name (:pointer win32:bstr) :out)
  (p-bstr-doc-string (:pointer win32:bstr) :out)
  (pdw-help-context (:pointer win32:dword) :out)
  (p-bstr-help-file (:pointer win32:bstr) :out))
```

This method accepts two values:

1. the **this** ptr
2. the **memid**

and returns **four** values:

``` common-lisp
(i-type-info-get-documentation ptr mem-id)
; =>
"name",
"doc string",
"help context",
"help file"
```

# Dependencies
 * [alexandria](http://quickdocs.org/alexandria/)
 * [babel](http://quickdocs.org/babel/)
 * [cffi](http://quickdocs.org/cffi/)
 * [cl-ppcre](http://quickdocs.org/cl-ppcre/)
 * [win32](https://github.com/Zulu-Inuoe/win32)\*
 * [trivial-cltl2](http://quickdocs.org/trivial-cltl2/)
 * [trivial-indent](http://quickdocs.org/trivial-indent/)
 * [exit-hooks](http://quickdocs.org/exit-hooks/)

\* This library isn't on quicklisp, and will likely end up being removed as a dependency.
Right now it's used primarily because it was convenient to have common types already defined.

# License
See [LICENSE](LICENSE.txt)
