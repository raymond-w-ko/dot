; Punctuation
[
  ","
  ";"
] @punctuation.delimiter

[
  "("
  ")"
  "<<"
  ">>"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

[
  "%"
] @punctuation.special

; Parser Errors
(ERROR) @error

; Identifiers
(identifier) @variable

; Unused Identifiers
((identifier) @comment (#match? @comment "^_"))

; Comments
(comment) @comment

; Strings
(string) @string

; Modules
(alias) @type

; Atoms & Keywords
[
  (atom)
  (quoted_atom) 
  (keyword)
  (quoted_keyword)
] @symbol

; Interpolation
(interpolation "#{" @string.escape "}" @string.escape)

; Escape sequences
(escape_sequence) @string.escape

; Integers
(integer) @number

; Floats
(float) @float

; Characters
[
  (char)
  (charlist)
] @character

; Booleans
(boolean) @boolean

; Nil
(nil) @constant.builtin

; Operators
(operator_identifier) @operator

(unary_operator operator: _ @operator)

(binary_operator operator: _ @operator)

; Pipe Operator
(binary_operator operator: "|>" right: (identifier) @function)

(dot operator: _ @operator)

(stab_clause operator: _ @operator)

; Local Function Calls
(call target: (identifier) @function) 

; Remote Function Calls
(call target: (dot left: [
  (atom) @type 
  (_)
] right: (identifier) @function) (arguments))

; Definition Function Calls
(call target: ((identifier) @keyword.function (#any-of? @keyword.function
  "def"
  "defdelegate"
  "defexception"
  "defguard"
  "defguardp"
  "defimpl"
  "defmacro"
  "defmacrop"
  "defmodule"
  "defn"
  "defnp"
  "defoverridable"
  "defp"
  "defprotocol"
  "defstruct"
)) (arguments [
  (identifier) @function
  (binary_operator left: (identifier) @function operator: "when")])?)

; Kernel Keywords & Special Forms
(call target: ((identifier) @keyword (#any-of? @keyword
  "alias"
  "case"
  "catch"
  "cond"
  "else"
  "for"
  "if"
  "import"
  "quote"
  "raise"
  "receive"
  "require"
  "reraise"
  "super"
  "throw"
  "try"
  "unless"
  "unquote"
  "unquote_splicing"
  "use"
  "with"
)))

; Special Constants
((identifier) @constant.builtin (#any-of? @constant.builtin
  "__CALLER__"
  "__DIR__"
  "__ENV__"
  "__MODULE__"
  "__STACKTRACE__"
))

; Reserved Keywords
[
  "after"
  "catch"
  "do"
  "end"
  "fn"
  "rescue" 
  "when"
  "else"
] @keyword

; Operator Keywords
[
  "and"
  "in"
  "not"
  "or"
] @keyword.operator

; Capture Operator
(unary_operator
  operator: "&"
  operand: [
    (integer) @operator
    (binary_operator
      left: [
        (call target: (dot left: (_) right: (identifier) @function))
        (identifier) @function
      ] operator: "/" right: (integer) @operator)
  ])

; Module attributes
(unary_operator 
  operator: "@" @constant 
  operand: [
    (identifier) @constant
    (call target: (identifier) @constant)])

; Sigils
(sigil 
  "~" @string.special
  ((sigil_name) @string.special) @_sigil_name
  quoted_start: _ @string.special 
  quoted_end: _ @string.special
  ((sigil_modifiers) @string.special)?
  (#not-any-of? @_sigil_name "s" "S"))

(sigil 
  "~" @string
  ((sigil_name) @string) @_sigil_name
  quoted_start: _ @string 
  (quoted_content) @string
  quoted_end: _ @string
  ((sigil_modifiers) @string)?
  (#any-of? @_sigil_name "s" "S"))

; Documentation
(unary_operator 
  operator: "@" @comment 
  operand: (call 
    target: (((identifier) @comment) @_identifier 
      (#any-of? @_identifier "moduledoc" "typedoc" "shortdoc" "doc"))
    (arguments [
        (string) @comment
        (charlist) @comment
        (sigil
          "~" @comment
          (sigil_name) @comment
          quoted_start: _ @comment
          (quoted_content) @comment
          quoted_end: _ @comment
          ((sigil_modifiers) @comment)?)
        (boolean) @comment
    ])))
