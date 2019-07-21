scriptencoding utf-8

let s:suite = themis#suite('Validator.Args')
let s:assert = themis#helper('assert')

function! s:suite.before()
  let s:A = vital#vital#import('Validator.Args')
  let s:T = s:A.TYPE
endfunction

function! s:suite.__of__()
  let of = themis#suite('of')

  function! of.of_should_throw_if_it_received_non_string_value() abort
    let [A, T] = [s:A, s:T]
    Throws /^vital: Validator.Args: of(): expected string argument but got number/
    \ A.of(42)
    call A.of('')
    Throws /^vital: Validator.Args: of(): expected string argument but got func/
    \ A.of(function('function'))
    Throws /^vital: Validator.Args: of(): expected string argument but got list/
    \ A.of([42])
    Throws /^vital: Validator.Args: of(): expected string argument but got dict/
    \ A.of({})
    Throws /^vital: Validator.Args: of(): expected string argument but got float/
    \ A.of(3.14)
    Throws /^vital: Validator.Args: of(): expected string argument but got bool/
    \ A.of(v:false)
    Throws /^vital: Validator.Args: of(): expected string argument but got none/
    \ A.of(v:null)
    Throws /^vital: Validator.Args: of(): expected string argument but got job/
    \ A.of(test_null_job())
    Throws /^vital: Validator.Args: of(): expected string argument but got channel/
    \ A.of(test_null_channel())
  endfunction

  function! of.of_should_not_validate_if_disabled() abort
    let [A, T] = [s:A, s:T]

    " disabled
    let v = A.of('func()', 0).type(T.STRING)
    try
      Assert Equals(v.validate([42]), [42])
    catch
      Assert 0, 'should not throw'
    endtry

    " enabled
    let v = A.of('func()', 1).type(T.STRING)
    Throws /^func(): invalid type arguments were given (expected: string, got: number)/
    \ v.validate([42])
  endfunction

  function! of.no_check()
    let A = s:A
    call A.of('test()').validate([])
    call A.of('test()').validate([1])
    call A.of('test()').validate([1,'foo'])
  endfunction

  function! of.validate_returns_given_args()
    let A = s:A
    Assert Equals(A.of('test()').validate([]), [])
    Assert Equals(A.of('test()').validate(['foo']), ['foo'])
    Assert Equals(
    \         A.of('test()').validate(['str', ['list'], {'value': 'dict'}]),
    \         ['str', ['list'], {'value': 'dict'}])
  endfunction

  function! of.union_types()
    let [A, T] = [s:A, s:T]
    Throws /^test(): invalid type arguments were given (expected: string or func, got: number)/
    \ A.of('test()').type([T.STRING, T.FUNC]).validate([42])
    call A.of('test()').type([T.STRING, T.FUNC]).validate([''])
    call A.of('test()').type([T.STRING, T.FUNC]).validate([function('function')])
    Throws /^test(): invalid type arguments were given (expected: string or func, got: list)/
    \ A.of('test()').type([T.STRING, T.FUNC]).validate([[]])
    Throws /^test(): invalid type arguments were given (expected: string or func, got: dict)/
    \ A.of('test()').type([T.STRING, T.FUNC]).validate([{}])
    Throws /^test(): invalid type arguments were given (expected: string or func, got: float)/
    \ A.of('test()').type([T.STRING, T.FUNC]).validate([3.14])
    Throws /^test(): invalid type arguments were given (expected: string or func, got: bool)/
    \ A.of('test()').type([T.STRING, T.FUNC]).validate([v:false])
    Throws /^test(): invalid type arguments were given (expected: string or func, got: none)/
    \ A.of('test()').type([T.STRING, T.FUNC]).validate([v:null])
    Throws /^test(): invalid type arguments were given (expected: string or func, got: job)/
    \ A.of('test()').type([T.STRING, T.FUNC]).validate([test_null_job()])
    Throws /^test(): invalid type arguments were given (expected: string or func, got: channel)/
    \ A.of('test()').type([T.STRING, T.FUNC]).validate([test_null_channel()])
  endfunction

  function! of.any_type() abort
    let [A, T] = [s:A, s:T]
    call A.of('test()').type(T.ANY).validate([42])
    call A.of('test()').type(T.ANY).validate([''])
    call A.of('test()').type(T.ANY).validate([function('function')])
    call A.of('test()').type(T.ANY).validate([[]])
    call A.of('test()').type(T.ANY).validate([{}])
    call A.of('test()').type(T.ANY).validate([3.14])
    call A.of('test()').type(T.ANY).validate([v:false])
    call A.of('test()').type(T.ANY).validate([v:null])
    call A.of('test()').type(T.ANY).validate([test_null_job()])
    call A.of('test()').type(T.ANY).validate([test_null_channel()])
  endfunction

  function! of.wrong_types_and_correct_types()
    let [A, T] = [s:A, s:T]
    Throws /^test(): invalid type arguments were given (expected: string, got: number)/
    \ A.of('test()').type(T.STRING).validate([42])
    call A.of('test()').type(T.STRING).validate([''])
    Throws /^test(): invalid type arguments were given (expected: string, got: func)/
    \ A.of('test()').type(T.STRING).validate([function('function')])
    Throws /^test(): invalid type arguments were given (expected: string, got: list)/
    \ A.of('test()').type(T.STRING).validate([[]])
    Throws /^test(): invalid type arguments were given (expected: string, got: dict)/
    \ A.of('test()').type(T.STRING).validate([{}])
    Throws /^test(): invalid type arguments were given (expected: string, got: float)/
    \ A.of('test()').type(T.STRING).validate([3.14])
    Throws /^test(): invalid type arguments were given (expected: string, got: bool)/
    \ A.of('test()').type(T.STRING).validate([v:false])
    Throws /^test(): invalid type arguments were given (expected: string, got: none)/
    \ A.of('test()').type(T.STRING).validate([v:null])
    Throws /^test(): invalid type arguments were given (expected: string, got: job)/
    \ A.of('test()').type(T.STRING).validate([test_null_job()])
    Throws /^test(): invalid type arguments were given (expected: string, got: channel)/
    \ A.of('test()').type(T.STRING).validate([test_null_channel()])
  endfunction

  function! of.validate_should_throw_if_it_received_non_list_value() abort
    let [A, T] = [s:A, s:T]
    Throws /^vital: Validator.Args: Validator.validate(): expected list argument but got number/
    \ A.of('test()').type(T.ANY).validate(42)
    Throws /^vital: Validator.Args: Validator.validate(): expected list argument but got string/
    \ A.of('test()').type(T.ANY).validate('')
    Throws /^vital: Validator.Args: Validator.validate(): expected list argument but got func/
    \ A.of('test()').type(T.ANY).validate(function('function'))
    call A.of('test()').type(T.ANY).validate([42])
    Throws /^vital: Validator.Args: Validator.validate(): expected list argument but got dict/
    \ A.of('test()').type(T.ANY).validate({})
    Throws /^vital: Validator.Args: Validator.validate(): expected list argument but got float/
    \ A.of('test()').type(T.ANY).validate(3.14)
    Throws /^vital: Validator.Args: Validator.validate(): expected list argument but got bool/
    \ A.of('test()').type(T.ANY).validate(v:false)
    Throws /^vital: Validator.Args: Validator.validate(): expected list argument but got none/
    \ A.of('test()').type(T.ANY).validate(v:null)
    Throws /^vital: Validator.Args: Validator.validate(): expected list argument but got job/
    \ A.of('test()').type(T.ANY).validate(test_null_job())
    Throws /^vital: Validator.Args: Validator.validate(): expected list argument but got channel/
    \ A.of('test()').type(T.ANY).validate(test_null_channel())
  endfunction

  function! of.arity_is_correct() abort
    let [A, T] = [s:A, s:T]
    call A.of('test()').type(T.STRING).validate(['foo'])
    call A.of('test()').type(T.STRING, T.OPTARG, T.STRING)
                        \.validate(['foo', 'bar'])
    call A.of('test()').type(T.STRING, T.OPTARG, T.STRING, T.STRING)
                        \.validate(['foo', 'bar'])
    " if the last type is OPTARG, skip validation of rest arguments
    " (but if any types were given after OPTARG, check the types and arity)
    call A.of('test()').type(T.STRING, T.OPTARG).validate(['foo'])
    call A.of('test()').type(T.STRING, T.OPTARG).validate(['foo', 'bar'])
    call A.of('test()').type(T.STRING, T.OPTARG).validate(['foo', 'bar', 'baz'])
  endfunction

  function! of.type_invalid_args() abort
    let [A, T] = [s:A, s:T]

    Throws /^vital: Validator.Args: Validator.type(): expected type or union types but got string/
    \ A.of('test()').type('string')
    Throws /^vital: Validator.Args: Validator.type(): expected type or union types but got number/
    \ A.of('test()').type(10)

    Throws /^vital: Validator.Args: Validator.type(): multiple OPTARG were given/
    \ A.of('test()').type(T.OPTARG, T.OPTARG)
  endfunction

  function! of.arity_is_wrong() abort
    let [A, T] = [s:A, s:T]
    Throws /^test(): too few arguments/
    \ A.of('test()').type(T.STRING, T.OPTARG).validate([])
    Throws /^test(): too few arguments/
    \ A.of('test()').type(T.STRING).validate([])
    Throws /^test(): too many arguments/
    \ A.of('test()').type(T.STRING).validate(['foo', 'bar'])
    Throws /^test(): too many arguments/
    \ A.of('test()').type(T.STRING, T.OPTARG, T.STRING)
                    \.validate(['foo', 'bar', 'baz'])
  endfunction

  function! of.assert_only() abort
    let [A, T] = [s:A, s:T]
    Throws /^test(): the first argument should be non empty string/
    \ A.of('test()').assert(1, 'type(v:val) is type('''') && v:val != ''''',
                   \           'the first argument should be non empty string')
                   \.validate([''])
    Throws /^test(): the 1th argument's assertion was failed/
    \ A.of('test()').assert(1, 'type(v:val) is type('''') && v:val != ''''')
                   \.validate([''])
    call
    \ A.of('test()').assert(1, 'type(v:val) is type('''') && v:val != ''''',
                   \           'the first argument should be non empty string')
                   \.validate(['foo'])
  endfunction

  function! of.assert_invalid_args() abort
    let [A, T] = [s:A, s:T]
    Throws /^vital: Validator.Args: Validator.assert(): the first argument number was not positive/
    \ A.of('test()').assert(-1, 'v:val != ''''',
                   \            'the first argument should be non empty string')
    Throws /^vital: Validator.Args: Validator.assert(): the first argument number was not positive/
    \ A.of('test()').assert(0, 'v:val != ''''',
                   \            'the first argument should be non empty string')
    " TODO: is type check necessary?
  endfunction

  function! of.assert_with_type() abort
    let [A, T] = [s:A, s:T]
    Throws /^test(): the first argument should be non empty string/
    \ A.of('test()').type(T.STRING)
                   \.assert(1, 'v:val != ''''',
                   \            'the first argument should be non empty string')
                   \.validate([''])
    Throws /^test(): the 1th argument's assertion was failed/
    \ A.of('test()').type(T.STRING)
                   \.assert(1, 'v:val != ''''')
                   \.validate([''])
    call
    \ A.of('test()').type(T.STRING)
                   \.assert(1, 'v:val != ''''',
                   \            'the first argument should be non empty string')
                   \.validate(['foo'])
  endfunction

  function! of.mixed_validation_with_assert_and_type() abort
    let [A, T] = [s:A, s:T]
    " .type() checking failure
    Throws /^test(): invalid type arguments were given (expected: string, got: number)/
    \ A.of('test()').type(T.STRING)
                   \.assert(1, 'v:val != ''''',
                   \            'the first argument should be non empty string')
                   \.validate([42])
    " too few arguments
    Throws /^test(): too few arguments/
    \ A.of('test()').type(T.STRING)
                   \.assert(1, 'v:val != ''''',
                   \            'the first argument should be non empty string')
                   \.validate([])
    " too many arguments
    Throws /^test(): too many arguments/
    \ A.of('test()').type(T.STRING)
                   \.assert(1, 'v:val != ''''',
                   \            'the first argument should be non empty string')
                   \.validate(['foo', 'bar'])
    " type() with OPTARG, and assert()
    call
    \ A.of('test()').type(T.STRING, T.OPTARG)
                   \.assert(1, 'v:val != ''''',
                   \            'the first argument should be non empty string')
                   \.assert(2, 'v:val != ''''',
                   \            'the second argument should be non empty string')
                   \.validate(['foo', 'bar'])
    Throws /^test(): the second argument should be non empty string/
    \ A.of('test()').type(T.STRING, T.OPTARG)
                   \.assert(1, 'v:val != ''''',
                   \            'the first argument should be non empty string')
                   \.assert(2, 'v:val != ''''',
                   \            'the second argument should be non empty string')
                   \.validate(['foo', ''])
    call
    \ A.of('test()').type(T.STRING, T.OPTARG, T.STRING)
                   \.assert(1, 'v:val != ''''',
                   \            'the first argument should be non empty string')
                   \.assert(2, 'v:val != ''''',
                   \            'the second argument should be non empty string')
                   \.validate(['foo', 'bar'])
    Throws /^test(): the second argument should be non empty string/
    \ A.of('test()').type(T.STRING, T.OPTARG, T.STRING)
                   \.assert(1, 'v:val != ''''',
                   \            'the first argument should be non empty string')
                   \.assert(2, 'v:val != ''''',
                   \            'the second argument should be non empty string')
                   \.validate(['foo', ''])
  endfunction

  function! of.assert_no_out_of_range()
    let [A, T] = [s:A, s:T]
    Throws /^vital: Validator.Args: Validator.assert(): the first argument number was out of range (type() defines 1 arguments)/
    \ A.of('test()').type(T.STRING)
                   \.assert(1, 'v:val != ''''',
                   \            'the first argument should be non empty string')
                   \.assert(2, 'v:val != ''''',
                   \            'the second argument?')
    Throws /^vital: Validator.Args: Validator.assert(): the first argument number was out of range (type() defines 1 arguments)/
    \ A.of('test()').assert(1, 'v:val != ''''',
                   \            'the first argument should be non empty string')
                   \.assert(2, 'v:val != ''''',
                   \            'the second argument?')
                   \.type(T.STRING)
    Throws /^vital: Validator.Args: Validator.assert(): the first argument number was out of range (type() defines 1-2 arguments)/
    \ A.of('test()').type(T.STRING, T.OPTARG, T.STRING)
                   \.assert(1, 'v:val != ''''',
                   \            'the first argument should be non empty string')
                   \.assert(2, 'v:val != ''''',
                   \            'the second argument should be non empty string')
                   \.assert(3, 'v:val != ''''',
                   \            'the third argument?')
    Throws /^vital: Validator.Args: Validator.assert(): the first argument number was out of range (type() defines 1-2 arguments)/
    \ A.of('test()').assert(1, 'v:val != ''''',
                   \            'the first argument should be non empty string')
                   \.assert(2, 'v:val != ''''',
                   \            'the second argument should be non empty string')
                   \.assert(3, 'v:val != ''''',
                   \            'the third argument?')
                   \.type(T.STRING, T.OPTARG, T.STRING)
    Throws /^vital: Validator.Args: Validator.assert(): the first argument number was out of range (type() defines 1-3 arguments)/
    \ A.of('test()').assert(1, 'v:val != ''''',
                   \            'the first argument should be non empty string')
                   \.assert(2, 'v:val != ''''',
                   \            'the second argument should be non empty string')
                   \.assert(3, 'v:val != ''''',
                   \            'the third argument should be non empty string')
                   \.assert(4, 'v:val != ''''',
                   \            'the fourth argument?')
                   \.type(T.STRING, T.OPTARG, T.STRING, T.STRING)
  endfunction

endfunction
