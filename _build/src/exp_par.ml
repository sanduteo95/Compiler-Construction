
module Basics = struct
  
  exception Error
  
  type token = 
    | WHILE
    | TYPE
    | TIMES
    | TEXT of (string)
    | SEMI_COLLON
    | RIGHT_ROUND_BRACKET
    | RIGHT_CURLY_BRACKET
    | RETURN
    | READ
    | PRINT
    | PLUS
    | OR
    | NOTEQ
    | NEGATE
    | MODULUS
    | MINUS
    | LET
    | LESS
    | LEQ
    | LEFT_ROUND_BRACKET
    | LEFT_CURLY_BRACKET
    | INT of (int)
    | IN
    | IF
    | ID of (string)
    | GREATER
    | GEQ
    | FUNCTION
    | EQ
    | EOF
    | ELSE
    | DIVIDE
    | COMMA
    | ASSIGN
    | AND
  
end

include Basics

let _eRR =
  Basics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState124
  | MenhirState119
  | MenhirState111
  | MenhirState100
  | MenhirState94
  | MenhirState91
  | MenhirState87
  | MenhirState85
  | MenhirState82
  | MenhirState79
  | MenhirState73
  | MenhirState68
  | MenhirState60
  | MenhirState55
  | MenhirState52
  | MenhirState50
  | MenhirState48
  | MenhirState46
  | MenhirState44
  | MenhirState42
  | MenhirState40
  | MenhirState38
  | MenhirState36
  | MenhirState34
  | MenhirState32
  | MenhirState30
  | MenhirState28
  | MenhirState20
  | MenhirState15
  | MenhirState14
  | MenhirState13
  | MenhirState12
  | MenhirState10
  | MenhirState3
  | MenhirState1
  
open Syntax

let rec _menhir_goto_separated_nonempty_list_COMMA_argument_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Syntax.expression list)) = _v in
        let _v : (Syntax.expression list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_argument__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Syntax.expression list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Syntax.expression))) = _menhir_stack in
        let _2 = () in
        let _v : (Syntax.expression list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_argument_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce9 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (o : (Syntax.expression))) = _menhir_stack in
    let _v : (Syntax.expression) =                             ( o ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_argument : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | LEFT_ROUND_BRACKET ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | NEGATE ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
    | RIGHT_ROUND_BRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (Syntax.expression))) = _menhir_stack in
        let _v : (Syntax.expression list) =     ( [ x ] ) in
        _menhir_goto_separated_nonempty_list_COMMA_argument_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_operator_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState13 | MenhirState14 | MenhirState85 | MenhirState94 | MenhirState68 | MenhirState73 | MenhirState60 | MenhirState52 | MenhirState50 | MenhirState48 | MenhirState46 | MenhirState44 | MenhirState42 | MenhirState40 | MenhirState38 | MenhirState36 | MenhirState34 | MenhirState32 | MenhirState30 | MenhirState28 | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState55 | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
            _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (o : (Syntax.expression))) = _menhir_stack in
            let _v : (Syntax.expression) =                             ( o ) in
            _menhir_goto_argument _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LEFT_CURLY_BRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ID _v ->
                    _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
                | IF ->
                    _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | INT _v ->
                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
                | LET ->
                    _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | PRINT ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | RETURN ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | TYPE ->
                    _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | WHILE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
            _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LEFT_CURLY_BRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ID _v ->
                    _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
                | IF ->
                    _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                | INT _v ->
                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
                | LET ->
                    _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                | PRINT ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                | RETURN ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                | TYPE ->
                    _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                | WHILE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
            _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run28 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | NEGATE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28

and _menhir_run30 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | NEGATE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run36 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | NEGATE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_run38 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | NEGATE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run32 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | NEGATE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run40 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | NEGATE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run42 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | NEGATE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run44 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | NEGATE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | NEGATE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run48 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | NEGATE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run50 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | NEGATE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run34 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | NEGATE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_run52 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | NEGATE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_goto_loption_separated_nonempty_list_COMMA_argument__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RIGHT_ROUND_BRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (f : (Syntax.expression))), _, (xs0 : (Syntax.expression list))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : (Syntax.expression) = let a =
          let xs = xs0 in
              ( xs )
        in
                                                                                                                  ( Application(f, a)) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
        _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack)
    | COMMA | RIGHT_ROUND_BRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (i : (int))) = _menhir_stack in
        let _v : (Syntax.expression) =             ( Const(i) ) in
        _menhir_goto_argument _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_ROUND_BRACKET ->
        _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)
    | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
        _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack)
    | COMMA | RIGHT_ROUND_BRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (id : (string))) = _menhir_stack in
        let _v : (Syntax.expression) =             ( Deref(Identifier(id)) ) in
        _menhir_goto_argument _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_statements : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), (id : (string))), _, (r : (Syntax.expression))), _, (s : (Syntax.expression))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (Syntax.expression) =                                                                              ( New(id, r, s) ) in
        _menhir_goto_statements _menhir_env _menhir_stack _menhir_s _v
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (s1 : (Syntax.expression))), _, (s2 : (Syntax.expression))) = _menhir_stack in
        let _v : (Syntax.expression) =                                     ( Seq(s1, s2) ) in
        _menhir_goto_statements _menhir_env _menhir_stack _menhir_s _v
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ELSE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LEFT_CURLY_BRACKET ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | ID _v ->
                        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
                    | IF ->
                        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                    | INT _v ->
                        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
                    | LET ->
                        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                    | PRINT ->
                        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                    | RETURN ->
                        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                    | TYPE ->
                        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                    | WHILE ->
                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (o : (Syntax.expression))), _, (s1 : (Syntax.expression))), _, (s2 : (Syntax.expression))) = _menhir_stack in
            let _11 = () in
            let _9 = () in
            let _8 = () in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                                                                                                                                                                    ( If(o, s1, s2) ) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (o : (Syntax.expression))), _, (s : (Syntax.expression))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                                                                                                      ( While(o, s) ) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (s : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                                         ( s ) in
            _menhir_goto_content _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState12 | MenhirState79 | MenhirState55 | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
        let _2 = () in
        let _v : (Syntax.expression) =                                             ( Operator(Times, e1, e2) ) in
        _menhir_goto_operator_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQ | GEQ | GREATER | IN | LEQ | LESS | MINUS | NOTEQ | OR | PLUS | RIGHT_ROUND_BRACKET | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                                             ( Operator(Plus, e1, e2) ) in
            _menhir_goto_operator_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
        let _2 = () in
        let _v : (Syntax.expression) =                                               ( Operator(Modulus, e1, e2)) in
        _menhir_goto_operator_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
        let _2 = () in
        let _v : (Syntax.expression) =                                              ( Operator(Divide, e1, e2) ) in
        _menhir_goto_operator_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | IN | OR | RIGHT_ROUND_BRACKET | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                                          ( Operator(Or, e1, e2) ) in
            _menhir_goto_operator_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQ | IN | NOTEQ | OR | RIGHT_ROUND_BRACKET | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                                             ( Operator(Noteq, e1, e2)) in
            _menhir_goto_operator_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQ | GEQ | GREATER | IN | LEQ | LESS | MINUS | NOTEQ | OR | PLUS | RIGHT_ROUND_BRACKET | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                                             ( Operator(Minus, e1, e2) ) in
            _menhir_goto_operator_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQ | GEQ | GREATER | IN | LEQ | LESS | NOTEQ | OR | RIGHT_ROUND_BRACKET | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                                            ( Operator(Less, e1, e2)) in
            _menhir_goto_operator_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQ | GEQ | GREATER | IN | LEQ | LESS | NOTEQ | OR | RIGHT_ROUND_BRACKET | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                                           ( Operator(Leq, e1, e2)) in
            _menhir_goto_operator_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQ | GEQ | GREATER | IN | LEQ | LESS | NOTEQ | OR | RIGHT_ROUND_BRACKET | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                                               ( Operator(Greater, e1, e2)) in
            _menhir_goto_operator_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQ | GEQ | GREATER | IN | LEQ | LESS | NOTEQ | OR | RIGHT_ROUND_BRACKET | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                                           ( Operator(Geq, e1, e2) ) in
            _menhir_goto_operator_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQ | IN | NOTEQ | OR | RIGHT_ROUND_BRACKET | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                                          ( Operator(Eq, e1, e2) ) in
            _menhir_goto_operator_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | IN | OR | RIGHT_ROUND_BRACKET | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                                           ( Operator(And, e1, e2) ) in
            _menhir_goto_operator_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (v : (Syntax.expression))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                        ( v ) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (v : (Syntax.expression))) = _menhir_stack in
            let _v : (Syntax.expression) =                    ( v ) in
            _menhir_goto_print_value _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), (id : (string))), _, (r : (Syntax.expression))), _, (e : (Syntax.expression))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                                                 ( Let(id, r, e) ) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState85 | MenhirState94 | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | IN | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Syntax.expression))) = _menhir_stack in
            let _v : (Syntax.expression) =                    ( e ) in
            _menhir_goto_right_assignment _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Syntax.expression))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                             ( e ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (e : (Syntax.expression))) = _menhir_stack in
        let _1 = () in
        let _v : (Syntax.expression) =                            ( Negate(e) ) in
        _menhir_goto_operator_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_function_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_ROUND_BRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
        | LEFT_ROUND_BRACKET ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | NEGATE ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState20 in
            let _v : (Syntax.expression list) =     ( [] ) in
            _menhir_goto_loption_separated_nonempty_list_COMMA_argument__ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState10 | MenhirState111 | MenhirState82 | MenhirState100 | MenhirState91 | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | IF ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | INT _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | LET ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | PRINT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | RETURN ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | TYPE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (s : (Syntax.expression))) = _menhir_stack in
            let _v : (Syntax.expression) =                  ( s ) in
            _menhir_goto_statements _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (s : (Syntax.expression))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                           ( s ) in
            _menhir_goto_function_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_right_assignment : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | INT _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | NEGATE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
            | IF ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | INT _v ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
            | LET ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | PRINT ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | RETURN ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | TYPE ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (l : (Syntax.expression))), _, (r : (Syntax.expression))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Syntax.expression) =                                                                    ( Asg(l, r)) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce7 : _menhir_env -> 'ttv_tail * _menhir_state * (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (i : (int))) = _menhir_stack in
    let _v : (Syntax.expression) =             ( Const(i) ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce8 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (id : (string))) = _menhir_stack in
    let _v : (Syntax.expression) =             ( Deref(Identifier(id)) ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce12 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (id : (string))) = _menhir_stack in
    let _v : (Syntax.expression) =             ( Identifier(id) ) in
    _menhir_goto_function_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_left_assignment : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSIGN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
        | INT _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
        | LEFT_ROUND_BRACKET ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | NEGATE ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | READ ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_separated_nonempty_list_FUNCTION_func_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.program) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, (f : (Syntax.program))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Syntax.program) =                                                                ( f ) in
            _menhir_goto_parse _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Syntax.fundef))), _, (xs : (Syntax.program))) = _menhir_stack in
        let _2 = () in
        let _v : (Syntax.program) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_FUNCTION_func_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_print_value : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RIGHT_ROUND_BRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (p : (Syntax.expression))) = _menhir_stack in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                                                  ( Print(p) ) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_ROUND_BRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                   ( Read ) in
            _menhir_goto_right_assignment _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce15 : _menhir_env -> 'ttv_tail * _menhir_state * (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (i : (int))) = _menhir_stack in
    let _v : (Syntax.expression) =             ( Const(i) ) in
    _menhir_goto_left_assignment _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | NEGATE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState14 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGN ->
            _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)
        | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | RIGHT_ROUND_BRACKET | TIMES ->
            _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | IF ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | INT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState14 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGN ->
            _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)
        | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | RIGHT_ROUND_BRACKET | TIMES ->
            _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | LEFT_ROUND_BRACKET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | LET ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | NEGATE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | PRINT ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | RETURN ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_ROUND_BRACKET ->
        _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)
    | AND | COMMA | DIVIDE | EQ | GEQ | GREATER | IN | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | RIGHT_ROUND_BRACKET | SEMI_COLLON | TIMES ->
        _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce14 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (id : (string))) = _menhir_stack in
    let _v : (Syntax.expression) =             ( Identifier(id) ) in
    _menhir_goto_left_assignment _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_COMMA_parameter_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.parameter list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (p : (Syntax.parameter list))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.parameter list) =                                                                        ( p ) in
            _menhir_goto_parameter_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Syntax.parameter))), _, (xs : (Syntax.parameter list))) = _menhir_stack in
        let _2 = () in
        let _v : (Syntax.parameter list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_parameter_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_ROUND_BRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | INT _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | LEFT_ROUND_BRACKET ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | NEGATE ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run83 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
            | INT _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | NEGATE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | READ ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_content : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (c : (Syntax.expression)) = _v in
    let ((_menhir_stack, _menhir_s, (id : (string))), _, (p : (Syntax.parameter list))) = _menhir_stack in
    let _4 = () in
    let _2 = () in
    let _v : (Syntax.fundef) =                                                                                      ( (id, p, c) ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FUNCTION ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124)
    | EOF ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (Syntax.fundef))) = _menhir_stack in
        let _v : (Syntax.program) =     ( [ x ] ) in
        _menhir_goto_separated_nonempty_list_FUNCTION_func_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | INT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | NEGATE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_ROUND_BRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | INT _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | LEFT_ROUND_BRACKET ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | NEGATE ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | TEXT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState60 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (text : (string)) = _v in
            let _v : (Syntax.expression) =                 ( Text(text) ) in
            _menhir_goto_print_value _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run66 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | INT _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | NEGATE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | READ ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run88 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run78 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_ROUND_BRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
        | INT _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
        | LEFT_ROUND_BRACKET ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | NEGATE ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run89 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (id : (string)) = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : (Syntax.parameter) =                   ( Param(id) ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TYPE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119)
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Syntax.parameter))) = _menhir_stack in
            let _v : (Syntax.parameter list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_parameter_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_parameter_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.parameter list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_CURLY_BRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | IF ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | INT _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | LET ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | PRINT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | RETURN ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState10 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (Syntax.expression) =                         ( Nothing ) in
            _menhir_goto_content _menhir_env _menhir_stack _menhir_s _v
        | TYPE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_ROUND_BRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState3 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (Syntax.parameter list) =                        ( [] ) in
            _menhir_goto_parameter_list _menhir_env _menhir_stack _menhir_s _v
        | TYPE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_parse : _menhir_env -> 'ttv_tail -> (Syntax.program) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Syntax.program)) = _v in
    Obj.magic _1

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and parse : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Syntax.program) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EOF ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = () in
        let _v : (Syntax.program) =         ( [("", [], Nothing)] ) in
        _menhir_goto_parse _menhir_env _menhir_stack _v
    | FUNCTION ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR)
  

