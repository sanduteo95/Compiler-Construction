
module Basics = struct
  
  exception Error
  
  type token = 
    | WHILE
    | TYPE
    | TO
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
    | NULL
    | NOTEQ
    | NEGATE
    | MODULUS
    | MINUS
    | LET
    | LESS
    | LEQ
    | LEFT_ROUND_BRACKET
    | LEFT_CURLY_BRACKET
    | LAMBDA
    | INT of (int)
    | IN
    | IF
    | ID of (string)
    | GREATER
    | GEQ
    | FUNCTION
    | FUN
    | FOR
    | FLOAT of (float)
    | EQ
    | EOF
    | ELSE
    | DIVIDE
    | CONTINUE
    | COMMA
    | BREAK
    | BOOL of (bool)
    | ASSIGN
    | AND
    | ADDRESS_OF
  
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
  | MenhirState271
  | MenhirState266
  | MenhirState258
  | MenhirState246
  | MenhirState242
  | MenhirState233
  | MenhirState229
  | MenhirState219
  | MenhirState212
  | MenhirState207
  | MenhirState202
  | MenhirState200
  | MenhirState194
  | MenhirState188
  | MenhirState183
  | MenhirState175
  | MenhirState168
  | MenhirState163
  | MenhirState158
  | MenhirState149
  | MenhirState136
  | MenhirState133
  | MenhirState131
  | MenhirState129
  | MenhirState125
  | MenhirState122
  | MenhirState116
  | MenhirState115
  | MenhirState113
  | MenhirState106
  | MenhirState103
  | MenhirState98
  | MenhirState92
  | MenhirState88
  | MenhirState85
  | MenhirState83
  | MenhirState81
  | MenhirState79
  | MenhirState77
  | MenhirState75
  | MenhirState73
  | MenhirState71
  | MenhirState69
  | MenhirState67
  | MenhirState65
  | MenhirState63
  | MenhirState61
  | MenhirState59
  | MenhirState55
  | MenhirState40
  | MenhirState37
  | MenhirState36
  | MenhirState34
  | MenhirState30
  | MenhirState28
  | MenhirState19
  | MenhirState18
  | MenhirState12
  | MenhirState10
  | MenhirState3
  | MenhirState1
  
open Syntax
let length = List.length

let rec _menhir_reduce35 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (o : (Syntax.expression))) = _menhir_stack in
    let _v : (Syntax.expression) =                             ( o ) in
    _menhir_goto_min_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run215 : _menhir_env -> (((('ttv_tail * _menhir_state) * _menhir_state * (Syntax.expression))) * _menhir_state * (Syntax.expression list)) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (((_menhir_stack, _menhir_s), _, (f : (Syntax.expression))), _, (xs0 : (Syntax.expression list))) = _menhir_stack in
    let _6 = () in
    let _5 = () in
    let _3 = () in
    let _1 = () in
    let _v : (Syntax.expression) = let a =
      let xs = xs0 in
          ( xs )
    in
                                                                                                                                                          ( Application(f, a) ) in
    _menhir_goto_left_assignment _menhir_env _menhir_stack _menhir_s _v

and _menhir_run171 : _menhir_env -> ((('ttv_tail * _menhir_state * (Syntax.expression))) * _menhir_state * (Syntax.expression list)) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let ((_menhir_stack, _menhir_s, (f : (Syntax.expression))), _, (xs0 : (Syntax.expression list))) = _menhir_stack in
    let _5 = () in
    let _4 = () in
    let _2 = () in
    let _v : (Syntax.expression) = let a =
      let xs = xs0 in
          ( xs )
    in
                                                                                                                             ( Application(f, a)) in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce7 : _menhir_env -> ((('ttv_tail * _menhir_state * (Syntax.expression))) * _menhir_state * (Syntax.expression list)) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (f : (Syntax.expression))), _, (xs0 : (Syntax.expression list))) = _menhir_stack in
    let _4 = () in
    let _2 = () in
    let _v : (Syntax.expression) = let a =
      let xs = xs0 in
          ( xs )
    in
                                                                                                                ( Application(f, a)) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_operator_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState18 | MenhirState98 | MenhirState246 | MenhirState242 | MenhirState233 | MenhirState113 | MenhirState212 | MenhirState200 | MenhirState129 | MenhirState168 | MenhirState163 | MenhirState92 | MenhirState88 | MenhirState85 | MenhirState83 | MenhirState81 | MenhirState79 | MenhirState77 | MenhirState75 | MenhirState73 | MenhirState71 | MenhirState69 | MenhirState67 | MenhirState65 | MenhirState63 | MenhirState61 | MenhirState59 | MenhirState55 | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState122 ->
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
                | ADDRESS_OF ->
                    _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | BREAK ->
                    _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | CONTINUE ->
                    _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | FOR ->
                    _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | ID _v ->
                    _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
                | IF ->
                    _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | LEFT_ROUND_BRACKET ->
                    _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | LET ->
                    _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | PRINT ->
                    _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | RETURN ->
                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | TIMES ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | TYPE ->
                    _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | WHILE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
            _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState133 ->
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
                | ADDRESS_OF ->
                    _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState136
                | BREAK ->
                    _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState136
                | CONTINUE ->
                    _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState136
                | FOR ->
                    _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState136
                | ID _v ->
                    _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
                | IF ->
                    _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState136
                | LEFT_ROUND_BRACKET ->
                    _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState136
                | LET ->
                    _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState136
                | PRINT ->
                    _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState136
                | RETURN ->
                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState136
                | TIMES ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState136
                | TYPE ->
                    _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState136
                | WHILE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState136
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
            _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState19 | MenhirState103 | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (o : (Syntax.expression))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                                      ( o ) in
            _menhir_goto_operator_expression _menhir_env _menhir_stack _menhir_s _v
        | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
            _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack)
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
                | ADDRESS_OF ->
                    _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState258
                | BREAK ->
                    _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState258
                | CONTINUE ->
                    _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState258
                | FOR ->
                    _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState258
                | ID _v ->
                    _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
                | IF ->
                    _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState258
                | LEFT_ROUND_BRACKET ->
                    _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState258
                | LET ->
                    _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState258
                | PRINT ->
                    _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState258
                | RETURN ->
                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState258
                | TIMES ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState258
                | TYPE ->
                    _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState258
                | WHILE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState258
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState258)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
            _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState242 | MenhirState233 | MenhirState212 | MenhirState168 | MenhirState55 | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Syntax.expression list)) = _v in
        let _v : (Syntax.expression list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Syntax.expression list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Syntax.expression))) = _menhir_stack in
        let _2 = () in
        let _v : (Syntax.expression list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run59 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | BOOL _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | FLOAT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | ID _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NEGATE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | TEXT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | TIMES ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run61 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | BOOL _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | FLOAT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | ID _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | NEGATE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | TEXT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | TIMES ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_run67 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | BOOL _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | FLOAT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | ID _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | NEGATE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | TEXT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | TIMES ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run69 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | BOOL _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | FLOAT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | ID _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | NEGATE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | TEXT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | TIMES ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | BOOL _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | FLOAT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | ID _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NEGATE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | TEXT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | TIMES ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run71 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | BOOL _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | FLOAT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | ID _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | NEGATE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | TEXT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | TIMES ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_run73 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | BOOL _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | FLOAT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | ID _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NEGATE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | TEXT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | TIMES ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_run75 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | BOOL _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | FLOAT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | ID _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | NEGATE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | TEXT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | TIMES ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75

and _menhir_run77 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | BOOL _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | FLOAT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | ID _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | NEGATE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | TEXT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | TIMES ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run79 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | BOOL _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | FLOAT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | ID _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | NEGATE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | TEXT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | TIMES ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79

and _menhir_run81 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | BOOL _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | FLOAT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | ID _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | NEGATE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | TEXT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | TIMES ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_run65 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | BOOL _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | FLOAT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | ID _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | NEGATE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | TEXT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | TIMES ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65

and _menhir_run85 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | BOOL _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | FLOAT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | ID _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | NEGATE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | TEXT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | TIMES ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85

and _menhir_goto_separated_nonempty_list_COMMA_value_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState219 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Syntax.expression))), _, (xs : (Syntax.expression list))) = _menhir_stack in
        let _2 = () in
        let _v : (Syntax.expression list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_value_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState19 | MenhirState103 | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (vs : (Syntax.expression list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                                                         ( MyTuple(vs) ) in
            _menhir_goto_values _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_values : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (vs : (Syntax.expression)) = _v in
    let _v : (Syntax.expression) =                 ( vs ) in
    _menhir_goto_min_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce6 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (me : (Syntax.expression))) = _menhir_stack in
    let _v : (Syntax.expression) =                         ( me ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RIGHT_ROUND_BRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _, (f : (Syntax.expression))), _, (xs0 : (Syntax.expression list))) = _menhir_stack in
                let _7 = () in
                let _6 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Syntax.expression) = let a =
                  let xs = xs0 in
                      ( xs )
                in
                                                                                                                                                                             ( Deref(Application(f, a)) ) in
                _menhir_goto_left_assignment _menhir_env _menhir_stack _menhir_s _v
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
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState168 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI_COLLON ->
                _menhir_run171 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState212 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RIGHT_ROUND_BRACKET ->
                _menhir_run215 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState233 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RIGHT_ROUND_BRACKET ->
                _menhir_run215 _menhir_env (Obj.magic _menhir_stack)
            | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
                _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState242 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI_COLLON ->
                _menhir_run171 _menhir_env (Obj.magic _menhir_stack)
            | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
                _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack)
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
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_ids_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState19 | MenhirState103 | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (ts : (Syntax.expression list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                                                       ( MyTuple(ts) ) in
            _menhir_goto_min_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState229 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Syntax.expression))), _, (xs : (Syntax.expression list))) = _menhir_stack in
        let _2 = () in
        let _v : (Syntax.expression list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_ids_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_ID_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (string list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (string))) = _menhir_stack in
        let _2 = () in
        let _v : (string list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_ID_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (string list)) = _v in
        let _v : (string list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce32 : _menhir_env -> (('ttv_tail * _menhir_state)) * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), (id : (string))) = _menhir_stack in
    let _2 = () in
    let _1 = () in
    let _v : (Syntax.expression) =                          ( Deref(Deref(Identifier(id))) ) in
    _menhir_goto_min_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce33 : _menhir_env -> (((('ttv_tail * _menhir_state) * _menhir_state)) * (string)) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s), _), (id : (string))) = _menhir_stack in
    let _5 = () in
    let _3 = () in
    let _2 = () in
    let _1 = () in
    let _v : (Syntax.expression) =                                                                   ( Deref(Deref(Identifier(id))) ) in
    _menhir_goto_min_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState242 | MenhirState233 | MenhirState212 | MenhirState168 | MenhirState40 | MenhirState83 | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADDRESS_OF ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | BOOL _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | FLOAT _v ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | ID _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | NEGATE ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | TEXT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | TIMES ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
        | DIVIDE ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Syntax.expression))) = _menhir_stack in
            let _v : (Syntax.expression list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
        let _2 = () in
        let _v : (Syntax.expression) =                                             ( Operator(Times, e1, e2) ) in
        _menhir_goto_operator_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
        let _2 = () in
        let _v : (Syntax.expression) =                                               ( Operator(Modulus, e1, e2)) in
        _menhir_goto_operator_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
        let _2 = () in
        let _v : (Syntax.expression) =                                              ( Operator(Divide, e1, e2) ) in
        _menhir_goto_operator_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Syntax.expression))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                        ( e ) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI_COLLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (e : (Syntax.expression))) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Syntax.expression) =                                                                                 ( Print(e) ) in
                _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | TIMES ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState98 | MenhirState113 | MenhirState200 | MenhirState129 | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | IN | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Syntax.expression))) = _menhir_stack in
            let _v : (Syntax.expression) =                   ( e ) in
            _menhir_goto_right_assignment _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState12 | MenhirState19 | MenhirState246 | MenhirState103 | MenhirState106 | MenhirState122 | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (e : (Syntax.expression))) = _menhir_stack in
        let _1 = () in
        let _v : (Syntax.expression) =                            ( Negate(e) ) in
        _menhir_goto_operator_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce70 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (x : (Syntax.expression))) = _menhir_stack in
    let _v : (Syntax.expression list) =     ( [ x ] ) in
    _menhir_goto_separated_nonempty_list_COMMA_value_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run219 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState219 _v
    | FLOAT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState219 _v
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState219 _v
    | TEXT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState219 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState219

and _menhir_reduce92 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (v : (Syntax.expression))) = _menhir_stack in
    let _v : (Syntax.expression) =               ( v ) in
    _menhir_goto_values _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_min_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState12 | MenhirState18 | MenhirState19 | MenhirState98 | MenhirState242 | MenhirState103 | MenhirState233 | MenhirState106 | MenhirState113 | MenhirState212 | MenhirState122 | MenhirState200 | MenhirState129 | MenhirState133 | MenhirState168 | MenhirState163 | MenhirState92 | MenhirState88 | MenhirState85 | MenhirState83 | MenhirState81 | MenhirState79 | MenhirState77 | MenhirState75 | MenhirState73 | MenhirState71 | MenhirState69 | MenhirState67 | MenhirState65 | MenhirState63 | MenhirState61 | MenhirState59 | MenhirState55 | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState246 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), (id : (string))), _, (r : (Syntax.expression))), _, (me : (Syntax.expression))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                                                      ( Let(id, r, me) ) in
            _menhir_goto_let_declaration _menhir_env _menhir_stack _menhir_s _v
        | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
            _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Syntax.expression list) =     ( [] ) in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_new_declaration : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (Syntax.expression)) = _v in
    let _v : (Syntax.expression) =                         ( n ) in
    _menhir_goto_statements _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_for_loop : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (f : (Syntax.expression)) = _v in
    let _v : (Syntax.expression) =                  ( f ) in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_run25 : _menhir_env -> (('ttv_tail * _menhir_state) * _menhir_state) * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (((_menhir_stack, _menhir_s), _), (id : (string))) = _menhir_stack in
    let _4 = () in
    let _2 = () in
    let _1 = () in
    let _v : (Syntax.expression) =                                                            ( Deref(Identifier(id)) ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run106 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | BOOL _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | FLOAT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | FUN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | ID _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | IF ->
        _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | LET ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | NEGATE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | TEXT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | TIMES ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState106 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _v
        | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | RIGHT_ROUND_BRACKET ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | SEMI_COLLON | TIMES ->
                        _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack)
                    | ASSIGN ->
                        _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
                    _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack)
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106

and _menhir_reduce15 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (id : (string))) = _menhir_stack in
    let _v : (Syntax.expression) =             ( Deref(Identifier(id)) ) in
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
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState229 in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState229)
    | RIGHT_ROUND_BRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (Syntax.expression))) = _menhir_stack in
        let _v : (Syntax.expression list) =     ( [ x ] ) in
        _menhir_goto_separated_nonempty_list_COMMA_ids_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run237 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | SEMI_COLLON | TIMES ->
            _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack)
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

and _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LAMBDA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADDRESS_OF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | BREAK ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | CONTINUE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | FOR ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | ID _v ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | IF ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | LEFT_ROUND_BRACKET ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | LET ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | PRINT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | RETURN ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | TIMES ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30)
    | LAMBDA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (string))) = _menhir_stack in
        let _v : (string list) =     ( [ x ] ) in
        _menhir_goto_separated_nonempty_list_COMMA_ID_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run14 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce4 : _menhir_env -> ('ttv_tail * _menhir_state) * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), (id : (string))) = _menhir_stack in
    let _1 = () in
    let _v : (Syntax.expression) =                    ( Deref(Identifier(id)) ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run21 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack)
        | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
            _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack)
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

and _menhir_run24 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RIGHT_ROUND_BRACKET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
    | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
        _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA | RIGHT_ROUND_BRACKET ->
        _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)
    | LEFT_ROUND_BRACKET ->
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
    | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
        _menhir_reduce3 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce3 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (id : (string))) = _menhir_stack in
    let _v : (Syntax.expression) =             ( Identifier(id) ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_value : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState12 | MenhirState18 | MenhirState246 | MenhirState98 | MenhirState242 | MenhirState233 | MenhirState212 | MenhirState200 | MenhirState168 | MenhirState163 | MenhirState133 | MenhirState129 | MenhirState122 | MenhirState113 | MenhirState92 | MenhirState88 | MenhirState85 | MenhirState83 | MenhirState81 | MenhirState79 | MenhirState77 | MenhirState75 | MenhirState73 | MenhirState71 | MenhirState69 | MenhirState67 | MenhirState65 | MenhirState63 | MenhirState61 | MenhirState59 | MenhirState55 | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce92 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState19 | MenhirState103 | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            _menhir_run219 _menhir_env (Obj.magic _menhir_stack)
        | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
            _menhir_reduce92 _menhir_env (Obj.magic _menhir_stack)
        | RIGHT_ROUND_BRACKET ->
            _menhir_reduce70 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState219 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            _menhir_run219 _menhir_env (Obj.magic _menhir_stack)
        | RIGHT_ROUND_BRACKET ->
            _menhir_reduce70 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce31 : _menhir_env -> ('ttv_tail * _menhir_state) * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), (id : (string))) = _menhir_stack in
    let _1 = () in
    let _v : (Syntax.expression) =                        ( Identifier(id) ) in
    _menhir_goto_min_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_function_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADDRESS_OF ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | BOOL _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
            | FLOAT _v ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
            | ID _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | NEGATE ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | TEXT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
            | TIMES ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | RIGHT_ROUND_BRACKET ->
                _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState12 | MenhirState18 | MenhirState19 | MenhirState98 | MenhirState242 | MenhirState233 | MenhirState113 | MenhirState212 | MenhirState122 | MenhirState200 | MenhirState129 | MenhirState133 | MenhirState168 | MenhirState163 | MenhirState92 | MenhirState88 | MenhirState85 | MenhirState83 | MenhirState81 | MenhirState79 | MenhirState77 | MenhirState75 | MenhirState73 | MenhirState71 | MenhirState69 | MenhirState67 | MenhirState65 | MenhirState63 | MenhirState61 | MenhirState59 | MenhirState55 | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADDRESS_OF ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | BOOL _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | FLOAT _v ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | ID _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | NEGATE ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | TEXT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | TIMES ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | RIGHT_ROUND_BRACKET ->
                _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState10 | MenhirState258 | MenhirState34 | MenhirState115 | MenhirState125 | MenhirState207 | MenhirState202 | MenhirState131 | MenhirState136 | MenhirState194 | MenhirState188 | MenhirState183 | MenhirState175 | MenhirState149 | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADDRESS_OF ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | BOOL _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
            | FLOAT _v ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
            | ID _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | NEGATE ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | TEXT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
            | TIMES ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | RIGHT_ROUND_BRACKET ->
                _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState168)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADDRESS_OF ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState212
            | BOOL _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState212 _v
            | FLOAT _v ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState212 _v
            | ID _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState212 _v
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState212 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState212
            | NEGATE ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState212
            | TEXT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState212 _v
            | TIMES ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState212
            | RIGHT_ROUND_BRACKET ->
                _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) MenhirState212
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState212)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADDRESS_OF ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState233
            | BOOL _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _v
            | FLOAT _v ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _v
            | ID _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _v
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState233
            | NEGATE ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState233
            | TEXT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _v
            | TIMES ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState233
            | RIGHT_ROUND_BRACKET ->
                _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) MenhirState233
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState233)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState246 | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADDRESS_OF ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState242
            | BOOL _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
            | FLOAT _v ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
            | ID _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState242
            | NEGATE ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState242
            | TEXT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
            | TIMES ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState242
            | RIGHT_ROUND_BRACKET ->
                _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) MenhirState242
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState242)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_let_declaration : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (l : (Syntax.expression)) = _v in
    let _v : (Syntax.expression) =                         ( l ) in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_right_assignment : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADDRESS_OF ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | BREAK ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | CONTINUE ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | FOR ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | ID _v ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | IF ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | LEFT_ROUND_BRACKET ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | LET ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | PRINT ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | RETURN ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | TIMES ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADDRESS_OF ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | BREAK ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | CONTINUE ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | FOR ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | ID _v ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
            | IF ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | LEFT_ROUND_BRACKET ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | LET ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | PRINT ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | RETURN ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | TIMES ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | TYPE ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState163 ->
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
    | MenhirState200 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADDRESS_OF ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState202
            | BREAK ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState202
            | CONTINUE ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState202
            | FOR ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState202
            | ID _v ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState202 _v
            | IF ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState202
            | LEFT_ROUND_BRACKET ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState202
            | LET ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState202
            | PRINT ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState202
            | RETURN ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState202
            | TIMES ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState202
            | TYPE ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState202
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState202
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState202)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADDRESS_OF ->
                _menhir_run237 _menhir_env (Obj.magic _menhir_stack) MenhirState246
            | BOOL _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
            | BREAK ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState246
            | CONTINUE ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState246
            | FLOAT _v ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
            | FOR ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState246
            | ID _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState246 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | SEMI_COLLON ->
                    _menhir_run138 _menhir_env (Obj.magic _menhir_stack)
                | ASSIGN ->
                    _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack)
                | LEFT_ROUND_BRACKET ->
                    _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
                | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
                    _menhir_reduce3 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | IF ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState246
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState246
            | LET ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState246
            | NEGATE ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState246
            | PRINT ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState246
            | RETURN ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState246
            | TEXT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
            | TIMES ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState246 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ID _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | ASSIGN ->
                        _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack)
                    | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
                        _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | LEFT_ROUND_BRACKET ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
                | TIMES ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState246
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState246)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_statements : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (id : (string))), (id1 : (string))), (id2 : (string))), _, (s : (Syntax.expression))) = _menhir_stack in
            let _13 = () in
            let _11 = () in
            let _10 = () in
            let _8 = () in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                                                                                                                                 ( For(id, Deref(Identifier(id1)), Deref(Identifier(id2)), s) ) in
            _menhir_goto_for_loop _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (s1 : (Syntax.expression))), _, (s2 : (Syntax.expression))) = _menhir_stack in
        let _v : (Syntax.expression) =                                     ( Seq(s1, s2) ) in
        _menhir_goto_statements _menhir_env _menhir_stack _menhir_s _v
    | MenhirState175 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (id : (string))), (id1 : (string))), (i : (int))), _, (s : (Syntax.expression))) = _menhir_stack in
            let _12 = () in
            let _10 = () in
            let _9 = () in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                                                                                                                         ( For(id, Deref(Identifier(id1)), MyInteger(i), s) ) in
            _menhir_goto_for_loop _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState183 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (id : (string))), (i : (int))), (id1 : (string))), _, (s : (Syntax.expression))) = _menhir_stack in
            let _12 = () in
            let _10 = () in
            let _9 = () in
            let _7 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                                                                                                                          ( For(id, MyInteger(i), Deref(Identifier(id1)), s) ) in
            _menhir_goto_for_loop _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState188 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (id : (string))), (i1 : (int))), (i2 : (int))), _, (s : (Syntax.expression))) = _menhir_stack in
            let _11 = () in
            let _9 = () in
            let _8 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                                                                                                                    ( For(id, MyInteger(i1), MyInteger(i2), s) ) in
            _menhir_goto_for_loop _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState136 ->
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
                    | ADDRESS_OF ->
                        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                    | BREAK ->
                        _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                    | CONTINUE ->
                        _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                    | FOR ->
                        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                    | ID _v ->
                        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
                    | IF ->
                        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                    | LEFT_ROUND_BRACKET ->
                        _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                    | LET ->
                        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                    | PRINT ->
                        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                    | RETURN ->
                        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                    | TIMES ->
                        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                    | TYPE ->
                        _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                    | WHILE ->
                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState194)
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
    | MenhirState194 ->
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
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), (id : (string))), _, (r : (Syntax.expression))), _, (s : (Syntax.expression))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (Syntax.expression) =                                                                                     ( New(id, r, s) ) in
        _menhir_goto_new_declaration _menhir_env _menhir_stack _menhir_s _v
    | MenhirState202 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), (id : (string))), _, (r : (Syntax.expression))), _, (s : (Syntax.expression))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (Syntax.expression) =                                                                              ( New(id, r, s) ) in
        _menhir_goto_new_declaration _menhir_env _menhir_stack _menhir_s _v
    | MenhirState125 ->
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
                    | ADDRESS_OF ->
                        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState207
                    | BREAK ->
                        _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState207
                    | CONTINUE ->
                        _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState207
                    | FOR ->
                        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState207
                    | ID _v ->
                        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState207 _v
                    | IF ->
                        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState207
                    | LEFT_ROUND_BRACKET ->
                        _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState207
                    | LET ->
                        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState207
                    | PRINT ->
                        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState207
                    | RETURN ->
                        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState207
                    | TIMES ->
                        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState207
                    | TYPE ->
                        _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState207
                    | WHILE ->
                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState207
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState207)
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
    | MenhirState207 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RIGHT_ROUND_BRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((((_menhir_stack, _menhir_s), _), _, (o : (Syntax.expression))), _, (s1 : (Syntax.expression))), _, (s2 : (Syntax.expression))) = _menhir_stack in
                let _13 = () in
                let _12 = () in
                let _10 = () in
                let _9 = () in
                let _8 = () in
                let _6 = () in
                let _5 = () in
                let _3 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Syntax.expression) =                                                                                                                                                                                                                                             ( If(o, s1, s2) ) in
                _menhir_goto_left_assignment _menhir_env _menhir_stack _menhir_s _v
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
    | MenhirState258 ->
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
        | ADDRESS_OF ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | BOOL _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | FLOAT _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | ID _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | INT _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | LEFT_ROUND_BRACKET ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | NEGATE ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | NULL ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | READ ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | TEXT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | TIMES ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState163)
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

and _menhir_run36 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_reduce18 : _menhir_env -> ('ttv_tail * _menhir_state) * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), (id : (string))) = _menhir_stack in
    let _1 = () in
    let _v : (Syntax.expression) =                   ( Deref(Identifier(id)) ) in
    _menhir_goto_left_assignment _menhir_env _menhir_stack _menhir_s _v

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
    | MenhirState271 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Syntax.fundef))), _, (xs : (Syntax.program))) = _menhir_stack in
        let _2 = () in
        let _v : (Syntax.program) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_FUNCTION_func_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run99 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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

and _menhir_run102 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Syntax.expression) =          ( MyNull ) in
    _menhir_goto_right_assignment _menhir_env _menhir_stack _menhir_s _v

and _menhir_run103 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run237 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | BOOL _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | BREAK ->
        _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | CONTINUE ->
        _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | FLOAT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | FOR ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | FUN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState103 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI_COLLON ->
            _menhir_run138 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RIGHT_ROUND_BRACKET ->
            _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
        | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
            _menhir_reduce3 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | IF ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | LET ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | NEGATE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | PRINT ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | RETURN ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | TEXT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | TIMES ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState103 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RIGHT_ROUND_BRACKET ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
            | ASSIGN ->
                _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack)
            | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
                _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | LEFT_ROUND_BRACKET ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103

and _menhir_reduce19 : _menhir_env -> (((('ttv_tail * _menhir_state) * _menhir_state)) * (string)) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s), _), (id : (string))) = _menhir_stack in
    let _5 = () in
    let _3 = () in
    let _2 = () in
    let _1 = () in
    let _v : (Syntax.expression) =                                                                   ( Deref(Identifier(id)) ) in
    _menhir_goto_left_assignment _menhir_env _menhir_stack _menhir_s _v

and _menhir_run111 : _menhir_env -> 'ttv_tail * _menhir_state -> _menhir_state -> 'ttv_return =
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
            | ADDRESS_OF ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | BOOL _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
            | FLOAT _v ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
            | ID _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | NEGATE ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | NULL ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | READ ->
                _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | TEXT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
            | TIMES ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
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

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FUN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run121 : _menhir_env -> 'ttv_tail * _menhir_state -> _menhir_state -> 'ttv_return =
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
        | ADDRESS_OF ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | BOOL _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
        | FLOAT _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
        | ID _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
        | INT _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
        | LEFT_ROUND_BRACKET ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | NEGATE ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | TEXT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
        | TIMES ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run28 : _menhir_env -> 'ttv_tail * _menhir_state -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | LAMBDA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState28 in
        let _v : (string list) =     ( [] ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack)
    | TIMES ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (t : (string)) = _v in
    let _v : (Syntax.expression) =              ( MyString(t) ) in
    _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | BOOL _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | FLOAT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | ID _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | NEGATE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | TEXT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | TIMES ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | BOOL _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | FLOAT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | FUN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | ID _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | NEGATE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | TEXT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | TIMES ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState19 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _v
        | TIMES ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (int)) = _v in
    let _v : (Syntax.expression) =             ( MyInteger(i) ) in
    _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_ROUND_BRACKET ->
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
    | AND | COMMA | DIVIDE | EQ | GEQ | GREATER | IN | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | RIGHT_ROUND_BRACKET | SEMI_COLLON | TIMES ->
        _menhir_reduce3 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> (float) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (f : (float)) = _v in
    let _v : (Syntax.expression) =               ( MyFloat(f) ) in
    _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v

and _menhir_run43 : _menhir_env -> 'ttv_tail -> _menhir_state -> (bool) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (b : (bool)) = _v in
    let _v : (Syntax.expression) =              ( MyBoolean(b) ) in
    _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce13 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (id : (string))) = _menhir_stack in
    let _v : (Syntax.expression) =             ( Identifier(id) ) in
    _menhir_goto_function_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce16 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (id : (string))) = _menhir_stack in
    let _v : (Syntax.expression) =             ( Identifier(id) ) in
    _menhir_goto_left_assignment _menhir_env _menhir_stack _menhir_s _v

and _menhir_run138 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (id : (string))) = _menhir_stack in
    let _2 = () in
    let _v : (Syntax.expression) =                         ( Identifier(id) ) in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState10 | MenhirState258 | MenhirState125 | MenhirState207 | MenhirState202 | MenhirState131 | MenhirState136 | MenhirState194 | MenhirState188 | MenhirState183 | MenhirState175 | MenhirState158 | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADDRESS_OF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | BREAK ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | CONTINUE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | FOR ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | ID _v ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v
        | IF ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | LEFT_ROUND_BRACKET ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | LET ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | PRINT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | RETURN ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | TIMES ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | TYPE ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (s : (Syntax.expression))) = _menhir_stack in
            let _v : (Syntax.expression) =                   ( s ) in
            _menhir_goto_statements _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState158)
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), _), (id : (string))), _, (r : (Syntax.expression))), _, (s : (Syntax.expression))) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                                                                            ( Let(id, r, s) ) in
            _menhir_goto_left_assignment _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState103 ->
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
            let _v : (Syntax.expression) =                                                            ( s ) in
            _menhir_goto_right_assignment _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState246 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), (id : (string))), _, (r : (Syntax.expression))), _, (s : (Syntax.expression))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (Syntax.expression) =                                                                  ( Let(id, r, s) ) in
        _menhir_goto_let_declaration _menhir_env _menhir_stack _menhir_s _v
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _), _, (xs0 : (string list))), _, (s : (Syntax.expression))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Syntax.expression) = let ids =
              let xs = xs0 in
                  ( xs )
            in
                                                                                                                    ( Lambda(ids, s) ) in
            _menhir_goto_function_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce17 : _menhir_env -> ('ttv_tail * _menhir_state) * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), (id : (string))) = _menhir_stack in
    let _1 = () in
    let _v : (Syntax.expression) =                        ( Identifier(id) ) in
    _menhir_goto_left_assignment _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_COMMA_parameter_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
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
            let (_menhir_stack, _menhir_s, (p : (string list))) = _menhir_stack in
            let _2 = () in
            let _v : (string list) =                                                                        ( p ) in
            _menhir_goto_parameter_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState266 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string))), _, (xs : (string list))) = _menhir_stack in
        let _2 = () in
        let _v : (string list) =     ( x :: xs ) in
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
        | ADDRESS_OF ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | BOOL _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | FLOAT _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | ID _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | INT _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | LEFT_ROUND_BRACKET ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | NEGATE ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | TEXT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | TIMES ->
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

and _menhir_run126 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            | ADDRESS_OF ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState200
            | BOOL _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState200 _v
            | FLOAT _v ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState200 _v
            | ID _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState200 _v
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState200 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState200
            | NEGATE ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState200
            | NULL ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState200
            | READ ->
                _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState200
            | TEXT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState200 _v
            | TIMES ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState200
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState200)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | TIMES ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
                | ADDRESS_OF ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState129
                | BOOL _v ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
                | FLOAT _v ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
                | ID _v ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
                | INT _v ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
                | LEFT_ROUND_BRACKET ->
                    _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState129
                | NEGATE ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState129
                | NULL ->
                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState129
                | READ ->
                    _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState129
                | TEXT _v ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
                | TIMES ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState129
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129)
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack)
    | LEFT_ROUND_BRACKET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
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
    let ((_menhir_stack, _menhir_s, (id : (string))), _, (p : (string list))) = _menhir_stack in
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
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState271 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState271)
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

and _menhir_run88 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | BOOL _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | FLOAT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | ID _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | NEGATE ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | TEXT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | TIMES ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88

and _menhir_run91 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | ADDRESS_OF ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | BOOL _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | FLOAT _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | ID _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | INT _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | LEFT_ROUND_BRACKET ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | NEGATE ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | TEXT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | TIMES ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run96 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            | ADDRESS_OF ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | BOOL _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
            | FLOAT _v ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
            | ID _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | NEGATE ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | NULL ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | READ ->
                _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | TEXT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
            | TIMES ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98)
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

and _menhir_run116 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FUN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | ID _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
    | IF ->
        _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | LEFT_ROUND_BRACKET ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | LET ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | TIMES ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState116 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | RIGHT_ROUND_BRACKET ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack)
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116

and _menhir_run132 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | ADDRESS_OF ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | BOOL _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
        | FLOAT _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
        | ID _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
        | INT _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
        | LEFT_ROUND_BRACKET ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | NEGATE ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | TEXT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
        | TIMES ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run137 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMI_COLLON ->
        _menhir_run138 _menhir_env (Obj.magic _menhir_stack)
    | ASSIGN ->
        _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack)
    | LEFT_ROUND_BRACKET ->
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run139 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                | INT _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | TO ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | INT _v ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_stack = (_menhir_stack, _v) in
                            let _menhir_env = _menhir_discard _menhir_env in
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
                                    | ADDRESS_OF ->
                                        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                                    | BREAK ->
                                        _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                                    | CONTINUE ->
                                        _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                                    | FOR ->
                                        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                                    | ID _v ->
                                        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
                                    | IF ->
                                        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                                    | LEFT_ROUND_BRACKET ->
                                        _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                                    | LET ->
                                        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                                    | PRINT ->
                                        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                                    | RETURN ->
                                        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                                    | TIMES ->
                                        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                                    | TYPE ->
                                        _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                                    | WHILE ->
                                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                                    | _ ->
                                        assert (not _menhir_env._menhir_error);
                                        _menhir_env._menhir_error <- true;
                                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState188)
                                | _ ->
                                    assert (not _menhir_env._menhir_error);
                                    _menhir_env._menhir_error <- true;
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let ((((_menhir_stack, _menhir_s), _), _), _) = _menhir_stack in
                                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let ((((_menhir_stack, _menhir_s), _), _), _) = _menhir_stack in
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                        | TIMES ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | ID _v ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_stack = (_menhir_stack, _v) in
                                let _menhir_env = _menhir_discard _menhir_env in
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
                                        | ADDRESS_OF ->
                                            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                                        | BREAK ->
                                            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                                        | CONTINUE ->
                                            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                                        | FOR ->
                                            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                                        | ID _v ->
                                            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
                                        | IF ->
                                            _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                                        | LEFT_ROUND_BRACKET ->
                                            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                                        | LET ->
                                            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                                        | PRINT ->
                                            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                                        | RETURN ->
                                            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                                        | TIMES ->
                                            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                                        | TYPE ->
                                            _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                                        | WHILE ->
                                            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                                        | _ ->
                                            assert (not _menhir_env._menhir_error);
                                            _menhir_env._menhir_error <- true;
                                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState183)
                                    | _ ->
                                        assert (not _menhir_env._menhir_error);
                                        _menhir_env._menhir_error <- true;
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let ((((_menhir_stack, _menhir_s), _), _), _) = _menhir_stack in
                                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                                | _ ->
                                    assert (not _menhir_env._menhir_error);
                                    _menhir_env._menhir_error <- true;
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let ((((_menhir_stack, _menhir_s), _), _), _) = _menhir_stack in
                                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | TIMES ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | ID _v ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = (_menhir_stack, _v) in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | TO ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | INT _v ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_stack = (_menhir_stack, _v) in
                                let _menhir_env = _menhir_discard _menhir_env in
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
                                        | ADDRESS_OF ->
                                            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                                        | BREAK ->
                                            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                                        | CONTINUE ->
                                            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                                        | FOR ->
                                            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                                        | ID _v ->
                                            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
                                        | IF ->
                                            _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                                        | LEFT_ROUND_BRACKET ->
                                            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                                        | LET ->
                                            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                                        | PRINT ->
                                            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                                        | RETURN ->
                                            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                                        | TIMES ->
                                            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                                        | TYPE ->
                                            _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                                        | WHILE ->
                                            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                                        | _ ->
                                            assert (not _menhir_env._menhir_error);
                                            _menhir_env._menhir_error <- true;
                                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState175)
                                    | _ ->
                                        assert (not _menhir_env._menhir_error);
                                        _menhir_env._menhir_error <- true;
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let ((((_menhir_stack, _menhir_s), _), _), _) = _menhir_stack in
                                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                                | _ ->
                                    assert (not _menhir_env._menhir_error);
                                    _menhir_env._menhir_error <- true;
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let ((((_menhir_stack, _menhir_s), _), _), _) = _menhir_stack in
                                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                            | TIMES ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_env = _menhir_discard _menhir_env in
                                let _tok = _menhir_env._menhir_token in
                                (match _tok with
                                | ID _v ->
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let _menhir_stack = (_menhir_stack, _v) in
                                    let _menhir_env = _menhir_discard _menhir_env in
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
                                            | ADDRESS_OF ->
                                                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState149
                                            | BREAK ->
                                                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState149
                                            | CONTINUE ->
                                                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState149
                                            | FOR ->
                                                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState149
                                            | ID _v ->
                                                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
                                            | IF ->
                                                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState149
                                            | LEFT_ROUND_BRACKET ->
                                                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState149
                                            | LET ->
                                                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState149
                                            | PRINT ->
                                                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState149
                                            | RETURN ->
                                                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState149
                                            | TIMES ->
                                                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState149
                                            | TYPE ->
                                                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState149
                                            | WHILE ->
                                                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState149
                                            | _ ->
                                                assert (not _menhir_env._menhir_error);
                                                _menhir_env._menhir_error <- true;
                                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149)
                                        | _ ->
                                            assert (not _menhir_env._menhir_error);
                                            _menhir_env._menhir_error <- true;
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            let ((((_menhir_stack, _menhir_s), _), _), _) = _menhir_stack in
                                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                                    | _ ->
                                        assert (not _menhir_env._menhir_error);
                                        _menhir_env._menhir_error <- true;
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let ((((_menhir_stack, _menhir_s), _), _), _) = _menhir_stack in
                                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                                | _ ->
                                    assert (not _menhir_env._menhir_error);
                                    _menhir_env._menhir_error <- true;
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
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
                    let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run150 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMI_COLLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Syntax.expression) =                           ( Continue ) in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run152 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMI_COLLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Syntax.expression) =                        ( Break ) in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run154 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

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
        let _v : (string) =                   ( id ) in
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
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState266)
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (string))) = _menhir_stack in
            let _v : (string list) =     ( [ x ] ) in
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

and _menhir_goto_parameter_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
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
        | ADDRESS_OF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | BREAK ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | CONTINUE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | FOR ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | ID _v ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | IF ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | LEFT_ROUND_BRACKET ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | LET ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | PRINT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | RETURN ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState10 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (Syntax.expression) =                         ( Nothing ) in
            _menhir_goto_content _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | TYPE ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState10
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
    | MenhirState271 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState266 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState258 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState246 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState242 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState233 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState229 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState219 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState212 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState207 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState202 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState200 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState194 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState188 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState183 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState175 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState168 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
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
            let _v : (string list) =                        ( [] ) in
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
        let _v : (Syntax.program) =         ( [] ) in
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
  

