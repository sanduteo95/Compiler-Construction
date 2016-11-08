
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
    | COMMA
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
  | MenhirState256
  | MenhirState251
  | MenhirState243
  | MenhirState231
  | MenhirState227
  | MenhirState219
  | MenhirState215
  | MenhirState205
  | MenhirState198
  | MenhirState193
  | MenhirState188
  | MenhirState186
  | MenhirState180
  | MenhirState174
  | MenhirState169
  | MenhirState162
  | MenhirState155
  | MenhirState150
  | MenhirState144
  | MenhirState141
  | MenhirState130
  | MenhirState127
  | MenhirState125
  | MenhirState123
  | MenhirState119
  | MenhirState116
  | MenhirState111
  | MenhirState110
  | MenhirState108
  | MenhirState102
  | MenhirState99
  | MenhirState94
  | MenhirState88
  | MenhirState84
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
  | MenhirState57
  | MenhirState55
  | MenhirState51
  | MenhirState35
  | MenhirState32
  | MenhirState31
  | MenhirState29
  | MenhirState25
  | MenhirState23
  | MenhirState17
  | MenhirState16
  | MenhirState12
  | MenhirState10
  | MenhirState3
  | MenhirState1
  
open Syntax
let length = List.length

let rec _menhir_reduce33 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (o : (Syntax.expression))) = _menhir_stack in
    let _v : (Syntax.expression) =                             ( o ) in
    _menhir_goto_min_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run201 : _menhir_env -> (((('ttv_tail * _menhir_state) * _menhir_state * (Syntax.expression))) * _menhir_state * (Syntax.expression list)) -> 'ttv_return =
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

and _menhir_run158 : _menhir_env -> ((('ttv_tail * _menhir_state * (Syntax.expression))) * _menhir_state * (Syntax.expression list)) -> 'ttv_return =
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

and _menhir_reduce5 : _menhir_env -> ((('ttv_tail * _menhir_state * (Syntax.expression))) * _menhir_state * (Syntax.expression list)) -> 'ttv_return =
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
    | MenhirState16 | MenhirState94 | MenhirState231 | MenhirState227 | MenhirState219 | MenhirState108 | MenhirState198 | MenhirState186 | MenhirState123 | MenhirState155 | MenhirState150 | MenhirState88 | MenhirState84 | MenhirState81 | MenhirState79 | MenhirState77 | MenhirState75 | MenhirState73 | MenhirState71 | MenhirState69 | MenhirState67 | MenhirState65 | MenhirState63 | MenhirState61 | MenhirState59 | MenhirState57 | MenhirState55 | MenhirState51 | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState116 ->
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
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | FOR ->
                    _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | ID _v ->
                    _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
                | IF ->
                    _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | LEFT_ROUND_BRACKET ->
                    _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | LET ->
                    _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | PRINT ->
                    _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | RETURN ->
                    _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | TIMES ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | TYPE ->
                    _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | WHILE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
            _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState127 ->
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
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | FOR ->
                    _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | ID _v ->
                    _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
                | IF ->
                    _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | LEFT_ROUND_BRACKET ->
                    _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | LET ->
                    _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | PRINT ->
                    _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | RETURN ->
                    _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | TIMES ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | TYPE ->
                    _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | WHILE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
            _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState17 | MenhirState99 | MenhirState102 ->
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
            _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack)
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
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState243
                | FOR ->
                    _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState243
                | ID _v ->
                    _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState243 _v
                | IF ->
                    _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState243
                | LEFT_ROUND_BRACKET ->
                    _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState243
                | LET ->
                    _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState243
                | PRINT ->
                    _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState243
                | RETURN ->
                    _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState243
                | TIMES ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState243
                | TYPE ->
                    _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState243
                | WHILE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState243
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState243)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
            _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState227 | MenhirState219 | MenhirState198 | MenhirState155 | MenhirState51 | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Syntax.expression list)) = _v in
        let _v : (Syntax.expression list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Syntax.expression list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Syntax.expression))) = _menhir_stack in
        let _2 = () in
        let _v : (Syntax.expression list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run55 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | BOOL _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | ID _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | NEGATE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | TEXT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | TIMES ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run57 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | BOOL _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | ID _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NEGATE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | TEXT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | TIMES ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | BOOL _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | ID _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NEGATE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | TEXT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | TIMES ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run65 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | BOOL _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | ID _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | NEGATE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | TEXT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | TIMES ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65

and _menhir_run59 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | BOOL _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | ID _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NEGATE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | TEXT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | TIMES ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run67 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | BOOL _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | ID _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | NEGATE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | TEXT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
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
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | BOOL _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | ID _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | NEGATE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | TEXT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | TIMES ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

and _menhir_run71 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | BOOL _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | ID _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | NEGATE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | TEXT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
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
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | BOOL _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | ID _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NEGATE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | TEXT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
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
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | BOOL _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | ID _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | NEGATE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | TEXT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
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
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | BOOL _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | ID _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | NEGATE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | TEXT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | TIMES ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run61 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | BOOL _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | ID _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | NEGATE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | TEXT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | TIMES ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_run81 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | BOOL _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | ID _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | NEGATE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | TEXT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | TIMES ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_goto_separated_nonempty_list_COMMA_value_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState205 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Syntax.expression))), _, (xs : (Syntax.expression list))) = _menhir_stack in
        let _2 = () in
        let _v : (Syntax.expression list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_value_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState17 | MenhirState99 | MenhirState102 ->
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

and _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState35 ->
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
                                                                                                                                                                             ( Application(f, a) ) in
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
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState155 ->
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
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState198 ->
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
                _menhir_run201 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState219 ->
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
                _menhir_run201 _menhir_env (Obj.magic _menhir_stack)
            | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
                _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState227 ->
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
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack)
            | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
                _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack)
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

and _menhir_goto_separated_nonempty_list_COMMA_ids_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState17 | MenhirState99 | MenhirState102 ->
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
    | MenhirState215 ->
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
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (string list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (string))) = _menhir_stack in
        let _2 = () in
        let _v : (string list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_ID_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (string list)) = _v in
        let _v : (string list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce32 : _menhir_env -> ((('ttv_tail * _menhir_state) * _menhir_state) * (string)) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s), _), (id : (string))) = _menhir_stack in
    let _4 = () in
    let _2 = () in
    let _1 = () in
    let _v : (Syntax.expression) =                                                            ( Deref(Deref(Identifier(id))) ) in
    _menhir_goto_min_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState227 | MenhirState219 | MenhirState198 | MenhirState155 | MenhirState35 | MenhirState79 | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADDRESS_OF ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | BOOL _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | FLOAT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | ID _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | INT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | NEGATE ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | TEXT _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | TIMES ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
        | DIVIDE ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
        let _2 = () in
        let _v : (Syntax.expression) =                                             ( Operator(Times, e1, e2) ) in
        _menhir_goto_operator_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
        let _2 = () in
        let _v : (Syntax.expression) =                                               ( Operator(Modulus, e1, e2)) in
        _menhir_goto_operator_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
        let _2 = () in
        let _v : (Syntax.expression) =                                              ( Operator(Divide, e1, e2) ) in
        _menhir_goto_operator_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState94 | MenhirState108 | MenhirState186 | MenhirState123 | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState12 | MenhirState17 | MenhirState231 | MenhirState99 | MenhirState102 | MenhirState116 | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (e : (Syntax.expression))) = _menhir_stack in
        let _1 = () in
        let _v : (Syntax.expression) =                            ( Negate(e) ) in
        _menhir_goto_operator_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce69 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (x : (Syntax.expression))) = _menhir_stack in
    let _v : (Syntax.expression list) =     ( [ x ] ) in
    _menhir_goto_separated_nonempty_list_COMMA_value_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run205 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
    | TEXT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState205

and _menhir_reduce89 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (v : (Syntax.expression))) = _menhir_stack in
    let _v : (Syntax.expression) =               ( v ) in
    _menhir_goto_values _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Syntax.expression list) =     ( [] ) in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_statements : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState141 ->
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
    | MenhirState144 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (s1 : (Syntax.expression))), _, (s2 : (Syntax.expression))) = _menhir_stack in
        let _v : (Syntax.expression) =                                     ( Seq(s1, s2) ) in
        _menhir_goto_statements _menhir_env _menhir_stack _menhir_s _v
    | MenhirState162 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (id : (string))), (i : (int))), (id1 : (string))), _, (s : (Syntax.expression))) = _menhir_stack in
            let _11 = () in
            let _9 = () in
            let _8 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                                                                                                                   ( For(id, MyInteger(i), Deref(Identifier(id1)), s) ) in
            _menhir_goto_for_loop _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState169 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (id : (string))), (id1 : (string))), (i : (int))), _, (s : (Syntax.expression))) = _menhir_stack in
            let _11 = () in
            let _9 = () in
            let _8 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                                                                                                                  ( For(id, Deref(Identifier(id1)), MyInteger(i), s) ) in
            _menhir_goto_for_loop _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState174 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (id : (string))), (id1 : (string))), (id2 : (string))), _, (s : (Syntax.expression))) = _menhir_stack in
            let _11 = () in
            let _9 = () in
            let _8 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                                                                                                                   ( For(id, Deref(Identifier(id1)), Deref(Identifier(id2)), s) ) in
            _menhir_goto_for_loop _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState130 ->
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
                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState180
                    | FOR ->
                        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState180
                    | ID _v ->
                        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _v
                    | IF ->
                        _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState180
                    | LEFT_ROUND_BRACKET ->
                        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState180
                    | LET ->
                        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState180
                    | PRINT ->
                        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState180
                    | RETURN ->
                        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState180
                    | TIMES ->
                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState180
                    | TYPE ->
                        _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState180
                    | WHILE ->
                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState180
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState180)
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
    | MenhirState180 ->
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
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), (id : (string))), _, (r : (Syntax.expression))), _, (s : (Syntax.expression))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (Syntax.expression) =                                                                                     ( New(id, r, s) ) in
        _menhir_goto_new_declaration _menhir_env _menhir_stack _menhir_s _v
    | MenhirState188 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), (id : (string))), _, (r : (Syntax.expression))), _, (s : (Syntax.expression))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (Syntax.expression) =                                                                              ( New(id, r, s) ) in
        _menhir_goto_new_declaration _menhir_env _menhir_stack _menhir_s _v
    | MenhirState119 ->
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
                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState193
                    | FOR ->
                        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState193
                    | ID _v ->
                        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
                    | IF ->
                        _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState193
                    | LEFT_ROUND_BRACKET ->
                        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState193
                    | LET ->
                        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState193
                    | PRINT ->
                        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState193
                    | RETURN ->
                        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState193
                    | TIMES ->
                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState193
                    | TYPE ->
                        _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState193
                    | WHILE ->
                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState193
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState193)
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
    | MenhirState193 ->
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
    | MenhirState243 ->
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

and _menhir_goto_let_declaration : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (l : (Syntax.expression)) = _v in
    let _v : (Syntax.expression) =                         ( l ) in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce4 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (me : (Syntax.expression))) = _menhir_stack in
    let _v : (Syntax.expression) =                         ( me ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_right_assignment : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState108 ->
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
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | FOR ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | ID _v ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
            | IF ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | LEFT_ROUND_BRACKET ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | LET ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | PRINT ->
                _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | RETURN ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | TIMES ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState123 ->
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
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | FOR ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | ID _v ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
            | IF ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | LEFT_ROUND_BRACKET ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | LET ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | PRINT ->
                _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | RETURN ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | TIMES ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | TYPE ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState125
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
    | MenhirState150 ->
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
    | MenhirState186 ->
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
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | FOR ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | ID _v ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | IF ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | LEFT_ROUND_BRACKET ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | LET ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | PRINT ->
                _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | RETURN ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | TIMES ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | TYPE ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState188
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
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState94 ->
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
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState231
            | BOOL _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState231 _v
            | FLOAT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState231 _v
            | FOR ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState231
            | ID _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState231 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | SEMI_COLLON ->
                    _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
                | ASSIGN ->
                    _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
                | LEFT_ROUND_BRACKET ->
                    _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
                | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
                    _menhir_reduce3 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | IF ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState231
            | INT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState231 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState231
            | LET ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState231
            | NEGATE ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState231
            | PRINT ->
                _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState231
            | RETURN ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState231
            | TEXT _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState231 _v
            | TIMES ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState231 in
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
                    | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | SEMI_COLLON | TIMES ->
                        _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack)
                    | ASSIGN ->
                        _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | LEFT_ROUND_BRACKET ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState231
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState231)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run102 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | BOOL _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | FUN ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | ID _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | IF ->
        _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | LET ->
        _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | NEGATE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | TEXT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | TIMES ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState102 in
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
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | SEMI_COLLON | TIMES ->
                    _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack)
                | ASSIGN ->
                    _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
                _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack)
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102

and _menhir_reduce13 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
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
            let _menhir_s = MenhirState215 in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState215)
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
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | FOR ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | ID _v ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
        | IF ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | LEFT_ROUND_BRACKET ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | LET ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | PRINT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | RETURN ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | TIMES ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
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
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25)
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

and _menhir_reduce31 : _menhir_env -> ('ttv_tail * _menhir_state) * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), (id : (string))) = _menhir_stack in
    let _1 = () in
    let _v : (Syntax.expression) =                   ( Deref(Deref(Identifier(id))) ) in
    _menhir_goto_min_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run20 : _menhir_env -> (('ttv_tail * _menhir_state) * _menhir_state) * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA | RIGHT_ROUND_BRACKET ->
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
    | LEFT_ROUND_BRACKET ->
        _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
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
    let _v : (Syntax.expression) =             ( Deref(Identifier(id)) ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_value : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState12 | MenhirState16 | MenhirState231 | MenhirState94 | MenhirState227 | MenhirState219 | MenhirState198 | MenhirState186 | MenhirState155 | MenhirState150 | MenhirState127 | MenhirState123 | MenhirState116 | MenhirState108 | MenhirState88 | MenhirState84 | MenhirState81 | MenhirState79 | MenhirState77 | MenhirState75 | MenhirState73 | MenhirState71 | MenhirState69 | MenhirState67 | MenhirState65 | MenhirState63 | MenhirState61 | MenhirState59 | MenhirState57 | MenhirState55 | MenhirState51 | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce89 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState17 | MenhirState99 | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            _menhir_run205 _menhir_env (Obj.magic _menhir_stack)
        | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
            _menhir_reduce89 _menhir_env (Obj.magic _menhir_stack)
        | RIGHT_ROUND_BRACKET ->
            _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState205 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            _menhir_run205 _menhir_env (Obj.magic _menhir_stack)
        | RIGHT_ROUND_BRACKET ->
            _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_function_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState31 ->
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
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | BOOL _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
            | FLOAT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
            | ID _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
            | INT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | NEGATE ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | TEXT _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
            | TIMES ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | RIGHT_ROUND_BRACKET ->
                _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState12 | MenhirState16 | MenhirState17 | MenhirState94 | MenhirState227 | MenhirState219 | MenhirState108 | MenhirState198 | MenhirState116 | MenhirState186 | MenhirState123 | MenhirState127 | MenhirState155 | MenhirState150 | MenhirState88 | MenhirState84 | MenhirState81 | MenhirState79 | MenhirState77 | MenhirState75 | MenhirState73 | MenhirState71 | MenhirState69 | MenhirState67 | MenhirState65 | MenhirState63 | MenhirState61 | MenhirState59 | MenhirState57 | MenhirState55 | MenhirState51 | MenhirState35 ->
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
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | BOOL _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
            | FLOAT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
            | ID _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
            | INT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | NEGATE ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | TEXT _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
            | TIMES ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | RIGHT_ROUND_BRACKET ->
                _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState10 | MenhirState243 | MenhirState29 | MenhirState110 | MenhirState119 | MenhirState193 | MenhirState188 | MenhirState125 | MenhirState130 | MenhirState180 | MenhirState174 | MenhirState169 | MenhirState162 | MenhirState141 | MenhirState144 ->
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
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | BOOL _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | FLOAT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | ID _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | INT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | NEGATE ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | TEXT _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | TIMES ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | RIGHT_ROUND_BRACKET ->
                _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155)
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
        | LEFT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADDRESS_OF ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | BOOL _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _v
            | FLOAT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _v
            | ID _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _v
            | INT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | NEGATE ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | TEXT _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _v
            | TIMES ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | RIGHT_ROUND_BRACKET ->
                _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState198)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState102 ->
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
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState219
            | BOOL _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState219 _v
            | FLOAT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState219 _v
            | ID _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState219 _v
            | INT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState219 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState219
            | NEGATE ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState219
            | TEXT _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState219 _v
            | TIMES ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState219
            | RIGHT_ROUND_BRACKET ->
                _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) MenhirState219
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState219)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState231 | MenhirState99 ->
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
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | BOOL _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState227 _v
            | FLOAT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState227 _v
            | ID _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState227 _v
            | INT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState227 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | NEGATE ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | TEXT _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState227 _v
            | TIMES ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | RIGHT_ROUND_BRACKET ->
                _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState227)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState10 | MenhirState243 | MenhirState119 | MenhirState193 | MenhirState188 | MenhirState125 | MenhirState130 | MenhirState180 | MenhirState174 | MenhirState169 | MenhirState162 | MenhirState144 | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADDRESS_OF ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | FOR ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | ID _v ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
        | IF ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | LEFT_ROUND_BRACKET ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | LET ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | PRINT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | RETURN ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | TIMES ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | TYPE ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (s : (Syntax.expression))) = _menhir_stack in
            let _v : (Syntax.expression) =                   ( s ) in
            _menhir_goto_statements _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState144)
    | MenhirState110 ->
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
    | MenhirState99 ->
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
    | MenhirState231 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), (id : (string))), _, (r : (Syntax.expression))), _, (s : (Syntax.expression))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (Syntax.expression) =                                                                   ( Let(id, r, s) ) in
        _menhir_goto_let_declaration _menhir_env _menhir_stack _menhir_s _v
    | MenhirState29 ->
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
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | BOOL _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
        | FLOAT _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
        | ID _v ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
        | LEFT_ROUND_BRACKET ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | NEGATE ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | NULL ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | READ ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | TEXT _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
        | TIMES ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState150)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_min_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState12 | MenhirState16 | MenhirState17 | MenhirState94 | MenhirState227 | MenhirState99 | MenhirState219 | MenhirState102 | MenhirState108 | MenhirState198 | MenhirState116 | MenhirState186 | MenhirState123 | MenhirState127 | MenhirState155 | MenhirState150 | MenhirState88 | MenhirState84 | MenhirState81 | MenhirState79 | MenhirState77 | MenhirState75 | MenhirState73 | MenhirState71 | MenhirState69 | MenhirState67 | MenhirState65 | MenhirState63 | MenhirState61 | MenhirState59 | MenhirState57 | MenhirState55 | MenhirState51 | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState231 ->
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
            _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_reduce16 : _menhir_env -> ('ttv_tail * _menhir_state) * (string) -> 'ttv_return =
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
    | MenhirState256 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Syntax.fundef))), _, (xs : (Syntax.program))) = _menhir_stack in
        let _2 = () in
        let _v : (Syntax.program) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_FUNCTION_func_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run95 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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

and _menhir_run98 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Syntax.expression) =          ( MyNull ) in
    _menhir_goto_right_assignment _menhir_env _menhir_stack _menhir_s _v

and _menhir_run99 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | BOOL _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | FOR ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | FUN ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState99 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI_COLLON ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RIGHT_ROUND_BRACKET ->
            _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
        | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
            _menhir_reduce3 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | IF ->
        _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | LET ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | NEGATE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | PRINT ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | RETURN ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | TEXT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | TIMES ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState99 in
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
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
            | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
                _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack)
            | ASSIGN ->
                _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | LEFT_ROUND_BRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99

and _menhir_reduce17 : _menhir_env -> ((('ttv_tail * _menhir_state) * _menhir_state) * (string)) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s), _), (id : (string))) = _menhir_stack in
    let _4 = () in
    let _2 = () in
    let _1 = () in
    let _v : (Syntax.expression) =                                                            ( Deref(Identifier(id)) ) in
    _menhir_goto_left_assignment _menhir_env _menhir_stack _menhir_s _v

and _menhir_run106 : _menhir_env -> 'ttv_tail * _menhir_state -> _menhir_state -> 'ttv_return =
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
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | BOOL _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | FLOAT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | ID _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | INT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | NEGATE ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | NULL ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | READ ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | TEXT _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | TIMES ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
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

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FUN ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run115 : _menhir_env -> 'ttv_tail * _menhir_state -> _menhir_state -> 'ttv_return =
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
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | BOOL _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
        | FLOAT _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
        | ID _v ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
        | LEFT_ROUND_BRACKET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | NEGATE ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | TEXT _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
        | TIMES ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run23 : _menhir_env -> 'ttv_tail * _menhir_state -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | LAMBDA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState23 in
        let _v : (string list) =     ( [] ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

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
        _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (t : (string)) = _v in
    let _v : (Syntax.expression) =              ( MyString(t) ) in
    _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | BOOL _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | ID _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | NEGATE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | TEXT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | TIMES ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | BOOL _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | FUN ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | ID _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | NEGATE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | TEXT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | TIMES ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState17 in
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
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
            | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | TIMES ->
                _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack)
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (int)) = _v in
    let _v : (Syntax.expression) =             ( MyInteger(i) ) in
    _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_ROUND_BRACKET ->
        _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
    | AND | COMMA | DIVIDE | EQ | GEQ | GREATER | IN | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | RIGHT_ROUND_BRACKET | SEMI_COLLON | TIMES ->
        _menhir_reduce3 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> (float) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (f : (float)) = _v in
    let _v : (Syntax.expression) =               ( MyFloat(f) ) in
    _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> (bool) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (b : (bool)) = _v in
    let _v : (Syntax.expression) =              ( MyBoolean(b) ) in
    _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce11 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (id : (string))) = _menhir_stack in
    let _v : (Syntax.expression) =             ( Identifier(id) ) in
    _menhir_goto_function_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce14 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (id : (string))) = _menhir_stack in
    let _v : (Syntax.expression) =             ( Identifier(id) ) in
    _menhir_goto_left_assignment _menhir_env _menhir_stack _menhir_s _v

and _menhir_run132 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (id : (string))) = _menhir_stack in
    let _2 = () in
    let _v : (Syntax.expression) =                         ( Identifier(id) ) in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce15 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (p : (Syntax.expression))) = _menhir_stack in
    let _v : (Syntax.expression) =                 ( p ) in
    _menhir_goto_left_assignment _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce30 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (p : (Syntax.expression))) = _menhir_stack in
    let _v : (Syntax.expression) =                 ( p ) in
    _menhir_goto_min_expression _menhir_env _menhir_stack _menhir_s _v

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
    | MenhirState251 ->
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
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | BOOL _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | FLOAT _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | ID _v ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | LEFT_ROUND_BRACKET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | NEGATE ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | TEXT _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
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

and _menhir_run120 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | BOOL _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
            | FLOAT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
            | ID _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
            | INT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | NEGATE ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | NULL ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | READ ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | TEXT _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
            | TIMES ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState186)
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
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                | BOOL _v ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
                | FLOAT _v ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
                | ID _v ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
                | INT _v ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
                | LEFT_ROUND_BRACKET ->
                    _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                | NEGATE ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                | NULL ->
                    _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                | READ ->
                    _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                | TEXT _v ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
                | TIMES ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123)
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

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack)
    | LEFT_ROUND_BRACKET ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState256 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState256)
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

and _menhir_run84 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS_OF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | BOOL _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | ID _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | NEGATE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | TEXT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | TIMES ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84

and _menhir_run87 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | BOOL _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | FLOAT _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | ID _v ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | LEFT_ROUND_BRACKET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | NEGATE ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | TEXT _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | TIMES ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run92 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | BOOL _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | FLOAT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | ID _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | INT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | NEGATE ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | NULL ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | READ ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | TEXT _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | TIMES ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
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

and _menhir_run111 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FUN ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | ID _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | IF ->
        _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | LEFT_ROUND_BRACKET ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | LET ->
        _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | TIMES ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState111 in
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
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111

and _menhir_run126 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | BOOL _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
        | FLOAT _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
        | ID _v ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
        | LEFT_ROUND_BRACKET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | NEGATE ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | TEXT _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
        | TIMES ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run131 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMI_COLLON ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
    | ASSIGN ->
        _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
    | LEFT_ROUND_BRACKET ->
        _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run133 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                                    | FOR ->
                                        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                                    | ID _v ->
                                        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _v
                                    | IF ->
                                        _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                                    | LEFT_ROUND_BRACKET ->
                                        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                                    | LET ->
                                        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                                    | PRINT ->
                                        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                                    | RETURN ->
                                        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                                    | TIMES ->
                                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                                    | TYPE ->
                                        _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                                    | WHILE ->
                                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                                    | _ ->
                                        assert (not _menhir_env._menhir_error);
                                        _menhir_env._menhir_error <- true;
                                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState174)
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
                                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState169
                                    | FOR ->
                                        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState169
                                    | ID _v ->
                                        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v
                                    | IF ->
                                        _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState169
                                    | LEFT_ROUND_BRACKET ->
                                        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState169
                                    | LET ->
                                        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState169
                                    | PRINT ->
                                        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState169
                                    | RETURN ->
                                        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState169
                                    | TIMES ->
                                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState169
                                    | TYPE ->
                                        _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState169
                                    | WHILE ->
                                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState169
                                    | _ ->
                                        assert (not _menhir_env._menhir_error);
                                        _menhir_env._menhir_error <- true;
                                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState169)
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
                                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState162
                                    | FOR ->
                                        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState162
                                    | ID _v ->
                                        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v
                                    | IF ->
                                        _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState162
                                    | LEFT_ROUND_BRACKET ->
                                        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState162
                                    | LET ->
                                        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState162
                                    | PRINT ->
                                        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState162
                                    | RETURN ->
                                        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState162
                                    | TIMES ->
                                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState162
                                    | TYPE ->
                                        _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState162
                                    | WHILE ->
                                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState162
                                    | _ ->
                                        assert (not _menhir_env._menhir_error);
                                        _menhir_env._menhir_error <- true;
                                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState162)
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
                                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState141
                                    | FOR ->
                                        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState141
                                    | ID _v ->
                                        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
                                    | IF ->
                                        _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState141
                                    | LEFT_ROUND_BRACKET ->
                                        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState141
                                    | LET ->
                                        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState141
                                    | PRINT ->
                                        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState141
                                    | RETURN ->
                                        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState141
                                    | TIMES ->
                                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState141
                                    | TYPE ->
                                        _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState141
                                    | WHILE ->
                                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState141
                                    | _ ->
                                        assert (not _menhir_env._menhir_error);
                                        _menhir_env._menhir_error <- true;
                                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141)
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

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        let _v : (Syntax.expression) =                        ( Identifier(id) ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState12 | MenhirState16 | MenhirState17 | MenhirState94 | MenhirState227 | MenhirState219 | MenhirState102 | MenhirState108 | MenhirState198 | MenhirState186 | MenhirState123 | MenhirState155 | MenhirState150 | MenhirState127 | MenhirState116 | MenhirState88 | MenhirState84 | MenhirState81 | MenhirState79 | MenhirState77 | MenhirState75 | MenhirState73 | MenhirState71 | MenhirState69 | MenhirState67 | MenhirState65 | MenhirState63 | MenhirState61 | MenhirState59 | MenhirState57 | MenhirState55 | MenhirState51 | MenhirState35 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack)
        | MenhirState10 | MenhirState243 | MenhirState29 | MenhirState110 | MenhirState119 | MenhirState193 | MenhirState188 | MenhirState125 | MenhirState130 | MenhirState180 | MenhirState174 | MenhirState169 | MenhirState162 | MenhirState141 | MenhirState144 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)
        | MenhirState231 | MenhirState99 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AND | DIVIDE | EQ | GEQ | GREATER | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | SEMI_COLLON | TIMES ->
                _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack)
            | ASSIGN ->
                _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
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
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState251
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState251)
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
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | FOR ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | ID _v ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | IF ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | LEFT_ROUND_BRACKET ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | LET ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | PRINT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | RETURN ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState10 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (Syntax.expression) =                         ( Nothing ) in
            _menhir_goto_content _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | TYPE ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState10
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
    | MenhirState256 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState251 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState243 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState231 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState227 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState219 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState215 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState205 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState198 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState193 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState188 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState186 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState180 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState174 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState169 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState162 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState144 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
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
  

