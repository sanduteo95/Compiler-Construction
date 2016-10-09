
module Basics = struct
  
  exception Error
  
  type token = 
    | WHILE
    | TYPE
    | TIMES
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
  | MenhirState103
  | MenhirState98
  | MenhirState94
  | MenhirState91
  | MenhirState90
  | MenhirState88
  | MenhirState87
  | MenhirState85
  | MenhirState84
  | MenhirState81
  | MenhirState78
  | MenhirState76
  | MenhirState73
  | MenhirState71
  | MenhirState69
  | MenhirState63
  | MenhirState54
  | MenhirState53
  | MenhirState51
  | MenhirState48
  | MenhirState44
  | MenhirState42
  | MenhirState40
  | MenhirState38
  | MenhirState36
  | MenhirState34
  | MenhirState32
  | MenhirState30
  | MenhirState28
  | MenhirState26
  | MenhirState24
  | MenhirState22
  | MenhirState19
  | MenhirState17
  | MenhirState13
  | MenhirState12
  | MenhirState10
  | MenhirState3
  | MenhirState1
  
open Syntax

let rec _menhir_run19 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | NEGATE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_run22 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | NEGATE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_run28 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
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
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | NEGATE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run24 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | NEGATE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24

and _menhir_run32 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | NEGATE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run34 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | NEGATE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_run36 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
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
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | NEGATE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run40 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
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
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | NEGATE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run26 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | NEGATE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run44 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | NEGATE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_run17 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | NEGATE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFT_ROUND_BRACKET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | AND | DIVIDE | EQ | GEQ | GREATER | IN | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | RIGHT_ROUND_BRACKET | SEMI_COLLON | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Syntax.expression))) = _menhir_stack in
            let _1 = () in
            let _v : (Syntax.expression) =                     ( Negate(e) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Syntax.expression) =                                                                 ( Application(e1, e2)) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFT_ROUND_BRACKET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | AND | DIVIDE | EQ | GEQ | GREATER | IN | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | RIGHT_ROUND_BRACKET | SEMI_COLLON | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                               ( Operator(Times, e1, e2) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | AND | EQ | GEQ | GREATER | IN | LEQ | LESS | MINUS | NOTEQ | OR | PLUS | RIGHT_ROUND_BRACKET | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                               ( Operator(Plus, e1, e2) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFT_ROUND_BRACKET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | AND | DIVIDE | EQ | GEQ | GREATER | IN | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | RIGHT_ROUND_BRACKET | SEMI_COLLON | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                                 ( Operator(Modulus, e1, e2)) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFT_ROUND_BRACKET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | AND | DIVIDE | EQ | GEQ | GREATER | IN | LEQ | LESS | MINUS | MODULUS | NOTEQ | OR | PLUS | RIGHT_ROUND_BRACKET | SEMI_COLLON | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                                ( Operator(Divide, e1, e2) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | AND | IN | OR | RIGHT_ROUND_BRACKET | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                            ( Operator(Or, e1, e2) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | AND | EQ | IN | NOTEQ | OR | RIGHT_ROUND_BRACKET | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                               ( Operator(Noteq, e1, e2)) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | AND | EQ | GEQ | GREATER | IN | LEQ | LESS | MINUS | NOTEQ | OR | PLUS | RIGHT_ROUND_BRACKET | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                               ( Operator(Minus, e1, e2) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | AND | EQ | GEQ | GREATER | IN | LEQ | LESS | NOTEQ | OR | RIGHT_ROUND_BRACKET | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                              ( Operator(Less, e1, e2)) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | AND | EQ | GEQ | GREATER | IN | LEQ | LESS | NOTEQ | OR | RIGHT_ROUND_BRACKET | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                             ( Operator(Leq, e1, e2)) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | AND | EQ | GEQ | GREATER | IN | LEQ | LESS | NOTEQ | OR | RIGHT_ROUND_BRACKET | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                                 ( Operator(Greater, e1, e2)) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | AND | EQ | GEQ | GREATER | IN | LEQ | LESS | NOTEQ | OR | RIGHT_ROUND_BRACKET | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                             ( Operator(Geq, e1, e2) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | AND | EQ | IN | NOTEQ | OR | RIGHT_ROUND_BRACKET | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                            ( Operator(Eq, e1, e2) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | AND | IN | OR | RIGHT_ROUND_BRACKET | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                             ( Operator(And, e1, e2) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
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
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
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
                    _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
                | IF ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState48
                | LET ->
                    _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState48
                | PRINT ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState48
                | READ ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState48
                | RETURN ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState48
                | TYPE ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState48
                | WHILE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState48
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
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
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
            | IF ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | LET ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | PRINT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | READ ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | RETURN ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | TYPE ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Syntax.expression))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                 ( e ) in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
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
                let _v : (Syntax.expression) =                                                                          ( Printint(e) ) in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
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
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
            | IF ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | LET ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | PRINT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | READ ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | RETURN ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | TYPE ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
        | LEFT_ROUND_BRACKET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
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
                    _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
                | IF ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                | LET ->
                    _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                | PRINT ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                | READ ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                | RETURN ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                | TYPE ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                | WHILE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MODULUS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (id : (string))), _, (e : (Syntax.expression))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Syntax.expression) =                                           ( Asg(Identifier(id), e)) in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

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
            let (_menhir_stack, _, (funcs : (Syntax.program))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Syntax.program) =                                                                    ( funcs ) in
            _menhir_goto_top _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Syntax.fundef))), _, (xs : (Syntax.program))) = _menhir_stack in
        let _2 = () in
        let _v : (Syntax.program) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_FUNCTION_func_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_stmt : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | IF ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | LET ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | PRINT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | READ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | RETURN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState81 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
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
                        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
                    | IF ->
                        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState84
                    | LET ->
                        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState84
                    | PRINT ->
                        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState84
                    | READ ->
                        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState84
                    | RETURN ->
                        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState84
                    | TYPE ->
                        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState84
                    | WHILE ->
                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState84
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
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
        | TYPE ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
        | IF ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | LET ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | PRINT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | READ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | RETURN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState85 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), _, (e : (Syntax.expression))), _, (s1 : (Syntax.expression))), _), _, (s2 : (Syntax.expression))) = _menhir_stack in
            let _11 = () in
            let _9 = () in
            let _8 = () in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                                                                                                                                        ( If(e, s1, s2) ) in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
        | TYPE ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85)
    | MenhirState94 | MenhirState91 | MenhirState90 | MenhirState88 | MenhirState81 | MenhirState87 | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | IF ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | LET ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | PRINT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | READ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | RETURN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | TYPE ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | RIGHT_CURLY_BRACKET | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (s1 : (Syntax.expression))), _, (s2 : (Syntax.expression))) = _menhir_stack in
            let _v : (Syntax.expression) =                          ( Seq(s1, s2) ) in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | IF ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | LET ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | PRINT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | READ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | RETURN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState88 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), (id : (string))), _, (e : (Syntax.expression))), _, (s : (Syntax.expression))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                              ( Let(id, e, s) ) in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
        | TYPE ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | IF ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | LET ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | PRINT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | READ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | RETURN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | TYPE ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | RIGHT_CURLY_BRACKET | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), (id : (string))), _, (e : (Syntax.expression))), _, (s : (Syntax.expression))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                           ( New(id, e, s) ) in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | IF ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | LET ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | PRINT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | READ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | RETURN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState91 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e : (Syntax.expression))), _, (s : (Syntax.expression))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                                                                                ( While(e, s) ) in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
        | TYPE ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
        | IF ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | LET ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | PRINT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | READ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | RETURN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState94 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (s : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                                   ( s ) in
            _menhir_goto_function_content _menhir_env _menhir_stack _menhir_s _v
        | TYPE ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
    | _ ->
        _menhir_fail ()

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | NEGATE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (int)) = _v in
    let _v : (Syntax.expression) =             ( Const(i) ) in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (id : (string)) = _v in
    let _v : (Syntax.expression) =             ( Deref(Identifier(id)) ) in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_COMMA_param_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.parameter list) -> 'ttv_return =
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
            let _v : (Syntax.parameter list) =                                                                    ( p ) in
            _menhir_goto_param_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Syntax.parameter))), _, (xs : (Syntax.parameter list))) = _menhir_stack in
        let _2 = () in
        let _v : (Syntax.parameter list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_param_ _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | INT _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
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

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
            | INT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
            | NEGATE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
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

and _menhir_goto_function_content : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (content : (Syntax.expression)) = _v in
    let ((_menhir_stack, _menhir_s, (id : (string))), _, (params : (Syntax.parameter list))) = _menhir_stack in
    let _4 = () in
    let _2 = () in
    let _v : (Syntax.fundef) =                                                                                                      ( (id, params, content) ) in
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
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
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

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | NEGATE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_run57 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                | SEMI_COLLON ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s), (i : (int))) = _menhir_stack in
                    let _5 = () in
                    let _4 = () in
                    let _2 = () in
                    let _1 = () in
                    let _v : (Syntax.expression) =                                                                         ( Readint ) in
                    _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
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

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | INT _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | NEGATE ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run67 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
            | INT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
            | NEGATE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
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

and _menhir_run72 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | INT _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
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
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run77 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSIGN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
        | INT _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
        | NEGATE ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98)
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Syntax.parameter))) = _menhir_stack in
            let _v : (Syntax.parameter list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_param_ _menhir_env _menhir_stack _menhir_s _v
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

and _menhir_goto_param_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.parameter list) -> 'ttv_return =
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
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | IF ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | LET ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | PRINT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | READ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | RETURN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState10 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (Syntax.expression) =                         ( Nothing ) in
            _menhir_goto_function_content _menhir_env _menhir_stack _menhir_s _v
        | TYPE ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState10
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
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState98 ->
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
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
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
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
            _menhir_goto_param_list _menhir_env _menhir_stack _menhir_s _v
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

and _menhir_goto_top : _menhir_env -> 'ttv_tail -> (Syntax.program) -> 'ttv_return =
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

and top : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Syntax.program) =
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
        _menhir_goto_top _menhir_env _menhir_stack _v
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
  

