
module Basics = struct
  
  exception Error
  
  type token = 
    | WHILE
    | TYPE
    | TIMES
    | SEMI_COLLON
    | RIGHT_ROUND_BRACKET
    | RIGHT_CURLY_BRACKET
    | READ
    | PRINT
    | PLUS
    | OR
    | NOTEQ
    | NOT
    | MINUS
    | LET
    | LEQ
    | LEFT_ROUND_BRACKET
    | LEFT_CURLY_BRACKET
    | INT of (int)
    | IN
    | IF
    | ID of (string)
    | GEQ
    | EQ
    | EOF
    | ELSE
    | DIVIDE
    | DEREF
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
  | MenhirState91
  | MenhirState81
  | MenhirState77
  | MenhirState73
  | MenhirState70
  | MenhirState68
  | MenhirState65
  | MenhirState63
  | MenhirState61
  | MenhirState56
  | MenhirState50
  | MenhirState48
  | MenhirState45
  | MenhirState39
  | MenhirState37
  | MenhirState35
  | MenhirState33
  | MenhirState31
  | MenhirState29
  | MenhirState27
  | MenhirState25
  | MenhirState23
  | MenhirState20
  | MenhirState18
  | MenhirState16
  | MenhirState12
  | MenhirState11
  | MenhirState9
  | MenhirState2
  
open Syntax

let rec _menhir_run70 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | IF ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | INT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | LET ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | PRINT ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | READ ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | TYPE ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_run16 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | INT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_run23 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | INT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_run27 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | INT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_run29 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | INT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | INT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | INT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run33 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | INT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_run20 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | INT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run35 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | INT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run37 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | INT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run25 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | INT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_run39 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | INT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_stmt : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState68 ->
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
                        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
                    | IF ->
                        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                    | INT _v ->
                        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
                    | LEFT_ROUND_BRACKET ->
                        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                    | LET ->
                        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                    | PRINT ->
                        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                    | READ ->
                        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                    | TYPE ->
                        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                    | WHILE ->
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
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
        | SEMI_COLLON ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState81 | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI_COLLON ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (s1 : (Syntax.expression))), _, (s2 : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                                       ( Seq(s1, s2) ) in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
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
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e : (Syntax.expression))), _, (s1 : (Syntax.expression))), _, (s2 : (Syntax.expression))) = _menhir_stack in
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
        | SEMI_COLLON ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
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
        | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | IF ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | INT _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | LET ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | PRINT ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | READ ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | TYPE ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | WHILE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | RIGHT_CURLY_BRACKET | SEMI_COLLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), (id : (string))), _, (e : (Syntax.expression))), _, (s : (Syntax.expression))) = _menhir_stack in
                let _7 = () in
                let _5 = () in
                let _3 = () in
                let _1 = () in
                let _v : (Syntax.expression) =                                                              ( Let(id, e, s) ) in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
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
        | SEMI_COLLON ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | RIGHT_CURLY_BRACKET ->
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
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
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
        | SEMI_COLLON ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState9 ->
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
            let _v : (Syntax.expression) =                                   ( s ) in
            _menhir_goto_function_content _menhir_env _menhir_stack _menhir_s _v
        | SEMI_COLLON ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DEREF ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (e : (Syntax.expression))) = _menhir_stack in
                let _4 = () in
                let _3 = () in
                let _1 = () in
                let _v : (Syntax.expression) =                                                             ( Deref(e) ) in
                _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | TIMES ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFT_ROUND_BRACKET ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | DIVIDE | EQ | GEQ | IN | LEQ | MINUS | NOTEQ | OR | PLUS | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | SEMI_COLLON | TIMES ->
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
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFT_ROUND_BRACKET ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | DIVIDE | EQ | GEQ | IN | LEQ | MINUS | NOTEQ | OR | PLUS | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | SEMI_COLLON | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                             (Operator(Not, e1, e2) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | EQ | GEQ | IN | LEQ | MINUS | NOTEQ | OR | PLUS | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | SEMI_COLLON ->
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
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFT_ROUND_BRACKET ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | DIVIDE | EQ | GEQ | IN | LEQ | MINUS | NOTEQ | OR | PLUS | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | SEMI_COLLON | TIMES ->
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
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | IN | OR | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | SEMI_COLLON ->
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
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | EQ | IN | NOTEQ | OR | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | SEMI_COLLON ->
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
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | EQ | GEQ | IN | LEQ | MINUS | NOTEQ | OR | PLUS | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | SEMI_COLLON ->
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
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | EQ | GEQ | IN | LEQ | NOTEQ | OR | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | SEMI_COLLON ->
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
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | EQ | GEQ | IN | LEQ | NOTEQ | OR | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | SEMI_COLLON ->
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
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | EQ | IN | NOTEQ | OR | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | SEMI_COLLON ->
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
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | IN | OR | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                             (Operator(And, e1, e2) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
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
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
                | IF ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | INT _v ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
                | LEFT_ROUND_BRACKET ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | LET ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | PRINT ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | READ ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | TYPE ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | WHILE ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | TIMES ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
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
        | AND ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | IF ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | INT _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | LET ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | PRINT ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | READ ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | TYPE ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | WHILE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
        | TIMES ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Syntax.expression))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                             ( Printint(e) ) in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | IF ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | INT _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | LET ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | PRINT ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | READ ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | TYPE ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | WHILE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
        | LEFT_ROUND_BRACKET ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
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
        | AND ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
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
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
                | IF ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState68
                | INT _v ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
                | LEFT_ROUND_BRACKET ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState68
                | LET ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState68
                | PRINT ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState68
                | READ ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState68
                | TYPE ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState68
                | WHILE ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState68
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | TIMES ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState9 | MenhirState45 | MenhirState50 | MenhirState63 | MenhirState81 | MenhirState68 | MenhirState77 | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | INT _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
        | DIVIDE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | RIGHT_CURLY_BRACKET | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Syntax.expression))) = _menhir_stack in
            let _v : (Syntax.expression) =             ( e ) in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LEFT_ROUND_BRACKET ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQ ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | RIGHT_CURLY_BRACKET | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.expression))), _, (e2 : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                                ( Asg(e1, e2)) in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_param_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.parameter list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState2 ->
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
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Syntax.parameter))), _, (xs : (Syntax.parameter list))) = _menhir_stack in
        let _2 = () in
        let _v : (Syntax.parameter list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_param_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | INT _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | LEFT_ROUND_BRACKET ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
            | INT _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48)
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
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EOF ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, (id : (string))), _, (params : (Syntax.parameter list))), _, (content : (Syntax.expression))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _v : (Syntax.fundef) =                                                                                                          ( (id, params, content) ) in
        _menhir_goto_top _menhir_env _menhir_stack _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), (i : (int))) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Syntax.expression) =                                                            ( Readint ) in
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
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | INT _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | LEFT_ROUND_BRACKET ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
            | INT _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
            | LEFT_ROUND_BRACKET ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
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

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | INT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | LEFT_ROUND_BRACKET ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (int)) = _v in
    let _v : (Syntax.expression) =             ( Const(i) ) in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | INT _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | LEFT_ROUND_BRACKET ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (id : (string)) = _v in
    let _v : (Syntax.expression) =             ( Identifier(id) ) in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
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
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | IF ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | INT _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | LEFT_ROUND_BRACKET ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | LET ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | PRINT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | READ ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState9 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (Syntax.expression) =                         ( Nothing ) in
            _menhir_goto_function_content _menhir_env _menhir_stack _menhir_s _v
        | TYPE ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | WHILE ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_top : _menhir_env -> 'ttv_tail -> (Syntax.fundef) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Syntax.fundef)) = _v in
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

and top : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Syntax.fundef) =
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
        let _v : (Syntax.fundef) =        ( ("", [], Nothing) ) in
        _menhir_goto_top _menhir_env _menhir_stack _v
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RIGHT_ROUND_BRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState2 in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _1 = () in
                let _v : (Syntax.parameter list) =                        ( [] ) in
                _menhir_goto_param_list _menhir_env _menhir_stack _menhir_s _v
            | TYPE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR)
  

