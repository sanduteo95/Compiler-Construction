
module Basics = struct
  
  exception Error
  
  type token = 
    | WHILE
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
    | NEW
    | NAME of (string)
    | MINUS
    | LET
    | LEQ
    | LEFT_ROUND_BRACKET
    | LEFT_CURLY_BRACKET
    | INT of (int)
    | IF
    | GEQ
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
  | MenhirState76
  | MenhirState74
  | MenhirState73
  | MenhirState71
  | MenhirState69
  | MenhirState68
  | MenhirState67
  | MenhirState66
  | MenhirState65
  | MenhirState64
  | MenhirState63
  | MenhirState62
  | MenhirState61
  | MenhirState59
  | MenhirState58
  | MenhirState55
  | MenhirState54
  | MenhirState52
  | MenhirState51
  | MenhirState50
  | MenhirState49
  | MenhirState48
  | MenhirState47
  | MenhirState46
  | MenhirState45
  | MenhirState44
  | MenhirState43
  | MenhirState42
  | MenhirState41
  | MenhirState40
  | MenhirState39
  | MenhirState38
  | MenhirState37
  | MenhirState36
  | MenhirState35
  | MenhirState34
  | MenhirState32
  | MenhirState31
  | MenhirState30
  | MenhirState29
  | MenhirState28
  | MenhirState27
  | MenhirState26
  | MenhirState25
  | MenhirState24
  | MenhirState23
  | MenhirState22
  | MenhirState19
  | MenhirState16
  | MenhirState13
  | MenhirState12
  | MenhirState6
  | MenhirState4
  
      open Syntax

let rec _menhir_run24 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | LET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | PRINT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | READ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | WHILE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24

and _menhir_run26 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | LET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | PRINT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | READ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | WHILE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run29 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | LET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | PRINT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | READ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | WHILE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run34 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | LET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | PRINT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | READ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | WHILE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_run36 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | LET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | PRINT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | READ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | WHILE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_run38 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | LET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | PRINT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | READ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | WHILE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run42 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | LET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | PRINT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | READ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | WHILE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | LET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | PRINT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | READ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | WHILE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run44 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | LET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | PRINT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | READ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | WHILE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | LET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | PRINT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | READ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | WHILE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run40 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | LET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | PRINT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | READ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | WHILE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run48 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | LET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | PRINT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | READ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | WHILE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run51 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | LET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | PRINT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | READ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | WHILE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | ASSIGN ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | DIVIDE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | LEFT_ROUND_BRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | NOTEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | OR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState23 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LEFT_CURLY_BRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IF ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState54
                | INT _v ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
                | LET ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState54
                | NEW ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState54
                | NOT ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState54
                | PRINT ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState54
                | READ ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState54
                | WHILE ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState54
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMI_COLLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | TIMES ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23)
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFT_ROUND_BRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | SEMI_COLLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | AND | ASSIGN | DIVIDE | EQ | GEQ | IF | INT _ | LEQ | LET | MINUS | NEW | NOT | NOTEQ | OR | PLUS | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | TIMES | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                             ( Operator(Times, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25)
    | MenhirState50 | MenhirState28 | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | ASSIGN ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | DIVIDE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | LEFT_ROUND_BRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | NOTEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | OR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState27 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | INT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | AND | ASSIGN | DIVIDE | EQ | GEQ | IF | LEFT_ROUND_BRACKET | LEQ | LET | MINUS | NEW | NOT | NOTEQ | OR | PLUS | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | SEMI_COLLON | TIMES | WHILE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))), _) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _v : (Syntax.expression) =                                                ( Seq(e, f) ) in
                _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
        | TIMES ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27)
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | LEFT_ROUND_BRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | SEMI_COLLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | TIMES ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | AND | ASSIGN | EQ | GEQ | IF | INT _ | LEQ | LET | MINUS | NEW | NOT | NOTEQ | OR | PLUS | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                             ( Operator(Plus, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | ASSIGN ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | DIVIDE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | LEFT_ROUND_BRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | NOTEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | OR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState32 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Syntax.expression) =                                                               ( Application(e, f)) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | SEMI_COLLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | TIMES ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | LEFT_ROUND_BRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | NOTEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | SEMI_COLLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | TIMES ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | AND | ASSIGN | IF | INT _ | LET | NEW | NOT | OR | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                          ( Operator(Or, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | LEFT_ROUND_BRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | SEMI_COLLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | TIMES ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | AND | ASSIGN | EQ | IF | INT _ | LET | NEW | NOT | NOTEQ | OR | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                             ( Operator(Noteq, e, f)) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | LEFT_ROUND_BRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | SEMI_COLLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | TIMES ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | AND | ASSIGN | EQ | GEQ | IF | INT _ | LEQ | LET | MINUS | NEW | NOT | NOTEQ | OR | PLUS | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                             ( Operator(Minus, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39)
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFT_ROUND_BRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | SEMI_COLLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | AND | ASSIGN | DIVIDE | EQ | GEQ | IF | INT _ | LEQ | LET | MINUS | NEW | NOT | NOTEQ | OR | PLUS | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | TIMES | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                              ( Operator(Divide, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | LEFT_ROUND_BRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | SEMI_COLLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | TIMES ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | AND | ASSIGN | EQ | GEQ | IF | INT _ | LEQ | LET | NEW | NOT | NOTEQ | OR | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                           ( Operator(Leq, e, f)) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | LEFT_ROUND_BRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | SEMI_COLLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | TIMES ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | AND | ASSIGN | EQ | GEQ | IF | INT _ | LEQ | LET | NEW | NOT | NOTEQ | OR | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                           ( Operator(Geq, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | LEFT_ROUND_BRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | SEMI_COLLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | TIMES ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | AND | ASSIGN | EQ | IF | INT _ | LET | NEW | NOT | NOTEQ | OR | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                          ( Operator(Eq, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47)
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | ASSIGN ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | DIVIDE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | LEFT_ROUND_BRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | NOTEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | OR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState49 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | INT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | AND | ASSIGN | DIVIDE | EQ | GEQ | IF | LEFT_ROUND_BRACKET | LEQ | LET | MINUS | NEW | NOT | NOTEQ | OR | PLUS | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | SEMI_COLLON | TIMES | WHILE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))), _) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _v : (Syntax.expression) =                                           ( Asg(e, f)) in
                _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
        | TIMES ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | LEFT_ROUND_BRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | NOTEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | SEMI_COLLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | TIMES ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | AND | ASSIGN | IF | INT _ | LET | NEW | NOT | OR | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                           (Operator(And, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52)
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | ASSIGN ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | DIVIDE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | LEFT_ROUND_BRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | NOTEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | OR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState55 in
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
                    | IF ->
                        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                    | INT _v ->
                        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
                    | LET ->
                        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                    | NEW ->
                        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                    | NOT ->
                        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                    | PRINT ->
                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                    | READ ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                    | WHILE ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
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
        | SEMI_COLLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | TIMES ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | ASSIGN ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | DIVIDE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | LEFT_ROUND_BRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | NOTEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | OR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState59 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))), _), _, (g : (Syntax.expression))) = _menhir_stack in
            let _11 = () in
            let _9 = () in
            let _8 = () in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                                                                                                                                    ( If(e, f, g) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | SEMI_COLLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | TIMES ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | ASSIGN ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | DIVIDE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | LEFT_ROUND_BRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | NOTEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | OR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState61 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IF ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | INT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
            | LET ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | NEW ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | NOT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | PRINT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | READ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | WHILE ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
        | TIMES ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFT_ROUND_BRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | AND | ASSIGN | DIVIDE | EQ | GEQ | IF | INT _ | LEQ | LET | MINUS | NEW | NOT | NOTEQ | OR | PLUS | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | SEMI_COLLON | TIMES | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (v : (string))), _, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                          ( Let(v, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | ASSIGN ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | DIVIDE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | LEFT_ROUND_BRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | NOTEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | OR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState64 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IF ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | INT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
            | LET ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | NEW ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | NOT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | PRINT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | READ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | WHILE ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
        | TIMES ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFT_ROUND_BRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | AND | ASSIGN | DIVIDE | EQ | GEQ | IF | INT _ | LEQ | LET | MINUS | NEW | NOT | NOTEQ | OR | PLUS | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | SEMI_COLLON | TIMES | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (v : (string))), _, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                          ( New(v, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | ASSIGN ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | DIVIDE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | IF ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | INT _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | LEFT_ROUND_BRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | LET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | NEW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | NOT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | NOTEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | OR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | PRINT ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | READ ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | SEMI_COLLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | TIMES ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | WHILE ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFT_ROUND_BRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | SEMI_COLLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | AND | ASSIGN | DIVIDE | EQ | GEQ | IF | INT _ | LEQ | LET | MINUS | NEW | NOT | NOTEQ | OR | PLUS | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | TIMES | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e : (Syntax.expression))), _, (f : (Syntax.expression))) = _menhir_stack in
            let _1 = () in
            let _v : (Syntax.expression) =                           (Operator(Not, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | ASSIGN ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | DIVIDE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | LEFT_ROUND_BRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | NOTEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | OR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState69 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Syntax.expression))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                             ( Printint(e) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | SEMI_COLLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | TIMES ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | ASSIGN ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | DIVIDE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | LEFT_ROUND_BRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | NOTEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | OR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState71 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LEFT_CURLY_BRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IF ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                | INT _v ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
                | LET ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                | NEW ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                | NOT ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                | PRINT ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                | READ ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                | WHILE ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMI_COLLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | TIMES ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | ASSIGN ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | DIVIDE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | LEFT_ROUND_BRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | NOTEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | OR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState74 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                                                                               ( While(e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | SEMI_COLLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | TIMES ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | ASSIGN ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | DIVIDE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LEFT_ROUND_BRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | NOTEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | OR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | PLUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState76 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, (name : (string))), _, (f : (Syntax.expression))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _3 = () in
            let _2 = () in
            let _v : (Syntax.fundef) =                                                                                                           ( (name, [], f) ) in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EOF ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, (e : (Syntax.fundef))) = _menhir_stack in
                let _2 = () in
                let _v : (Syntax.fundef) =                   ( e ) in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_1 : (Syntax.fundef)) = _v in
                Obj.magic _1
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | SEMI_COLLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | TIMES ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
    | _ ->
        ();
        Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
        assert false

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
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
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | IF ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | INT _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
        | LET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | NEW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | NOT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | PRINT ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | READ ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | WHILE ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
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
        | IF ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | INT _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | LET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | NEW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | NOT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | PRINT ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | READ ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | WHILE ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState12
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

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | LET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | NEW ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | NOT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | PRINT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | READ ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | WHILE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState13
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
    | NAME _v ->
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
            | IF ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | INT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
            | LET ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | NEW ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | NOT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | PRINT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | READ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | WHILE ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
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

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | NAME _v ->
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
            | IF ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | INT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
            | LET ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | NEW ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | NOT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | PRINT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | READ ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | WHILE ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19)
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

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (int)) = _v in
    let _v : (Syntax.expression) =             ( Const(i) ) in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | IF ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | INT _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | LET ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | NEW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | NOT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | PRINT ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | READ ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | WHILE ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

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
    | NAME _v ->
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
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LEFT_CURLY_BRACKET ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | IF ->
                        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState4
                    | INT _v ->
                        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
                    | LET ->
                        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState4
                    | NEW ->
                        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState4
                    | NOT ->
                        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState4
                    | PRINT ->
                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState4
                    | READ ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState4
                    | WHILE ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4)
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
  

