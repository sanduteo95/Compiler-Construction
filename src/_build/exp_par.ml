
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
    | LEQ
    | LEFT_ROUND_BRACKET
    | LEFT_CURLY_BRACKET
    | INT of (int)
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
  | MenhirState89
  | MenhirState86
  | MenhirState84
  | MenhirState80
  | MenhirState77
  | MenhirState76
  | MenhirState74
  | MenhirState72
  | MenhirState71
  | MenhirState70
  | MenhirState69
  | MenhirState68
  | MenhirState67
  | MenhirState65
  | MenhirState64
  | MenhirState63
  | MenhirState61
  | MenhirState60
  | MenhirState57
  | MenhirState56
  | MenhirState54
  | MenhirState53
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
  | MenhirState33
  | MenhirState32
  | MenhirState30
  | MenhirState29
  | MenhirState28
  | MenhirState27
  | MenhirState26
  | MenhirState25
  | MenhirState24
  | MenhirState23
  | MenhirState21
  | MenhirState18
  | MenhirState17
  | MenhirState11
  | MenhirState8
  | MenhirState6
  | MenhirState2
  
open Syntax

let rec _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_run27 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | ID _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | READ ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | TYPE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | WHILE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_run32 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | ID _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | READ ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | TYPE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | WHILE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run36 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | ID _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | READ ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | TYPE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | WHILE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState36
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
    | DEREF ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | ID _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | READ ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | TYPE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | WHILE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run40 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | ID _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | READ ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | TYPE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | WHILE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run42 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | ID _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | READ ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | TYPE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | WHILE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run44 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | ID _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | READ ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | TYPE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | WHILE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState44
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
    | DEREF ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | ID _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | READ ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | TYPE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | WHILE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run34 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | ID _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | READ ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | TYPE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | WHILE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_run48 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | ID _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | READ ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | TYPE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | WHILE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState48
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
    | DEREF ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | ID _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | READ ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | TYPE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | WHILE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_run25 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | ID _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | READ ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | TYPE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | WHILE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_run29 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | ID _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | READ ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | TYPE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | WHILE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

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
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LEFT_CURLY_BRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | DEREF ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState84
                | ID _v ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
                | IF ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState84
                | INT _v ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
                | NOT ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState84
                | PRINT ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState84
                | READ ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState84
                | RIGHT_CURLY_BRACKET ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState84 in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, (id : (string))), _, (p : (Syntax.parameter list))) = _menhir_stack in
                    let _6 = () in
                    let _5 = () in
                    let _4 = () in
                    let _2 = () in
                    let _v : (Syntax.fundef) =                                                                                                                                         ( (id, p, Nothing) ) in
                    _menhir_goto_func _menhir_env _menhir_stack _v
                | TYPE ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState84
                | WHILE ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState84
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
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
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Syntax.parameter))), _, (xs : (Syntax.parameter list))) = _menhir_stack in
        let _2 = () in
        let _v : (Syntax.parameter list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_param_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFT_ROUND_BRACKET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | SEMI_COLLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | AND | ASSIGN | DEREF | DIVIDE | EQ | GEQ | ID _ | IF | INT _ | LEFT_CURLY_BRACKET | LEQ | MINUS | NOT | NOTEQ | OR | PLUS | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | TIMES | TYPE | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Syntax.expression))) = _menhir_stack in
            let _1 = () in
            let _v : (Syntax.expression) =                    ( Deref(e) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24)
    | MenhirState72 | MenhirState53 | MenhirState50 | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | ASSIGN ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | LEFT_ROUND_BRACKET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | NOTEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState26 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
            | INT _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
            | AND | ASSIGN | DEREF | DIVIDE | EQ | GEQ | IF | LEFT_CURLY_BRACKET | LEFT_ROUND_BRACKET | LEQ | MINUS | NOT | NOTEQ | OR | PLUS | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | SEMI_COLLON | TIMES | TYPE | WHILE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))), _) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _v : (Syntax.expression) =                                                ( Seq(e, f) ) in
                _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFT_ROUND_BRACKET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | SEMI_COLLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | AND | ASSIGN | DEREF | DIVIDE | EQ | GEQ | ID _ | IF | INT _ | LEFT_CURLY_BRACKET | LEQ | MINUS | NOT | NOTEQ | OR | PLUS | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | TIMES | TYPE | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                             ( Operator(Times, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | ASSIGN ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | LEFT_ROUND_BRACKET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | NOTEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState30 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Syntax.expression) =                                                               ( Application(e, f)) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | SEMI_COLLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30)
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | LEFT_ROUND_BRACKET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | SEMI_COLLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | AND | ASSIGN | DEREF | EQ | GEQ | ID _ | IF | INT _ | LEFT_CURLY_BRACKET | LEQ | MINUS | NOT | NOTEQ | OR | PLUS | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | TYPE | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                             ( Operator(Plus, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFT_ROUND_BRACKET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | SEMI_COLLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | AND | ASSIGN | DEREF | DIVIDE | EQ | GEQ | ID _ | IF | INT _ | LEFT_CURLY_BRACKET | LEQ | MINUS | NOT | NOTEQ | OR | PLUS | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | TIMES | TYPE | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                              ( Operator(Divide, e, f) ) in
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
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | LEFT_ROUND_BRACKET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | NOTEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | SEMI_COLLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | AND | ASSIGN | DEREF | ID _ | IF | INT _ | LEFT_CURLY_BRACKET | NOT | OR | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | TYPE | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                          ( Operator(Or, e, f) ) in
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
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | LEFT_ROUND_BRACKET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | SEMI_COLLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | AND | ASSIGN | DEREF | EQ | ID _ | IF | INT _ | LEFT_CURLY_BRACKET | NOT | NOTEQ | OR | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | TYPE | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                             ( Operator(Noteq, e, f)) in
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
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | LEFT_ROUND_BRACKET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | SEMI_COLLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | AND | ASSIGN | DEREF | EQ | GEQ | ID _ | IF | INT _ | LEFT_CURLY_BRACKET | LEQ | MINUS | NOT | NOTEQ | OR | PLUS | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | TYPE | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                             ( Operator(Minus, e, f) ) in
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
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | LEFT_ROUND_BRACKET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | SEMI_COLLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | AND | ASSIGN | DEREF | EQ | GEQ | ID _ | IF | INT _ | LEFT_CURLY_BRACKET | LEQ | NOT | NOTEQ | OR | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | TYPE | WHILE ->
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
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | LEFT_ROUND_BRACKET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | SEMI_COLLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | AND | ASSIGN | DEREF | EQ | GEQ | ID _ | IF | INT _ | LEFT_CURLY_BRACKET | LEQ | NOT | NOTEQ | OR | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | TYPE | WHILE ->
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
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | LEFT_ROUND_BRACKET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | SEMI_COLLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | AND | ASSIGN | DEREF | EQ | ID _ | IF | INT _ | LEFT_CURLY_BRACKET | NOT | NOTEQ | OR | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | TYPE | WHILE ->
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
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | LEFT_ROUND_BRACKET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | NOTEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState49 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | INT _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | AND | ASSIGN | DEREF | DIVIDE | EQ | GEQ | IF | LEFT_CURLY_BRACKET | LEFT_ROUND_BRACKET | LEQ | MINUS | NOT | NOTEQ | OR | PLUS | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | SEMI_COLLON | TIMES | TYPE | WHILE ->
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
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState49
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
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | LEFT_ROUND_BRACKET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | NOTEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | SEMI_COLLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | AND | ASSIGN | DEREF | ID _ | IF | INT _ | LEFT_CURLY_BRACKET | NOT | OR | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | TYPE | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                           (Operator(And, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52)
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | ASSIGN ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | LEFT_ROUND_BRACKET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | NOTEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState54 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LEFT_CURLY_BRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | DEREF ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                | ID _v ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
                | IF ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                | INT _v ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
                | NOT ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                | PRINT ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                | READ ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                | TYPE ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                | WHILE ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMI_COLLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | ASSIGN ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | LEFT_ROUND_BRACKET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | NOTEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState57 in
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
                    | DEREF ->
                        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState60
                    | ID _v ->
                        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
                    | IF ->
                        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState60
                    | INT _v ->
                        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
                    | NOT ->
                        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState60
                    | PRINT ->
                        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState60
                    | READ ->
                        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState60
                    | TYPE ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState60
                    | WHILE ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState60
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
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
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | ASSIGN ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | LEFT_ROUND_BRACKET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | NOTEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState61 in
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
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | ASSIGN ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | DEREF ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | ID _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | IF ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | INT _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | LEFT_ROUND_BRACKET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | NOTEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | PRINT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | READ ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | SEMI_COLLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | TYPE ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | WHILE ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFT_ROUND_BRACKET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | SEMI_COLLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | AND | ASSIGN | DEREF | DIVIDE | EQ | GEQ | ID _ | IF | INT _ | LEFT_CURLY_BRACKET | LEQ | MINUS | NOT | NOTEQ | OR | PLUS | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | TIMES | TYPE | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e : (Syntax.expression))), _, (f : (Syntax.expression))) = _menhir_stack in
            let _1 = () in
            let _v : (Syntax.expression) =                           (Operator(Not, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | ASSIGN ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | LEFT_ROUND_BRACKET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | NOTEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState65 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Syntax.expression))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                             ( Printint(e) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | SEMI_COLLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | ASSIGN ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | LEFT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState67 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DEREF ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | ID _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
            | IF ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | INT _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
            | NOT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | PRINT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | READ ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | TYPE ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | WHILE ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70)
        | LEFT_ROUND_BRACKET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | NOTEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState67 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DEREF ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | ID _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | IF ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | INT _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | NOT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | PRINT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | READ ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | TYPE ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | WHILE ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFT_ROUND_BRACKET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | AND | ASSIGN | DEREF | DIVIDE | EQ | GEQ | ID _ | IF | INT _ | LEFT_CURLY_BRACKET | LEQ | MINUS | NOT | NOTEQ | OR | PLUS | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | SEMI_COLLON | TIMES | TYPE | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (id : (string))), _, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                          ( New(id, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | ASSIGN ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | LEFT_ROUND_BRACKET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | NOTEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState71 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DEREF ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | ID _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | IF ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | INT _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | NOT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | PRINT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | READ ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | RIGHT_CURLY_BRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState72 in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((((_menhir_stack, _menhir_s), (id : (string))), _, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))), _) = _menhir_stack in
                let _8 = () in
                let _7 = () in
                let _5 = () in
                let _3 = () in
                let _1 = () in
                let _v : (Syntax.expression) =                                                                                                   ( Let(id, e, f) ) in
                _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
            | TYPE ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | WHILE ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | ASSIGN ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | LEFT_ROUND_BRACKET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | NOTEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState74 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LEFT_CURLY_BRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | DEREF ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                | ID _v ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
                | IF ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                | INT _v ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
                | NOT ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                | PRINT ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                | READ ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                | TYPE ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                | WHILE ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMI_COLLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | ASSIGN ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | LEFT_ROUND_BRACKET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | NOTEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState77 in
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
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | ASSIGN ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | LEFT_ROUND_BRACKET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | NOTEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState80 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, (id : (string))), _), _, (e : (Syntax.expression))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _3 = () in
            let _2 = () in
            let _v : (Syntax.fundef) =                                                                                                       ( (id, [None], e) ) in
            _menhir_goto_func _menhir_env _menhir_stack _v
        | SEMI_COLLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | ASSIGN ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | DIVIDE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | EQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | GEQ ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | LEFT_ROUND_BRACKET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | LEQ ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | MINUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | NOTEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | OR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | PLUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState86 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, (id : (string))), _, (p : (Syntax.parameter list))), _, (e : (Syntax.expression))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _v : (Syntax.fundef) =                                                                                                                                                  ( (id, p, e) ) in
            _menhir_goto_func _menhir_env _menhir_stack _v
        | SEMI_COLLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86)
    | _ ->
        _menhir_fail ()

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
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
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

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
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
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
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
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
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
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

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
        | DEREF ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | ID _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | IF ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | INT _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | PRINT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | READ ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | TYPE ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | WHILE ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            | DEREF ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | ID _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
            | IF ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | INT _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
            | NOT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | PRINT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | READ ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | TYPE ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | WHILE ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11)
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

and _menhir_goto_func : _menhir_env -> 'ttv_tail -> (Syntax.fundef) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EOF ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, (f : (Syntax.fundef))) = _menhir_stack in
        let _2 = () in
        let _v : (Syntax.fundef) =                   ( f ) in
        _menhir_goto_top _menhir_env _menhir_stack _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | DEREF ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | ID _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
        | IF ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | INT _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | PRINT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | READ ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | TYPE ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | WHILE ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | ID _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | READ ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | TYPE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | WHILE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (int)) = _v in
    let _v : (Syntax.expression) =             ( Const(i) ) in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | DEREF ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | ID _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
        | IF ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | INT _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | PRINT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | READ ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | TYPE ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | WHILE ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (id : (string)) = _v in
    let _v : (Syntax.expression) =             ( Identifier(id) ) in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | ID _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | READ ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | TYPE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | WHILE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

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
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LEFT_CURLY_BRACKET ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | DEREF ->
                        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState6
                    | ID _v ->
                        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
                    | IF ->
                        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState6
                    | INT _v ->
                        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
                    | NOT ->
                        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState6
                    | PRINT ->
                        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState6
                    | READ ->
                        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState6
                    | RIGHT_CURLY_BRACKET ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_s = MenhirState6 in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let ((_menhir_stack, (id : (string))), _) = _menhir_stack in
                        let _5 = () in
                        let _4 = () in
                        let _3 = () in
                        let _2 = () in
                        let _v : (Syntax.fundef) =                                                                                              ( (id, [None], Nothing) ) in
                        _menhir_goto_func _menhir_env _menhir_stack _v
                    | TYPE ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState6
                    | WHILE ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
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
  

