
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
  | MenhirState88
  | MenhirState85
  | MenhirState83
  | MenhirState79
  | MenhirState76
  | MenhirState75
  | MenhirState73
  | MenhirState71
  | MenhirState70
  | MenhirState69
  | MenhirState68
  | MenhirState67
  | MenhirState66
  | MenhirState64
  | MenhirState63
  | MenhirState62
  | MenhirState60
  | MenhirState59
  | MenhirState56
  | MenhirState55
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
  | MenhirState33
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

and _menhir_run49 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | ID _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | READ ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | TYPE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | WHILE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

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

and _menhir_run30 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | ID _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | READ ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | TYPE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | WHILE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run35 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | ID _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | READ ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | TYPE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | WHILE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run37 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | ID _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | READ ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | TYPE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | WHILE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run39 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | ID _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | READ ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | TYPE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | WHILE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run43 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | ID _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | READ ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | TYPE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | WHILE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

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

and _menhir_run45 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | ID _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | READ ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | TYPE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | WHILE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_run47 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | ID _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | READ ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | TYPE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | WHILE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | ID _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | PRINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | READ ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | TYPE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | WHILE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

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
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                | ID _v ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
                | IF ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                | INT _v ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
                | NOT ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                | PRINT ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                | READ ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                | RIGHT_CURLY_BRACKET ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState83 in
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
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                | WHILE ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
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
    | MenhirState88 ->
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
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | EQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | GEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | LEFT_ROUND_BRACKET ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | LEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | NOTEQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | OR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | SEMI_COLLON ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | TIMES ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | ASSIGN | DEREF | ID _ | IF | INT _ | LEFT_CURLY_BRACKET | NOT | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | TYPE | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Syntax.expression))) = _menhir_stack in
            let _1 = () in
            let _v : (Syntax.expression) =                    ( Deref(e) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24)
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFT_ROUND_BRACKET ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | SEMI_COLLON ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | AND | ASSIGN | DEREF | DIVIDE | EQ | GEQ | ID _ | IF | INT _ | LEFT_CURLY_BRACKET | LEQ | MINUS | NOT | NOTEQ | OR | PLUS | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | TIMES | TYPE | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                             ( Operator(Times, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26)
    | MenhirState71 | MenhirState29 | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | ASSIGN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | EQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | GEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | LEFT_ROUND_BRACKET ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | LEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | NOTEQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | OR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState28 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | INT _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29)
        | TIMES ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | LEFT_ROUND_BRACKET ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | SEMI_COLLON ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | TIMES ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | AND | ASSIGN | DEREF | EQ | GEQ | ID _ | IF | INT _ | LEFT_CURLY_BRACKET | LEQ | MINUS | NOT | NOTEQ | OR | PLUS | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | TYPE | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                             ( Operator(Plus, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | ASSIGN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | EQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | GEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | LEFT_ROUND_BRACKET ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | LEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | NOTEQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | OR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState33 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Syntax.expression) =                                                               ( Application(e, f)) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | SEMI_COLLON ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | TIMES ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | EQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | GEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | LEFT_ROUND_BRACKET ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | LEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | NOTEQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | SEMI_COLLON ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | TIMES ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | AND | ASSIGN | DEREF | ID _ | IF | INT _ | LEFT_CURLY_BRACKET | NOT | OR | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | TYPE | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                          ( Operator(Or, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | GEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | LEFT_ROUND_BRACKET ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | LEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | SEMI_COLLON ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | TIMES ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | AND | ASSIGN | DEREF | EQ | ID _ | IF | INT _ | LEFT_CURLY_BRACKET | NOT | NOTEQ | OR | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | TYPE | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                             ( Operator(Noteq, e, f)) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | LEFT_ROUND_BRACKET ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | SEMI_COLLON ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | TIMES ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | AND | ASSIGN | DEREF | EQ | GEQ | ID _ | IF | INT _ | LEFT_CURLY_BRACKET | LEQ | MINUS | NOT | NOTEQ | OR | PLUS | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | TYPE | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                             ( Operator(Minus, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40)
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFT_ROUND_BRACKET ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | SEMI_COLLON ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | AND | ASSIGN | DEREF | DIVIDE | EQ | GEQ | ID _ | IF | INT _ | LEFT_CURLY_BRACKET | LEQ | MINUS | NOT | NOTEQ | OR | PLUS | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | TIMES | TYPE | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                              ( Operator(Divide, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | LEFT_ROUND_BRACKET ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | SEMI_COLLON ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | TIMES ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | AND | ASSIGN | DEREF | EQ | GEQ | ID _ | IF | INT _ | LEFT_CURLY_BRACKET | LEQ | NOT | NOTEQ | OR | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | TYPE | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                           ( Operator(Leq, e, f)) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44)
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | LEFT_ROUND_BRACKET ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | SEMI_COLLON ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | TIMES ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | AND | ASSIGN | DEREF | EQ | GEQ | ID _ | IF | INT _ | LEFT_CURLY_BRACKET | LEQ | NOT | NOTEQ | OR | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | TYPE | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                           ( Operator(Geq, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46)
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | GEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | LEFT_ROUND_BRACKET ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | LEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | SEMI_COLLON ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | TIMES ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | AND | ASSIGN | DEREF | EQ | ID _ | IF | INT _ | LEFT_CURLY_BRACKET | NOT | NOTEQ | OR | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | TYPE | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                          ( Operator(Eq, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48)
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | ASSIGN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | EQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | GEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | LEFT_ROUND_BRACKET ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | LEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | NOTEQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | OR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | SEMI_COLLON ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | TIMES ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | DEREF | ID _ | IF | INT _ | LEFT_CURLY_BRACKET | NOT | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | TYPE | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Syntax.expression))), _), _, (f : (Syntax.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.expression) =                              ( Asg(e, f)) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | EQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | GEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | LEFT_ROUND_BRACKET ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | LEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | NOTEQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | SEMI_COLLON ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | TIMES ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState52
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
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | ASSIGN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | EQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | GEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | LEFT_ROUND_BRACKET ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | LEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | NOTEQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | OR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState53 in
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
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState55
                | ID _v ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
                | IF ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState55
                | INT _v ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
                | NOT ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState55
                | PRINT ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState55
                | READ ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState55
                | TYPE ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState55
                | WHILE ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState55
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMI_COLLON ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | TIMES ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | ASSIGN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | EQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | GEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | LEFT_ROUND_BRACKET ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | LEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | NOTEQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | OR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState56 in
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
                        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                    | ID _v ->
                        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
                    | IF ->
                        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                    | INT _v ->
                        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
                    | NOT ->
                        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                    | PRINT ->
                        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                    | READ ->
                        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                    | TYPE ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                    | WHILE ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
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
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | TIMES ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | ASSIGN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | EQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | GEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | LEFT_ROUND_BRACKET ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | LEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | NOTEQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | OR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState60 in
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
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | TIMES ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | ASSIGN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | DEREF ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | EQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | GEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | ID _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | IF ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | INT _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | LEFT_ROUND_BRACKET ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | LEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | NOTEQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | OR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | PRINT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | READ ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | SEMI_COLLON ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | TIMES ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | TYPE ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | WHILE ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFT_ROUND_BRACKET ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | SEMI_COLLON ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | AND | ASSIGN | DEREF | DIVIDE | EQ | GEQ | ID _ | IF | INT _ | LEFT_CURLY_BRACKET | LEQ | MINUS | NOT | NOTEQ | OR | PLUS | PRINT | READ | RIGHT_CURLY_BRACKET | RIGHT_ROUND_BRACKET | TIMES | TYPE | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e : (Syntax.expression))), _, (f : (Syntax.expression))) = _menhir_stack in
            let _1 = () in
            let _v : (Syntax.expression) =                           (Operator(Not, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | ASSIGN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | EQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | GEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | LEFT_ROUND_BRACKET ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | LEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | NOTEQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | OR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState64 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Syntax.expression))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Syntax.expression) =                                                             ( Printint(e) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | SEMI_COLLON ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | TIMES ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | ASSIGN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | EQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | GEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | LEFT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState66 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DEREF ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | ID _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
            | IF ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | INT _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
            | NOT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | PRINT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | READ ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | TYPE ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | WHILE ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
        | LEFT_ROUND_BRACKET ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | LEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | NOTEQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | OR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState66 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DEREF ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | ID _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | IF ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | INT _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | NOT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | PRINT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | READ ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | TYPE ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | WHILE ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
        | TIMES ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFT_ROUND_BRACKET ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState68
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | ASSIGN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | EQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | GEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | LEFT_ROUND_BRACKET ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | LEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | NOTEQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | OR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | SEMI_COLLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState70 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DEREF ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | ID _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
            | IF ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | INT _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
            | NOT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | PRINT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | READ ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | RIGHT_CURLY_BRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState71 in
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
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | WHILE ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
        | TIMES ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70)
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | ASSIGN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | EQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | GEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | LEFT_ROUND_BRACKET ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | LEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | NOTEQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | OR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | RIGHT_ROUND_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState73 in
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
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | ID _v ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
                | IF ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | INT _v ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
                | NOT ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | PRINT ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | READ ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | TYPE ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | WHILE ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMI_COLLON ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | TIMES ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | ASSIGN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | EQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | GEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LEFT_ROUND_BRACKET ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | NOTEQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | OR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState76 in
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
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | TIMES ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | ASSIGN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | EQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | GEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | LEFT_ROUND_BRACKET ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | LEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | NOTEQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | OR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState79 in
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
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | TIMES ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | ASSIGN ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | DIVIDE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | EQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | GEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | LEFT_ROUND_BRACKET ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | LEQ ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | NOTEQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | OR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | RIGHT_CURLY_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState85 in
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
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | TIMES ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85)
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
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
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
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
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
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
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
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
  

