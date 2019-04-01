% vim: ts=4 sw=4 et ft=mercury

:- module exist.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module string.
:- import_module std_util.

main(!IO) :-
    print_showables(['new mkshowable'(1001), 'new mkshowable'("abc")], !IO),
    print_same_showables([1001, 1002], !IO).

:- typeclass showable(T) where [
    func show(T) = string
].

:- instance showable(int) where [
    func(show/1) is string.int_to_string_thousands
].

:- instance showable(string) where [
    func(show/1) is id
].

:- type showable
    --->    some [Item] mkshowable(Item) => showable(Item).

:- pred print_showables(list(showable)::in, io::di, io::uo) is det.

print_showables([], !IO).
print_showables([mkshowable(I) | Xs], !IO) :-
    io.write_string(show(I), !IO),
    io.nl(!IO),
    print_showables(Xs, !IO).

:- pred print_same_showables(list(T)::in, io::di, io::uo) is det <= showable(T).

print_same_showables([], !IO).
print_same_showables([X | Xs], !IO) :-
    io.write_string(show(X), !IO),
    io.nl(!IO),
    print_same_showables(Xs, !IO).
