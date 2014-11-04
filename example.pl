read_all(S, Cs) :-
    get_char(S, C),
    keep_reading(S, C, Cs).

keep_reading(_, end_of_file, []).

keep_reading(S, C, [C|Out]) :-
    read_all(S, Out).

histogram([], Fs, Fs).

histogram([C|Cs], Fs, Out) :-
    is_in(C, Fs),
    incr(C, Fs, R),
    histogram(Cs, R, Out).
    
histogram([C|Cs], Fs, Out) :-
    not(is_in(C, Fs)),
    histogram(Cs, [letter(C,1)|Fs], Out).

histogram(Cs,Fs) :-
    histogram(Cs,[],Fs).

is_in(C, [letter(C,_)|_]).

is_in(C, [_|Fs]) :-
    is_in(C, Fs).

incr(_, [], []).

incr(C, [letter(C,N)|Fs], [letter(C,M)|Fs]) :-
    M is N + 1.

incr(C, [F|Fs], [F|Out]) :-
    incr(C, Fs, Out).

min([letter(C,F)], F, letter(C,F), []). 

min([node(L,F,R)], F, node(L,F,R), []).

min([letter(C,F)|Fs], F, letter(C,F), [E|R]) :-
    min(Fs, Min, E, R),
    F < Min.

min([node(LC,F,RC)|Fs], F, node(LC,F,RC), [E|R]) :-
    min(Fs, Min, E, R),
    F < Min.

min([letter(C,F)|Fs], Min, E, [letter(C,F)|R]) :-
    min(Fs, Min, E, R),
    F >= Min.

min([node(LC,F,RC)|Fs], Min, E, [node(LC,F,RC)|R]) :-
    min(Fs, Min, E, R),
    F >= Min.

merge([node(LC,F,RC)], node(LC,F,RC)).

merge(Ns, R) :-
    min(Ns, LF, LC, Ms),
    min(Ms, RF, RC, Os),
    F is LF + RF,
    merge([node(LC,F,RC)|Os], R).

build_tree(Cs, T) :-
    histogram(Cs, Fs),
    merge(Fs, T).

build_dictionnary(T, D) :-
    build_dictionnary(T, [], D).
 
build_dictionnary(letter(C,F), Acc, [code(letter(C,RAcc))]) :-
    reverse(Acc, RAcc).

build_dictionnary(node(L,_,R), Acc, D) :-
    build_dictionnary(L, [bit(0)|Acc], E),
    build_dictionnary(R, [bit(1)|Acc], F),
    append(E, F, D).

get_sequence(C, [code(letter(C,Seq))|_], Seq).

get_sequence(C, [code(letter(D, _))|Cs], Seq) :-
    C \= D,
    get_sequence(C, Cs, Seq).

zipper(z([],[])).

zipper_move(z([Xs],[Y|Ys]),z([Y|Xs],Ys)).

zipper_insert(X,z(Xs,Ys),z([X|Xs],Ys)).

zipper_list(z(Xs,Ys), Zs) :-
    reverse(Xs, As),
    append(As, Ys, Zs).

write_sequence([], Bs, Bs).

write_sequence([X|Xs], Bs, Cs) :-
    zipper_insert(X, Bs, R),
    write_sequence(Xs, R, Cs).

process([], D, Z, Z).

process([C|Cs], D, Z, R) :-
    get_sequence(C, D, S),
    write_sequence(S, Z, ZZ),
    process(Cs, D, ZZ, R).

start(F, Cs, D, R) :-
    open(F, read, In, [eof_action(eof_code)]),
    read_all(In, Cs),
    build_tree(Cs, T),
    build_dictionnary(T, D),
    process(Cs, D, z([],[]), R),
    close(In).
