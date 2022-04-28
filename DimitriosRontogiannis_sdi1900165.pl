% Otan theloume na veltistopoihsoume tin thesi tou K tote tha tou dwsoume tin kalyteri thesi (N pontous)
% kai tha dwsoume se auton pou exei tous ligoterous pontous tin deuteri kalyteri thesi (N - 1 pontous) klp
% Epomenws ean i akolouthia 'a' einai sortarismeni se auksousa, i nea akolouthia 'b' tha einai b(i) = N - i + 1 + a(i).
% Sto telos mporoume apla na sugkrinoume to max(b) me to a(k) + N gia na doume ean mporei na nikisei

champions(L, N):-
    length(L, Len),
    msort(L, A),
    add_val(A, R, Len),
    maximum(R, Mx),
    solve_champ(L, N, Len, Mx).

add_val([], [], 0).
add_val([X|Xs], [Y|Ys], K):-
    Y is X + K,
    K1 is K - 1,
    add_val(Xs, Ys, K1).

max_pair(A, B, B):-
    A =< B.
max_pair(A, B, A):-
    A > B.

maximum([X], X).
maximum([X|L], M):-
    maximum(L, P),
    max_pair(X, P, M).

solve_champ([], 0, _, _).
solve_champ([X|L], N, Len, Mx):-
    X + Len < Mx,
    solve_champ(L, N, Len, Mx).
solve_champ([X|L], N, Len, Mx):-
    X + Len >= Mx,
    solve_champ(L, N1, Len, Mx),
    N is N1 + 1.

%------------------------------------------------------------

% Kratame mia stack me ta active grammata (pou den exoun kanei match).
% Otan mas erthei to gramma 'a' tote ean to top tis stack einai 'a' ta kanoume match kai ta diagrafoume.
% Alliws to kanoume push sti stack. Omoiws gia to 'b'

beautiful(L, N):-
    solve_blists(L, N).

solve_blists([], 0).
solve_blists([L|Ls], N):-
    solve_b(L, [], T),
    solve_blists(Ls, N1),
    N is N1 + T.

solve_b([], [], 1).
solve_b([], [X|_], 0).
solve_b([X|Xs], [], T):-
    solve_b(Xs, [X], T).
solve_b([X|Xs], [X|Ys], T):-
    solve_b(Xs, Ys, T).
solve_b([X|Xs], [Y|Ys], T):-
    X \= Y,
    solve_b(Xs, [X,Y|Ys], T).

%------------------------------------------------------------

% Otan exoume ena string S[1..N] pou teleiwnei se 'a' tote i apantisi mas einai
% to solve(S[1...N-1]) kathws den epireazei tin lusi mas.
% Otan omws teleiwnei se 'b' tote i apantisi mas einai to elaxisto anamesa sta
% solve(S[1...N-1]) + 1, solve(R[1...N-1]) + 1, opou R einai to S alla opou 'a' exoume 'b'
% kai opou 'b' exoume 'a'

changes([X|L], N):-
    init([X|L], FN, GN),
    solve_changes(L, FN, GN, N).

init([a|L], 0, 1).
init([b|L], 1, 0).

minimum(A, B, A):- A =< B.
minimum(A, B, B):- B < A.

solve_changes([], FN, _, FN).
solve_changes([a|L], FN, GN, N):-
    GN1 is GN + 1,
    GN2 is FN + 1,
    minimum(GN1, GN2, GGN),
    solve_changes(L, FN, GGN, N).

solve_changes([b|L], FN, GN, N):-
    FN1 is FN + 1,
    FN2 is GN + 1,
    minimum(FN1, FN2, FFN),
    solve_changes(L, FFN, GN, N).
%------------------------------------------------------------

% Arkei na ftiaksoume ta relations pou exoun ta gitonika strings stin lista kai meta
% na kanoume floyd warshall gia na broume ola ta relations.
% Elegxoume gia many ean indegree[i] + outdegree[i] < (A - 1) opou A to alphavito.
% Elegxoume gia none ean exei prokupsei sto telos kapoio relation (a < a)

alphabet(L, M):-
    count_alphabet(L, T, D, C, O),
    floyd(T, T, T, D, D1),
    sort(D1, F1),
    solve_alpha(M, T, F1, C, O).

transit(K, I, J, D):-
    member((I, K), D),
    member((K, J), D).

floyd([], T, T, D, D).
floyd([K|T1], T, T, D, DL):-
    floyd_K(K, T, T, D, D1),
    append(D, D1, ND),
    sort(ND, ND1),
    floyd(T1, T, T, ND1, DL).

floyd_K(K, [], T2, D, []).
floyd_K(K, [I|T1], T2, D, DL):-
    floyd_KI(K, I, T2, D, DL1),
    floyd_K(K, T1, T2, D, DL2),
    append(DL1, DL2, DL).

floyd_KI(K, I, [], D, []).
floyd_KI(K, I, [J|T2], D, [(I, J)|DL]):-
    transit(K, I, J, D),
    floyd_KI(K, I, T2, D, DL).
floyd_KI(K, I, [J|T2], D, DL):-
    not(transit(K, I, J, D)),
    floyd_KI(K, I, T2, D, DL).

exists_dub([(X, X)|D]).
exists_dub([(X, Y)|D]):-
    X \= Y,
    exists_dub(D).

count_degrees(V, [], 0, 0).
count_degrees(V, [(X, Y)|D], A, B):-
    V == Y,
    count_degrees(V, D, A, B1),
    B is B1 + 1.
count_degrees(V, [(X, Y)|D], A, B):-
    V == X,
    count_degrees(V, D, A1, B),
    A is A1 + 1.
count_degrees(V, [(X, Y)|D], A, B):-
    V \= X,
    V \= Y,
    count_degrees(V, D, A, B).

check_many([X], D, C):-
    count_degrees(X, D, A, B),
    (A + B) =\= (C - 1).
check_many([X, Y|T], D, C):-
    count_degrees(X, D, A, B),
    (A + B) =\= (C - 1).
check_many([X, Y|T], D, C):-
    count_degrees(X, D, A, B),
    (A + B) =:= (C - 1),
    check_many([Y|T], D, C).

build_array([], [], D, C).
build_array([(B, Y)|M], [Y|T], D, C):-
    count_degrees(Y, D, A, B),
    build_array(M, T, D, C).

solve_alpha(none, T, D, C, 0).
solve_alpha(none, T, D, C, 1):-
    exists_dub(D).
solve_alpha(many, T, D, C, 1):-
    not(exists_dub(D)),
    check_many(T, D, C).
solve_alpha(M, T, D, C, 1):-
    not(check_many(T, D, C)),
    build_array(E, T, D, C),
    sort(E, Q),
    transf(Q, M).

transf([], []).
transf([(A, B)|D], [B|M]):-
    transf(D, M).

count_alphabet(L, T, D1, C, O):-
    appendl(L, Q),
    sort(Q, T),
    length(T, C),
    fix_order(L, D, O),
    sort(D, D1).

appendl([], []).
appendl([L|Ls], Q):-
    appendl(Ls, P),
    append(P, L, Q).

fix_order([L], [], 1).
fix_order([L1, L2|_], [], 0):-
   forder(L1, L2, [], 0).
fix_order([L1, L2|L], D, 1):-
   forder(L1, L2, [], 1),
   fix_order([L2|L], D, 1).
fix_order([L1, L2|L], [(X, Y)|D], 1):-
   forder(L1, L2, (X, Y), 1),
   fix_order([L2|L], D, 1).

forder([], _, [], 1).
forder([X|L], [], [], 0).
forder([X|L1], [X|L2], D, O):-
    forder(L1, L2, D, O).
forder([X|_], [Y|_], (X, Y), 1):-
    X \= Y.


%------------------------------------------------------------

% Ean topothetisoume enan arithmo K stin arxi tote den exei noima se 
% kapoia kinisi na mpei mprosta tou kapoios megaluteros. Epomenws,
% oi arithmoi K + 1, K + 2, ..., N den tha prepei na mpoun me kapoia kinisi stin korifi 
% tis stivas kai sunepws prepei na emfanizontai me auth ti seira stin akolouthia.
% Epomenws, arkei na vroume tin megisti akolouthia K + 1, K + 2, ..., N kai i apantisi mas tha einai to K.

booksort(L, M):-
    length(L, N),
    reverse(L, R),
    solve_books(R, N, M).

solve_books([], N, N).
solve_books([N|R], N, M):-
    N1 is N - 1,
    solve_books(R, N1, M).
solve_books([X|R], N, M):-
    X =\= N,
    solve_books(R, N, M).
