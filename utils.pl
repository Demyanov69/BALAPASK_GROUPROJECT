% utils.pl
% Вспомогательные предикаты: печать спичек, случайное число, разделитель
:- encoding(utf8).
:- module(utils, [print_sticks/1, rand_between/3, print_line/0]).

% печать спичек в группах по 4, визуально приятнее
print_sticks(0) :- nl.
print_sticks(N) :-
    N > 0,
    print_sticks_groups(N), nl.

print_sticks_groups(0) :- !.
print_sticks_groups(N) :-
    N > 0,
    Group is min(4, N),
    write('['),
    print_group_symbols(Group),
    write('] '),
    N1 is N - Group,
    print_sticks_groups(N1).

print_group_symbols(0) :- !.
print_group_symbols(K) :-
    K > 0,
    write('▒'),
    K1 is K - 1,
    print_group_symbols(K1).


% вывод разделительной линии
print_line :- write('--------------------------'), nl.

