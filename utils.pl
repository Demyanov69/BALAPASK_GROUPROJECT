% utils.pl

% вывод спичек в виде ASCII
print_sticks(0) :- nl.
print_sticks(N) :-
    N > 0,
    write('▒'),
    N1 is N - 1,
    print_sticks(N1).

% генерация случайного числа от Min до Max
rand_between(Min, Max, Rand) :-
    RandomMax is Max - Min + 1,
    Rand is Min + random(RandomMax).

% вывод разделительной линии
print_line :- write('--------------------------'), nl.
