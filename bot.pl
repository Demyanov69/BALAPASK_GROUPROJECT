% bot.pl
:- use_module(utils).

% бот выбирает количество спичек и объясняет ход
bot_move(Sticks, Move, Explanation) :-
    MaxTake is min(3, Sticks),
    % проверка: можно ли выиграть прямо
    (Sticks =< MaxTake ->
        Move is Sticks,  % берём все оставшиеся спички и выигрываем
        format(atom(Explanation), 'Беру последние спички и выигрываю: ~w', [Move])
    ;
        % стратегия (2^n-1)
        find_target(Sticks, Target),
        Desired is Sticks - Target,
        Move is min(Desired, MaxTake),
        format(atom(Explanation), 'Стремлюсь оставить (2^n-1) = ~w спичек', [Target])
    ).

% найти ближайшее (2^n - 1) меньше текущих спичек
find_target(Sticks, Target) :-
    findall(X, (between(0, 10, N), X is 2^N - 1, X < Sticks), L),
    max_list(L, Target).
