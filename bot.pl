% bot.pl
% Бот использует правило кратности (MaxTake+1) = 4 для игры с ходами 1..3.
% В проигрышной позиции (кратно 4) бот делает случайный ход, чтобы выглядеть "человечнее".

:- use_module(utils).

% bot_move(+Sticks, -Move, -Explanation)
bot_move(Sticks, Move, Explanation) :-
    MaxTake is min(3, Sticks),
    ( Sticks =< MaxTake ->
        % можно взять все оставшиеся и выиграть
        Move = Sticks,
        format(atom(Explanation), 'Беру последние ~w спичек и выигрываю.', [Move])
    ;
        R is Sticks mod (MaxTake + 1),  % mod 4
        ( R =\= 0 ->
            % оптимальный ход: взять R, чтобы оставить кратное 4
            Move = R,
            Target is Sticks - Move,
            format(atom(Explanation), 'Оставляю кратное ~w: беру ~w, остаётся ~w.', [MaxTake+1, Move, Target])
        ;
            % позиция проигрышна при идеальной игре соперника — делаем случайный ход
            rand_between(1, MaxTake, Move),
            Target is Sticks - Move,
            format(atom(Explanation), 'Позиция проигрышна (кратно ~w). Делаю случайный ход ~w, остаётся ~w.', [MaxTake+1, Move, Target])
        )
    ).
