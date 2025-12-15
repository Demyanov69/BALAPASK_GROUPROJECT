% Три уровня сложности: easy, medium, hard.
:- module(bot, [bot_move_level/4]).
:- encoding(utf8).
:- use_module(utils).

bot_move_level(Sticks, Level, Move, Explanation) :-
    MaxTake is min(3, Sticks),
    ( Sticks =< MaxTake ->
        Move = Sticks,
        format(atom(Explanation), 'Беру последние ~w спичек и выигрываю.', [Move])
    ;
        ( Level == easy ->
            % полностью случайный ход
            rand_between(1, MaxTake, Move),
            Target is Sticks - Move,
            format(atom(Explanation), 'Easy: случайный ход: беру ~w, остаётся ~w.', [Move, Target])
        ; Level == medium ->
            % в 50% случаев оптимально, в 50% случайно
            rand_between(1, 2, Choice),
            ( Choice =:= 1 ->
                % оптимальный ход (mod 4)
                optimal_move(Sticks, MaxTake, Move, Explanation)
            ;
                rand_between(1, MaxTake, Move),
                Target is Sticks - Move,
                format(atom(Explanation), 'Medium: случайный ход: беру ~w, остаётся ~w.', [Move, Target])
            )
        ; % hard
            optimal_move(Sticks, MaxTake, Move, Explanation)
        )
    ).

optimal_move(Sticks, MaxTake, Move, Explanation) :-
    R is Sticks mod (MaxTake + 1), % mod 4
    ( R =\= 0 ->
        Move = R,
        Target is Sticks - Move,
        format(atom(Explanation), 'Оптимальный ход (оставляю кратное ~w): беру ~w, остаётся ~w.', [MaxTake+1, Move, Target])
    ;
        % проигрышная позиция — берем 1 (детерминированно)
        Move = 1,
        Target is Sticks - Move,
        format(atom(Explanation), 'Проигрышная позиция (кратно ~w). Беру ~w, остаётся ~w.', [MaxTake+1, Move, Target])
    ).
