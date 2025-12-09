% Консольный игровой цикл Nim (игрок vs бот)
:- encoding(utf8).
:- use_module(utils).
:- use_module(bot).

% start_game(+InitialSticks)
start_game(InitialSticks) :-
    nl,
    write('Добро пожаловать в игру Nim!'), nl,
    format('Всего спичек: ~w~n', [InitialSticks]),
    print_sticks(InitialSticks),
    write('Вы ходите первым.'), nl,
    game_loop(InitialSticks, player).

% главный игровой цикл
game_loop(0, Player) :-
    other_player(Player, Winner),
    print_line,
    format('Игра окончена! Победил ~w~n', [Winner]).

% ход игрока
game_loop(Sticks, player) :-
    print_line,
    format('Осталось спичек: ~w~n', [Sticks]),
    print_sticks(Sticks),
    MaxTake is min(3, Sticks),
    format('Доступно взять от 1 до ~w спичек.~n', [MaxTake]),
    write('Сколько спичек взять (1-3)? '),
    read(Input),
    ( valid_move(Input, Sticks)
    -> NewSticks is Sticks - Input,
       format('Вы взяли ~w спичек.~n', [Input]),
       game_loop(NewSticks, bot)
    ;  write('Неверный ход, попробуйте снова.'), nl,
       game_loop(Sticks, player)
    ).

% ход бота
game_loop(Sticks, bot) :-
    print_line,
    format('Осталось спичек: ~w~n', [Sticks]),
    print_sticks(Sticks),
    bot_move(Sticks, Move, Explanation),
    format('Бот берёт ~w спичек.~n', [Move]),
    write('Объяснение хода: '), write(Explanation), nl,
    NewSticks is Sticks - Move,
    game_loop(NewSticks, player).

% проверка допустимого хода
valid_move(Move, Sticks) :-
    integer(Move),
    Move >= 1,
    Move =< 3,
    Move =< Sticks.

% смена игрока
other_player(player, bot).
other_player(bot, player).
