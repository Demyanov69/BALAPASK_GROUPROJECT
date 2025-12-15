:- module(game, [start_game/0]).
:- encoding(utf8).
:- use_module(utils).
:- use_module(bot).

% старт игры
start_game :-
    nl,
    write('Добро пожаловать в игру Nim!'), nl,
    write('Введите количество спичек для игры: '),
    read(InitialSticks),
    write('Выберите сложность бота (easy, medium, hard): '),
    read(Difficulty),
    format('Игра начинается! Спичек: ~w, сложность бота: ~w~n', [InitialSticks, Difficulty]),
    print_sticks(InitialSticks),
    write('Вы ходите первым.'), nl,
    game_loop(InitialSticks, player, Difficulty).

% конец
game_loop(0, Player, _) :-
    other_player(Player, Winner),
    print_line,
    format('Игра окончена! Победил ~w~n', [Winner]).

% ход игрока
game_loop(Sticks, player, Difficulty) :-
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
       game_loop(NewSticks, bot, Difficulty)
    ;  write('Неверный ход, попробуйте снова.'), nl,
       game_loop(Sticks, player, Difficulty)
    ).

% ход бота
game_loop(Sticks, bot, Difficulty) :-
    print_line,
    format('Осталось спичек: ~w~n', [Sticks]),
    print_sticks(Sticks),
    bot_move_level(Sticks, Difficulty, Move, Explanation),
    format('Бот берёт ~w спичек.~n', [Move]),
    write('Объяснение хода: '), write(Explanation), nl,
    NewSticks is Sticks - Move,
    game_loop(NewSticks, player, Difficulty).

% проверка допустимого хода
valid_move(Move, Sticks) :-
    integer(Move),
    Move >= 1,
    Move =< 3,
    Move =< Sticks.

other_player(player, bot).
other_player(bot, player).
