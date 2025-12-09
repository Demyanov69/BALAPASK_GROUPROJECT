% Загрузка и запуск консольной игры
:- encoding(utf8).
:- ensure_loaded(utils).
:- ensure_loaded(bot).
:- ensure_loaded(game).

main :-
    write('Введите количество спичек для игры: '),
    read(Sticks),
    ( integer(Sticks), Sticks > 0
    -> start_game(Sticks)
    ;  write('Неверное количество. Попробуйте снова.'), nl,
       main
    ).
