% main.pl

% сначала загружаем все файлы вручную
:- [utils].
:- [bot].
:- [game].

main :-
    write('Введите количество спичек для игры: '),
    read(Sticks),
    ( integer(Sticks), Sticks > 0
    -> start_game(Sticks)
    ;  write('Неверное количество. Попробуйте снова.'), nl,
       main
    ).
