% server.pl
% Исправленный HTTP сервер для Nim (SWI-Prolog).
% Поддерживает старт игры с выбором сложности: easy, medium, hard.
% Подразумевается, что utils.pl и bot.pl лежат в той же папке.
:- encoding(utf8).
:- set_prolog_flag(encoding, utf8).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).

% Загружаем локальные файлы (ensure_loaded гарантирует порядок загрузки)
:- ensure_loaded('utils.pl').
:- ensure_loaded('bot.pl').

% Динамическое состояние игры: game_state(Sticks, Turn, Difficulty).
:- dynamic game_state/3.

% Роуты API
:- http_handler(root('start'), start_handler, []).
:- http_handler(root('player_move'), player_move_handler, []).
:- http_handler(root('state'), state_handler, []).
% Все остальные запросы обслуживаем как статические файлы из ./www
:- http_handler(root(.), static_handler, [prefix]).

% -----------------------
% Запуск сервера
% -----------------------
start_server(Port) :-
    http_server(http_dispatch, [port(Port)]),
    format('Server started at http://localhost:~w~n', [Port]).

% -----------------------
% Статический обработчик файлов
% -----------------------
static_handler(Request) :-
    ( memberchk(path_info(PathInfo), Request) -> true ; PathInfo = '' ),
    ( PathInfo = '/' -> Clean = '' ;
      ( sub_atom(PathInfo, 0, 1, _, '/') -> sub_atom(PathInfo, 1, _, 0, Clean) ; Clean = PathInfo ) ),
    ( Clean = '' -> File = 'www/index.html' ; atom_concat('www/', Clean, File) ),
    ( exists_file(File) ->
        http_reply_file(File, [], Request)
    ;
        reply_json_dict(_{ok:false, error: 'File not found', file: File})
    ).

% -----------------------
% /start  POST {sticks: N, difficulty: "easy"|"medium"|"hard"}
% -----------------------
start_handler(Request) :-
    http_read_json_dict(Request, DictIn),
    % Проверяем поле sticks
    ( get_dict(sticks, DictIn, St0), integer(St0), St0 > 0 ->
        Sticks = St0
    ; reply_json_dict(_{ok:false, error:'Поле sticks обязателен и должно быть положительным целым'}), !
    ),
    % Проверяем необязательное поле difficulty
    ( get_dict(difficulty, DictIn, DiffIn) ->
        normalize_difficulty(DiffIn, Diff)
    ; Diff = hard
    ),
    retractall(game_state(_,_,_)),
    assertz(game_state(Sticks, player, Diff)),
    reply_json_dict(_{ok:true, sticks:Sticks, turn:player, difficulty:Diff}).

% normalize_difficulty(+Raw, -Atom) - приводим к одному из easy|medium|hard
normalize_difficulty(Raw, Diff) :-
    ( string(Raw) -> atom_string(A, Raw) ; A = Raw ),
    downcase_atom(A, Lower),
    ( member(Lower, [easy, medium, hard]) -> Diff = Lower ; Diff = hard ).

% -----------------------
% /state GET - вернуть текущее состояние
% -----------------------
state_handler(_Request) :-
    ( game_state(Sticks, Turn, Difficulty) ->
        reply_json_dict(_{ok:true, sticks:Sticks, turn:Turn, difficulty:Difficulty})
    ;
        reply_json_dict(_{ok:false, error:'Игра не запущена'})
    ).

% -----------------------
% /player_move POST {take: K}
% -----------------------
player_move_handler(Request) :-
    http_read_json_dict(Request, DictIn),
    ( get_dict(take, DictIn, Take), integer(Take), Take >= 1 ->
        true
    ; reply_json_dict(_{ok:false, error:'Неверный формат тела запроса. Ожидается {\"take\": integer >= 1}'}), !
    ),
    ( game_state(Sticks, player, Difficulty) ->
        MaxTake is min(3, Sticks),
        ( Take >= 1, Take =< MaxTake ->
            AfterPlayer is Sticks - Take,
            ( AfterPlayer =:= 0 ->
                retractall(game_state(_,_,_)),
                reply_json_dict(_{ok:true, sticks:0, playerMove:Take, botMove:null, winner:player, message:'Игрок выиграл!'})
            ;
                % Ход бота в зависимости от уровня сложности
                bot_move_level(AfterPlayer, Difficulty, BotMove, BotExp),
                AfterBot is AfterPlayer - BotMove,
                ( AfterBot =:= 0 ->
                    retractall(game_state(_,_,_)),
                    reply_json_dict(_{ok:true, sticks:0, playerMove:Take, botMove:BotMove, botExplanation:BotExp, winner:bot, message:'Бот выиграл!'})
                ;
                    retractall(game_state(_,_,_)),
                    assertz(game_state(AfterBot, player, Difficulty)),
                    reply_json_dict(_{ok:true, sticks:AfterBot, playerMove:Take, botMove:BotMove, botExplanation:BotExp, winner:null, difficulty:Difficulty})
                )
            )
        ;
            reply_json_dict(_{ok:false, error:'Недопустимый ход (вне допустимого диапазона или больше оставшихся спичек)'})
        )
    ;
        reply_json_dict(_{ok:false, error:'Не ваша очередь или игра не запущена'})
    ).

% -----------------------
% Конец файла
% -----------------------
