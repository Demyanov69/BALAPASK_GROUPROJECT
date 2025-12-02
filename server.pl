% server.pl
% Простой HTTP сервер на SWI-Prolog для веб-интерфейса игры Nim

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).

% Сначала загружаем локальные файлы (ensure_loaded надёжнее, чем use_module без пути)
:- ensure_loaded('utils.pl').
:- ensure_loaded('bot.pl').

% динамическое состояние игры: game_state(Sticks, Turn).
:- dynamic game_state/2.

% регистрация маршрутов: статические файлы + API
% Сначала API, затем статические файлы (prefix) — чтобы API-пути перехватывались корректно.
:- http_handler(root('start'), start_handler, []).
:- http_handler(root('player_move'), player_move_handler, []).
:- http_handler(root('state'), state_handler, []).
% сервируем каталог ./www при всех остальных запросах
% используем собственный обработчик файлов:
:- http_handler(root(.), static_handler, [prefix]).

% static_handler(+Request)
% отдаёт файл из каталога ./www; если путь пустой — отдает index.html
static_handler(Request) :-
    % извлекаем path_info, если нет — считаем пустым
    ( memberchk(path_info(PathInfo), Request) -> true ; PathInfo = '' ),
    % path_info может начинаться с '/', уберём ведущий '/':
    ( PathInfo = '/' -> Clean = '' ;
      ( sub_atom(PathInfo, 0, 1, _, '/') -> sub_atom(PathInfo, 1, _, 0, Clean) ; Clean = PathInfo ) ),
    ( Clean = '' -> File = 'www/index.html'
    ; atom_concat('www/', Clean, File)
    ),
    % если файл существует — отдать, иначе вернуть 404
    ( exists_file(File) ->
        http_reply_file(File, [], Request)
    ;
        % отдаём 404 (можно заменить на кастомную страницу)
        format(string(Msg), 'Файл не найден: ~w', [File]),
        reply_html_page(title('404 Not Found'),
                        h1('404 Not Found'),
                        p(Msg)),
        % или: throw(http_reply(status(404, 'Not found')))
        !
    ).

% Запуск сервера
start_server(Port) :-
    http_server(http_dispatch, [port(Port)]),
    format('Server started at http://localhost:~w~n', [Port]).

% /start  POST {sticks: N}
start_handler(Request) :-
    http_read_json_dict(Request, DictIn),
    ( _{sticks:Sticks} :< DictIn, integer(Sticks), Sticks > 0 ->
        retractall(game_state(_,_)),
        assertz(game_state(Sticks, player)),
        reply_json_dict(_{ok:true, sticks:Sticks, turn:player})
    ;
        reply_json_dict(_{ok:false, error:'Неверные данные. Ожидается {\"sticks\": positive_integer}'})
    ).

% /state GET -> текущие данные игры
state_handler(_Request) :-
    ( game_state(Sticks, Turn) ->
        reply_json_dict(_{ok:true, sticks:Sticks, turn:Turn})
    ;
        reply_json_dict(_{ok:false, error:'Игра не запущена'})
    ).

% /player_move POST {take: K}
player_move_handler(Request) :-
    http_read_json_dict(Request, DictIn),
    ( _{take:Take} :< DictIn, integer(Take), Take >= 1 ->
        ( game_state(Sticks, player) ->
            MaxTake is min(3, Sticks),
            ( Take >=1, Take =< MaxTake ->
                New1 is Sticks - Take,
                ( New1 =:= 0 ->
                    retractall(game_state(_,_)),
                    reply_json_dict(_{ok:true, sticks:0, playerMove:Take, botMove:null, winner:player, message:'Игрок выиграл!'}) 
                ;
                    % ход бота
                    bot_move(New1, BotMove, BotExp),
                    New2 is New1 - BotMove,
                    ( New2 =:= 0 ->
                        retractall(game_state(_,_)),
                        reply_json_dict(_{ok:true, sticks:0, playerMove:Take, botMove:BotMove, botExplanation:BotExp, winner:bot, message:'Бот выиграл!'}) 
                    ;
                        retractall(game_state(_,_)),
                        assertz(game_state(New2, player)),
                        reply_json_dict(_{ok:true, sticks:New2, playerMove:Take, botMove:BotMove, botExplanation:BotExp, winner:null})
                    )
                )
            ;
                reply_json_dict(_{ok:false, error:'Недопустимый ход (вне допустимого диапазона)'})
            )
        ;
            reply_json_dict(_{ok:false, error:'Не ваша очередь или игра не запущена'})
        )
    ;
        reply_json_dict(_{ok:false, error:'Неверный формат тела запроса. Ожидается {\"take\": integer}'})
    ).
