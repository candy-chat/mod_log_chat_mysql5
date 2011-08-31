%%%----------------------------------------------------------------------
%%% File    : mod_log_chat_mysql5.erl
%%% Author  : Jérôme Sautret <jerome.sautret@process-one.net>
%%% Author  : Michael Weibel <michael.weibel@amiadogroup.com>
%%% Purpose : Log chat messages in a mysql dbs
%%%----------------------------------------------------------------------

-module(mod_log_chat_mysql5).
-author('michael.weibel@amiadogroup.com').

-behaviour(gen_mod).

-export([start/2,
	init/1,
	stop/1,
	log_packet_send/3,
	handle_info/2, terminate/2]).

%%-define(ejabberd_debug, true).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(PROCNAME, ?MODULE).

-record(state, {dbref, server, port, db, user, password}).

table_name() ->
	"mod_log_chat_mysql5".

start(Host, Opts) ->
	?INFO_MSG(" ~p  ~p~n", [Host, Opts]),
	ejabberd_hooks:add(user_send_packet, Host, ?MODULE, log_packet_send, 55),
	Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
	gen_server:start({local, Proc}, ?MODULE, [Opts], []).

init([Opts]) ->
    ?INFO_MSG("Starting ~p", [?MODULE]),

	crypto:start(),

	Server = gen_mod:get_opt(server, Opts, "localhost"),
	Port = gen_mod:get_opt(port, Opts, 3306),
	DB = gen_mod:get_opt(db, Opts, "logdb"),
	User = gen_mod:get_opt(user, Opts, "root"),
	Password = gen_mod:get_opt(password, Opts, ""),

	St = #state{server=Server, port=Port, db=DB,
	               user=User, password=Password},

	case open_mysql_connection(St) of
	    {ok, DBRef} ->
	        State = St#state{dbref=DBRef},

			ets:new(mod_log_chat_mysql5, [named_table, protected, set, {keypos, 1}]),
			ets:insert(mod_log_chat_mysql5, {dbref, DBRef}),

	        Result = create_table(State),
			?INFO_MSG("create table: ~p", [Result]),
	        erlang:monitor(process, DBRef),
	        {ok, State};
	    {error, Reason} ->
	        ?ERROR_MSG("MySQL connection failed: ~p~n", [Reason]),
	        {stop, db_connection_failed}
	end.

open_mysql_connection(#state{server=Server, port=Port, db=DB,
                             user=DBUser, password=Password} = _State) ->
   LogFun = fun(debug, _Format, _Argument) ->
                %?DEBUG(Format, Argument);
                 ok;
               (error, Format, Argument) ->
                 ?ERROR_MSG(Format, Argument);
               (Level, Format, Argument) ->
                 ?DEBUG("MySQL (~p)~n", [Level]),
                 ?DEBUG(Format, Argument)
            end,
   ?INFO_MSG("Opening mysql connection ~s@~s:~p/~s", [DBUser, Server, Port, DB]),
   mysql_conn:start(Server, Port, DBUser, Password, DB, LogFun).

cleanup(Host) ->
	close_mysql_connection(),
    ejabberd_hooks:delete(user_send_packet, Host,
			  ?MODULE, log_packet_send, 55),
	ok.

close_mysql_connection() ->
	[{_, DBRef}] = ets:lookup(mod_log_chat_mysql5, dbref),
	?DEBUG("Closing ~p mysql connection", [DBRef]),
	mysql_conn:stop(DBRef).

stop(Host) ->
    cleanup(Host),
	ok.

handle_info({'DOWN', _MonitorRef, process, _Pid, _Info}, State) ->
    {stop, connection_dropped, State};
handle_info(Info, State) ->
    ?INFO_MSG("Got Info:~p, State:~p", [Info, State]),
    {noreply, State}.

terminate(_Reason, #state{}=_State) ->
	close_mysql_connection(),
	ok.

log_packet_send(From, To, Packet) ->
    log_packet(From, To, Packet).

log_packet(From, To, Packet = {xmlelement, "message", Attrs, _Els}) ->
    case xml:get_attr_s("type", Attrs) of
	"error" -> %% we don't log errors
	    ?DEBUG("dropping error: ~s", [xml:element_to_string(Packet)]),
	    ok;
	_ ->
	    write_packet(From, To, Packet, xml:get_attr_s("type", Attrs))
    end;
log_packet(_From, _To, _Packet) ->
    ok.

write_packet(From, To, Packet, Type) ->
    Body = escape(html, xml:get_path_s(Packet, [{elem, "body"}, cdata])),
    case Body of
        "" -> %% don't log empty messages
            ?DEBUG("not logging empty message from ~s",[jlib:jid_to_string(From)]),
            ok;
        _ ->
	    FromJid = From#jid.luser++"@"++From#jid.lserver++"/"++From#jid.resource,
		ResourceLen = length(To#jid.resource),
		%% don't include resource when target is muc room
		if
			ResourceLen > 0 ->
				ToJid = To#jid.luser++"@"++To#jid.lserver++"/"++To#jid.resource;
			true ->
				ToJid = To#jid.luser++"@"++To#jid.lserver
		end,
		insert_row(FromJid, ToJid, Body, Type)
    end.

escape(text, Text) ->
    Text;
escape(_, "") ->
    "";
escape(html, [$< | Text]) ->
    "&lt;" ++ escape(html, Text);
escape(html, [$& | Text]) ->
    "&amp;" ++ escape(html, Text);
escape(html, [Char | Text]) ->
    [Char | escape(html, Text)].


create_table(#state{dbref=DBRef}) ->
	?INFO_MSG("Mysql Conn.: ~p", [DBRef]),
	Query =  ["CREATE TABLE IF NOT EXISTS ", table_name(), "(",
                "id INT AUTO_INCREMENT PRIMARY KEY, ",
                "fromJid VARCHAR(255) NOT NULL,",
				"toJid VARCHAR(255) NOT NULL,",
				"sentDate TIMESTAMP NOT NULL,",
				"body TEXT,",
				"type VARCHAR(10)",
                ") ENGINE=MyISAM CHARACTER SET utf8"],
	case sql_query_internal(DBRef, Query) of
		{updated, _} -> ok;
		{error, _Reason} -> error
	end.

insert_row(FromJid, ToJid, Body, Type) ->
	[{_, DBRef}] = ets:lookup(mod_log_chat_mysql5, dbref),
	Query = ["INSERT INTO ", table_name(), " (fromJid, toJid, sentDate, body, type) VALUES",
	 		"(\"",
	FromJid,
	"\", \"",
	ToJid, "\", NOW(), \"",
	Body, "\", \"", Type ,"\");"],

	case sql_query_internal(DBRef, Query) of
		{updated, _} -> ok;
		{error, _Reason} -> error
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% SQL internals
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sql_query_internal(DBRef, Query) ->
    case sql_query_internal_silent(DBRef, Query) of
         {error, Reason} ->
            ?ERROR_MSG("~p while ~p", [Reason, lists:append(Query)]),
            {error, Reason};
         Rez -> Rez
    end.

sql_query_internal_silent(DBRef, Query) ->
    ?DEBUG("DOING: \"~s\"", [lists:append(Query)]),
    get_result(mysql_conn:fetch(DBRef, Query, self())).

get_result({updated, MySQLRes}) ->
    {updated, mysql:get_result_affected_rows(MySQLRes)};
get_result({data, MySQLRes}) ->
    {data, mysql:get_result_rows(MySQLRes)};
get_result({error, "query timed out"}) ->
    {error, "query timed out"};
get_result({error, MySQLRes}) ->
    Reason = mysql:get_result_reason(MySQLRes),
    {error, Reason}.
