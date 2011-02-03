%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Simple one-shot client using esmtp_fsm.
%% @end
%%%-------------------------------------------------------------------
-module(esmtp_client).

%% API
-export([send/6
         ,start_link/6
         ,init/6
         ,sendemail/5]).

%%====================================================================
%% API
%%====================================================================

send(MX, Ehlo, From, To, Msg, Opts) when is_list(From), is_list(To) ->
    is_mx(MX),
    supervisor:start_child(esmtp_sup, [MX, Ehlo, From, To, Msg, Opts]).

start_link(MX, Ehlo, From, To, Msg, Opts) ->
    proc_lib:start_link(?MODULE, init, [MX, Ehlo, From, To, Msg, Opts]).

%%====================================================================
%% Internal functions
%%====================================================================

init({Host,Port},Ehlo,From,To,Msg, Opts) ->
    init({Host,Port,tcp,no_login},Ehlo,From,To,Msg, Opts);
init(MX,Ehlo,From,To,Msg, Opts) ->
    proc_lib:init_ack({ok, self()}),
    case catch sendemail(MX,Ehlo,From,To,Msg) of
        ok ->
            ok;
        Error ->
            send_error_report(Error, Opts)
    end.

sendemail({Host,Port,SSL,Login},Ehlo,From,To,Msg) ->
    {ok, S0} = esmtp_sock:connect(Host, Port, SSL),
    {ok, S1, {220, _Banner}} = esmtp_sock:read_response(S0),
    {ok, S2, {250, _Msg}} = esmtp_sock:command(S1, {ehlo, Ehlo}),
    AuthS = case Login of
                {User,Pass} ->
                    {ok, S3, {334, _}} = esmtp_sock:command(S2, {auth, "PLAIN"}),
                    {ok, S4, {235, _}} = esmtp_sock:command(S3, {auth_plain, User, Pass}),
                    S4;
                no_login ->
                    S2
            end,
    {ok, S10, {250, _}} = esmtp_sock:command(AuthS, {mail_from, From}),
    {ok, S11, {250, _}} = esmtp_sock:command(S10, {rcpt_to, To}),
    {ok, S12, {250, _}} = esmtp_sock:send_data(S11, Msg),
    ok = esmtp_sock:close(S12).

is_mx({_Host,Port}) when is_integer(Port) -> true;
is_mx({_Host,Port,new_ssl,_Login}) when is_integer(Port) -> true;
is_mx({_Host,Port,gen_tcp,no_login}) when is_integer(Port) -> true.

send_error_report(Error, Opts) ->
    case proplist:get_value(callback, Opts) of
        undefined ->
            Error; %If callback is not defined fail as we would have done in the
                  %old versions of esmtp
        Callback ->
            %We must include either a reference or the initial call. initial
            %call is probably best
            Callback:delivery_failure({error, Error})
    end.
