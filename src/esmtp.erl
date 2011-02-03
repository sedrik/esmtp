%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc ESMTP api module
%% @end
%%%-------------------------------------------------------------------
-module(esmtp).

-include("../include/esmtp_mime.hrl").

%% API
-export([start/0
         ,send/2
         ,send/3
         ,send/4
         ,send/6
         ,mailq/0]).

start() ->
    application:start(esmtp).

%%====================================================================
%% API
%%====================================================================

%====
% The Opts options is a proplist that can contain any value the user wants it
% to contain.  Values that are checked for in esmtp are
%   callback - Callback module that implements the esmtp_delivery_failure/1
%              It will return Opts with the additional entry {error, Error}
%              stating what went wrong and every argument to send/6 tagged with
%              its name
% The reason for returning called_with is so that the user can act upon the
% error he recieves since we currently dont do anything in esmtp, it is safe to
% call esmtp with only the arguments that was used in the first place.
%
% Note Error is unformated as of now.
%====

send(Msg= #mime_msg{}, Opts) ->
    send(esmtp_mime:from(Msg),
         esmtp_mime:to(Msg),
         esmtp_mime:encode(Msg), Opts);
send(To, Msg) ->
    send(To, Msg, []).

send(To, Msg, Opts) ->
    send(undefined, To, Msg, Opts).

send(undefined, To, Msg, Opts) ->
    From = esmtp_app:config(default_from),
    send(From, To, Msg, Opts);
send(From, To, Message, Opts) ->
    {Host, Port} = esmtp_app:config(smarthost),
    MX = case esmtp_app:need_ssl(Port) of
             true -> {Host, Port, new_ssl, esmtp_app:config(login)};
             false -> {Host, Port, gen_tcp, no_login}
         end,
    Ehlo = esmtp_app:config(default_ehlo),
    send(MX, Ehlo, From, To, Message, Opts).

send(MX, Ehlo, From, To, Msg, Opts) ->
    NewOpts = [{mx, MX}, {ehlo, Ehlo}, {from, From}, {to, To}, {msg, Msg}] ++ Opts,
    esmtp_client:send(MX, Ehlo, From, To, Msg, NewOpts).

mailq() ->
    supervisor:which_children(esmtp_sup).

%%====================================================================
%% Internal functions
%%====================================================================

