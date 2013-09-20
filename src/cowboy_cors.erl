-module(cowboy_cors).

-export([execute/2]).
-export([init/0]).
-export([init/1]).

execute(Req, Env) ->
  case fast_key:get(cors, Env) of
    undefined ->
      {ok, Req, Env};
    Options ->
      Fun = init(Options),
      Fun(Req, Env)
  end.

init() ->
  init([]).
init(Options) ->
  Origin = fast_key:get(origin, Options, <<"*">>),
  Headers = fast_key:get(headers, Options, <<"origin, x-requested-with, authorization, content-type, cache-control">>),
  Methods = fast_key:get(methods, Options, <<"GET, POST, PUT, DELETE, HEAD">>),
  MaxAge = fast_key:get(max_age, Options, <<"31556926">>),
  HandleOptions = lists:member(handle_options, Options),
  fun (Req, Env) ->
    Req2 = apply_headers([
      {<<"access-control-allow-origin">>, Origin},
      {<<"access-control-allow-headers">>, Headers},
      {<<"access-control-allow-methods">>, Methods},
      {<<"access-control-max-age">>, MaxAge}
    ], Req),
    case cowboy_req:method(Req2) of
      {<<"OPTIONS">>, Req3} when HandleOptions ->
        {halt, Req3};
      {_, Req3} ->
        {ok, Req3, Env}
    end
  end.

apply_headers([], Req) ->
  Req;
apply_headers([{Header, Value}|Headers], Req) ->
  Req2 = cowboy_req:set_resp_header(Header, Value, Req),
  apply_headers(Headers, Req2).
