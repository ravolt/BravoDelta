-module(bdd_utils).
-export([assert/1, assert/2, config/2, tokenize/1, clean_line/1]).
-export([http_get/2, http_get/3, html_peek/2, html_search/2, html_search/3, html_find_link/2]).

assert(Bools) ->
	assert(Bools, true).
assert(Bools, Test) ->
	F = fun(X) -> case X of Test -> true; _ -> false end end,
	lists:any(F, Bools).
	
html_search(Match, Results, Test) ->
	assert([html_peek(Match,Result) || Result <- Results, Result =/= [no_op]], Test).
html_search(Match, Results) ->
	html_search(Match, Results, true).


html_peek(Match, Input) ->
%	RegEx = "\\<(html|HTML)(.*)"++Match++"(.*)\\<\\/(html|HTML)\\>",
	RegEx = "\\<(html|HTML)(.*)"++Match,
	{ok, RE} = re:compile(RegEx),
	%io:format("DEBUG html_peek compile: ~p on ~p~n", [RegEx, Input]),
	Result = re:run(Input, RE),
	%io:format("DEBUG html_peek match: ~p~n", [Result]),
	%{ match, [ {_St, _Ln} | _ ] } = Result,
	%io:format("DEBUG html_peek substr: ~p~n", [string:substr(Input, _St, _Ln)]),
	case Result of
		{match, _} -> true;
		_ -> Result
	end.
	
% return the HREF part of an anchor tag given the content of the link
html_find_link(Match, Input) ->
	RegEx = "\\<(a href|A HREF)=\"(.*)\"\\>"++Match++"(.*)\\<\\/(a|A)\\>",
	%io:format("DEBUG html_find_link regex ~p~n", [RegEx]),
	%io:format("DEBUG html_find_link compile ~p~n", [re:compile(RegEx)]),
	{ok, RE} = re:compile(RegEx),
	%io:format("DEBUG html_find_link run ~p~n", [re:run(HTML, RE)]),
	case re:run(Input, RE) of
		{match, S} -> [_, _, {Start, Length} | _] = S, 		% we only want the 3rd expression!
					  string:substr(Input, Start+1,Length);
		_ -> []
	end.

% get a page from a server
http_get(Config, Page) ->
	http_get(Config, Page, ok).
http_get(Config, Page, not_found) ->
	{url, Url} = lists:keyfind(url,1,Config),
	http_get(Url++Page, 404, "Not Found");
http_get(Config, Page, ok) ->
	{url, Url} = lists:keyfind(url,1,Config),
	http_get(Url++Page, 200, "OK");
http_get(URL, ReturnCode, State) ->
	{ok, {{"HTTP/1.1",ReturnCode,State}, _Head, Body}} = httpc:request(URL),
	Body.

config(Config, Key) ->
	{Key, Value} = lists:keyfind(Key,1,Config),
	Value.
	

clean_line(Raw) ->
	CleanLine0 = string:strip(Raw),
	CleanLine1 = string:strip(CleanLine0, left, $\t),
	CleanLine2 = string:strip(CleanLine1),
	string:strip(CleanLine2, right, $.).

tokenize(Step) ->
	Tokens = string:tokens(Step,"\""),
	[string:strip(X) || X<- Tokens].