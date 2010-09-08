-module(bdd_webrat).
-export([step/3]).
-import(bdd_utils).

step(_Config, _Global, {step_given, _N, ["I am on the home page"]}) -> 
	bdd_utils:http_get(_Config, []);

step(_Config, _Global, {step_given, _N, ["I went to the", Page, "page"]}) ->
	bdd_utils:http_get(_Config, Page);
	
step(_Config, _Given, {step_when, _N, ["I go to the home page"]}) -> 
	%io:format("DEBUG go home.~n"), 
	bdd_utils:http_get(_Config, []);

step(_Config, _Given, {step_when, _N, ["I go to the", Page, "page"]}) -> 
	%io:format("DEBUG go to the ~p page~n", [Page]), 
	bdd_utils:http_get(_Config, Page);

step(_Config, _Given, {step_when, _N, ["I try to go to the", Page, "page"]}) ->
	%io:format("DEBUG expect FAIL when going to the ~p page~n", [Page]), 
	bdd_utils:http_get(_Config, Page, not_found);

step(_Config, _Given, {step_when, _N, ["I click on the",Link,"link"]}) -> 
	[URL | _] = [bdd_utils:html_find_link(Link, HTML) || HTML <- (_Given), HTML =/= []],
	%io:format("DEBUG URL ~p FROM ~p~n",[URL, _Given]),
	Result = case URL of
		[] -> io:format("CANNOT FIND LINK ~s~n", [Link]), error;
		_ -> bdd_utils:http_get(_Config, URL, ok)
	end,
	%io:format("DEBUG: when I click result ~p~n", [Result]),
	Result;

step(_Config, _Result, {step_then, _N, ["I should not see", Text]}) -> 
	%io:format("DEBUG step_then result ~p should NOT have ~p on the page~n", [_Result, Text]),
	bdd_utils:html_search(Text,_Result, false);
	
step(_Config, _Result, {step_then, _N, ["I should see", Text]}) -> 
	%io:format("DEBUG step_then result ~p should have ~p on the page~n", [_Result, Text]),
	bdd_utils:html_search(Text,_Result).
