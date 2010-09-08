-module(bdd_selftest).
-export([test/1, source/0, get_data/2]). 
-import(bdd_webrat).
-import(bdd_utils).
-import(bdd).

assert(Bools) ->
	assert(Bools, true).
assert(Bools, Test) ->
	F = fun(X) -> case X of Test -> true; _ -> false end end,
	lists:all(F, Bools).
assert_sets(Set) ->
	assert([Expected =:= Test || {_Name, Expected, Test} <- Set]).
assert_result(Test, Set) ->
	Result = case assert_sets(Set) of
		true -> io:format("\tPass \"~p\"~n", [Test]), true;
		false -> io:format("\t*** FAIL \"~p\"! results: ~p~n", [Test, Set]), Test
	end,
	Result.
	
source() ->	
	{ok, Result} = file:consult("bdd_selftest.data"),
	Result.
		
test(all) ->
	io:format("Running all tests:~n"),
	All = [assert, html_peek, html_link, html_bus],
	Source = source(),
	Results = [test(Test, Source) || Test <- All],
	case assert(Results) of
		true -> io:format("Completed, All Pass.~n"), pass;
		_ -> io:format("Completed, FAIL.~n"), [Failed || Failed <- Results, Failed =/= true]
	end;

test(Test) ->
	test(Test, source()).
	
get_data(Data, Key) ->
	{Key, Value} = lists:keyfind(Key,1,Data),
	Value.
	
test(assert, Source) ->
	Cases = [{true, assert_true}, {true, assert_1true}, {true, assert_mixed}, {true, assert_truefirst}, {true, assert_truelast}, {false, assert_1false}, {false, assert_false}],
	Result = [{Test, Expected, bdd_utils:assert(get_data(Source, Test))} || {Expected, Test} <- Cases],
	assert_result(assert, Result);

test(html_peek, Source) ->
	{LookFor, HTML} = get_data(Source, html_simple),
	Result1 = {result1, true, bdd_utils:html_peek(LookFor, HTML)},
	Result2 = {result2, nomatch, bdd_utils:html_peek(LookFor++"not findable", HTML)},
	assert_result(html_peek, [Result1, Result2]);

test(html_bus, Source) ->
	{LookFor, HTML} = get_data(Source, html_bus),
	Result1 = {result1, true, bdd_utils:html_peek(LookFor, HTML)},
	Result2 = {result2, "/map/bus/xref_5", bdd_utils:html_find_link(LookFor, HTML)},
	assert_result(html_bus, [Result1, Result2]);

test(html_link, Source) ->
	{LookFor, HTML} = get_data(Source, html_simple_link),
	Result1 = {result1, "url", bdd_utils:html_find_link(LookFor, HTML)},
	Result2 = {result2, [], bdd_utils:html_find_link(LookFor++"not findable", HTML)},
	assert_result(html_link, [Result1, Result2]);
	
test(Test, _Source) ->
	io:format("ERROR: Could not match ~p test atom!~n", [Test]),
	throw("no such test"),
	false.