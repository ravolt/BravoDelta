-module(bdd).
-export([test/1]).  %this is the final one
-import(bdd_utils).

test (FileName) -> 
	{ok, ConfigBase} = file:consult("bdd.config"),
	Config = [{feature, FileName} | ConfigBase],		% stuff the file name into the config set for later
	CorrectFileName = FileName ++ "." ++ bdd_utils:config(Config,extension),
	{feature, Name, Scenarios} = feature_import(CorrectFileName),
	[ScenarioName, ScenarioIn, ScenarioWho, ScenarioWhy | _ ] = [string:strip(S) || S <- Name, S =/= []],
	io:format("SCENARIO ~s: ~s, ~s, ~s.~n", [ScenarioName, ScenarioIn, ScenarioWho, ScenarioWhy]),
	application:start(inets),		% needed for getting we pages
	Results = [test_scenario(Config, Scenario) || Scenario <- Scenarios],
	application:stop(inets),
	Result = [R || R <- Results, R =/= pass],
	case Result of
		[] -> pass;
		_ -> bdd_selftest:test(all), Result
	end.

% read in the feature file
feature_import(FileName) ->
	{ok, Features} = file:read_file(FileName),
	[Header | Body] = re:split(Features,"Scenario:"),
	Name = string:tokens(binary_to_list(Header),"\n"),
	Scenarios = [binary_to_list(S) || S <- Body],
	{feature, Name, Scenarios}.
	
% decompose each scearion into the phrases, must be executed in the right order (Given -> When -> Then)
test_scenario(Config, Scenario) ->
	[RawName | RawSteps] = string:tokens(Scenario, "\n"),
	Name = string:strip(RawName),
	{N, GivenSteps, WhenSteps, ThenSteps} = scenario_steps(RawSteps),
	% execute all the given steps & put their result into GIVEN
	Given = [step_run(Config, [], GS) || GS <- GivenSteps],
	% now, excute the when steps & put the result into RESULT
	When = [step_run(Config, Given, WS) || WS <- WhenSteps],
	% now, check the results
	Result = [step_run(Config, When, TS) || TS <- ThenSteps],
	% now, run the then steps
	io:format("\tSTEP: ~p (~p steps) ", [Name, N]),
	case bdd_utils:assert(Result) of
		true -> io:format("PASSED!~n",[]), pass;
		_ -> io:format("~n\t\t*** FAIL (Result: ~p)***~n\t\tSTEPS:~n\t\t\tGIVEN: ~p~n\t\t\tWHEN: ~p~n\t\t\tTHEN: ~p~n", [Result, GivenSteps, WhenSteps, ThenSteps]), Name
	end.

% Inital request to run a step does not know where to look for the code, it will iterate until it finds the step match or fails
step_run(Config, Input, Step) ->
	StepFiles = [list_to_atom(bdd_utils:config(Config, feature)) | bdd_utils:config(Config, secondary_step_files)],
	step_run(Config, Input, Step, StepFiles).
% recursive attempts to run steps
step_run(Config, Input, Step, [Feature | Features]) ->
	try apply(Feature, step, [Config, Input, Step]) of
		error -> {error, Step};
		Result -> Result
	catch
		error: undef -> step_run(Config, Input, Step, Features);
		error: function_clause -> step_run(Config, Input, Step, Features);
		X: Y -> io:format("ERROR: step run found ~p:~p~n", [X, Y]), throw("Unknown error type in BDD step_run.")
	end;
% no more places to try, fail and tell the user to create the missing step
step_run(_Config, _Input, Step, []) ->
	{StepType, _N, StepAction} = Step,
	case StepType of
		step_given -> io:format("MISSING STEP: \"step(_Config, _Global, {step_given, _N, ~p}) -> false;\"~n", [StepAction]);
		step_when -> io:format("MISSING STEP: \"step(_Config, _Given, {step_when, _N, ~p}) -> false;\"~n", [StepAction]);
		step_then -> io:format("MISSING STEP: \"step(_Config, _Result, {step_then, _N, ~p}) -> false;\"~n", [StepAction]);
		_ -> io:format("MISSING STEP: \"step(_Config, _, {~p, _N, ~p}) -> false;\"~n", [StepType, StepAction])
	end,
	throw("FAIL: no matching expression found for Step"), 
	error.
	
% Split our steps into discrete types for sequential processing
% Each step is given a line number to help w/ debug
scenario_steps(Steps) ->
	%io:format("\t\tDEBUG: processing steps ~p~n", [Steps]),
	scenario_steps(Steps, 1, [], [], []).
scenario_steps([H | T], N, Given, When, Then) ->
	CleanStep = bdd_utils:clean_line(H),
	{Type, StepRaw} = step_type(CleanStep),
	Step = {Type, bdd_utils:tokenize(StepRaw)},
	%io:format("\t\tDEBUG: line ~p~n", [Step]),
	case Step of
		{step_given, S} -> scenario_steps(T, N+1, [{step_given, N, S} | Given], When, Then);
		{step_when, S} -> scenario_steps(T, N+1, Given, [{step_when, N, S} | When], Then);
		{step_then, S} -> scenario_steps(T, N+1, Given, When, [{step_then, N, S} | Then]);
		{empty, _} -> scenario_steps(T, N, Given, When, Then);
		{unknown, Mystery} -> io:format("\t\tWARNING: No prefix match for ~p~n", [Mystery]), scenario_steps(T, N+1, Given, When, Then)		
	end;
scenario_steps([], N, Given, When, Then) ->
	% returns number of steps and breaks list into types, may be expanded for more times in the future!
	{N, Given, When, Then}.
	
% figure out what type of step we are doing (GIVEN, WHEN, THEN, etc), return value
step_type([$G, $i, $v, $e, $n, 32 | Given]) ->
	{ step_given, Given };
step_type([$W, $h, $e, $n, 32 | When] ) ->
	{ step_when, When };
step_type([$T, $h, $e, $n, 32 | Then ]) -> 
	{ step_then, Then };
step_type([]) ->
	{ empty, []};
step_type(Step) ->
	{ unknown, Step }.


