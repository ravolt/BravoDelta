-module(bravodelta).
-export([step/3]).
-import(bdd_utils).

step(_Config, _Given, {step_when, _N, ["I have a test that is not in WebRat"]}) -> true;

step(_Config, _Result, {step_then, _N, ["I should use my special step file"]}) -> true.
