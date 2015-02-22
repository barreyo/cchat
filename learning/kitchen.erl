-module(kitchen).
-compile(export_all).

fridge() ->
	receive
		{From, {store, _Food}} ->
			From ! {self(), ok},
			fridge();
		{From, {take, _Food}} ->
			From ! {self(), not_found},
			fridge();
		terminate ->
			ok
	end.

fridge2(FoodList) ->
	receive
		{From, {store, Food}} ->
			From ! {self(), ok},
			fridge2([Food | FoodList]);
		{From, {take, food}} ->
			case lists:member(Food, FoodList) of
				true ->
					From ! {self(), {ok, Food}},
					fridge2(lists:delete(Food, FoodList));
				false ->
					From ! {self(), not_found},
					fridge2(FoodList)
			end;
	terminate ->
		ok
	end.