-module(model1).
-import(lists, [append/2]).
-export([get/3, rotorI/0, increment/1]).

%TODO: Implement rotors, reflectors etc. from SOURCE: http://www.cryptomuseum.com/crypto/enigma/wiring.htm

rotorI() -> [] %Enigma I Rotor I

ukwA() -> [] %Enigma I UKW-A

get([Key0, Key1], [Head|Tail], List) ->
	case Head of
		{Key1, Val} -> append([Val], get(Key2, List, List);
			->find([Key1, Key2], Tail, List)
	end;
	get([], _, _) -> [];
	get(_, [], _) -> [].
