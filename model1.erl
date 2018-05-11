-module(model1).
-import(lists, [append/2]).
-export([get/3, rotorI/0, ukwA/0]).

%TODO: Implement rotors, reflectors etc. from SOURCE: http://www.cryptomuseum.com/crypto/enigma/wiring.htm

rotorI() -> "EKMFLGDQVZNTOWYHXUSPAIBRCJ". %Enigma I Rotor I

ukwA() -> "EJMZALYXVBWFCRQUONTSPIKHGD". %Enigma I UKW-A

get([Key0|Key1], [Head|Tail], List) ->
	case Head of
		{Key0, Val} -> append([Val], get(Key1, List, List));
		_ -> get([Key0, Key1], Tail, List)
	end;
	get([], _, _) -> [];
	get(_, [], _) -> [].
