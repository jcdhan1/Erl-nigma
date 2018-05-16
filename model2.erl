-module(model2).
-compile(export_all).

f_refl(X, REFL) ->
	element(2,find(X,from_list(REFL))).

setup(Reflector, Rotors, Ring_Settings, Plugboard, Initial_Setting) ->
	E = #enigma{reflector = Reflector, rotors = Rotors, ring_settings = Ring_Settings, plugboard = Plugboard, initial_setting = Initial_Setting}.

reflectorAux(REFL) ->
	case REFL of
		"A" -> _refl = reflectorA();
		"B" -> _refl = reflectorB();
		"C" -> _refl = reflectorC();
		"ThinB" -> _refl = reflectorThinB();
		"ThinC" -> _refl = reflectorThinC()
	end,
	spawn(model1, f_refl, {$A, _refl}).
