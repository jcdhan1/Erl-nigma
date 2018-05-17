-module(model2_tests).
-export([encrypt_decrypt/0, lower_UPPER/0]).

%Using a randomly generated string as the message. If an Enigma machine
%encrypts a message, then another Enigma machine with the same initial
%parameters i.e. rotors, rotor positions etc. should be able to decrypt
%it.
encrypt_decrypt() ->
	MSG = [64+rand:uniform(26) || _ <- lists:seq(1, 100)],
	Encrypter = model2:setup("B", {"III","I","II"}, {12,6,18}, [{$A,$E},
							 {$F,$J},{$P,$R}], {$D,$F,$R}),
	Decrypter = model2:setup("B", {"III","I","II"}, {12,6,18}, [{$A,$E},
							 {$F,$J},{$P,$R}], {$D,$F,$R}),
	Encrypted = model2:crypt(Encrypter, MSG),
	Decrypted = model2:crypt(Decrypter, Encrypted),
	Decrypted =:= MSG.

%Messages identical besides whether they're upper or lower case will
%encrypt in the same way.
lower_UPPER() ->
	MSG = [64+rand:uniform(26) || _ <- lists:seq(1, 100)],
	_ENIGMA = model2:setup("B", {"III","I","II"}, {12,6,18}, [{$A,$E},
							 {$F,$J},{$P,$R}], {$D,$F,$R}),
	_enigma = model2:setup("B", {"III","I","II"}, {12,6,18}, [{$A,$E},
							 {$F,$J},{$P,$R}], {$D,$F,$R}),
	_ENCRYPTED = model2:crypt(_ENIGMA, MSG),
	_encrypted = model2:crypt(_enigma, MSG),
	_ENCRYPTED =:= _encrypted.
