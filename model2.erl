-module (model2).
-import(lists, [zip/2]).
-import(maps, [from_list/1, find/2]).
-include("Enigma.hrl").
-compile(export_all).

%Helpers used by most functions.
receiver(From,Channel) ->
	From!{rcv,Channel,self()},
	receive
		{rcv,Channel,Result} ->
			Result
		end.

sender(From,Channel,Value) ->
	From!{snd,Channel,Value,self()},
	if Channel==final_refl ->%If the channel is the atom final_refl, it means the rotor sending the signal is the lastmost and thus sends to the reflector rather than another rotor.
		ok;
		true ->
			receive
				{ok,{snd,Channel,Value}} ->
					ok
			end
	end.

%Keyboard process
keyboard(From,Inc,Key,Lamp) ->
	receive
		{x,X,PID} ->
			sender(From,Inc,1), %As per the model , an increment signal of 1 is sent first.
			sender(From,Key,X), %Then the character to be encrypted is sent.
			PID!receiver(From,Lamp), %Then it recieves a signal over the Lamp channel.
			keyboard(From,Inc,Key,Lamp);
		{cmd,stop} -> ok
	end.

%Plugboard process
plugboard(From,Plugboard,R,L) ->
	X=receiver(From,R),
	sender(From,L,f_plug(Plugboard,X)),
	plugboard(From,Plugboard,L,R).
f_plug(Plugboard,X) ->
	case Plugboard of
		[{X,Y}|_T] ->
			Y;
		[{Y,X}|_T] ->
			Y;
		[_|T] ->
			f_plug(T,X);
		[] ->
			X
	end.

%Rotor process
rotor(From,C,P,Frotor,IncR,IncL,R,L) ->
	case C of
		26 ->
			I=receiver(From,IncR),
			if I==1 ->
					sender(From,IncL,1);
				true ->
					sender(From,IncL,0)
			end,
			X=receiver(From,R),
			sender(From,L,Frotor(P+I,X,true)),
			X1=receiver(From,L),
			sender(From,R,Frotor(P+I,X1,false)),
			if I==1->
					rotor(From,0,P-26,Frotor,IncR,IncL,R,L);
				true->
					rotor(From,C,P,Frotor,IncR,IncL,R,L)
			end;
		_-> I=receiver(From,IncR),
			sender(From,IncL,0),
			X=receiver(From,R),
			sender(From,L,Frotor(P+I,X,true)),
			X1=receiver(From,L),
			sender(From,R,Frotor(P+I,X1,false)),
			rotor(From,C+I,P+I,Frotor,IncR,IncL,R,L)
	end.
f_rotor(Rotor,P,X,Switch)->
	if P<$A ->
		f_rotor(Rotor,$Z-($A-P)+1,X,Switch);
		P>$Z ->
			f_rotor(Rotor,$A+(P-$Z)-1,X,Switch);
		Switch==true ->
			lists:nth(((X+P-$A-$A) rem 26)+1,Rotor);
		Switch==false ->
			$A + ((string:chr(Rotor,X)+25+$A-P) rem 26)
	end.

%Reflector process
reflector(From,In,Out,F_refl) ->
	X = receiver(From,In),
	sender(From,Out,F_refl(X)),
	reflector(From,In,Out,F_refl).
f_refl(Reflector, X)->
	case f_refl_helper(Reflector, X)=:=[] of
		true -> f_refl(tupListSwap(Reflector),X);
		false -> element(1,lists:nth(1,f_refl_helper(Reflector, X)))
	end.
f_refl_helper(Reflector, X) ->
	[Tup || Tup <- Reflector, secondElem(X,Tup)].
%Returns true when X is the second element of Tup.
secondElem(X, Tup)->
	element(2, Tup)=:=X.
%Switches the elements of every tuple in a list of tuples.
tupListSwap(TupList)->
	[{X,Y}||{Y,X}<-TupList].

%Setup an Enigma Machine
setup(Reflector,Rotors,Rings,Plugboard,Initial_Setting)->
	Ciphers=[
		{"I",unzip( rotorI())},
		{"II",unzip( rotorII())},
		{"III",unzip( rotorIII())},
		{"IV",unzip( rotorIV())},
		{"V",unzip( rotorV())},
		{"VI",unzip( rotorVI())},
		{"Beta",unzip( rotorBeta())},
		{"Gamma",unzip( rotorGamma())},
	 	{"A",reflectorA()},
		{"B",reflectorB()},
		{"C",reflectorC()},
		{"ThinB",reflectorThinB()},
		{"ThinC",reflectorThinC()}],
	{Rot1,Rot2,Rot3}=Rotors,
	Rot_Tuple={key_find(Rot1,Ciphers),key_find(Rot2,Ciphers),key_find(Rot3,Ciphers)},
	Setup_PID=self(),
	spawn(fun()->enigma_maker(Setup_PID,key_find(Reflector,Ciphers),Rot_Tuple,Rings,Plugboard,Initial_Setting)end),
	receive Enigma_PID->Enigma_PID end.
unzip(Tuple_list) ->
	[element(2,X) || X <- Tuple_list].
key_find(Key,Ls)->
	case lists:keyfind(Key,1,Ls) of
		false ->
			lists:seq($A,$Z);
		{_,L} ->
			L
	end.
enigma_maker(From,Reflector,Rotors,Rings,Plugboard,Initial_Setting)->
	Making_PID=self(),
	{R1,R2,R3}=Rotors,
	{C1,C2,C3}=Rings,
	{P1,P2,P3}=Initial_Setting,
	Frotor1=fun(P,X,R)->f_rotor(R1,P,X,R)end,
	Frotor2=fun(P,X,R)->f_rotor(R2,P,X,R)end,
	Frotor3=fun(P,X,R)->f_rotor(R3,P,X,R)end,
	From!spawn_link(fun()->keyboard(Making_PID,i1,keys,keys)end),
	spawn_link(fun()->plugboard(Making_PID,Plugboard,keys,m3)end),
	spawn_link(fun()->rotor(Making_PID,C1,P1,Frotor1,i1,i2,m3,m2)end),
	spawn_link(fun()->rotor(Making_PID,C2,P2,Frotor2,i2,i3,m2,m1)end),
	spawn_link(fun()->rotor(Making_PID,C3,P3,Frotor3,i3,final_refl,m1,ref)end),
	spawn_link(fun()->reflector(Making_PID,ref,ref,fun(X)->f_refl(Reflector,X)end)end),
	send_receive([],[]).
send_receive(List1, List2) ->
	receive
		{snd,Channel,Value,Sender} ->
			case lists:keyfind(Channel,1,List1) of
				false ->
					send_receive(List1,[{Channel,Value,Sender}|List2]);
				{Channel,Reciever} ->
					Reciever!{rcv,Channel,Value},
					Sender!{ok,{snd,Channel,Value}},
					send_receive(lists:keydelete(Channel,1,List1),List2)
			end;

		{rcv,Channel,Reciever} ->
			case lists:keyfind(Channel,1,List2) of
				false ->
					send_receive([{Channel,Reciever}|List1],List2);
				{Channel,Value,Sender} ->
					Reciever!{rcv,Channel,Value},
					Sender!{ok,{snd,Channel,Value}},
					send_receive(List1,lists:keydelete(Channel,1,List2))
			end
	end.

%Encrypt using an Enigma Machine
crypt(Enigma_Machine,Unencrypted) ->
	Majuscule=[X || X <-  string:uppercase(Unencrypted), (X>=$A) and (X=<$Z)],
	crypt_helper(Enigma_Machine,Majuscule,"").
crypt_helper(Enigma_Machine,Unencrypted,Crypted)->
	case Unencrypted of
		[] ->
			lists:reverse(Crypted);
		[H|T] ->
			Enigma_Machine!{x,H,self()},
			receive
				X ->
					crypt_helper(Enigma_Machine,T,[X|Crypted])
			end
	end.

%Kill an Enigma Machine process.
kill(Enigma_Machine)->
	Enigma_Machine!{cmd,stop},
	ok.
