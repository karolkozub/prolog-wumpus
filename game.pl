% dynamic
:- dynamic gameover/0.
:- dynamic room_has/2.
% main
main :-
	init_game,
	show_message(startup), nl,
	describe_situation, nl,
	game_loop.

init_game :-
	assert(room_has(7, 'live wumpus')),
	assert(room_has(18, gold)),
	assert(room_has(3, arrow)),
	assert(room_has(14, arrow)),
	assert(player_position(0)),
	assert(player_arrows(3)),
	assert(player_points(1000)).

game_loop:-
	repeat,
	game_step,
	(gameover, show_message(summary), !; fail).

game_step :-
	get_command(X),nl,nl,
	do(X),nl.

% actions

do(take(gold)) :- take(gold),!.
do(take(arrow)) :- take(arrow),!.
do(shoot(X)) :- shoot(X),!.
do(move(X)) :- move(X),!.
do(exit) :- exit,!.
do(help) :- help,!.
do(X) :- unknown_command(X),!.

take(gold) :-
	player_position(X),
	room_has(X, gold),
	add_player_points(1000),
	room_remove(X, gold),
	show_message('take gold'),
	describe_situation.

take(gold) :-
	show_message('room doesnt contain gold'),
	describe_situation.

take(arrow) :-
	player_position(X),
	room_has(X, arrow),
	add_player_points(10),
	add_player_arrow,
	room_remove(X, arrow),
 	show_message('take arrow'),
	describe_situation.

take(arrow) :-
	show_message('room doesnt have arrows'),
	describe_situation.

move(X) :-
	player_position(P),
	rooms_are(P, X, connected),
	room_has(X, 'live wumpus'),
	show_message('eaten by wumpus'),
	die.

move(X) :-
	player_position(P),
	rooms_are(P, X, connected),
	room_has(X, pit),
	show_message('fall into a pit'),
	die.

move(X) :-
	player_position(P),
	rooms_are(P, X, connected),
	remove_player_points(1),
	set_player_position(X),
	show_message('move', X),
	describe_situation.

move(_) :-
	show_message('cannot move there'),
	describe_situation.

shoot(X) :-
	player_position(P),
	rooms_are(P, X, connected),
	player_has_arrows,
	room_has(X, 'live wumpus'),
	remove_player_points(10),
	remove_player_arrow,
	kill_wumpus(X),
	add_player_points(1000),
	show_message('kill wumpus'),
	describe_situation.
	
shoot(X) :-
	player_position(P),
	rooms_are(P, X, connected),
	player_has_arrows,
	remove_player_points(10),
	remove_player_arrow,
	show_message('wasted arrow'),
	describe_situation.
	
shoot(_) :-
	player_has_arrows,
	show_message('cannot shoot there'),
	describe_situation.
	
shoot(_) :-
	show_message('no more arrows'),
	describe_situation.

exit :-
	show_message(exit),
	end_game.

help :-
	show_message(help).

unknown_command(X) :-
	show_message(unknown(X)).

die :-
	show_message(death),
	remove_player_points(1000),
	end_game.

end_game :-
	assert(gameover).

% descriptions
describe('glitter') :-
	player_position(X),
	room_has(X, glitter),!,
	write('You see glitter. The gold is near'), nl.

describe('glitter') :-
	true.

describe('gold') :-
	player_position(X),
	room_has(X, gold),!,
	write('You see a pot of gold.'), nl.

describe('gold') :-
	true.

describe('breeze') :-
	player_position(X),
	room_has(X, breeze),!,
	write('You feel breeze. There is a pit nearby.'), nl.

describe('breeze') :-
	true.

describe('stench') :-
	player_position(X),
	room_has(X, stench),!,
	write('You smell stench. The wumpus is near.'), nl.

describe('stench') :-
	true.

describe('dead wumpus') :-
	player_position(X),
	room_has(X, 'dead wumpus'),!,
	write('You see a dead wumpus. It stinks.'), nl.

describe('dead wumpus') :-
	true.

describe('arrow') :-
	player_position(X),
	room_has(X, arrow),!,
	write('You see an arrow. It could be useful.'), nl.

describe('arrow') :-
	true.

describe('can go to') :-
	player_position(Position),
	write('You can go to rooms '), 
	rooms_are(Position, CanGoTo, connected),
	write(CanGoTo), write(' '), fail.

describe('can go to') :-
	nl, true.

% nie wiem dlaczego to nie dziala

describe_all :-
	describe(_),
	fail.

describe_all :-
	true.

describe_situation :-
	player_position(Position),
	player_arrows(Arrows),
	player_points(Points),
	nl,
	write('You are in room '), write(Position), nl,
	write('You have '), write(Arrows), write(' arrows'), nl,
	write('You have '), write(Points), write(' points'), nl,
	describe(glitter),
	describe(gold),
	describe(breeze),
	describe(stench),
	describe('dead wumpus'),
	describe(arrow),
	describe('can go to').

% rules
rooms_are(X, Y, connected) :-
	connection(X, Y).

rooms_are(X, Y, connected) :-
	connection(Y, X).

room_has(X, glitter) :-
	rooms_are(X, Y, connected),
	room_has(Y, gold).

room_has(X, breeze) :-
	rooms_are(X, Y, connected),
	room_has(Y, pit).
	
room_has(X, stench) :-
	rooms_are(X, Y, connected),
	room_has(Y, wumpus).

room_has(X, wumpus) :-
	room_has(X, 'live wumpus').
room_has(X, wumpus) :-
	room_has(X, 'dead wumpus').

% procedures
set_player_position(X) :-
	retract(player_position(_)),
   	assert(player_position(X)).

set_player_arrows(X) :-
	retract(player_arrows(_)),
	assert(player_arrows(X)).

add_player_arrow :-
	player_arrows(X),
	Y is X + 1,
	set_player_arrows(Y).

remove_player_arrow :-
	player_arrows(X),
	Y is X - 1,
	set_player_arrows(Y).

player_has_arrows :-
	player_arrows(X),
	X > 0.
    
set_player_points(X) :-
	retract(player_points(_)),
	assert(player_points(X)).

add_player_points(P) :-
	player_points(X),
	Y is X + P,
	set_player_points(Y).

remove_player_points(P) :-
	player_points(X),
	Y is X - P,
	set_player_points(Y).	

kill_wumpus(X) :-
	retract(room_has(X, 'live wumpus')),
	assert(room_has(X, 'dead wumpus')).

room_remove(X, gold) :-
	retract(room_has(X, gold)).

room_remove(X, arrow) :-
	retract(room_has(X, arrow)).

% messages

show_message('move', X) :-
	write('You move to room '), write(X), nl.

show_message('cannot move there') :-
	write('You cannot move there'), nl.

show_message('room doesnt have arrows') :-
	write('There are no arrows here'), nl.

show_message('wasted arrow') :-
	write('You wasted an arrow'), nl.
	
show_message('cannot shoot there') :-
	write('You cannot shoot there'), nl.
	
show_message('no more arrows') :-
	write('You have no more arrows'), nl.

show_message(summary) :-
	player_points(Points),
	write('You have collected '), write(Points), write(' points'), nl.

show_message('eaten by wumpus') :- 
	write('The wumpus devours you'), nl.

show_message(exit) :-
	write('You exit the labirynth. You are happy to be alive.'), nl.

show_message('take gold') :-
	write('You take the gold'), nl.

show_message('room doesnt contain gold') :-
	write('There is no gold here'), nl.

show_message(help) :-
	write('Valid commands are:'), nl,
	write('  move X     - move to the adjacent room X'), nl,
	write('  shoot X    - shoot an arrow to adjacent room X'), nl,
	write('  take gold  - take the gold from the ground'), nl,
	write('  take arrow - take an arrow from the ground'), nl,
	write('  exit       - exit the game'), nl,
	write('  help       - show this help message'), nl.

show_message('fall into a pit') :-
	write('You fall into a pit'),nl.

show_message(unknown(X)) :-
	write('Invalid command '), write(X), nl,
	write('Type help for the list of valid commands'),nl.

show_message(death) :-
	write('You die'), nl.

show_message(startup) :-
	write('Welcome to "Hunt the Wumpus in Prolog"'), nl,
	write('This game was created by Karol Kozub'), nl,
	write('Enjoy the hunt'), nl.

show_message('kill wumpus') :-
	write('You have shot and killed the wumpus'), nl.
	
show_message('take arrow') :-
	write('You pick up an arrow'), nl.

% input

respond([]):-
  write('.'),nl,nl.
respond([H|T]):-
  write(H),
  respond(T).

get_command(C):-
  readlist(X),    
  C =.. X,!.      
get_command(_):-
  respond(['I don''t understand, try again or type help']),fail.

readlist(L):-
  write('> '),
  read_word_list(L).

getech(C) :- get_single_char(C), put(C), ttyflush.

read_word_list([W|Ws]) :-
  getech(C), 
  readword(C, W, C1),       % Read word starting with C, C1 is first new
  restsent(C1, Ws), !.      % character - use it to get rest of sentence

restsent(C,[]) :- lastword(C), !. % Nothing left if hit last-word marker
restsent(C,[W1|Ws]) :-
  readword(C,W1,C1),        % Else read next word and rest of sentence
  restsent(C1,Ws).

readword(C,W,C1) :-         % Some words are single characters
  single_char(C),           % i.e. punctuation
  !, 
  name(W, [C]),             % get as an atom
  getech(C1).
readword(C, W, C1) :-
  is_num(C),                % if we have a number --
  !,
  number_word(C, W, C1, _). % convert it to a genuine number
readword(C,W,C2) :-         % otherwise if character does not
  in_word(C, NewC),         % delineate end of word - keep
  getech(C1),                 % accumulating them until 
  restword(C1,Cs,C2),       % we have all the word     
  name(W, [NewC|Cs]).       % then make it an atom
readword(_,W,C2) :-         % otherwise
  getech(C1),       
  readword(C1,W,C2).        % start a new word

restword(C, [NewC|Cs], C2) :-
  in_word(C, NewC),
  getech(C1),
  restword(C1, Cs, C2).
restword(C, [], C).


single_char(44).
single_char(59).
single_char(58).
single_char(63).
single_char(33).
single_char(46).


in_word(C, C) :- C >= 97, C =< 122.
in_word(C, L) :- C >= 65, C =< 90, L is C + 32.
in_word(39,39).
in_word(45,45).

number_word(C, W, C1, Pow10) :- 
  is_num(C),
  !,
  get0(C2),
  number_word(C2, W1, C1, P10),
  Pow10 is P10 * 10,
  W is integer(((C - 48) * Pow10) + W1).
number_word(C, 0, C, 0.1).


is_num(C) :-
  C =< 57,
  C >= 48.

lastword(10).
lastword(13).
lastword(46).
lastword(33).
lastword(63).

% facts

connection(0, 1).
connection(0, 4).
connection(0, 5).
connection(1, 2).
connection(1, 6).
connection(2, 3).
connection(2, 7).
connection(3, 4).
connection(3, 8).
connection(4, 9).
connection(5, 10).
connection(5, 11).
connection(6, 11).
connection(6, 12).
connection(7, 12).
connection(7, 13).
connection(8, 13).
connection(8, 14).
connection(9, 10).
connection(9, 14).
connection(10, 15).
connection(11, 16).
connection(12, 17).
connection(13, 18).
connection(14, 19).
connection(15, 16).
connection(15, 19).
connection(16, 17).
connection(17, 18).
connection(18, 19).

room_has(2, pit).
room_has(6, pit).
room_has(8, pit).
room_has(9, pit).
room_has(15, pit).

