% agent.pl

% BDI Agent

% COMP9814 Artificial Intelligence, UNSW, Assignment3
% Group member 1: z5181211 Haixin CHEN
% Group member 2: z5140181 Han YANG

% This agent program works with land.pl and gridworld.pl.

% initial_intentions(Intentions)
% 	Intentions need not be instantiated.
% 	This function is implemented by finding the position of monster and then,
% 	calling the function find_shortest_path(S,Start_X, Start_Y,Goal_X,Goal_Y), and finally,
%	return the Intentions in the form of intents([[goal(X1,Y1),[]], ... , [goal(Xn,Yn),[]]],[]).
initial_intentions(Intentions) :-
	monster(X, Y),
	find_shortest_path(Result, 1, 1, X, Y, _),
	Intentions = intents(Result, []), !.

% find_shortest_path(Plan, Start_X, Start_Y, Goal_X, Goal_Y, G)
%	Start_X, Start_Y, Goal_X and Goal_Y need to be instantiated, S need not be instantiated.
%	Start_X and Start_Y are both set 1, since we always start the game at position (1,1),
%	the value of the Goal_X and Goal_Y is determined by the position of the goal.
%	
%	This function is implemented by calling the function solve1((Start_X, Start_Y), List, G, _),
%	that is to find the path with minimum number of water, G is the number of water, List is the path.
%	And we call the function get_water(Plan1, Plan) to extra the points of water from the list(Plan1).
find_shortest_path(Plan, Start_X, Start_Y, Goal_X, Goal_Y, G) :-
	assert(goal((Goal_X, Goal_Y))),
	solve1((Start_X, Start_Y), [(X, Y) | T], G, _),!,
	retractall(goal((_, _))),
	reverse([(X, Y) | T], Plan1),
	get_water(Plan1, Plan).

% get_water(Plan1, Plan)
%	Plan1 need to be instantiated, Plan need not be instantiated.
get_water([], []).
get_water([(X, Y) | T], Plan) :-
	land_or_dropped(X, Y),
	get_water(T, Plan).
get_water([(X, Y) | T], Plan) :-
	not(land_or_dropped(X, Y)),
	get_water(T, Temp),
	append([[goal(X, Y), []]], Temp, Plan).


% The code below is from pathsearch.pl.

insert_legs1(Generated, [], Generated).
insert_legs1(Generated, [Leg | Legs], Generated2) :-
	insert_one_leg1(Generated, Leg, Generated1),
	insert_legs1(Generated1, Legs, Generated2).

head_member1(Node, [[Node,_] | _]).
head_member1(Node, [_ | Tail]) :-
	head_member1(Node, Tail).

build_path1([[Next, Start], [Start, Start]], [Next, Start]).
build_path1([[C, B],[B, A] | Expanded],[C, B, A | Path]) :-
	build_path1([[B, A] | Expanded],[B, A | Path]), !.
build_path1([Leg, _SkipLeg | Expanded], Path) :-
	build_path1([Leg | Expanded], Path).


% s(Node, SuccessorNode, Cost)
%	Node need to be instantiated, but SuccessorNode and Cost need not be instantiated.
% We have modified this part of code to fit our environment.
% If the new node is water.
s1((X, Y), (X1, Y), 1) :-        % Arc costs are 1
	X1 is X + 1,
	not(land_or_dropped(X1, Y)).
s1((X, Y), (X1, Y), 1) :-        % Arc costs are 1
	X1 is X - 1,
	not(land_or_dropped(X1, Y)).
s1((X, Y), (X, Y1), 1) :-        % Arc costs are 1
	Y1 is Y + 1,
	not(land_or_dropped(X, Y1)).
s1((X, Y), (X, Y1), 1) :-        % Arc costs are 1
	Y1 is Y - 1,
	not(land_or_dropped(X, Y1)).

% If the node is land or dropped.
s1((X, Y), (X1, Y), 0) :-        % Arc costs are 0
	X1 is X + 1,
	land_or_dropped(X1, Y).
s1((X, Y), (X1, Y), 0) :-        % Arc costs are 0
	X1 is X - 1,
	land_or_dropped(X1, Y).
s1((X, Y), (X, Y1), 0) :-        % Arc costs are 0
	Y1 is Y + 1,
	land_or_dropped(X, Y1).
s1((X, Y), (X, Y1), 0) :-        % Arc costs are 0
	Y1 is Y - 1,
	land_or_dropped(X, Y1).

% The code below is from ucsdijkstar.pl.

solve1(Start, Solution, G, N)  :-
    ucsdijkstra1([[Start, Start, 0]], [], Solution, G, 1, N).

ucsdijkstra1([[Node, Pred, G] | _Generated], Expanded, Path, G, N, N)  :-
    goal(Node),
    build_path1([[Node, Pred] | Expanded], Path).
ucsdijkstra1([[Node, Pred, G] | Generated], Expanded, Solution, G1, L, N) :-
    extend1(Node, G, Expanded, NewLegs),
    M is L + 1,
    insert_legs1(Generated, NewLegs, Generated1),
    ucsdijkstra1(Generated1, [[Node, Pred] | Expanded], Solution, G1, M, N).

extend1(Node, G, Expanded, NewLegs) :-
    % write(Node),nl,   % print nodes as they are expanded
    findall([NewNode, Node, G1], (s1(Node, NewNode, C), not(head_member1(NewNode, Expanded)), G1 is G + C), NewLegs).

insert_one_leg1([], Leg, [Leg]).
insert_one_leg1([Leg1 | Generated], Leg, [Leg1 | Generated]) :-
    Leg  = [Node,_Pred, G ],
    Leg1 = [Node,_Pred1,G1],
    G >= G1, ! .
insert_one_leg1([Leg1 | Generated], Leg, [Leg, Leg1 | Generated]) :-
    Leg  = [_Node, _Pred, G ],
    Leg1 = [_Node1,_Pred1,G1],
    G < G1, ! .
insert_one_leg1([Leg1 | Generated], Leg, [Leg1 | Generated1]) :-
    insert_one_leg1(Generated, Leg, Generated1).

% trigger(Percepts, Goals)
% 	Percepts need to be instantiated, but Goals need not be instantiated.
% 	This function is to convert the form of stone(X,Y) to the form of goal(X,Y).

% base case
trigger([], []).

% recursive branch
trigger([stone(X, Y) | Tail1], [goal(X, Y) | Tail2]) :-
	trigger(Tail1, Tail2).
	
% incorporate_goals(Goals, Intentions, Intentions1)
% 	Goals and Intentions need to be instantiated, but Intentions1 need not be instantiated.
%	Goals in the form of a list [goal(X1,Y1), ... , goal(Xn,Yn)].
%	Intentions and Intentions1 are in the form intents(Int_drop,Int_pick),
%	where Int_drop, Int_pick are lists of intentions in the form [goal(X,Y), Plan].
%	This function is to find the accessible goals from the goal list,
%	then insert them into Int_pick in the decreasing order of manhattan distance.

% If the goal list is empty, Intentions1 is equal to Intentions. 
incorporate_goals([], Intentions, Intentions).

% If the gaol list is not empty, we call inner_incorporate_goals() to find all accessible goals,
% then we call find_manhattan() to calculate the number of steps between current position and goal position,
% finally, we sort the Int_pick list in decreasing order of manhattan distance.
incorporate_goals(Goals, intents(Drop1, Pick1), intents(Drop1, Pick2)) :-
	inner_incorporate_goals(Goals, Pick1, Pick3),!,
	find_manhattan(Pick3),
	sorted_stone(Pick2),!.

% inner_incorporate_goals(Goals, Intents_pick, Intents_pick1)
%	Goals and Intents_pick need to be instantiated, but Intents_pick1 need not be instantiated.
% 	This function calls find_shortest_path() to check whether this goal is accessible.

% base case
inner_incorporate_goals([], Intents_Pick, Intents_Pick).

% recursive branch

% We call the function find_shortest_path(_, Start_X, Start_Y, X, Y, G) that we have used in initial_intentions
% G is the number of water between the goal and current point, if G is 0, it means this goal is accessible.
% If the goal is not in the Intents_Pick list and is accessible, we add it into Intents_Pick1.
inner_incorporate_goals([goal(X, Y) | T1], Intents_Pick, Intents_Pick1) :-
	not(member([goal(X, Y), _], Intents_Pick)),
	agent_at(Start_X, Start_Y),
	find_shortest_path(_, Start_X, Start_Y, X, Y, G),
	G =:= 0,
	inner_incorporate_goals(T1, Intents_Pick, Temp),
	append([[goal(X, Y), []]], Temp, Intents_Pick1).

% If the goal is not accessible, we ignore it.
inner_incorporate_goals([goal(X, Y) | T1], Intents_Pick, Intents_Pick1) :-
	not(member([goal(X, Y), _], Intents_Pick)),
	agent_at(Start_X, Start_Y),
	find_shortest_path(_, Start_X, Start_Y, X, Y, G),
	G =\= 0,
	inner_incorporate_goals(T1, Intents_Pick, Intents_Pick1).
	
% If the goal is already in the Intents_Pick list, we ignore it.
inner_incorporate_goals([goal(X, Y) | T1], Intents_Pick, Intents_Pick1) :-
	member([goal(X, Y), _], Intents_Pick),
	inner_incorporate_goals(T1, Intents_Pick, Intents_Pick1).

% base case
find_manhattan([]) :-
	retractall(goal_D(_, _)).

% recursive branch
% In this function, we call the function get_shortest_path(_, Start_X, Start_Y, X, Y, D1)
% to calculate the number of steps between current position and goal position.
find_manhattan([[goal(X, Y), Plan] | T]) :-
	agent_at(Start_X, Start_Y),
	distance((Start_X, Start_Y), (X, Y), D),
	D =\= 0,
	get_shortest_path(_, Start_X, Start_Y, X, Y, D1),
	find_manhattan(T),
	assert(goal_D(D1, [goal(X, Y), Plan])).

find_manhattan([[goal(X, Y), Plan] | T]) :-
	agent_at(Start_X, Start_Y),
	distance((Start_X, Start_Y), (X, Y), D),
	D =:= 0,
	find_manhattan(T),
	assert(goal_D(D, [goal(X, Y), Plan])).

sorted_stone(Pick_List) :-
	findall(D-Goal, goal_D(D, Goal), List),
	keysort(List, Pairs),
  	pairs_values(Pairs, Pick_List).


% get_action(Intentions, Intentions1, Action)
%	Intentions need to be instantiated, but Intentions1 and Action need not be instantiated.
%	This function is to make plans for the first goal in Pick_list or Drop_list.
%	In this function we have used the path search code in pathsearch.pl and ucsdijkstra.pl to find shortest path to the goal.

% Pick case
get_action(intents(Drop_list, Pick_list), intents(Drop_list, Pick_list1), Action) :-
	agent_stones(0),
	inner_get_action(Pick_list, Pick_list1, Action), !.

% Drop case
get_action(intents(Drop_list, Pick_list), intents(Drop_list1, Pick_list), Action) :-
	agent_stones(1),
	inner_get_action(Drop_list, Drop_list1, Action), !.

% If there is no goal in the list, we stay at the current position.
inner_get_action([], [], Action) :-
	agent_at(X, Y),
	Action = move(X, Y).

% If the agent is in the same place as the stone, we step to the side, and plan to pick it up.
inner_get_action([[goal(X, Y), _] | Tail], [[goal(X, Y), Plan1] | Tail], Action) :-
	agent_at(X1, Y1),
	distance((X, Y), (X1, Y1), D),
	D = 0,
	X2 is X1 - 1,
	land_or_dropped(X2, Y1),
	Plan1 = [pick(X1, Y1)],
	Action = move(X2, Y1),!.
inner_get_action([[goal(X, Y), _] | Tail], [[goal(X, Y), Plan1] | Tail], Action) :-
	agent_at(X1, Y1),
	distance((X, Y), (X1, Y1), D),
	D = 0,
	X2 is X1 + 1,
	land_or_dropped(X2, Y1),
	Plan1 = [pick(X1, Y1)],
	Action = move(X2, Y1),!.
inner_get_action([[goal(X, Y), _] | Tail], [[goal(X, Y), Plan1] | Tail], Action) :-
	agent_at(X1, Y1),
	distance((X, Y), (X1, Y1), D),
	D = 0,
	Y2 is Y1 - 1,
	land_or_dropped(X1, Y2),
	Plan1 = [pick(X1, Y1)],
	Action = move(X1, Y2),!.
inner_get_action([[goal(X, Y), _] | Tail], [[goal(X, Y), Plan1] | Tail], Action) :-
	agent_at(X1, Y1),
	distance((X, Y), (X1, Y1), D),
	D = 0,
	Y2 is Y1 + 1,
	land_or_dropped(X1, Y2),
	Plan1 = [pick(X1, Y1)],
	Action = move(X1, Y2),!.

% Otherwise we make plan for the first goal in the list.
inner_get_action([[goal(X, Y), _] | Tail], [[goal(X, Y), Plan1] | Tail], Action) :-
	agent_at(X1, Y1),
	get_shortest_path([_, Action | Plan1], X1, Y1, X, Y, _).
	
% get_shortest_path(Plan, Start_X, Start_Y, Goal_X, Goal_Y)
% In this function we call path search function solve().

% Pick case
get_shortest_path(Plan, Start_X, Start_Y, Goal_X, Goal_Y, G) :-
	assert(goal((Goal_X, Goal_Y))),
	agent_stones(0),
	solve((Start_X, Start_Y), [(X, Y) | T], G, _),
	retractall(goal((_, _))),
	convert_path(T, P),
	reverse([pick(X, Y) | P], Plan).

% Drop case
get_shortest_path(Plan, Start_X, Start_Y, Goal_X, Goal_Y, G) :-
	assert(goal((Goal_X, Goal_Y))),
	agent_stones(1),
	solve((Start_X, Start_Y), [(X, Y) | T], G, _),
	retractall(goal((_, _))),
	convert_path(T, P),
	reverse([drop(X, Y) | P], Plan).

convert_path([], []).
convert_path([(X, Y) | T], Result) :-
	convert_path(T, Temp),
	append([move(X, Y)], Temp, Result).
	
reverse([], []). 
reverse([Item], [Item]).
reverse([Item | T], R) :-
	reverse(T, Temp), 
	append(Temp, [Item], R). 

% The code below is from pathsearch.pl.

insert_legs(Generated, [], Generated).
insert_legs(Generated, [Leg | Legs], Generated2) :-
	insert_one_leg(Generated, Leg, Generated1),
	insert_legs(Generated1, Legs, Generated2).

head_member(Node, [[Node,_] | _]).
head_member(Node, [_ | Tail]) :-
	head_member(Node, Tail).

build_path([[Next, Start], [Start, Start]], [Next, Start]).
build_path([[C, B],[B, A] | Expanded],[C, B, A | Path]) :-
	build_path([[B, A] | Expanded],[B, A | Path]), !.
build_path([Leg, _SkipLeg | Expanded], Path) :-
	build_path([Leg | Expanded], Path).


% s(Node, SuccessorNode, Cost)
%	Node need to be instantiated, but SuccessorNode and Cost need not be instantiated.
% We have modified this part of code to fit our environment.
% If the new node is goal, we return it directly.
s((X, Y), (X1, Y), 1) :-        % All arc costs are 1
	X1 is X + 1,
	goal((X1, Y)).
s((X, Y), (X1, Y), 1) :-        % All arc costs are 1
	X1 is X - 1,
	goal((X1, Y)).
s((X, Y), (X, Y1), 1) :-        % All arc costs are 1
	Y1 is Y + 1,
	goal((X, Y1)).
s((X, Y), (X, Y1), 1) :-        % All arc costs are 1
	Y1 is Y - 1,
	goal((X, Y1)).

% If the node is land or dropped, we return it.
s((X, Y), (X1, Y), 1) :-        % All arc costs are 1
	X1 is X + 1,
	land_or_dropped(X1, Y).
s((X, Y), (X1, Y), 1) :-        % All arc costs are 1
	X1 is X - 1,
	land_or_dropped(X1, Y).
s((X, Y), (X, Y1), 1) :-        % All arc costs are 1
	Y1 is Y + 1,
	land_or_dropped(X, Y1).
s((X, Y), (X, Y1), 1) :-        % All arc costs are 1
	Y1 is Y - 1,
	land_or_dropped(X, Y1).

% The code below is from ucsdijkstar.pl.
solve(Start, Solution, G, N)  :-
    ucsdijkstra([[Start, Start, 0]], [], Solution, G, 1, N).

ucsdijkstra([[Node, Pred, G] | _Generated], Expanded, Path, G, N, N)  :-
    goal(Node),
    build_path([[Node, Pred] | Expanded], Path).
ucsdijkstra([[Node, Pred, G] | Generated], Expanded, Solution, G1, L, N) :-
    extend(Node, G, Expanded, NewLegs),
    M is L + 1,
    insert_legs(Generated, NewLegs, Generated1),
    ucsdijkstra(Generated1, [[Node, Pred] | Expanded], Solution, G1, M, N).

extend(Node, G, Expanded, NewLegs) :-
    % write(Node),nl,   % print nodes as they are expanded
    findall([NewNode, Node, G1], (s(Node, NewNode, C), not(head_member(NewNode, Expanded)), G1 is G + C), NewLegs).

insert_one_leg([], Leg, [Leg]).
insert_one_leg([Leg1 | Generated], Leg, [Leg1 | Generated]) :-
    Leg  = [Node,_Pred, G ],
    Leg1 = [Node,_Pred1,G1],
    G >= G1, ! .
insert_one_leg([Leg1 | Generated], Leg, [Leg, Leg1 | Generated]) :-
    Leg  = [_Node, _Pred, G ],
    Leg1 = [_Node1,_Pred1,G1],
    G < G1, ! .
insert_one_leg([Leg1 | Generated], Leg, [Leg1 | Generated1]) :-
    insert_one_leg(Generated, Leg, Generated1).

% update_intentions(Observation, Intentions, Intentions1)
%	Observation and Intentions need to be instantiated, but Intentions1 need not be instantiated.
%	This function is to update the agent's intentions, based on observation.

% An at(X, Y) observation should not change the agent's intentions.
update_intentions(at(_, _), Intentions, Intentions).

% In the case of a picked() or dropped() observation, 
% the agent should remove the corresponding plan from its list of intentions.
update_intentions(picked(_, _), intents(Drop, [_ | T]), intents(Drop, T)).
update_intentions(dropped(_, _), intents([_ | T], Pick), intents(T, Pick)).