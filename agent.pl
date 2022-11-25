https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
% the structure of intents
% intentions should be a list like intents([[goal(5,3),[]],[goal(7,6),[]],[goal(8,6),[]],[goal(9,9),[]]],[])

initial_intentions(Intentions).

Path(From, To):-
    traverse(From),
    edge([To|ReversedPath], Dist)-> % if edge() is true, attempt the condition before ; otherwise, attempt the one after;
        reverse([To|ReversedPath], Path),   % path is a list of nodes that will be visited from start to end   
        Distance is Dist, % the length of the path
        writef('Path ~w and the distance is ~w = ~w ~n',
           [Path, Dist, Distance]);
        writef('There is no path that connects %w with %w\n', [From, To]). % there is no path from the start to the goal

% ----------------------traverse the nodes-------------------------
traverse(From):-
    retractall((edge(_,_))),
    traverse(From,[],0) % begin to construct the path, start form the origin node.

traverse(From, Path, Dist):-
    path(From, T, Dist), % path for each neighbour T
    not(membercheck(T, Path)), % check if T has been vistied before, if not, T might be in the path
    shorterPath([T, From[Path]], Dist+D), % if there is no shorterpath, create an edge(Path, Dist), if it is exists, update it
    traverse(T,[From|Path], Dist+D). % recursion to do the same thing to node T, find all neighbors 

traverse(_).


trigger(stone(X,Y)|Percepts, goal(X,Y)|Goals)：-
    trigger(Percepts, Goals).


incorporate_goals(Goals, Intentions, Intentions1):-
% ----- Part Two --------------------------------------------------------------------------
%

% incorporate_goals(Goals, Beliefs, Intentions, Intentions1)
%
% Do incorporate_goals_process for two parts : restaurant and truffle
% Insert goals which in Goals_rest and Goals_truff into corresponding part(Int_sell and Int_pick)
%
incorporate_goals(goals(Goals_rest,Goals_truff), Beliefs, intents(Int_sell,Int_pick), intents(Int_sell1, Int_pick1)) :-
    incorporate_goals_process(Goals_rest, Beliefs, Int_sell, Int_sell1),
    incorporate_goals_process(Goals_truff, Beliefs, Int_pick, Int_pick1).



% incorporate_goals_process(Goal, Beliefs, Int, Int1)
%
% Insert goals in Goal into Int in decreasing order of S, S is the value in goal(X, Y, S)
%

% base case, Goal is empty
incorporate_goals_process([], _, Int, Int).

% if goal already in Int, skip it.
% using the helper function "check"
incorporate_goals_process([H|T], Beliefs, Int, Int1) :-
    check(H, Int),
    incorporate_goals_process(T, Beliefs, Int, Int1).

% insert goal if its not in Int_sell
% using the helper function "check"
% using the helper function "insert"
incorporate_goals_process([H|T], Beliefs, Int, Int1) :-
    not(check(H, Int)),
    insert(H, Beliefs, Int, Int_new),
    incorporate_goals_process(T, Beliefs, Int_new, Int1).



% check(Goal, Intentions)
%
% It is a helper function for checking if Goal is in Intentions.
% return true if Goal is in Intentions, return false if not.
% Goal is in form of goal(X, Y, S),it is member of Goals_rest or Goals_truff
% Intentions is in form of [goal(X,Y,S), Plan], it is member of Int_sell or Int_pick
%

check(Goal, [H|_]) :-
    member(Goal, H).

check(Goal, [H|T]) :-
    not(member(Goal, H)),
    check(Goal, T).



% insert(Goal, Beliefs, Int, Int1)
%
% Int is one of Int_sell and Int_pick
% It is a helper function for inserting Goal into Int in decreasing order of S (Goal = goal(X, Y, S))
% using helper function "compare_goal"
%

insert(Goal, Beliefs, [Int_H|Int_old], [Int_H|Int_new]) :-
    not(compare_goal(Goal, Int_H, Beliefs)), !,
    insert(Goal, Beliefs, Int_old, Int_new).

insert(Goal, _, Int, [[Goal, []]|Int]).



% compare_goal(Goal, Int_element, Beliefs)
%
% It is a helper function for determining the order of two goals
% Goal in form of goal(X1, Y1, S1)
% Int_element is element in Int, in form of [goal(X2, Y2, S2), Plan]
% Beliefs is in form of (at(X,Y),stock(T))
% compare goal(X1, Y1, S1) with goal(X2, Y2, S2)
% return true if S1 > S2
% return true if S1 = S2 and (|X - X1| + |Y - Y1|) < (|X - X2| + |Y - Y2|)
%

compare_goal(goal(_, _, S1), [goal(_, _, S2), _], _) :-
    S1 > S2.

compare_goal(goal(X1, Y1 , S1), [goal(X2, Y2, S2), _], (at(X,Y), _)) :-
    S1 = S2,
    D1 is abs(X - X1) + abs(Y - Y1),
    D2 is abs(X - X2) + abs(Y - Y2),
    D1 < D2.


% --------- Part Three ---------------------------------------------------------------------------------------



get_action(Intentions, Intentions1, Action):-
% get_action(Beliefs, Intentions, Intentions1, Action)
%

% if Intentions empty, stay at current location.
get_action(beliefs(at(X,Y),_), intents([], []), intents([], []), move(X, Y)).

% if Int_pick is empty and Int_sell not satisfies the property S ≤ T, stay at current location.
get_action(beliefs(at(X,Y),stock(T)), intents([H_sell|T_sell], []), intents([H_sell|T_sell], []), move(X, Y)) :-
    not(compare_truffle(stock(T), H_sell)).

% if Int_sell satisfies the property S ≤ T, get action from sell
get_action(beliefs(at(X,Y),stock(T)), intents([H_sell|T_sell], Int_pick), intents([H2_sell|T_sell], Int_pick), Action) :-
    compare_truffle(stock(T), H_sell),
    select_action_sell(beliefs(at(X,Y),stock(T)), H_sell, H2_sell, Action).

% if Int_pick is not empty and Int_sell not satisfies the property S ≤ T, get action from pick
get_action(beliefs(at(X,Y),stock(T)), intents([H_sell|T_sell], [H_pick|T_pick]), intents([H_sell|T_sell], [H2_pick|T_pick]), Action) :-
    not(compare_truffle(stock(T), H_sell)),
    select_action_pick(beliefs(at(X,Y),stock(T)), H_pick, H2_pick, Action).

% if Int_sell is empty and Int_pick is not empty, get action from pick
get_action(beliefs(at(X,Y),stock(T)), intents([], [H_pick|T_pick]), intents([], [H2_pick|T_pick]), Action) :-
    select_action_pick(beliefs(at(X,Y),stock(T)), H_pick, H2_pick, Action).




% compare_truffle(stock(T), Int_sell_element)
%
% check if the number of truffles in the agents stock is greater than
% or equal to the number of truffles that the restaurant wants to buy
%
compare_truffle(stock(T), [goal(_,_,S), _]) :-
    T >= S.



% select_action_sell(Beliefs, Int, Int1, Action).
%
% get action from sell
%

% if the first action in sell is applicable.
select_action_sell(Beliefs, Int, [Goal, Action_T], Action) :-
    split_goal_plan(Int, Goal, [Action|Action_T]),
    applicable(Beliefs, Action).

% if the first action in sell is not applicable.
select_action_sell(Beliefs, Int, [Goal, Plan], Action) :-
    split_goal_plan(Int, Goal, [Action_H|_]),
    not(applicable(Beliefs, Action_H)),
    update_plan_sell(Goal, Beliefs, [Action|Plan]).



% select_action_pick(Beliefs, Int, Int1, Action).
%
% get action from pick
%

% if the first action in pick is applicable
select_action_pick(Beliefs, Int, [Goal, Action_T], Action) :-
    split_goal_plan(Int, Goal, [Action|Action_T]),
    applicable(Beliefs, Action).

% if the first action in pick is not applicable.
select_action_pick(Beliefs, Int, [Goal, Plan], Action) :-
    split_goal_plan(Int, Goal, [Action_H|_]),
    not(applicable(Beliefs, Action_H)),
    update_plan_pick(Goal, Beliefs, [Action|Plan]).



% split_goal_plan([Goal|Plan], Goal, Plan)
%
% get the first goal of the plan
%
split_goal_plan([Goal|Plan], Goal, Plan).



% update_plan_sell(Goal, Beliefs, Plan)
%
% make the plan which takes agent to the restaurant and sell the truffles.
%

% start function
update_plan_sell(Goal, Beliefs, Plan) :-
    update_plan_sell(Goal, Beliefs, [], Plan).

% base case, when agent reach the restaurant.
update_plan_sell(goal(X, Y, _), beliefs(at(X, Y), _), Plan, Plan1) :-
    add_tail(Plan, [sell(X, Y)], Plan1).

% due to the agent current location, find the movement which takes agent closer to goal
% update the plan and the current location of agent
update_plan_sell(Goal, beliefs(at(X, Y), stock(T)), Plan, Plan1) :-
    move_choise(X, Y, move(X1, Y1)),
    closer(move(X1, Y1), Goal, at(X, Y)),
    add_tail(Plan, [move(X1, Y1)], Plan_New),
    update_plan_sell(Goal, beliefs(at(X1, Y1), stock(T)), Plan_New, Plan1).



% update_plan_pick(Goal, Beliefs, Plan)
%
% make the plan which takses agent to pick the truffles
%

% start function
update_plan_pick(Goal, Beliefs, Plan) :-
    update_plan_pick(Goal, Beliefs, [], Plan).

% base case, when agent reach the truffle.
update_plan_pick(goal(X, Y, _), beliefs(at(X, Y), _), Plan, Plan1) :-
    add_tail(Plan, [pick(X, Y)], Plan1).

% due to the agent current location, find the movement which takes agent closer to goal
% update the plan and the current location of agent
update_plan_pick(Goal, beliefs(at(X, Y), stock(T)), Plan, Plan1) :-
    move_choise(X, Y, move(X1, Y1)),
    closer(move(X1, Y1), Goal, at(X, Y)),
    add_tail(Plan, [move(X1, Y1)], Plan_New),
    update_plan_pick(Goal, beliefs(at(X1, Y1), stock(T)), Plan_New, Plan1).



% closer (move(X, Y), goal(X1, Y1, S), at(X2, Y2))
%
% select the movement which takes agent closer to the goal.
%
closer(move(X, Y), goal(X1, Y1, _), at(X2, Y2)) :-
    D1 is abs(X - X1) + abs(Y - Y1),
    D2 is abs(X1 - X2) + abs(Y1 - Y2),
    D1 < D2.



% move_choise(X, Y, Move).
%
% agent have 4 move choises depends on his current location.
%
move_choise(X, Y, M) :-
    X1 is X + 1, M = move(X1, Y);
    X1 is X - 1, M = move(X1, Y);
    Y1 is Y + 1, M = move(X, Y1);
    Y1 is Y - 1, M = move(X, Y1).



% add_tail(List, Element, New_List).
%
% helper function for adding element into the tail of list.
%

add_tail([X|Y],Z,[X|W]) :-
    add_tail(Y,Z,W).

add_tail([],X,X).




% update_intentions(Observation, Intentions, Intentions1):-

% -------------- Part Five ------------------------------------------------------------

% update_intentions(Observation, Intentions, Intentions1)

% if Observation is at(X, Y), do nothing.
update_intentions(at(_, _), Intentions, Intentions).

% if Observation is picked(X, Y, S), delete the corresponding plan in Int_pick.
update_intentions(picked(X, Y, S), intents(Int_sell, Int_pick), intents(Int_sell, Int_pick1)) :-
    delete_plan(picked(X, Y, S), Int_pick, Int_pick1).

% if Observation is sold(X, Y, S), delete the corresponding plan in Int_sell.
update_intentions(sold(X, Y, S), intents(Int_sell, Int_pick), intents(Int_sell1, Int_pick)) :-
    delete_plan(sold(X, Y, S), Int_sell, Int_sell1).

% delete_plan(Observation, Int, Int1)
% delete the corresponding plan in Int.

% delete plan in Int_sell.
delete_plan(sold(X, Y, _),  [[goal(X, Y, _)|_]|Int1], Int1).

% delete plan in Int_pick.
delete_plan(picked(X, Y, _),  [[goal(X, Y, _)|_]|Int1], Int1).

% incase no match, do nothing.
delete_plan(_, Int, Int).


