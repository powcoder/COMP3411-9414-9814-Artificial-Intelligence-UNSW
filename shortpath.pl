https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
:- initialization(writeln("Finding shortest path!")).
:- initialization(writeln('Usage: path((x1,y1),(x2,y2)).')).


% --------------------------------------------------- The execution starts here -----------------------------------------------------------------------------
 
land(1,1).
land(2,1).
land(3,1).
land(4,1).
land(5,1).
land(6,1).
land(1,2).
land(2,2).
land(3,2).
land(4,2).
land(5,2).
land(1,3).
land(2,3).
land(3,3).
land(4,3).
land(1,4).
land(2,4).
land(3,4).
land(1,5).
land(2,5).
land(1,6).

land(9,1).
land(8,2).
land(9,2).
land(7,3).
land(8,3).
land(5,4).
land(6,4).
land(7,4).
land(5,5).
land(6,5).
land(4,6).
land(5,6).
land(6,6).
land(3,7).
land(4,7).
land(2,8).
land(3,8).
land(1,9).
land(2,9).

land(9,6).
land(8,7).
land(9,7).
land(7,8).
land(8,8).
land(9,8).
land(6,9).
land(7,9).
land(8,9).

path(From, To) :-
	traverse(From),                   
	edge([To|ReversedPath], Dist)->   % If the destination is reachable, display an appropriate message.
	  reverse([To|ReversedPath], Path),      
	  Distance is Dist,
	  writef('Path = %w and the distance is %w = %w\n',
	       [Path, Dist, Distance]);
	writef('There is no path that connects %w with %w\n', [From, To]).
 
% -----------------------------------------------------------------------------------------------------------------------------------------------------------


% --------------------------------------------------- Traverse the nodes ------------------------------------------------------------------------------------
traverse(From) :-
	retractall(edge(_,_)),           % Remove all the facts edge(_,_)
	% From is ((1,1)), % begin with agent at (1,1) holding the stone
	traverse(From,[],0).  % begin to construct the path, starting from the origin node.
	
traverse(From, Path, Dist) :-		    % Traverse all the neighbors of node From.
	path(From, T, D),		    % For each neighbour T
	not(memberchk(T, Path)),	    %	check if T is in the Path, if node T hasnt been visited earlier (its not in the path)
	shorterPath([T,From|Path], Dist+D), %	Create a fact edge(Path, Dist) if that fact doesnt exist, or update it if it exists
	traverse(T,[From|Path],Dist+D).	    %	Do the same thing for node T, find all its neighbors...
	
traverse(_).
 
% -----------------------------------------------------------------------------------------------------------------------------------------------------------

% --------------------------------------------------- Finding the neighbors --------------------------------------------------------------------------------
 
path(From,To,Dist) :- neighbour(From,To,Dist).
 
% -----------------------------------------------------------------------------------------------------------------------------------------------------------	


% -------------------------------------Create or update (when the fact exists) facts edge(_,_). -------------------------------------------------------------
 
shorterPath([H|Path], Dist) :-		       % if there is a fact edge(H|_, D), this means that we know the distance from node From to node H.
	edge([H|_], D), !, Dist < D,          %  But if the distance D is greater than the new distance Dist that we discovered, 
	retract(edge([H|_],_)),				%  then we are going to remove (assert) the fact edge([H|_], D) and add a new one edge([H|Path], Dist).  
	assert(edge([H|Path], Dist)).		% This is the shortest path that we have found so far to reach the node H.
	
shorterPath(Path, Dist) :-		       % Lets create a new fact to memorize the path from the origin node From, to another node.
	assert(edge(Path,Dist)).			% Keep track the distance Dist of that path.
 
% -----------------------------------------------------------------------------------------------------------------------------------------------------------

	
% ------------------ Check if there is an edge that connects [(X1, Y1) (X2,Y2)] and verify if they both are within the allowed area -------------------------

checkCell((X1,Y1),(X2,Y2)) :- checkCell1(X1), checkCell1(Y1), checkCell1(X2), checkCell1(Y2).
checkCell1(X) :-     1 =< X, X =< 9.


% if there is a land location, just keep looping 
% if there is no land，this is a node to be visit, add 1 to path distance,and keep looping and follow ucs algrithom
% the following function is to check the location is land or not

% notNeighbour((X1,Y1),(X2,Y2)):- % move to the right, but (x2,y2) is a land,which is not the neighbour of (x1, y1).
%	X2 is X1+1,
%	Y2 is Y1,
%	isLand(X2, Y2).

checkNeiRight(not(land(X,Y))):- writef('Iamhere X is %w, Y is %w\n',[X,Y]),!.
checkNeiRight(X,Y):- % move to the right
	writef('Iamhereright X is %w, Y is %w\n',[X,Y]),
	land(X,Y),
	M is X+1,
	N is Y,
	writef(' Iamhereright M is %w, N is %w\n',[M,N]),
	checkNeiRight(M, N).
	
neighbour((X1,Y1),(X2,Y2), 1):- % move to the right and get the nearest water node
	write('Im here right\n'),
	Temp is X1,
	writef('%w is Temp, and is X1 \n', [Temp]),
	not(checkNeiRight(X1, Y1))->
	X2 is X1.
	writef(' Iamhereright X1 is %w, Y1 is %w\n',[X1,Y1]),
	X2 is X1,
	Y2 is Y1,
	writef('(%w, %w) and (%w, %w) are neighbour\n', [X1, Y1, X2, Y2]),
	checkCell((Temp,Y1),(X2,Y2)).

checkNeiLeft(not(land(X,Y))):- !.
checkNeiLeft(X,Y):- % move to the left
	writef(' Iamhereleft X is %w, Y is %w\n',[X,Y]),
	land(X,Y),
	M is X-1,
	N is Y,
	checkNeiLeft(M, N).

neighbour((X1,Y1),(X2,Y2), 1):- % move to the left and get the nearest water node
	write('Im here left\n'),
	Temp is X1,
	notcheckNeiLeft(X1, Y1),
	writef('heeeeeeeeeeeeeeeeeeeeeeeeeeeee %w %w',[X1,Y1]),
	X2 is X1,
	Y2 is Y1,
	writef('(%w, %w) and (%w, %w) are neighbour\n', [X1, Y1, X2, Y2]),
	checkCell((Temp,Y1),(X2,Y2)).

checkNeiUp(not(land(X,Y))):- !.
checkNeiUp(X,Y):- % move to up
	writef(' Iamhereup X is %w, Y is %w\n',[X,Y]),
	land(X,Y),
	M is X,
	N is Y+1,
	checkNeiUp(M, N).

neighbour((X1,Y1),(X2,Y2), 1):- % move up and get the nearest water node
	write('Im here up\n'),
	Temp is Y1,
	not(checkNeiUp(X1, Y1)),
	X2 is X1,
	Y2 is Y1,
	writef('(%w, %w) and (%w, %w) are neighbour\n', [X1, Y1, X2, Y2]),
	checkCell((X1,Temp),(X2,Y2)).

checkNeiDown(not(land(X,Y))):- !.
checkNeiDown(X,Y):- % move down
	land(X,Y),
	M is X,
	N is Y-1,
	checkNeiDown(M, N).

neighbour((X1,Y1),(X2,Y2), 1):- % move down and get the nearest water node
	writef(' Iamheredown X is %w, Y is %w\n',[X,Y]),
	Temp is Y1,
	checkNeiDown(X1, Y1),
	X2 is X1,
	Y2 is Y1,
	writef('(%w, %w) and (%w, %w) are neighbour\n', [X1, Y1, X2, Y2]),
	checkCell((X1,Temp),(X2,Y2)).


% neighbour((X1,Y1),(X2,Y2), 0) :- % move to the right and there is a land
%	X2 is X1+1, Y2 is Y1, 
%	(land(X2,Y2)), 
%	checkCell((X1,Y1),(X2,Y2)),
%	neighbour()

% neighbour((X1,Y1),(X2,Y2), 1) :- % move to the right and there is no land
%	X2 is X1+1, Y2 is Y1, 
%	not(land(X2,Y2)), 
%	checkCell((X1,Y1),(X2,Y2)). 

% neighbour((X1,Y1),(X2,Y2), 1) :- X2 is X1-1, Y2 is Y1, (land(X2,Y2)), checkCell((X1,Y1),(X2,Y2)). % move to the left and there is a land
% neighbour((X1,Y1),(X2,Y2), 1) :- X2 is X1-1, Y2 is Y1, (land(X2,Y2)), checkCell((X1,Y1),(X2,Y2)). % move to the left and there is no land

% neighbour((X1,Y1),(X2,Y2), 1) :- X2 is X1, Y2 is Y1-1, (land(X2,Y2)), checkCell((X1,Y1),(X2,Y2)). % move down 

% neighbour((X1,Y1),(X2,Y2), 1) :- X2 is X1, Y2 is Y1+1, (land(X2,Y2)), checkCell((X1,Y1),(X2,Y2)). % move up


% -----------------------------------------------------------------------------------------------------------------------------------------------------------


% Usage: path(startNode, endNode).