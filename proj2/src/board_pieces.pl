
ulc :- put_code(9487). % ┏ : upper left corner 
urc :- put_code(9491). % ┓ : upper right corner
llc :- put_code(9495). % ┗ : lower left corner
lrc :- put_code(9499). % ┛ : lower right corner

hdiv :- put_code(9473), put_code(9473), put_code(9473). % ━━ : horizontal division
vdiv :- put_code(9475). % ┃ : vertical division
mdiv :- put_code(9547). % ╋ : middle division


tr :- put_code(9507). % ┣ : T right
tl :- put_code(9515). % ┫ : T left
td :- put_code(9523). % ┳ : T down
tu :- put_code(9531). % ┻ : T up


%! display_element(+Element)
% Transforms the board element into the corresponding letter
% Prints empty space if the variable is empty
display_element(H) :- var(H), write('   ').
% Prints C in case of 1
display_element(1) :- write(' C ').
% Writes F in case of 2
display_element(2) :- write(' F ').
% Prints spaec if the element is 0
display_element(0) :- write('   ').