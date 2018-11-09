/*
 * Project 2 for COMP30020 Declarative Programming
 * 
 * Author:  Wenqing Xue (wenqingx)
 * Origin:  October 2018
 * Purpose: The project is aimed to write Prolog code to solve the 
 *          fillin puzzles. Unfilled puzzle and list of words are
 *          provided at the beginning, and solved puzzle will match
 *          to every word in the list horizontally and veritically.
 *          main/3 takes the puzzle and word list file, and match
 *          with a solution file (if there are more than one results,
 *          only one solution will be found). Part of codes (except
 *          solve_puzzle/3 and related predicates) are provided on
 *          LMS. More information please check the specification.
 */

% Loads the library to ensure the transpose/2 is correct.
:- ensure_loaded(library(clpfd)).

% There are 5 steps executed in the main/3 predicate.
% 1. Reads the input PuzzleFile file into Puzzle.
% 2. Reads the input WordlistFile file into Wordlist.
% 3. Check if Puzzle is valid (each row is the same length).
% 4. Solves the Puzzle with Wordlist into the Solved puzzle.
% 5. Prints the Solved puzzle with SolutionFile file name.
main(PuzzleFile, WordlistFile, SolutionFile) :-
    read_file(PuzzleFile, Puzzle),
    read_file(WordlistFile, Wordlist),
    valid_puzzle(Puzzle),
    solve_puzzle(Puzzle, Wordlist, Solved),
    print_puzzle(SolutionFile, Solved).

% Opens the file by Filename as file name.
% Reads all of the lines into the Content.
read_file(Filename, Content) :-
    open(Filename, read, Stream),
    read_lines(Stream, Content),
    close(Stream).

% Reads all of the lines into the Content.
% Content contains a list of strings.
read_lines(Stream, Content) :-
    read_line(Stream, Line, Last),
    (   Last = true
    ->  (   Line = []
        ->  Content = []
        ;   Content = [Line]
        )
    ;   Content = [Line|Content1],
        read_lines(Stream, Content1)
    ).

% Reads all of the characters into the Line.
% When reaches the end of the file, sets Last = true.
read_line(Stream, Line, Last) :-
    get_char(Stream, Char),
    (   Char = end_of_file
    ->  Line = [],
        Last = true
    ; 	Char = '\n'
    ->  Line = [],
        Last = false
    ;   Line = [Char|Line1],
        read_line(Stream, Line1, Last)
    ).

% Opens the file by SolutionFile as file name.
% Prints each row in file, and close the file.
print_puzzle(SolutionFile, Puzzle) :-
    open(SolutionFile, write, Stream),
    maplist(print_row(Stream), Puzzle),
    close(Stream).

% Maps all the characters in row to the Stream.
print_row(Stream, Row) :-
    maplist(put_puzzle_char(Stream), Row),
    nl(Stream).

% Checks if a free variable, true then replace with underscore.
% Otherwise, just put the origin character into the Stream.
put_puzzle_char(Stream, Char) :-
    (   var(Char)
    ->  put_char(Stream, '_')
    ;   put_char(Stream, Char)
    ).

% Checks the input puzzle is valid if all rows are the same length.
% maplist/2: samelength(Row) can be applied to all elements in Rows.
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
    maplist(samelength(Row), Rows).

% Checks the two input lists are the same length.
% Base case: Two lists reach the end, so two empty lists.
% Main case: Traversals all the elements in two lists recursively.			 
samelength([], []).
samelength([_|L1], [_|L2]) :-
    same_length(L1, L2).


% -----------------------------------------------
% The implementation of project starts from here.
% -----------------------------------------------

% solve_puzzle(Puzzle0, WordList, Puzzle)
% should hold when Puzzle is a solved version of Puzzle0, with the
% empty slots filled in with words from WordList.  Puzzle0 and Puzzle
% should be lists of lists of characters (single-character atoms), one
% list per puzzle row.  WordList is also a list of lists of
% characters, one list per word.

% Solves the Puzzle0 and WordList into the Puzzle.
% It takes three distinct steps in main predicate.
% Base case: Puzzle0 is the same as Puzzle, and WordList is
%            empty, which is the final solution as expected.
% Main case: 1. Fill puzzle with the logical variables.
%            2. Get logical variables in Puzzle into Slots.
%            3. Insert all of the words into Slots correctly.
solve_puzzle(Puzzle, [], Puzzle) :- !.
solve_puzzle(Puzzle0, WordList, Puzzle) :-
    fill_puzzle_with_vars(Puzzle0, Puzzle),
    get_slots_from_puzzle(Puzzle, Slots),
    insert_words_to_slots(Slots, WordList).


% -------------------------------------------------------------
% Step 1: Fill Puzzle into FilledPuzzle with logical variables.
% -------------------------------------------------------------
% Hint 3: Constucts a list of slots with the logical variables.
% Fills the Puzzle with logical variables instead of underscore.
% Using maplist/3 to map all the rows in Puzzle into FilledPuzzle.
fill_puzzle_with_vars(Puzzle, FilledPuzzle) :-
    maplist(fill_row_with_vars, Puzzle, FilledPuzzle).

% Fills each Row with logical variables instead of underscore.
% Using maplist/3 to map all the elements in Row into FilledRow.
fill_row_with_vars(Row, FilledRow) :-
    maplist(replace_with_var, Row, FilledRow).

% Replaces the underscore symbol into the logical variables.
% If matches underscore, replace with the logical variables.
% Otherwise, makes sure the input character is not underscore.
replace_with_var('_', _).
replace_with_var(Char, Char) :- Char \= '_'.


% ---------------------------------------------------------------
% Step 2: Get the logical variables from FilledPuzzle into Slots.
% ---------------------------------------------------------------
get_slots_from_puzzle(FilledPuzzle, Slots) :-
    % Gets the horizontal slots from FilledPuzzle.
    get_slots_from_rows(FilledPuzzle, HSlots),
    % Hint 2: Avoid to handling filling slots vertically.
    % Transposes the Puzzle to handle vertical slots.
    transpose(FilledPuzzle, TransposedPuzzle),
    % Gets the horizontal (originally vertical) slots.
    get_slots_from_rows(TransposedPuzzle, VSlots),
    % Appends two lists into the total list TSlots.
    append(HSlots, VSlots, TSlots),
    % Filters the length restriction for each slots.
    % Slots would be the filtered final return list.
    filter_length(TSlots, Slots).

% Gets all of the slots from all of the rows in Puzzle.
% Base case: Reaches the end of list, which becomes empty.
% Main case: Gets the slots from the first row, and uses
%            recursion to find the slots for next rows,
%            and append them altogether to become the
%            complete list of slots called Slots.
get_slots_from_rows([], []).
get_slots_from_rows([R|Rs], Slots) :-
    % Gets the slots from row R into RSlots.
    get_slots_from_row(R, RSlots),
    % Appends the slots from rows altogether.
    append(RSlots, NSlots, Slots),
    % Recursively gets the slots from others.
    get_slots_from_rows(Rs, NSlots).

% Gets all of the slots from the single row from rows.
% Uses tail recursion with empty list as accumulator.
get_slots_from_row(Row, Slots) :-
    get_slots_from_row(Row, [], Slots).

% Gets all of the slots from the single row from rows.
% Base case: The list of row is empty, accumulator is 
%            the list of slots in this row.
% Main case: Checks that the element is the hash symbol,
%            which means the current slot is finished.
%            Therefore, the accumulator needs to set as
%            the empty list. Otherwise (not hash symbol),
%            just simply add the element into accumulator.
%            Slots is the list of accumulator Acc, which
%            matches the base case reversely.
get_slots_from_row([], Acc, [Acc]).
get_slots_from_row([E|Es], Acc, Slots) :-
    % Checks if that the element is the hash symbol.
    ( 	E == '#' 
    ->	% Adds the complete slot Acc into Slots.
        Slots = [Acc|NewSlots],
        % Sets the accumulator into empty list.
        get_slots_from_row(Es, [], NewSlots)
    ;	% Otherwise, adds element into NewAcc.
        append(Acc, [E], NewAcc),
        % Executes the recursion to get next element.
        get_slots_from_row(Es, NewAcc, Slots)
    ).


% From specification, it mentioned clearly that
% "more than one square long must have one word"

% Filters the length of each element in the list,
% Keeps element if the length is greater than one,
% Returns a new list under the length restriction.
filter_length([], []).
filter_length([X|Xs], Rs):-
    length(X, Len),
    % Check if that length is greater than one.
    (   Len > 1
    ->  % Appends the X at the begining.
        Rs = [X|R]
    ;   % Skips if the X is not satisfied
        Rs = R
    ),
    % Executes the recursion to the next.
    filter_length(Xs, R).


% ----------------------------------------------------------
% Step 3: Insert the matching words into Slots until filled.
% ----------------------------------------------------------
% Inserts all of the words from WordList into Slots.
% Base case: Insertion is done, both lists are empty.
% Main case: Each recursion finds the most appropriate slot,
%            finds a list of unifiable words, assign value,
%            and delete both of them in the corresponding
%            list, and repeat recursively until both lists
%            becomes to the empty list, which means both
%            Slots and WordList matches each other correctly.
insert_words_to_slots([], []).
insert_words_to_slots(Slots, WordList) :-
    % Gets the next most appropriate slot.
    get_next_slot(Slots, WordList, Slot),
    % From the selected Slot, run through WordList.
    % Finds all candidates that unifiable with Slot.
    get_unifiable(Slot, WordList, WordsToTry),
    % Get the word from the list of WordsToTry.
    member(Word, WordsToTry),
    % Since Word is the member of WordsToTry,
    % Slot can be unified and assigned by Word.
    Slot = Word,
    % Once the operation above can be satisfied,
    % Delete Word from WordList to be NewWordList.
    delete_element(Word, WordList, NewWordList),
    % Delete Slot from Slots to be NewSlots.
    delete_element(Slot, Slots, NewSlots),
    % Recursively execute the next round until empty.
    insert_words_to_slots(NewSlots, NewWordList).

% Hint 6: Places the word with the minimum group of length.
% Gets the most appropriate slot with the Slots and WordList given.
% It will recursively checks each memeber in the Slots with counting,
% And it returns the slot with the minimum count of candidates.
get_next_slot([Slot|Slots], WordList, BestSlot) :-
    % Counts the unifiable candidates from WordList.
    count_slot_candidates(Slot, WordList, Count),
    % Takes the Slot and Count into the tail recursion.
    % It traversals the list of slots, and compares each other,
    % To find the most appropriate slot from list of slots.
    get_next_slot(Slots, WordList, BestSlot, Slot, Count).

% Uses the tail recursion to get the most appropriate slot.
% Base case: Slots reaches the end, BestSlot will be assigned
%            with the value of temporary best slot TemSlot.
% Main case: Recursively gets the count number of the unifiable
%            candidates for each slot, and updates the minimum
%            count if the smaller count is founded, and updates
%            the corresponding temporary best slot which will
%            become the best slot when the recursion completes.
get_next_slot([], _, BestSlot, BestSlot, _).
get_next_slot([Slot|Slots], WordList, BestSlot, TemSlot, MinCount) :-
    % Counts the unifiable candidates from WordList.
    count_slot_candidates(Slot, WordList, Count),
    % Checks if the current count is smaller than minimum count.
    (	Count < MinCount 
    ->	% Replaces the minimum count with the current count.
        NewMinCount = Count,
        % Replaces the temporary best slot with the current slot.
        NewTemSlot = Slot
    ;  	% Keeps the minimum count for the next recursion/
        NewMinCount = MinCount,
        % Keeps the temporary best slot for the next recursion.
        NewTemSlot = TemSlot
    ),
    % Executes the recursion with the new modified arguments.
    get_next_slot(Slots, WordList, BestSlot, NewTemSlot, NewMinCount).


% Uses the tail recursion to count the candidates for each slot.
% It counts candidates in the WordList which can be unifiable.
count_slot_candidates(Slot, WordList, Count) :-
    count_slot_candidates(Slot, WordList, 0, Count).

% Base case: WordList reaches the end, which means the recursion
%            already traversaled the whole WordList. Therefore,
%            Count is assigned by the value of accumulator.
% Main case: Traversals the WordList one by one, if the element
%            is unifiable with the Slot, accumulator adds one.
%            Otherwise, moves to the next word for execution.
count_slot_candidates(_, [], Acc, Acc).
count_slot_candidates(Slot, [Word|Words], Acc, Count) :-
    % Checks if that Slot and Word can unify
    (	unifiable(Slot, Word, _)
    ->	% Accumulator adds 1 if condition satisfies
        NewAcc is Acc + 1
    ;   NewAcc is Acc
    ), 
    % Executes the recursion to the next.
    count_slot_candidates(Slot, Words, NewAcc, Count).


% Inputs with the given slot and a list of words,
% Returns a list of words can be unifiable by slot.
get_unifiable(_, [], []).
get_unifiable(T, [X|Xs], Rs) :-
    % Checks if that T and X can unify
    (   unifiable(T, X, _)
    ->  % Appends the X at the begining.
        Rs = [X|R]
    ;   % Skips if X is not satisfied
        Rs = R
    ),
    % Executes the recursion to the next.
    get_unifiable(T, Xs, R).


% Inputs with the given element and list,
% Returns a new list without any element inside.
% delete_element(Element, InputList, ReturnList).
delete_element(E, [E|Es], Es).
delete_element(E, [X|Xs], [X|Ys]) :-
    delete_element(E, Xs, Ys).
