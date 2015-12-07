-module(office).
-export([room/4, student/2, officedemo/0, busyOffice/0, checkFront/2, putTail/2,indexOf/2]).

room(Students, Capacity, Queue,Helping) ->
  receive
    %check if student is at front of queue
    %If the queue is empty, student can be admitted automatically
    %If the student is at the front of the queue admit them and modify the queue list and the students list appropriately
    %If the student is not at the front of the queue send this:
    %From ! {self(), room_full, rand:uniform(5000)},
    % student entering, not at capacity
    {From, enter, Name} when Capacity > 0 ->
      case office:checkFront(Name,Queue) of
        %student is at front of queue or there is no queue
        true ->
          From ! {self(), ok},
          room([Name|Students], Capacity - 1,lists:delete(Name, Queue),Helping);
        %student is not at front of queue
        false->
          QueueWait = 1000 * office:indexOf(Name,[Queue]),
          From ! {self(), room_full, QueueWait},
          room([Students], Capacity, [Queue],Helping)
        end;


    % student entering, at capacity
    {From, enter, Name} ->
      case lists:member(Name,Queue) of
      %student is in the queue
        true ->
          QueueWait = 1000 * office:indexOf(Name,Queue),
          From ! {self(), ok, QueueWait},
          room([Students], Capacity, [Queue],Helping);
          %student is not in the queue
        false->
          %need to construct the queue appropriately with reverse
          NewQueue = office:putTail(Name,Queue),
          QueueWait = 1000 * office:indexOf(Name,NewQueue),
          From ! {self(), room_full, QueueWait},
          room([Students], Capacity, [NewQueue],Helping)
        end;
      %If the student is in the queue reply with message giving the sleep time for this student

      %if the student is not in the queue append the new student in the queue
      %must use reverse to append at end of list
      %modify wait time for each student based on their postion in the queue


    {From, help_me, Name} ->
      case Helping of
        true->
          From ! {self(), busy},
          room([Students],Capacity,[Queue],Helping);
        false->
          From ! {self(), ok},
          room([Students],Capacity,[Queue],true)
      end;

    % student leaving
    {From, leave, Name} ->
      % make sure they are already in the room
      case lists:member(Name, Students) of
        true ->
          From ! {self(), ok},
          room(lists:delete(Name, Students), Capacity + 1,[Queue],false);
        false ->
          From ! {self(), not_found},
          room([Students], Capacity,[Queue],false)
      end;

    % student thanks
    {From, thanks, Name} ->
      room([Students],Capacity,Queue,false)
end.

studentWork(Name) ->
  SleepTime = rand:uniform(5000) + 5000,
  io:format("~s entered the Office and will work for ~B ms.~n", [Name, SleepTime]),
  timer:sleep(SleepTime).

student(Office, Name) ->
  timer:sleep(rand:uniform(3000)),
  Office ! {self(), enter, Name},
  Office ! {self(), help_me, Name},
  receive
    % Success; can enter room.
    {_, ok} ->
      studentWork(Name),
      Office ! {self(), leave, Name},
      Office ! {self(), thanks, Name},
      io:format("~s left the Office.~n", [Name]);

  % Success; can enter room.
    {_, ok, SleepTime} ->
      studentWork(Name),
      Office ! {self(), thanks, Name},
      io:format("~s left the Office.~n", [Name]);

  % Instructor is busy
    {_, busy} ->
      office:busyOffice(),
      io:format("~s wanted help but the instructor was busy, asking again.~n", [Name]),
      Office ! {self(), help_me, Name},
      student(Office, Name);

    % Office is full; sleep and try again.
    {_, room_full, SleepTime} ->
      io:format("~s could not enter and must wait ~B ms.~n", [Name, SleepTime]),
      timer:sleep(SleepTime),
      student(Office, Name)
  end.

busyOffice() ->
  io:format("Instructor is busy.~n"),
  timer:sleep(1000).

checkFront(_,[]) -> true;
checkFront(Check,[H]) -> Check =:= H;
checkFront(Check,[H|_])-> Check =:= H.

putTail(Element, List) -> lists:reverse([Element |lists:reverse(List)]).

indexOf(_, []) -> notFound;
indexOf(Val,[Val|_]) -> 1;
indexOf(Val,[_|T]) -> 1+indexOf(Val,[T]).

officedemo() ->
  R = spawn(office, room, [[], 3,[],false]), % start the room process with an empty list of students
  spawn(office, student, [R, "Ada"]),
  spawn(office, student, [R, "Barbara"]),
  spawn(office, student, [R, "Charlie"]),
  spawn(office, student, [R, "Donald"]),
  spawn(office, student, [R, "Elaine"]),
  spawn(office, student, [R, "Frank"]),
  spawn(office, student, [R, "George"]).