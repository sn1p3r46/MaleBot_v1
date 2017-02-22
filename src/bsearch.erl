-module(bsearch).

-export([search_word/2,get_word_list/0]).

search(_,[]) -> false;

search(_V,[_V|[]]) -> true;

search(_,[_ |[]]) -> false;

search (Val, Arr) ->

    L = length(Arr)+1,
    P = lists:nth(L div 2, Arr),

    case P of
        Val -> true;
        _ -> 
            case (Val>P) of 
                true  -> search(Val,lists:sublist(Arr,(L div 2)+1,L));
                false -> search(Val,lists:sublist(Arr,(L div 2)-1))
            end
    end.


read_cfile() ->

  {ok, IoDevice} = file:open("res/lista_badwords.txt", [read]),
  Words = read_words(IoDevice, []),
  file:close(IoDevice),
  Words.

read_words(IoDevice, Words) ->
  case file:read_line(IoDevice) of
    {ok, Line} ->
      V = string:tokens(Line,"\n"),
      read_words(IoDevice, Words ++ V);
    eof -> Words
  end.

string_array_to_num(Array) ->
    string_array_to_num(Array,0).

string_array_to_num([],N) -> round(N);

string_array_to_num([H|T],N) ->
    string_array_to_num(T,N*math:pow(10,round(math:log10(H)+1))+H).

get_word_list() ->
    lists:sort(lists:map(fun string_array_to_num/1 ,read_cfile())).

search_word(Word,SortedWList) ->
    search(string_array_to_num(Word),SortedWList).
