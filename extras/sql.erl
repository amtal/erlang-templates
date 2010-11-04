-module(sql).

build_query(Tbl_,Cols_) -> 
    Tbl = atom_to_list(Tbl_),
    Cols = lists:map(fun atom_to_list/1, Cols_),
    lists:flatten([ "FROM ", Tbl, " SELECT "
                  , lists:nth(1,Cols)
                  , [[", ", C] || C<-lists:nthtail(1,Cols)]
                  ]).


%% Useful macro to define:
%% -define(FETCH(C,R), fetch(C,R, record_info(fields,R))).
-spec fetch(atom(),atom(),[atom()])->record().
fetch(Conn,Rec,Fields) ->
    case mysql:fetch(Conn,build_query(Rec,Fields)) of
        {error, Reason} -> {error, Reason};
        {Success, List} -> {Success, list_to_tuple([Rec|List])}
    end.
    
    
-record(employees,
    { id         = 0    :: integer()
    , name       = ""   :: string()
    , department = none :: atom()
    , pay        = 0    :: integer()
    }).

test()-> build_query(employees, record_info(fields,employees)).
