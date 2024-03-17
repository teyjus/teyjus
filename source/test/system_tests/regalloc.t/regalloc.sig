sig regalloc.

%% From issue #105
%% This seems like a register allocation issue.
%% After temp calls true_fact (which uses the A1 register),
%% the temporary variable Y1 (which stores the initial value of A1)
%% is not restored into A1.

% temp                switch_on_reg            #1, L5, L6
% L5                  try                      #0, L6
%                     trust_ext                #0, #1
%                     try_me_else              #0, L0
% L6                  allocate                 #1
%                     init_variable_t          A1, Y1
%                     call_name                #0, true_fact
%                     put_type_const           A3, int
%                     head_normalize_t         A1
%                     put_integer              A2, 1
%                     deallocate               
%                     builtin                  #3

%% We should have a
%%    put_value_p     Y1,A1
%% immediately before
%%    head_normalize_t   A1


type true_fact o.
type temp o.
type is_one int -> o.