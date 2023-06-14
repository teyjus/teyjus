sig printing.

infixr ++ 10.
type ++ A -> B -> C.

%% Use (printtm X) to print a single term or string:
%%  - if X is a string, this is equivalent to (print X)
%%  - otherwise, this is equivalent to (printterm std_out X)
type printtm C -> o.
%% Use (prints (T1 ++ ... ++ Tn)) to print out a concatenation of terms and strings
type prints  C -> o.
%% Liek (prints XS) but with a newline at the end
type println C -> o.

%% Check if input is a string
%% Note: Use with care, this cannot be used in an environment in which
%% the type of T in (stringp T) is statically determined to be a string
%% (ie. in the same clause as print)
type stringp A -> o.
%% Check if input is a variable
%% Note: this will not succeed for eta-expanded variables (ie. x \ X x)
type var A -> o.