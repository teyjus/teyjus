sig string.
accum_sig blists.

% Find the character at the given index
type charAt string -> int -> string -> o.

% Determine if the first strings begins with the second string
type startsWith string -> string -> o.

% Determine if the first strings ends with the second string
type endsWith string -> string -> o.

% Find the index of the second string in the first string starting
% at the first int
type indexOf string -> string -> int -> int -> o.

% Replace all occurrences in the first string of the second
% string with the third string
type replaceAll string -> string -> string -> string -> o.

% Trim white space off the left, right, and both
type ltrim, rtrim, trim string -> string -> o.

% Split a string into a list of strings with the delimiters
% in the list
type split_str string -> (list string) -> (list string) -> o.

% Determine if the first string contains the second
type contains string -> string -> o.

% Combine members of a list of strings into a single string,
% adding the delimiter specified as the second string
type unsplit_str (list string) -> string -> string -> o.
